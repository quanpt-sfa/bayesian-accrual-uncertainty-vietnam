# -----------------------------------------------------------------------------
# Script: 17_sensitivity_stacking.R
# Purpose: Recompute LOO stacking weights separately for each sensitivity scenario.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/00_helpers.R")
ensure_analysis_dirs()
ensure_sensitivity_dirs()
validate_final_analysis_config("sensitivity stacking", final_mode = TRUE)

dry_run <- env_flag("ACCRUAL_DRY_RUN", "TRUE")
validation_engine <- tolower(env_value("ACCRUAL_VALIDATION_ENGINE", "row_loo"))
moment_match <- env_flag("ACCRUAL_SENS_LOO_MOMENT_MATCH", "FALSE")
if (!validation_engine %in% c("row_loo", "firm_lofo", "grouped_kfold")) {
  stop("[BLOCKER] ACCRUAL_VALIDATION_ENGINE must be row_loo, firm_lofo, or grouped_kfold.")
}

scenarios <- selected_sensitivity_scenarios()
diag_path <- file.path(sensitivity_root(), "tables", "sensitivity_mcmc_diagnostics_summary.csv")
if (!file.exists(diag_path)) stop("[BLOCKER] Missing sensitivity MCMC diagnostics. Run script 16 first.")
diag_df <- read.csv(diag_path, stringsAsFactors = FALSE)

if (!dry_run && (!requireNamespace("brms", quietly = TRUE) || !requireNamespace("loo", quietly = TRUE))) {
  stop("[BLOCKER] brms and loo are required for non-dry-run sensitivity stacking.")
}

weight_rows <- list()
comparison_rows <- list()

normalize_target_space <- function(x) {
  out <- tolower(as.character(x))
  out[out %in% c("real_time", "no_lookahead")] <- "real_time"
  out
}

resolve_sensitivity_kfold_base_root <- function(scenario) {
  explicit_root <- trimws(env_value("ACCRUAL_SENS_KFOLD_ROOT", ""))
  if (nzchar(explicit_root)) return(explicit_root)
  file.path(sensitivity_root(scenario), "kfold_firm")
}

resolve_sensitivity_kfold_run_root <- function(scenario) {
  base_root <- resolve_sensitivity_kfold_base_root(scenario)
  baseline_root <- normalizePath(file.path(output_root, "kfold_firm"), winslash = "/", mustWork = FALSE)
  base_root_norm <- normalizePath(base_root, winslash = "/", mustWork = FALSE)

  if (identical(base_root_norm, baseline_root)) {
    stop("[BLOCKER] grouped_kfold for sensitivity stacking cannot reuse baseline K-fold outputs. Generate scenario-specific exact K-fold outputs under '",
         file.path(sensitivity_root(scenario), "kfold_firm"),
         "' or set ACCRUAL_SENS_KFOLD_ROOT to a scenario-specific K-fold root.")
  }
  if (!dir.exists(base_root)) {
    stop("[BLOCKER] Missing scenario-specific grouped K-fold root for scenario '", scenario, "': ", base_root,
         ". Run exact grouped K-fold for that sensitivity scenario first.")
  }

  latest_run_path <- file.path(base_root, "LATEST_RUN.txt")
  if (file.exists(latest_run_path)) {
    latest_run <- trimws(readLines(latest_run_path, warn = FALSE, n = 1))
    if (!nzchar(latest_run) || !dir.exists(latest_run)) {
      stop("[BLOCKER] Invalid LATEST_RUN.txt under scenario-specific grouped K-fold root: ", latest_run_path)
    }
    return(latest_run)
  }

  candidate_scores <- list.files(base_root,
                                 pattern = "table_winsor_kfold_model_scores\\.csv$",
                                 recursive = TRUE,
                                 full.names = TRUE)
  if (length(candidate_scores) == 0) {
    stop("[BLOCKER] No scenario-specific grouped K-fold model scores found under: ", base_root)
  }
  if (length(candidate_scores) > 1) {
    stop("[BLOCKER] Multiple grouped K-fold runs found under scenario-specific root without LATEST_RUN.txt: ",
         base_root,
         ". Add LATEST_RUN.txt or set ACCRUAL_SENS_KFOLD_ROOT to one exact run root.")
  }

  dirname(dirname(candidate_scores[[1]]))
}

for (sidx in seq_len(nrow(scenarios))) {
  scenario <- scenarios$Scenario[sidx]
  scenario_root <- sensitivity_root(scenario)
  ensure_sensitivity_dirs(scenario)
  rows <- diag_df %>% filter(scenario == !!scenario)
  if (!dry_run) rows <- rows %>% filter(stacking_allowed == TRUE)
  if (nrow(rows) > 0) {
    rows <- rows %>%
      filter(mapply(function(space, id) id %in% main_model_ids_for_space(space), target_space, model_id)) %>%
      arrange(target_space, model_id, heterogeneity_variant)
  }

  model_list <- unique(paste(rows$target_space, rows$model_id, sep = ":"))
  write_run_manifest(
    file.path(scenario_root, "manifests", "stacking_manifest.csv"),
    scenario = scenario,
    prior_set_id = scenarios$Prior_Set_ID[sidx],
    family = scenarios$Likelihood_Family[sidx],
    model_structure = scenarios$Model_Structure[sidx],
    model_list = model_list,
    seed = as.integer(env_value("ACCRUAL_SENS_SEED", "20260614")),
    sampling_config = sprintf("validation_engine=%s; moment_match=%s; dry_run=%s", validation_engine, moment_match, dry_run),
    status = if (dry_run) "DRY_RUN_PLANNED" else "STARTED",
    notes = "Stacking weights are computed from scenario posterior fits only.",
    input_paths = c(diag_path)
  )

  if (dry_run) {
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, ]
      weight_rows[[length(weight_rows) + 1]] <- data.frame(
        scenario = scenario,
        validation_engine = validation_engine,
        target_space = row$target_space,
        sample_group = row$sample_group,
        model_id = row$model_id,
        model_name = row$model_name,
        heterogeneity_variant = row$heterogeneity_variant,
        elpd = NA_real_,
        se_elpd = NA_real_,
        stacking_weight = NA_real_,
        diagnostics_status = row$diagnostics_status,
        pareto_k_flags = "NOT_RUN_DRY_RUN",
        primary_evidence_usable = FALSE,
        fit_path = row$fit_path,
        notes = "Dry run only.",
        stringsAsFactors = FALSE
      )
    }
    next
  }

  for (space in c("ex_post", "real_time")) {
    space_rows <- rows %>% filter(target_space == space)
    if (nrow(space_rows) < 2) stop("[BLOCKER] Fewer than two diagnostics-PASS models for scenario ", scenario, " / ", space)

    loo_list <- list()
    meta <- list()

    if (validation_engine == "row_loo") {
      for (i in seq_len(nrow(space_rows))) {
        row <- space_rows[i, ]
        key <- model_key_sampled(row$model_id, row$target_space, row$sample_group, row$heterogeneity_variant, paste0("_", scenario, "_winsor"))
        cache_path <- file.path(scenario_root, "cache", paste0(key, "_row_loo.rds"))
        if (file.exists(cache_path)) {
          loo_obj <- readRDS(cache_path)
        } else {
          fit <- readRDS(row$fit_path)
          loo_obj <- loo::loo(fit, cores = 1)
          if (moment_match && any(loo_obj$diagnostics$pareto_k > 0.7, na.rm = TRUE)) {
            loo_obj <- loo::loo(fit, moment_match = TRUE, cores = 1)
          }
          saveRDS(loo_obj, cache_path)
        }
        loo_list[[key]] <- loo_obj
        meta[[key]] <- row
      }

      weights <- as.numeric(loo::loo_model_weights(loo_list, method = "stacking"))
      if (abs(sum(weights) - 1) > 1e-5) stop("[BLOCKER] Stacking weights do not sum to one for ", scenario, " / ", space)
      keys <- names(loo_list)
      for (kidx in seq_along(keys)) {
        key <- keys[kidx]
        row <- meta[[key]]
        loo_obj <- loo_list[[key]]
        pareto_high <- sum(loo_obj$diagnostics$pareto_k > 0.7, na.rm = TRUE)
        pareto_very_high <- sum(loo_obj$diagnostics$pareto_k > 1.0, na.rm = TRUE)

        weight_rows[[length(weight_rows) + 1]] <- data.frame(
          scenario = scenario,
          validation_engine = validation_engine,
          target_space = row$target_space,
          sample_group = row$sample_group,
          model_id = row$model_id,
          model_name = row$model_name,
          heterogeneity_variant = row$heterogeneity_variant,
          elpd = loo_obj$estimates["elpd_loo", "Estimate"],
          se_elpd = loo_obj$estimates["elpd_loo", "SE"],
          stacking_weight = weights[kidx],
          diagnostics_status = row$diagnostics_status,
          pareto_k_flags = sprintf("k_gt_0_7=%d;k_gt_1=%d", pareto_high, pareto_very_high),
          primary_evidence_usable = pareto_high == 0,
          fit_path = row$fit_path,
          notes = "Row-level PSIS-LOO direct calculation.",
          stringsAsFactors = FALSE
        )
      }
    } else if (validation_engine == "firm_lofo") {
      aggregate_log_lik_by_firm <- function(ll_obs, firm_ids, firm_levels) {
        if (length(dim(ll_obs)) != 2) stop("log_lik output is not a draws x observations matrix.")
        if (ncol(ll_obs) != length(firm_ids)) {
          stop(sprintf("log_lik observation count mismatch: ncol=%d, sample rows=%d.", ncol(ll_obs), length(firm_ids)))
        }
        ll_firm <- sapply(firm_levels, function(f) {
          rowSums(ll_obs[, firm_ids == f, drop = FALSE])
        })
        if (is.null(dim(ll_firm))) ll_firm <- matrix(ll_firm, ncol = length(firm_levels))
        colnames(ll_firm) <- firm_levels
        ll_firm
      }

      sample_file <- if (space == "ex_post") "final_common_ex_post_sample_winsor.csv" else "final_common_realtime_sample_winsor.csv"
      sample_df <- read_winsor_sample(sample_file)
      firm_ids <- sample_df$company
      firm_levels <- unique(firm_ids)

      for (i in seq_len(nrow(space_rows))) {
        row <- space_rows[i, ]
        key <- model_key_sampled(row$model_id, row$target_space, row$sample_group, row$heterogeneity_variant, paste0("_", scenario, "_winsor"))
        cache_path <- file.path(scenario_root, "cache", paste0(key, "_firm_lofo.rds"))
        if (file.exists(cache_path)) {
          loo_obj <- readRDS(cache_path)
        } else {
          fit <- readRDS(row$fit_path)
          ll_obs <- brms::log_lik(fit, re_formula = NA)
          ll_firm <- aggregate_log_lik_by_firm(ll_obs, firm_ids, firm_levels)
          loo_obj <- loo::loo(ll_firm, cores = 1)
          saveRDS(loo_obj, cache_path)
        }
        loo_list[[key]] <- loo_obj
        meta[[key]] <- row
      }

      weights <- as.numeric(loo::loo_model_weights(loo_list, method = "stacking"))
      if (abs(sum(weights) - 1) > 1e-5) stop("[BLOCKER] Stacking weights do not sum to one for ", scenario, " / ", space)
      keys <- names(loo_list)
      for (kidx in seq_along(keys)) {
        key <- keys[kidx]
        row <- meta[[key]]
        loo_obj <- loo_list[[key]]
        pareto_high <- sum(loo_obj$diagnostics$pareto_k > 0.7, na.rm = TRUE)
        pareto_very_high <- sum(loo_obj$diagnostics$pareto_k > 1.0, na.rm = TRUE)

        weight_rows[[length(weight_rows) + 1]] <- data.frame(
          scenario = scenario,
          validation_engine = validation_engine,
          target_space = row$target_space,
          sample_group = row$sample_group,
          model_id = row$model_id,
          model_name = row$model_name,
          heterogeneity_variant = row$heterogeneity_variant,
          elpd = loo_obj$estimates["elpd_loo", "Estimate"],
          se_elpd = loo_obj$estimates["elpd_loo", "SE"],
          stacking_weight = weights[kidx],
          diagnostics_status = row$diagnostics_status,
          pareto_k_flags = sprintf("k_gt_0_7=%d;k_gt_1=%d", pareto_high, pareto_very_high),
          primary_evidence_usable = pareto_high == 0,
          fit_path = row$fit_path,
          notes = "Firm-level LOFO direct calculation.",
          stringsAsFactors = FALSE
        )
      }
    } else if (validation_engine == "grouped_kfold") {
      kfold_run_root <- resolve_sensitivity_kfold_run_root(scenario)
      manifest_path <- file.path(kfold_run_root, "logs", "run_config_manifest.csv")
      if (file.exists(manifest_path)) {
        manifest_df <- read.csv(manifest_path, stringsAsFactors = FALSE)
        if (nrow(manifest_df) > 0 && "Prior_Set_ID" %in% names(manifest_df)) {
          expected_prior <- scenarios$Prior_Set_ID[sidx]
          actual_prior <- manifest_df$Prior_Set_ID[1]
          if (!identical(as.character(actual_prior), as.character(expected_prior))) {
            stop("[BLOCKER] Scenario-specific grouped K-fold run prior mismatch for scenario '", scenario,
                 "': expected Prior_Set_ID='", expected_prior,
                 "' but found '", actual_prior,
                 "' in ", manifest_path)
          }
        }
      }

      model_scores_path <- file.path(kfold_run_root, "tables", "table_winsor_kfold_model_scores.csv")
      if (!file.exists(model_scores_path)) {
        stop("[BLOCKER] Missing scenario-specific grouped K-fold model scores: ", model_scores_path)
      }
      model_scores_df <- read.csv(model_scores_path, stringsAsFactors = FALSE)
      if (!all(c("Target_Space", "Model_ID", "Heterogeneity_Variant", "elpd_kfold", "se_elpd_fold") %in% names(model_scores_df))) {
        stop("[BLOCKER] Scenario-specific grouped K-fold model scores have unexpected schema: ", model_scores_path)
      }
      model_scores_df$Target_Space_Normalized <- normalize_target_space(model_scores_df$Target_Space)

      kfold_weights_file <- if (space == "ex_post") {
        "table_winsor_kfold_weights_ex_post.csv"
      } else {
        "table_winsor_kfold_weights_no_lookahead.csv"
      }

      weights_path <- file.path(kfold_run_root, "tables", kfold_weights_file)
      if (!file.exists(weights_path)) {
        stop("[BLOCKER] Missing scenario-specific grouped K-fold weights: ", weights_path)
      }
      weights_df_kf <- read.csv(weights_path, stringsAsFactors = FALSE)
      if (!all(c("Target_Space", "Model_ID", "Heterogeneity_Variant", "Weight_KFold") %in% names(weights_df_kf))) {
        stop("[BLOCKER] Scenario-specific grouped K-fold weights have unexpected schema: ", weights_path)
      }
      weights_df_kf$Target_Space_Normalized <- normalize_target_space(weights_df_kf$Target_Space)

      for (i in seq_len(nrow(space_rows))) {
        row <- space_rows[i, ]
        match_space <- normalize_target_space(space)
        score_row <- model_scores_df %>%
          filter(Target_Space_Normalized == match_space,
                 Model_ID == row$model_id,
                 Heterogeneity_Variant == row$heterogeneity_variant)

        if (nrow(score_row) == 0) {
          stop("[BLOCKER] Scenario-specific grouped K-fold scores missing for scenario '", scenario,
               "', space '", space,
               "', model '", row$model_id,
               "', variant '", row$heterogeneity_variant,
               "'.")
        }

        elpd_kf <- score_row$elpd_kfold[1]
        se_elpd_kf <- score_row$se_elpd_fold[1]

        weight_row <- weights_df_kf %>%
          filter(Target_Space_Normalized == match_space,
                 Model_ID == row$model_id,
                 Heterogeneity_Variant == row$heterogeneity_variant)
        if (nrow(weight_row) == 0) {
          stop("[BLOCKER] Scenario-specific grouped K-fold weight missing for scenario '", scenario,
               "', space '", space,
               "', model '", row$model_id,
               "', variant '", row$heterogeneity_variant,
               "'.")
        }
        w_kf <- weight_row$Weight_KFold[1]

        weight_rows[[length(weight_rows) + 1]] <- data.frame(
          scenario = scenario,
          validation_engine = validation_engine,
          target_space = row$target_space,
          sample_group = row$sample_group,
          model_id = row$model_id,
          model_name = row$model_name,
          heterogeneity_variant = row$heterogeneity_variant,
          elpd = elpd_kf,
          se_elpd = se_elpd_kf,
          stacking_weight = w_kf,
          diagnostics_status = row$diagnostics_status,
          pareto_k_flags = "NOT_APPLICABLE_KFOLD",
          primary_evidence_usable = TRUE,
          fit_path = row$fit_path,
          notes = "Grouped K-fold results loaded from exact run.",
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

weights_df <- bind_rows(weight_rows)
tables_root <- file.path(sensitivity_root(), "tables")
write.csv(weights_df, file.path(tables_root, "sensitivity_stacking_weights_by_scenario.csv"), row.names = FALSE)

if (nrow(weights_df) > 0 && any(is.finite(weights_df$stacking_weight))) {
  ranked <- weights_df %>%
    filter(is.finite(stacking_weight)) %>%
    group_by(scenario, target_space) %>%
    arrange(desc(stacking_weight), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  top <- ranked %>% filter(rank <= 5)
  baseline <- ranked %>%
    filter(scenario == "baseline") %>%
    select(target_space, model_id, heterogeneity_variant, baseline_weight = stacking_weight, baseline_rank = rank)
  comparison <- ranked %>%
    left_join(baseline, by = c("target_space", "model_id", "heterogeneity_variant")) %>%
    mutate(
      weight_change_vs_baseline = stacking_weight - baseline_weight,
      rank_change_vs_baseline = rank - baseline_rank,
      absolute_weight_change = abs(weight_change_vs_baseline),
      rank_shift = rank_change_vs_baseline
    ) %>%
    filter(rank <= 5 | !is.na(baseline_rank) & baseline_rank <= 5) %>%
    arrange(target_space, scenario, rank)
  write.csv(top, file.path(tables_root, "sensitivity_top_models_by_scenario.csv"), row.names = FALSE)
  write.csv(comparison, file.path(tables_root, "sensitivity_top_models_comparison.csv"), row.names = FALSE)
  comparison_rows[[1]] <- comparison
} else {
  write.csv(data.frame(), file.path(tables_root, "sensitivity_top_models_by_scenario.csv"), row.names = FALSE)
  write.csv(data.frame(), file.path(tables_root, "sensitivity_top_models_comparison.csv"), row.names = FALSE)
}

for (scenario in unique(weights_df$scenario)) {
  write.csv(weights_df[weights_df$scenario == scenario, , drop = FALSE],
            file.path(sensitivity_root(scenario), "stacking", paste0("table_sensitivity_stacking_weights_", scenario, ".csv")),
            row.names = FALSE)
}

writeLines(c(
  "Sensitivity stacking notes",
  sprintf("Dry run: %s", dry_run),
  sprintf("Validation engine: %s", validation_engine),
  "Weights are recomputed by scenario from scenario posterior fits. Baseline posterior draws are not reused.",
  "Row-level PSIS-LOO is direct in this script. Firm LOFO/grouped K-fold require scenario-specific grouped runs and never receive placeholder weights."
), file.path(sensitivity_root(), "logs", "sensitivity_stacking_notes.txt"))

cat("\n[SUCCESS] Sensitivity stacking completed.\n")
