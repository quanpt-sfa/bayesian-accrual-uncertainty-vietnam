# -----------------------------------------------------------------------------
# Script: 14_sensitivity_prior_predictive.R
# Purpose: Prior predictive gate for the full-refit sensitivity scenarios.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("se01", "Sensitivity prior predictive")
ensure_analysis_dirs()
ensure_sensitivity_dirs()
write_method_design_files()
write_prior_registry()

dry_run <- env_flag("ACCRUAL_DRY_RUN", "TRUE")
allow_prior_fail <- env_flag("ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL", "FALSE")
prior_cfg <- accrual_sampler_config("prior_predictive")
n_draws <- prior_cfg$iter
options(mc.cores = prior_cfg$cores)

scenarios <- selected_sensitivity_scenarios()
formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) stop("[BLOCKER] Missing winsor formula table: ", formulas_path)
formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)

truthy <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  toupper(as.character(x)) %in% c("TRUE", "1", "YES", "Y")
}

eligible_formulas <- formulas_df %>%
  filter(Sample_Group == "main_common") %>%
  filter(vapply(Main_Stack_Inclusion, truthy, logical(1))) %>%
  filter(mapply(function(space, id) id %in% main_model_ids_for_space(space), Target_Space, Model_ID)) %>%
  distinct(Model_ID, Model_Name, Target_Space, Sample_Group, Heterogeneity_Variant, Target_Sample, brms_Formula, .keep_all = TRUE) %>%
  arrange(Target_Space, Model_ID, Heterogeneity_Variant)

if (nrow(eligible_formulas) == 0) stop("[BLOCKER] No eligible main-stack formulas for sensitivity prior predictive checks.")

classify_prior_pp <- function(vals, observed) {
  prior_q <- stats::quantile(vals, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE, type = 7)
  obs_q <- stats::quantile(observed, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE, type = 7)
  out <- classify_chapter3_prior_predictive(
    share_gt_1 = mean(abs(vals) > 1, na.rm = TRUE),
    share_gt_2 = mean(abs(vals) > 2, na.rm = TRUE),
    prior_p01 = prior_q[[1]],
    prior_p99 = prior_q[[2]],
    observed_p01 = obs_q[[1]],
    observed_p99 = obs_q[[2]]
  )
  c(out$status, out$reason, as.character(out$range_ratio))
}

scenario_rows <- list()
gate_rows <- list()

if (!dry_run && !requireNamespace("brms", quietly = TRUE)) {
  stop("[BLOCKER] brms is required for non-dry-run prior predictive sensitivity.")
}

for (sidx in seq_len(nrow(scenarios))) {
  sc <- scenarios[sidx, ]
  scenario <- sc$Scenario
  scenario_root <- sensitivity_root(scenario)
  ensure_sensitivity_dirs(scenario)

  model_list <- unique(paste(eligible_formulas$Target_Space, eligible_formulas$Model_ID, sep = ":"))
  write_run_manifest(
    file.path(scenario_root, "manifests", "prior_predictive_manifest.csv"),
    scenario = scenario,
    prior_set_id = sc$Prior_Set_ID,
    family = sc$Likelihood_Family,
    model_structure = sc$Model_Structure,
    model_list = model_list,
    seed = accrual_seed_for(paste0("sensitivity_prior_predictive_manifest_", scenario), offset = sidx),
    sampling_config = sprintf("sample_prior=only; draws=%d; chains=%d; cores=%d; iter=%d; warmup=%d; refresh=%d; dry_run=%s",
                              n_draws, prior_cfg$chains, prior_cfg$cores, prior_cfg$iter,
                              prior_cfg$warmup, prior_cfg$refresh, dry_run),
    status = if (dry_run) "DRY_RUN_PLANNED" else "STARTED",
    notes = "Prior predictive gate for sensitivity full-refit scenarios.",
    input_paths = c(formulas_path),
    rng_context = paste0("sensitivity_prior_predictive_manifest_", scenario),
    rng_offset = sidx
  )

  for (i in seq_len(nrow(eligible_formulas))) {
    row <- eligible_formulas[i, ]
    model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, paste0("_", scenario, "_priorpred"))
    out_fit <- file.path(scenario_root, "prior_predictive", paste0("fit_", model_key, ".rds"))

    if (dry_run) {
      scenario_rows[[length(scenario_rows) + 1]] <- data.frame(
        scenario = scenario,
        model_id = row$Model_ID,
        model_name = row$Model_Name,
        target_space = row$Target_Space,
        family = sc$Likelihood_Family,
        prior_set_id = sc$Prior_Set_ID,
        model_structure = sc$Model_Structure,
        p_abs_gt_1 = NA_real_,
        p_abs_gt_2 = NA_real_,
        yrep_mean = NA_real_,
        yrep_sd = NA_real_,
        status = "NOT_RUN_DRY_RUN",
        reason = "ACCRUAL_DRY_RUN=TRUE; no brms prior predictive sampling executed.",
        output_path = out_fit,
        stringsAsFactors = FALSE
      )
      next
    }

    df_scaled <- read_winsor_sample(row$Target_Sample)
    observed <- df_scaled$TA_scaled
    formula_str <- fix_formula(row$brms_Formula)
    prior_list <- default_prior_list(
      row$Heterogeneity_Variant,
      model_structure = sc$Model_Structure,
      prior_set_id = sc$Prior_Set_ID,
      family = sc$Likelihood_Family
    )

    fit <- tryCatch({
      message(
        "brms/rstan sampler controls: chains=", prior_cfg$chains,
        ", cores=", prior_cfg$cores,
        ", iter=", prior_cfg$iter,
        ", warmup=", prior_cfg$warmup,
        ", refresh=", prior_cfg$refresh
      )
      brms::brm(
        formula = brms::bf(as.formula(formula_str)),
        data = df_scaled,
        family = brms_family(sc$Likelihood_Family),
        prior = prior_list,
        sample_prior = "only",
        chains = prior_cfg$chains,
        cores = prior_cfg$cores,
        iter = prior_cfg$iter,
        warmup = prior_cfg$warmup,
        seed = accrual_seed_for(
          paste0("sensitivity_prior_predictive_", scenario, "_", row$Target_Space, "_", row$Model_ID),
          offset = sidx * 1000L + i
        ),
        refresh = prior_cfg$refresh
      )
    }, error = function(e) {
      e
    })

    if (inherits(fit, "error")) {
      scenario_rows[[length(scenario_rows) + 1]] <- data.frame(
        scenario = scenario,
        model_id = row$Model_ID,
        model_name = row$Model_Name,
        target_space = row$Target_Space,
        family = sc$Likelihood_Family,
        prior_set_id = sc$Prior_Set_ID,
        model_structure = sc$Model_Structure,
        p_abs_gt_1 = NA_real_,
        p_abs_gt_2 = NA_real_,
        yrep_mean = NA_real_,
        yrep_sd = NA_real_,
        status = "FAIL",
        reason = paste("brms prior predictive failed:", fit$message),
        output_path = out_fit,
        stringsAsFactors = FALSE
      )
      next
    }

    saveRDS(fit, out_fit)
    yrep <- brms::posterior_predict(fit, ndraws = n_draws)
    vals <- as.numeric(yrep)
    pp <- classify_prior_pp(vals, observed)

    scenario_rows[[length(scenario_rows) + 1]] <- data.frame(
      scenario = scenario,
      model_id = row$Model_ID,
      model_name = row$Model_Name,
      target_space = row$Target_Space,
      family = sc$Likelihood_Family,
      prior_set_id = sc$Prior_Set_ID,
      model_structure = sc$Model_Structure,
      p_abs_gt_1 = mean(abs(vals) > 1, na.rm = TRUE),
      p_abs_gt_2 = mean(abs(vals) > 2, na.rm = TRUE),
      yrep_mean = mean(vals, na.rm = TRUE),
      yrep_sd = sd(vals, na.rm = TRUE),
      range_ratio_to_observed = as.numeric(pp[3]),
      status = pp[1],
      reason = pp[2],
      output_path = out_fit,
      stringsAsFactors = FALSE
    )
  }

  sc_rows <- bind_rows(scenario_rows) %>% filter(scenario == !!scenario)
  write.csv(sc_rows, file.path(scenario_root, "prior_predictive", paste0("table_sensitivity_prior_predictive_", scenario, ".csv")), row.names = FALSE)

  gate_status <- if (dry_run) {
    "DRY_RUN_NOT_EVALUATED"
  } else if (any(sc_rows$status == "FAIL", na.rm = TRUE)) {
    if (allow_prior_fail) "FAIL_OVERRIDDEN" else "BLOCKED_FAIL"
  } else if (any(sc_rows$status == "REVIEW", na.rm = TRUE)) {
    "PASS_WITH_REVIEW"
  } else {
    "PASS"
  }
  gate_rows[[length(gate_rows) + 1]] <- data.frame(
    scenario = scenario,
    prior_set_id = sc$Prior_Set_ID,
    family = sc$Likelihood_Family,
    model_structure = sc$Model_Structure,
    gate_status = gate_status,
    proceed_to_refit = gate_status %in% c("PASS", "PASS_WITH_REVIEW", "FAIL_OVERRIDDEN"),
    allow_prior_predictive_fail = allow_prior_fail,
    dry_run = dry_run,
    stringsAsFactors = FALSE
  )
}

summary_df <- bind_rows(scenario_rows)
gate_df <- bind_rows(gate_rows)

sens_tables <- file.path(sensitivity_root(), "tables")
write.csv(summary_df, file.path(sens_tables, "sensitivity_prior_predictive_summary.csv"), row.names = FALSE)
write.csv(gate_df, file.path(sens_tables, "sensitivity_prior_predictive_gate.csv"), row.names = FALSE)

writeLines(c(
  "Sensitivity prior predictive gate",
  sprintf("Dry run: %s", dry_run),
  sprintf("Scenarios: %s", paste(scenarios$Scenario, collapse = ", ")),
  sprintf("Allow prior predictive fail override: %s", allow_prior_fail),
  "Chapter 3 PASS thresholds: share |TA_scaled| > 1 <= 0.05; share |TA_scaled| > 2 <= 0.01; prior predictive p01-p99 range <= 3 times observed p01-p99 range.",
  "Scenario FAIL blocks full refit unless ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE."
), file.path(sensitivity_root(), "logs", "sensitivity_prior_predictive_notes.txt"))

if (!dry_run && any(gate_df$gate_status == "BLOCKED_FAIL", na.rm = TRUE)) {
  stop("[BLOCKER] One or more sensitivity scenarios failed prior predictive checks. Set ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE only for an intentional diagnostic override.")
}

cat("\n[SUCCESS] Sensitivity prior predictive gate completed.\n")
phase_end("se01", "Sensitivity prior predictive")
