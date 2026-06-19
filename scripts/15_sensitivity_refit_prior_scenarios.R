# -----------------------------------------------------------------------------
# Script: 15_sensitivity_refit_prior_scenarios.R
# Purpose: Full MCMC refits for baseline/tight/wide prior sensitivity scenarios.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/00_helpers.R")
ensure_analysis_dirs()
ensure_sensitivity_dirs()
validate_final_analysis_config("sensitivity full refit", final_mode = TRUE)

dry_run <- env_flag("ACCRUAL_DRY_RUN", "TRUE")
force_refit <- env_flag("ACCRUAL_FORCE_REFIT", "FALSE")
include_secondary <- env_flag("ACCRUAL_SENS_INCLUDE_SECONDARY", "FALSE")
sampler_cfg <- accrual_sampler_config("sensitivity")
chains <- sampler_cfg$chains
iter <- sampler_cfg$iter
warmup <- sampler_cfg$warmup
adapt_delta <- sampler_cfg$adapt_delta
max_treedepth <- sampler_cfg$max_treedepth

scenarios <- selected_sensitivity_scenarios()
formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
gate_path <- file.path(sensitivity_root(), "tables", "sensitivity_prior_predictive_gate.csv")
if (!file.exists(formulas_path)) stop("[BLOCKER] Missing winsor formula table: ", formulas_path)
if (!dry_run && !file.exists(gate_path)) stop("[BLOCKER] Missing sensitivity prior predictive gate. Run script 14 first.")

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
gate_df <- if (file.exists(gate_path)) read.csv(gate_path, stringsAsFactors = FALSE) else data.frame()

truthy <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  toupper(as.character(x)) %in% c("TRUE", "1", "YES", "Y")
}

eligible_formulas <- formulas_df %>%
  filter(Sample_Group == "main_common") %>%
  filter(vapply(Main_Stack_Inclusion, truthy, logical(1))) %>%
  filter(mapply(function(space, id) id %in% main_model_ids_for_space(space), Target_Space, Model_ID))

if (include_secondary) {
  secondary <- formulas_df %>%
    filter(vapply(Secondary_Robustness, truthy, logical(1))) %>%
    filter(Model_ID %in% c("M08", "M10"))
  eligible_formulas <- bind_rows(eligible_formulas, secondary)
}

eligible_formulas <- eligible_formulas %>%
  distinct(Model_ID, Model_Name, Target_Space, Sample_Group, Heterogeneity_Variant, Target_Sample, brms_Formula, .keep_all = TRUE) %>%
  arrange(Target_Space, Model_ID, Heterogeneity_Variant)

if (nrow(eligible_formulas) == 0) stop("[BLOCKER] No eligible formulas for sensitivity refit.")
if (!dry_run && !requireNamespace("brms", quietly = TRUE)) stop("[BLOCKER] brms is required for non-dry-run sensitivity refits.")

sampling_config <- sprintf("chains=%d; iter=%d; warmup=%d; adapt_delta=%.3f; max_treedepth=%d; seed=%d; dry_run=%s",
                           chains, iter, warmup, adapt_delta, max_treedepth, accrual_seed("sensitivity"), dry_run)
plan_rows <- list()
diag_rows <- list()

gate_allows <- function(scenario) {
  if (dry_run) return(TRUE)
  rows <- gate_df[gate_df$scenario == scenario, , drop = FALSE]
  if (nrow(rows) == 0) return(FALSE)
  isTRUE(rows$proceed_to_refit[1]) || identical(as.character(rows$proceed_to_refit[1]), "TRUE")
}

write_metadata <- function(path, expected) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(as.data.frame(expected, stringsAsFactors = FALSE), path, row.names = FALSE)
}

audit_summary_rows <- list()

for (sidx in seq_len(nrow(scenarios))) {
  sc <- scenarios[sidx, ]
  scenario <- sc$Scenario
  scenario_root <- sensitivity_root(scenario)
  ensure_sensitivity_dirs(scenario)
  proceed <- gate_allows(scenario)
  model_list <- unique(paste(eligible_formulas$Target_Space, eligible_formulas$Model_ID, sep = ":"))

  write_run_manifest(
    file.path(scenario_root, "manifests", "refit_manifest.csv"),
    scenario = scenario,
    prior_set_id = sc$Prior_Set_ID,
    family = sc$Likelihood_Family,
    model_structure = sc$Model_Structure,
    model_list = model_list,
    seed = seed,
    sampling_config = sampling_config,
    status = if (dry_run) "DRY_RUN_PLANNED" else if (proceed) "STARTED" else "BLOCKED_BY_PRIOR_PREDICTIVE_GATE",
    notes = "Sensitivity refit never reuses baseline posterior draws; existing scenario fits are reused only if metadata matches.",
    input_paths = c(formulas_path, gate_path)
  )

  for (i in seq_len(nrow(eligible_formulas))) {
    row <- eligible_formulas[i, ]
    model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, paste0("_", scenario, "_winsor"))
    fit_path <- file.path(scenario_root, "fits", paste0("fit_", model_key, ".rds"))
    draws_path <- file.path(scenario_root, "draws", paste0("draws_", model_key, ".rds"))
    meta_path <- file.path(scenario_root, "fits", paste0("fit_", model_key, "_metadata.csv"))
    expected_meta <- data.frame(
      scenario = scenario,
      prior_set_id = sc$Prior_Set_ID,
      family = sc$Likelihood_Family,
      model_structure = sc$Model_Structure,
      model_id = row$Model_ID,
      model_name = row$Model_Name,
      target_space = row$Target_Space,
      sample_group = row$Sample_Group,
      heterogeneity_variant = row$Heterogeneity_Variant,
      target_sample = row$Target_Sample,
      brms_formula = row$brms_Formula,
      chains = chains,
      iter = iter,
      warmup = warmup,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth,
      seed = accrual_seed("sensitivity"),
      save_pars_all = TRUE,
      stringsAsFactors = FALSE
    )

    existing_match <- file.exists(fit_path) && file.exists(meta_path) && metadata_matches(meta_path, expected_meta)
    plan_rows[[length(plan_rows) + 1]] <- cbind(expected_meta, data.frame(
      fit_path = fit_path,
      draws_path = draws_path,
      dry_run = dry_run,
      prior_predictive_gate_allows_refit = proceed,
      existing_fit_metadata_matches = existing_match,
      action = if (dry_run) "PLAN_ONLY" else if (!proceed) "BLOCKED_BY_PRIOR_PREDICTIVE_GATE" else if (existing_match && !force_refit) "SKIP_EXISTING_MATCHED_FIT" else "REFIT",
      stringsAsFactors = FALSE
    ))

    if (dry_run) next

    if (!proceed) {
      audit_summary_rows[[length(audit_summary_rows) + 1]] <- data.frame(
        scenario = scenario,
        model_id = row$Model_ID,
        status = "SKIPPED",
        warning_count = 0,
        error_message = "Blocked by prior predictive gate",
        elapsed_seconds = 0,
        fit_path = fit_path,
        metadata_path = meta_path,
        reused_existing_fit = FALSE,
        metadata_match = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }

    if (file.exists(fit_path) && !existing_match && !force_refit) {
      stop("[BLOCKER] Existing sensitivity fit metadata does not match requested configuration: ", fit_path,
           ". Set ACCRUAL_FORCE_REFIT=TRUE only if overwrite is intentional.")
    }

    if (existing_match && !force_refit) {
      audit_summary_rows[[length(audit_summary_rows) + 1]] <- data.frame(
        scenario = scenario,
        model_id = row$Model_ID,
        status = "SKIPPED",
        warning_count = 0,
        error_message = "",
        elapsed_seconds = 0,
        fit_path = fit_path,
        metadata_path = meta_path,
        reused_existing_fit = TRUE,
        metadata_match = TRUE,
        stringsAsFactors = FALSE
      )
      next
    }

    df_scaled <- tryCatch(read_winsor_sample(row$Target_Sample), error = function(e) e)
    if (inherits(df_scaled, "error")) {
      diag_rows[[length(diag_rows) + 1]] <- cbind(expected_meta, data.frame(Fit_Status = "FAILED", error_message = df_scaled$message, stringsAsFactors = FALSE))

      audit_summary_rows[[length(audit_summary_rows) + 1]] <- data.frame(
        scenario = scenario,
        model_id = row$Model_ID,
        status = "ERROR",
        warning_count = 0,
        error_message = df_scaled$message,
        elapsed_seconds = 0,
        fit_path = fit_path,
        metadata_path = meta_path,
        reused_existing_fit = FALSE,
        metadata_match = TRUE,
        stringsAsFactors = FALSE
      )
      next
    }

    formula_str <- fix_formula(row$brms_Formula)
    prior_list <- default_prior_list(
      row$Heterogeneity_Variant,
      model_structure = sc$Model_Structure,
      prior_set_id = sc$Prior_Set_ID,
      family = sc$Likelihood_Family
    )

    captured_warnings <- character()
    captured_messages <- character()

    start_time <- Sys.time()
    fit <- tryCatch({
      withCallingHandlers(
        {
          brms::brm(
            formula = brms::bf(as.formula(formula_str)),
            data = df_scaled,
            family = brms_family(sc$Likelihood_Family),
            prior = prior_list,
            chains = chains,
            iter = iter,
            warmup = warmup,
            control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
            seed = seed,
            save_pars = brms::save_pars(all = TRUE),
            refresh = 500
          )
        },
        warning = function(w) {
          captured_warnings <<- c(captured_warnings, w$message)
          if (exists("muffleWarning", mode = "function")) {
            invokeRestart("muffleWarning")
          }
        },
        message = function(m) {
          captured_messages <<- c(captured_messages, m$message)
        }
      )
    }, error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    # Log individual fit audit
    log_dir <- file.path(scenario_root, row$Model_ID)
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    log_file_path <- file.path(log_dir, "refit_log.txt")

    log_lines <- c(
      paste0("Scenario: ", scenario),
      paste0("Model ID: ", row$Model_ID),
      paste0("Seed: ", seed),
      paste0("Sampling Config: ", sampling_config),
      paste0("Status: ", if (inherits(fit, "error")) "ERROR" else if (length(captured_warnings) > 0) "WARNING" else "SUCCESS"),
      paste0("Elapsed seconds: ", elapsed),
      "",
      "=== ERROR MESSAGE ===",
      if (inherits(fit, "error")) fit$message else "None",
      "",
      "=== WARNINGS ===",
      if (length(captured_warnings) > 0) captured_warnings else "None",
      "",
      "=== MESSAGES ===",
      if (length(captured_messages) > 0) captured_messages else "None"
    )
    writeLines(log_lines, log_file_path)

    if (inherits(fit, "error")) {
      diag_rows[[length(diag_rows) + 1]] <- cbind(expected_meta, data.frame(Fit_Status = "FAILED", elapsed_seconds = elapsed, error_message = fit$message, stringsAsFactors = FALSE))

      audit_summary_rows[[length(audit_summary_rows) + 1]] <- data.frame(
        scenario = scenario,
        model_id = row$Model_ID,
        status = "ERROR",
        warning_count = 0,
        error_message = fit$message,
        elapsed_seconds = elapsed,
        fit_path = fit_path,
        metadata_path = meta_path,
        reused_existing_fit = FALSE,
        metadata_match = TRUE,
        stringsAsFactors = FALSE
      )
      next
    }

    saveRDS(fit, fit_path)
    write_metadata(meta_path, expected_meta)
    ep_draws <- brms::posterior_epred(fit)
    pp_draws <- brms::posterior_predict(fit)
    saveRDS(list(epred = ep_draws, predict = pp_draws), draws_path)

    diag_rows[[length(diag_rows) + 1]] <- cbind(expected_meta, data.frame(
      Fit_Status = "SUCCESS",
      N_Obs = brms::nobs(fit),
      elapsed_seconds = elapsed,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    ))

    audit_summary_rows[[length(audit_summary_rows) + 1]] <- data.frame(
      scenario = scenario,
      model_id = row$Model_ID,
      status = if (length(captured_warnings) > 0) "WARNING" else "SUCCESS",
      warning_count = length(captured_warnings),
      error_message = "",
      elapsed_seconds = elapsed,
      fit_path = fit_path,
      metadata_path = meta_path,
      reused_existing_fit = FALSE,
      metadata_match = TRUE,
      stringsAsFactors = FALSE
    )
  }
}

plan_df <- bind_rows(plan_rows)
diag_df <- bind_rows(diag_rows)
audit_summary_df <- bind_rows(audit_summary_rows)

tables_root <- file.path(sensitivity_root(), "tables")
write.csv(plan_df, file.path(tables_root, "sensitivity_refit_plan.csv"), row.names = FALSE)
write.csv(diag_df, file.path(tables_root, "sensitivity_refit_fit_status.csv"), row.names = FALSE)
write.csv(audit_summary_df, file.path(tables_root, "sensitivity_refit_audit_summary.csv"), row.names = FALSE)

# Make sure it is also written in output root tables
dir.create(file.path(output_root, "tables"), recursive = TRUE, showWarnings = FALSE)
write.csv(audit_summary_df, file.path(output_root, "tables", "sensitivity_refit_audit_summary.csv"), row.names = FALSE)

writeLines(c(
  "Sensitivity full-refit notes",
  sprintf("Dry run: %s", dry_run),
  sprintf("Force refit: %s", force_refit),
  sprintf("Include secondary robustness models M08/M10: %s", include_secondary),
  sprintf("Sampling config: %s", sampling_config),
  "No posterior baseline draws are reused. Existing scenario fits require metadata match before skip/resume."
), file.path(sensitivity_root(), "logs", "sensitivity_refit_notes.txt"))

cat("\n[SUCCESS] Sensitivity refit phase completed.\n")
