# -----------------------------------------------------------------------------
# Script: ma07b_collect_brms_fit_outputs.R
# Purpose: Collect ma07a fit artifacts into diagnostics, draws, audit, and tables.
#          This script does not fit models.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(brms)
  library(loo)
})

source("scripts/ma00_setup.R")
phase_begin("ma07b", "Collect baseline brms fit outputs")
ensure_analysis_dirs()
validate_final_analysis_config("ma07b baseline fit collection", final_mode = TRUE)

backfill_diagnostics_only <- env_flag("ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY", "FALSE")
remediation_targets_raw <- trimws(env_value("ACCRUAL_MCMC_REMEDIATION_TARGETS", ""))
remediation_targets <- if (nzchar(remediation_targets_raw)) trimws(unlist(strsplit(remediation_targets_raw, ";", fixed = TRUE))) else character()
ma07_strict_review_blocker <- env_flag("ACCRUAL_MA07_STRICT_REVIEW_BLOCKER", "FALSE")
run_varying_slope_models <- identical(model_structure, "breuer_varying_slopes")
phase_root <- if (run_varying_slope_models) varyslopes_root else output_root
for (d in file.path(phase_root, c("", "tables", "models", "draws", "logs", "figures", "manifests"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

task_manifest_path <- file.path(phase_root, "tables", "table_ma07_fit_task_manifest.csv")
task_status_path <- file.path(phase_root, "tables", "table_ma07_fit_task_status.csv")
if (!file.exists(task_manifest_path)) stop("[BLOCKER] Missing ma07a task manifest: ", task_manifest_path)
if (!file.exists(task_status_path)) stop("[BLOCKER] Missing ma07a task status table: ", task_status_path)
task_manifest <- read.csv(task_manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
task_status <- read.csv(task_status_path, stringsAsFactors = FALSE, check.names = FALSE)

formulas_path <- file.path(phase_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) stop("[BLOCKER] Missing formula table for ma07b collection: ", formulas_path)
formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE, check.names = FALSE)
formula_out_cols <- intersect(
  c("Model_ID", "Model_Name", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
    "Target_Sample", "brms_Formula", "Main_Stack_Inclusion", "Secondary_Robustness", "Reason"),
  names(task_manifest)
)
write.csv(task_manifest[, formula_out_cols, drop = FALSE],
          file.path(phase_root, "tables", "table_named_model_formulas_winsor.csv"),
          row.names = FALSE)
if (run_varying_slope_models) {
  write.csv(task_manifest[, formula_out_cols, drop = FALSE],
            file.path(phase_root, "tables", "table_varyslopes_model_registry.csv"),
            row.names = FALSE)
}

diag_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_varyslopes_diagnostics.csv")
} else {
  file.path(phase_root, "tables", "table_brms_diagnostics_winsor.csv")
}
coeff_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_varyslopes_coefficient_summary.csv")
} else {
  file.path(phase_root, "tables", "table_coefficient_summary_winsor.csv")
}
ma07_artifact_audit_path <- file.path(phase_root, "tables", "table_ma07_fit_draw_artifact_audit.csv")
ma07_hard_gate_failures_path <- file.path(phase_root, "tables", "table_ma07_hard_gate_failures.csv")
ma07_remediation_helper_path <- file.path(phase_root, "logs", "ma07_suggested_remediation_targets.ps1")

diagnostic_key_for_row <- function(row) {
  paste(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, sep = "|")
}

load_step7_sample_info <- function(row) {
  df_scaled <- read_winsor_sample(row$Target_Sample)
  company_values <- trimws(as.character(df_scaled$company))
  company_values[company_values == ""] <- NA_character_
  list(df_scaled = df_scaled, n_obs_fit = nrow(df_scaled), n_firms_fit = length(unique(company_values[!is.na(company_values)])))
}

classify_ma07_mcmc_gate <- function(max_rhat, divergences, min_bulk_ess, min_tail_ess) {
  divergences <- suppressWarnings(as.numeric(divergences))
  if (!is.finite(max_rhat) || max_rhat > 1.01) return("FAIL")
  if (!is.finite(divergences) || divergences > 0) return("FAIL")
  if (!is.finite(min_bulk_ess) || min_bulk_ess < 400) return("FAIL")
  if (!is.finite(min_tail_ess) || min_tail_ess < 400) return("FAIL")
  if (min_bulk_ess < 1000 || min_tail_ess < 1000) return("REVIEW")
  "PASS"
}

ma07_mcmc_gate_reason <- function(max_rhat, divergences, min_bulk_ess, min_tail_ess) {
  divergences <- suppressWarnings(as.numeric(divergences))
  reasons <- c(
    if (!is.finite(max_rhat)) "max_rhat is non-finite" else if (max_rhat > 1.01) sprintf("max_rhat %.6f > 1.01", max_rhat),
    if (!is.finite(divergences)) "divergences is non-finite" else if (divergences > 0) sprintf("divergences=%d", as.integer(divergences)),
    if (!is.finite(min_bulk_ess)) "min_bulk_ess is non-finite" else if (min_bulk_ess < 400) sprintf("min_bulk_ess %.2f < 400", min_bulk_ess),
    if (!is.finite(min_tail_ess)) "min_tail_ess is non-finite" else if (min_tail_ess < 400) sprintf("min_tail_ess %.2f < 400", min_tail_ess),
    if (is.finite(min_bulk_ess) && min_bulk_ess >= 400 && min_bulk_ess < 1000) sprintf("min_bulk_ess %.2f below strict marker 1000", min_bulk_ess),
    if (is.finite(min_tail_ess) && min_tail_ess >= 400 && min_tail_ess < 1000) sprintf("min_tail_ess %.2f below strict marker 1000", min_tail_ess)
  )
  reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
  if (!length(reasons)) "MCMC diagnostics passed ma07 hard gate." else paste(reasons, collapse = "; ")
}

classify_loo_result <- function(fit) {
  loo_res <- tryCatch(loo::loo(fit), error = function(e) NULL)
  if (is.null(loo_res)) {
    return(list(pareto_k_above_07 = NA_integer_, elpd_loo = NA_real_,
                loo_status = "LOO_FAILED", loo_warning_reason = "loo() failed; elpd_loo unavailable"))
  }
  pareto <- sum(loo_res$diagnostics$pareto_k > 0.7)
  list(
    pareto_k_above_07 = pareto,
    elpd_loo = loo_res$estimates["elpd_loo", "Estimate"],
    loo_status = if (pareto > 0) "PSIS_REVIEW_REQUIRED" else "PSIS_OK",
    loo_warning_reason = if (pareto > 0) "pareto_k_above_07 > 0; consider reloo, moment matching, or grouped K-fold before relying on PSIS-LOO" else NA_character_
  )
}

empty_artifact_audit <- function() {
  data.frame(
    Model_ID = character(), Model_Name = character(), Target_Space = character(), Sample_Group = character(),
    Heterogeneity_Variant = character(), diagnostic_key = character(), Main_Stack_Inclusion = logical(),
    Secondary_Robustness = logical(), fit_path = character(), draws_path = character(),
    fit_exists_before = logical(), draws_exists_before = logical(), fit_exists_after = logical(),
    draws_exists_after = logical(), Fit_Status = character(), max_rhat = numeric(), divergences = numeric(),
    treedepth_warnings = numeric(), min_bulk_ess = numeric(), min_tail_ess = numeric(), converged = logical(),
    stacking_eligible = logical(), draw_generation_attempted = logical(), draw_generation_status = character(),
    draw_generation_skip_reason = character(), hard_gate_status = character(), hard_gate_reason = character(),
    recommended_remediation_key = character(), timestamp = character(), stringsAsFactors = FALSE
  )
}

empty_hard_gate_failures <- function() {
  data.frame(
    diagnostic_key = character(),
    Model_ID = character(),
    Model_Name = character(),
    Target_Space = character(),
    Sample_Group = character(),
    Heterogeneity_Variant = character(),
    Main_Stack_Inclusion = logical(),
    hard_gate_status = character(),
    hard_gate_reason = character(),
    max_rhat = numeric(),
    divergences = numeric(),
    min_bulk_ess = numeric(),
    min_tail_ess = numeric(),
    fit_path = character(),
    draws_path = character(),
    fit_exists_after = logical(),
    draws_exists_after = logical(),
    recommended_remediation_key = character(),
    timestamp = character(),
    stringsAsFactors = FALSE
  )
}

diagnostics_rows <- list()
audit_rows <- list()
failure_rows <- list()
coeff_rows <- list()

for (i in seq_len(nrow(task_manifest))) {
  task <- task_manifest[i, ]
  key <- task$task_key
  status_row <- task_status[task_status$task_key == key, , drop = FALSE]
  fit_path <- task$fit_path
  draws_path <- task$draw_path
  row <- task
  sample_info <- tryCatch(load_step7_sample_info(row), error = function(e) NULL)
  n_obs_fit <- if (is.null(sample_info)) NA_integer_ else sample_info$n_obs_fit
  n_firms_fit <- if (is.null(sample_info)) NA_integer_ else sample_info$n_firms_fit

  if (!file.exists(fit_path)) {
    if (task$Main_Stack_Inclusion %in% TRUE) stop("[BLOCKER] Required ma07 fit artifact is missing: ", fit_path)
    diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
      Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group, Main_Stack_Inclusion = task$Main_Stack_Inclusion,
      Secondary_Robustness = task$Secondary_Robustness, Heterogeneity_Variant = task$Heterogeneity_Variant,
      N_Obs = n_obs_fit, N_Firms = n_firms_fit, Fit_Status = "FAILED", Rhat_Max = NA_real_,
      ESS_Min = NA_real_, Min_Tail_ESS = NA_real_, Divergences = NA_integer_, converged = FALSE,
      stacking_eligible = FALSE, max_rhat = NA_real_, divergences = NA_integer_, treedepth_warnings = NA_integer_,
      pareto_k_above_07 = NA_integer_, loo_status = "LOO_FAILED", loo_warning_reason = "fit artifact missing",
      random_intercept_sd = NA_real_, elpd_loo = NA_real_,
      error_message = if (nrow(status_row)) status_row$error_message[1] else "fit artifact missing",
      Notes = task$Reason, Prior_Set_ID = prior_set_id, Likelihood_Family = likelihood_family,
      Model_Structure = model_structure, Output_Root = phase_root, save_pars_all = FALSE,
      stringsAsFactors = FALSE
    )
    next
  }

  fit <- readRDS(fit_path)
  post_summary <- summary(fit)
  rhats <- post_summary$fixed[, "Rhat"]
  bulk_esses <- post_summary$fixed[, "Bulk_ESS"]
  tail_esses <- if ("Tail_ESS" %in% colnames(post_summary$fixed)) post_summary$fixed[, "Tail_ESS"] else rep(NA_real_, length(bulk_esses))
  if ("random" %in% names(post_summary) && !is.null(post_summary$random)) {
    for (group in names(post_summary$random)) {
      rhats <- c(rhats, post_summary$random[[group]][, "Rhat"])
      bulk_esses <- c(bulk_esses, post_summary$random[[group]][, "Bulk_ESS"])
      tail_esses <- c(tail_esses, if ("Tail_ESS" %in% colnames(post_summary$random[[group]])) post_summary$random[[group]][, "Tail_ESS"] else rep(NA_real_, nrow(post_summary$random[[group]])))
    }
  }
  max_rhat <- suppressWarnings(max(rhats, na.rm = TRUE)); if (!is.finite(max_rhat)) max_rhat <- NA_real_
  min_ess <- suppressWarnings(min(bulk_esses, na.rm = TRUE)); if (!is.finite(min_ess)) min_ess <- NA_real_
  min_tail_ess <- suppressWarnings(min(tail_esses, na.rm = TRUE)); if (!is.finite(min_tail_ess)) min_tail_ess <- NA_real_
  np <- brms::nuts_params(fit)
  divergences <- sum(subset(np, Parameter == "divergent__")$Value)
  treedepths <- subset(np, Parameter == "treedepth__")$Value
  treedepth_warnings <- sum(treedepths >= task$max_treedepth)
  hard_gate_status <- classify_ma07_mcmc_gate(max_rhat, divergences, min_ess, min_tail_ess)
  hard_gate_reason <- ma07_mcmc_gate_reason(max_rhat, divergences, min_ess, min_tail_ess)
  converged <- hard_gate_status %in% c("PASS", "REVIEW")
  stacking_eligible <- converged
  random_intercept_sd <- NA_real_
  if (grepl("Firm RE", task$Heterogeneity_Variant) && !is.null(post_summary$random$company)) {
    random_intercept_sd <- post_summary$random$company["sd(Intercept)", "Estimate"]
  }
  loo_diag <- classify_loo_result(fit)

  blocker_for_mcmc <- isTRUE(task$Main_Stack_Inclusion) &&
    (identical(hard_gate_status, "FAIL") || (identical(hard_gate_status, "REVIEW") && isTRUE(ma07_strict_review_blocker)))

  draw_generation_attempted <- FALSE
  draw_generation_status <- "SKIPPED"
  draw_generation_skip_reason <- if (file.exists(draws_path)) "existing draws already present" else "draw generation not required by current mode"
  regenerate_draws <- !backfill_diagnostics_only && stacking_eligible &&
    (key %in% remediation_targets || !file.exists(draws_path) || force_refit)
  if (!blocker_for_mcmc && regenerate_draws) {
    draw_generation_attempted <- TRUE
    draw_error <- tryCatch({
      ep_draws <- brms::posterior_epred(fit)
      pp_draws <- brms::posterior_predict(fit)
      saveRDS(list(epred = ep_draws, predict = pp_draws), draws_path)
      NULL
    }, error = function(e) conditionMessage(e))
    if (is.null(draw_error) && file.exists(draws_path)) {
      draw_generation_status <- "GENERATED"
      draw_generation_skip_reason <- ""
    } else {
      draw_generation_status <- "FAILED"
      draw_generation_skip_reason <- paste0("draw generation failed: ", draw_error)
    }
  } else if (!stacking_eligible) {
    draw_generation_skip_reason <- "model is not stacking eligible"
  } else if (backfill_diagnostics_only) {
    draw_generation_skip_reason <- "diagnostics-only/backfill mode"
  }

  diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
    Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group, Main_Stack_Inclusion = task$Main_Stack_Inclusion,
    Secondary_Robustness = task$Secondary_Robustness, Heterogeneity_Variant = task$Heterogeneity_Variant,
    N_Obs = as.integer(n_obs_fit), N_Firms = as.integer(n_firms_fit), Fit_Status = "SUCCESS",
    Rhat_Max = max_rhat, ESS_Min = min_ess, Min_Tail_ESS = min_tail_ess, Divergences = divergences,
    converged = converged, stacking_eligible = stacking_eligible, max_rhat = max_rhat,
    divergences = divergences, treedepth_warnings = treedepth_warnings,
    pareto_k_above_07 = loo_diag$pareto_k_above_07, loo_status = loo_diag$loo_status,
    loo_warning_reason = loo_diag$loo_warning_reason, random_intercept_sd = random_intercept_sd,
    elpd_loo = loo_diag$elpd_loo, error_message = NA_character_, Notes = task$Reason,
    Prior_Set_ID = prior_set_id, Likelihood_Family = likelihood_family, Model_Structure = model_structure,
    Output_Root = phase_root,
    save_pars_all = isTRUE(fit$save_pars$all) || (is.list(fit$save_pars) && "all" %in% names(fit$save_pars) && isTRUE(fit$save_pars$all)),
    stringsAsFactors = FALSE
  )

  audit_row <- data.frame(
    Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group, Heterogeneity_Variant = task$Heterogeneity_Variant,
    diagnostic_key = key, Main_Stack_Inclusion = isTRUE(task$Main_Stack_Inclusion),
    Secondary_Robustness = isTRUE(task$Secondary_Robustness), fit_path = fit_path, draws_path = draws_path,
    fit_exists_before = if (nrow(status_row)) status_row$fit_exists_before[1] else TRUE,
    draws_exists_before = NA, fit_exists_after = file.exists(fit_path), draws_exists_after = file.exists(draws_path),
    Fit_Status = "SUCCESS", max_rhat = max_rhat, divergences = divergences,
    treedepth_warnings = treedepth_warnings, min_bulk_ess = min_ess, min_tail_ess = min_tail_ess,
    converged = converged, stacking_eligible = stacking_eligible,
    draw_generation_attempted = draw_generation_attempted, draw_generation_status = draw_generation_status,
    draw_generation_skip_reason = draw_generation_skip_reason, hard_gate_status = hard_gate_status,
    hard_gate_reason = hard_gate_reason, recommended_remediation_key = key, timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  audit_rows[[length(audit_rows) + 1]] <- audit_row

  if (blocker_for_mcmc || (isTRUE(task$Main_Stack_Inclusion) && hard_gate_status %in% c("PASS", "REVIEW") && !file.exists(draws_path))) {
    failure_rows[[length(failure_rows) + 1]] <- audit_row[, c(
      "diagnostic_key", "Model_ID", "Model_Name", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
      "Main_Stack_Inclusion", "hard_gate_status", "hard_gate_reason", "max_rhat", "divergences",
      "min_bulk_ess", "min_tail_ess", "fit_path", "draws_path", "fit_exists_after",
      "draws_exists_after", "recommended_remediation_key", "timestamp"
    )]
  }

  fix_effects <- as.data.frame(brms::fixef(fit))
  for (pname in rownames(fix_effects)) {
    coeff_rows[[length(coeff_rows) + 1]] <- data.frame(
      Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group, Main_Stack_Inclusion = task$Main_Stack_Inclusion,
      Secondary_Robustness = task$Secondary_Robustness, Heterogeneity_Variant = task$Heterogeneity_Variant,
      Parameter = pname, Estimate = fix_effects[pname, "Estimate"], Est_Error = fix_effects[pname, "Est.Error"],
      CI_Lower = fix_effects[pname, "Q2.5"], CI_Upper = fix_effects[pname, "Q97.5"],
      Prior_Set_ID = prior_set_id, Likelihood_Family = likelihood_family, Model_Structure = model_structure,
      Output_Root = phase_root, stringsAsFactors = FALSE
    )
  }
}

diagnostics_df <- bind_rows(diagnostics_rows) %>% arrange(match(paste(Model_ID, Target_Space, Sample_Group, Heterogeneity_Variant, sep = "|"), task_manifest$task_key))
audit_df <- if (length(audit_rows)) bind_rows(audit_rows) else empty_artifact_audit()
fail_df <- if (length(failure_rows)) bind_rows(failure_rows) else empty_hard_gate_failures()
coeff_df <- if (length(coeff_rows)) bind_rows(coeff_rows) else data.frame()

write.csv(diagnostics_df, diag_path, row.names = FALSE)
write.csv(audit_df, ma07_artifact_audit_path, row.names = FALSE)
write.csv(fail_df, ma07_hard_gate_failures_path, row.names = FALSE)
write.csv(coeff_df, coeff_path, row.names = FALSE)

if (nrow(fail_df) > 0) {
  failed_main_keys <- unique(fail_df$recommended_remediation_key[fail_df$Main_Stack_Inclusion %in% TRUE])
  failed_main_keys <- failed_main_keys[!is.na(failed_main_keys) & nzchar(failed_main_keys)]
  helper_lines <- c(
    "# Suggested ma07 remediation targets generated from hard-gate failures.",
    paste0("$env:ACCRUAL_MCMC_REMEDIATION_TARGETS = \"", paste(failed_main_keys, collapse = ";"), "\""),
    "Remove-Item Env:\\ACCRUAL_FORCE_REFIT -ErrorAction SilentlyContinue",
    "Rscript .\\scripts\\ma07_fit_brms_named_models.R",
    "Rscript .\\scripts\\ma08_mcmc_diagnostics.R"
  )
  writeLines(helper_lines, ma07_remediation_helper_path, useBytes = TRUE)
}

sampler_cfg <- accrual_sampler_config("baseline", varying_slopes = run_varying_slope_models)
baseline_rng_meta <- accrual_rng_metadata_list("baseline_fit_brms_named_models")
phase3_notes <- sprintf(
  paste0(
    "ma07 winsorized BRMS fit notes\n",
    "Fit-stage artifacts are created by scripts/ma07a_fit_brms_named_models.R.\n",
    "Shared diagnostics, audit, draw, and coefficient outputs are collected by scripts/ma07b_collect_brms_fit_outputs.R.\n",
    "Sampling settings: chains=%d, cores=%d, iter=%d, warmup=%d, adapt_delta=%.2f, max_treedepth=%d, canonical_seed=%d, effective_seed=%d.\n",
    "Prior_Set_ID: %s.\nLikelihood_Family: %s.\nModel_Structure: %s.\n"
  ),
  sampler_cfg$chains, sampler_cfg$cores, sampler_cfg$iter, sampler_cfg$warmup,
  sampler_cfg$adapt_delta, sampler_cfg$max_treedepth,
  baseline_rng_meta$Canonical_Seed, baseline_rng_meta$Effective_Seed,
  prior_set_id, likelihood_family, model_structure
)
notes_file <- if (run_varying_slope_models) file.path(phase_root, "logs", "varyslopes_notes.txt") else file.path(phase_root, "logs", "ma07_fit_notes_winsor.txt")
writeLines(phase3_notes, con = notes_file)

manifest_path <- file.path(phase_root, "manifests", "baseline_manifest.csv")
write_run_manifest(
  path = manifest_path,
  scenario = "baseline",
  prior_set_id = prior_set_id,
  family = likelihood_family,
  model_structure = model_structure,
  model_list = unique(task_manifest$Model_ID),
  seed = baseline_rng_meta$Effective_Seed,
  sampling_config = sprintf("chains=%d;cores=%d;iter=%d;warmup=%d", sampler_cfg$chains, sampler_cfg$cores, sampler_cfg$iter, sampler_cfg$warmup),
  status = "SUCCESS",
  notes = "ma07 split fit/collect run",
  input_paths = c(formulas_path, task_manifest_path, task_status_path),
  rng_context = baseline_rng_meta$RNG_Context,
  rng_offset = baseline_rng_meta$RNG_Offset
)

if (run_varying_slope_models) {
  empty_weights <- data.frame(
    Status = "NOT_COMPUTED_BY_ma07",
    Notes = "Varying-slope fits are a Breuer-structure robustness check. Run a separate varying-slope stacking analysis before using weights.",
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = phase_root,
    stringsAsFactors = FALSE
  )
  write.csv(empty_weights, file.path(phase_root, "tables", "table_varyslopes_loo_weights.csv"), row.names = FALSE)
}

if (nrow(fail_df) > 0 && any(fail_df$Main_Stack_Inclusion %in% TRUE)) {
  stop("[MA07 HARD GATE BLOCKER] Main-stack model failed diagnostics or draw artifact collection: ",
       paste(fail_df$diagnostic_key[fail_df$Main_Stack_Inclusion %in% TRUE], collapse = "; "))
}

cat("[SUCCESS] ma07b collection completed.\n")
phase_end("ma07b", "Collect baseline brms fit outputs")
