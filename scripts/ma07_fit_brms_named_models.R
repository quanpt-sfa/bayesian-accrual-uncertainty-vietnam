# -----------------------------------------------------------------------------
# Script: 07_fit_brms_named_models.R
# Purpose: Fit named brms models on winsorized samples.
# -----------------------------------------------------------------------------

library(dplyr)
library(brms)

source("scripts/ma00_setup.R")
phase_begin("ma07", "Fit baseline brms models")
ensure_analysis_dirs()
write_method_design_files()
write_prior_registry()
write_execution_config_registry()
validate_final_analysis_config("ma07 baseline brms fit", final_mode = TRUE)

backfill_diagnostics_only <- env_flag("ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY", "FALSE")
remediation_targets_raw <- trimws(env_value("ACCRUAL_MCMC_REMEDIATION_TARGETS", ""))
remediation_mode <- nzchar(remediation_targets_raw)
ma07_strict_review_blocker <- env_flag("ACCRUAL_MA07_STRICT_REVIEW_BLOCKER", "FALSE")
if (backfill_diagnostics_only && force_refit) {
  stop("[BLOCKER] ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE cannot be combined with ACCRUAL_FORCE_REFIT=TRUE.")
}
if (remediation_mode && force_refit) {
  stop("[BLOCKER] ACCRUAL_MCMC_REMEDIATION_TARGETS cannot be combined with ACCRUAL_FORCE_REFIT=TRUE.")
}
if (remediation_mode && backfill_diagnostics_only) {
  stop("[BLOCKER] ACCRUAL_MCMC_REMEDIATION_TARGETS cannot be combined with ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE.")
}

# Check prior predictive check gatekeeper status
gate_csv_path <- file.path(output_root, "prior_predictive_gate_status.csv")
if (!file.exists(gate_csv_path)) {
  stop("[BLOCKER] Prior predictive gate status file does not exist. Please run '06_prior_predictive_checks.R' first.")
}
gate_df <- read.csv(gate_csv_path, stringsAsFactors = FALSE)
has_prior_pred_fail <- any(gate_df$status == "FAIL")
prior_pred_override_used <- FALSE

if (has_prior_pred_fail) {
  allow_fail <- env_flag("ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL", "FALSE")
  if (!allow_fail) {
    stop("[BLOCKER] Prior predictive check gate contains FAIL. Fitting blocked. Run 06_prior_predictive_checks.R or set ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE.")
  } else {
    prior_pred_override_used <- TRUE
    message("[OVERRIDE] Prior predictive check FAIL bypassed via ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE.")
  }
}

options(mc.cores = parallel::detectCores())

run_varying_slope_models <- identical(model_structure, "breuer_varying_slopes")
if (run_varying_slope_models && !run_varying_slopes) {
  stop("[BLOCKER] ACCRUAL_MODEL_STRUCTURE='breuer_varying_slopes' requires ACCRUAL_RUN_VARYING_SLOPES='TRUE'.")
}

phase_root <- if (run_varying_slope_models) varyslopes_root else output_root
for (d in file.path(phase_root, c("", "tables", "models", "draws", "logs", "figures"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) {
  stop("[BLOCKER] Winsorized formula table not found. Run 05_winsorize_common_samples.R first.")
}

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
if (run_varying_slope_models) {
  formulas_df <- formulas_df %>%
    filter(Main_Stack_Inclusion == TRUE) %>%
    filter(mapply(varying_slope_candidate, Model_ID, Target_Space)) %>%
    group_by(Model_ID, Target_Space, Sample_Group) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      Heterogeneity_Variant = paste0("Breuer-like varying slopes (", varyslope_group, ")"),
      brms_Formula = vapply(Base_Formula, varying_slope_formula, character(1), group = varyslope_group),
      Model_Structure = model_structure,
      VarySlope_Group = varyslope_group,
      VarySlope_Scope = varyslope_scope
    )
  write.csv(formulas_df, file.path(phase_root, "tables", "table_varyslopes_model_registry.csv"), row.names = FALSE)
}
write.csv(formulas_df, file.path(phase_root, "tables", "table_named_model_formulas_winsor.csv"), row.names = FALSE)

diag_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_varyslopes_diagnostics.csv")
} else {
  file.path(phase_root, "tables", "table_brms_diagnostics_winsor.csv")
}
ma07_artifact_audit_path <- file.path(phase_root, "tables", "table_ma07_fit_draw_artifact_audit.csv")
ma07_hard_gate_failures_path <- file.path(phase_root, "tables", "table_ma07_hard_gate_failures.csv")
ma07_remediation_helper_path <- file.path(phase_root, "logs", "ma07_suggested_remediation_targets.ps1")
coeff_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_varyslopes_coefficient_summary.csv")
} else {
  file.path(phase_root, "tables", "table_coefficient_summary_winsor.csv")
}

ensure_diag_columns <- function(df) {
  expected_columns <- list(
    Model_ID = character(),
    Model_Name = character(),
    Target_Space = character(),
    Sample_Group = character(),
    Main_Stack_Inclusion = logical(),
    Secondary_Robustness = logical(),
    Heterogeneity_Variant = character(),
    N_Obs = integer(),
    N_Firms = integer(),
    Fit_Status = character(),
    Rhat_Max = double(),
    ESS_Min = double(),
    Min_Tail_ESS = double(),
    Divergences = integer(),
    converged = logical(),
    stacking_eligible = logical(),
    max_rhat = double(),
    divergences = integer(),
    treedepth_warnings = integer(),
    pareto_k_above_07 = integer(),
    loo_status = character(),
    loo_warning_reason = character(),
    random_intercept_sd = double(),
    elpd_loo = double(),
    error_message = character(),
    Notes = character(),
    Prior_Set_ID = character(),
    Likelihood_Family = character(),
    Model_Structure = character(),
    Output_Root = character(),
    save_pars_all = logical()
  )

  for (nm in names(expected_columns)) {
    if (!nm %in% names(df)) {
      template <- expected_columns[[nm]]
      n_rows <- nrow(df)
      if (is.logical(template)) {
        df[[nm]] <- rep(NA, n_rows)
      } else if (is.integer(template)) {
        df[[nm]] <- rep(NA_integer_, n_rows)
      } else if (is.double(template)) {
        df[[nm]] <- rep(NA_real_, n_rows)
      } else {
        df[[nm]] <- rep(NA_character_, n_rows)
      }
    }
  }
  df
}

load_step7_sample_info <- function(row) {
  df_scaled <- read_winsor_sample(row$Target_Sample)
  if (!"company" %in% names(df_scaled)) {
    stop(
      "[BLOCKER] Winsorized sample '", row$Target_Sample,
      "' is missing required company column for Step 7 diagnostics. Available columns: ",
      paste(names(df_scaled), collapse = ", ")
    )
  }

  company_values <- trimws(as.character(df_scaled$company))
  company_values[company_values == ""] <- NA_character_
  n_firms_fit <- length(unique(company_values[!is.na(company_values)]))
  if (n_firms_fit <= 0) {
    stop(
      "[BLOCKER] Winsorized sample '", row$Target_Sample,
      "' has no valid company identifiers for Step 7 diagnostics."
    )
  }

  list(
    df_scaled = df_scaled,
    n_obs_fit = nrow(df_scaled),
    n_firms_fit = n_firms_fit
  )
}

classify_loo_status <- function(loo_res, pareto_k_above_07) {
  if (is.null(loo_res)) {
    return(list(
      loo_status = "LOO_FAILED",
      loo_warning_reason = "loo() failed; elpd_loo unavailable"
    ))
  }

  if (!is.na(pareto_k_above_07) && pareto_k_above_07 > 0) {
    return(list(
      loo_status = "PSIS_REVIEW_REQUIRED",
      loo_warning_reason = "pareto_k_above_07 > 0; consider reloo, moment matching, or grouped K-fold before relying on PSIS-LOO"
    ))
  }

  list(
    loo_status = "PSIS_OK",
    loo_warning_reason = NA_character_
  )
}

classify_loo_result <- function(loo_res) {
  if (is.null(loo_res)) {
    loo_class <- classify_loo_status(NULL, NA_integer_)
    return(list(
      pareto_k_above_07 = NA_integer_,
      elpd_loo = NA_real_,
      loo_status = loo_class$loo_status,
      loo_warning_reason = loo_class$loo_warning_reason
    ))
  }

  pareto_k_above_07 <- sum(loo_res$diagnostics$pareto_k > 0.7)
  loo_class <- classify_loo_status(loo_res, pareto_k_above_07)

  list(
    pareto_k_above_07 = pareto_k_above_07,
    elpd_loo = loo_res$estimates["elpd_loo", "Estimate"],
    loo_status = loo_class$loo_status,
    loo_warning_reason = loo_class$loo_warning_reason
  )
}

lookup_existing_diag_row <- function(df, row) {
  df %>%
    filter(
      Model_ID == row$Model_ID,
      Target_Space == row$Target_Space,
      Sample_Group == row$Sample_Group,
      Heterogeneity_Variant == row$Heterogeneity_Variant
    ) %>%
    tail(1)
}

diagnostic_key <- function(df) {
  paste(df$Model_ID, df$Target_Space, df$Sample_Group, df$Heterogeneity_Variant, sep = "|")
}

diagnostic_key_for_row <- function(row) {
  paste(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, sep = "|")
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
  if (!length(reasons)) "MCMC diagnostics passed ma07 hard gate."
  else paste(reasons, collapse = "; ")
}

parse_remediation_targets <- function(raw_value) {
  if (!nzchar(raw_value)) {
    return(character())
  }

  entries <- trimws(unlist(strsplit(raw_value, ";", fixed = TRUE)))
  entries <- entries[nzchar(entries)]
  invalid_entries <- entries[vapply(entries, function(entry) {
    parts <- trimws(unlist(strsplit(entry, "|", fixed = TRUE)))
    length(parts) != 4 || any(!nzchar(parts))
  }, logical(1))]
  if (length(invalid_entries) > 0) {
    stop(
      "[BLOCKER] Invalid ACCRUAL_MCMC_REMEDIATION_TARGETS key(s): ",
      paste(invalid_entries, collapse = "; "),
      ". Expected format Model_ID|Target_Space|Sample_Group|Heterogeneity_Variant"
    )
  }

  unique(entries)
}

reuse_existing_loo_diag <- function(existing_diag_row) {
  if (nrow(existing_diag_row) == 0) {
    return(NULL)
  }

  existing_pareto <- suppressWarnings(as.integer(existing_diag_row$pareto_k_above_07[[1]]))
  existing_elpd <- suppressWarnings(as.numeric(existing_diag_row$elpd_loo[[1]]))
  if (is.na(existing_pareto) && is.na(existing_elpd)) {
    return(NULL)
  }

  loo_class <- classify_loo_status(TRUE, existing_pareto)
  list(
    pareto_k_above_07 = existing_pareto,
    elpd_loo = existing_elpd,
    loo_status = loo_class$loo_status,
    loo_warning_reason = loo_class$loo_warning_reason
  )
}

reconcile_step7_diagnostics <- function(diagnostics_df, formulas_df) {
  sample_rows <- formulas_df[!duplicated(diagnostic_key(formulas_df)), c(
    "Model_ID", "Target_Space", "Sample_Group", "Heterogeneity_Variant", "Target_Sample"
  )]
  diagnostics_keys <- diagnostic_key(diagnostics_df)

  for (i in seq_len(nrow(sample_rows))) {
    sample_row <- sample_rows[i, ]
    sample_info <- load_step7_sample_info(sample_row)
    row_key <- diagnostic_key(sample_row)
    match_idx <- which(diagnostics_keys == row_key)
    if (length(match_idx) == 0) {
      next
    }
    diagnostics_df$N_Obs[match_idx] <- as.integer(sample_info$n_obs_fit)
    diagnostics_df$N_Firms[match_idx] <- as.integer(sample_info$n_firms_fit)
  }

  loo_fields <- lapply(seq_len(nrow(diagnostics_df)), function(i) {
    fit_status_i <- diagnostics_df$Fit_Status[[i]]
    pareto_i <- suppressWarnings(as.integer(diagnostics_df$pareto_k_above_07[[i]]))
    elpd_i <- suppressWarnings(as.numeric(diagnostics_df$elpd_loo[[i]]))
    error_i <- diagnostics_df$error_message[[i]]

    if (identical(fit_status_i, "SUCCESS") && (!is.na(pareto_i) || !is.na(elpd_i))) {
      loo_class <- classify_loo_status(TRUE, pareto_i)
    } else if (identical(fit_status_i, "SUCCESS")) {
      loo_class <- classify_loo_status(NULL, NA_integer_)
    } else {
      loo_class <- list(
        loo_status = "LOO_FAILED",
        loo_warning_reason = if (!is.na(error_i) && nzchar(error_i)) {
          paste0("model fitting failed or LOO unavailable: ", error_i)
        } else {
          "loo() failed; elpd_loo unavailable"
        }
      )
    }

    data.frame(
      loo_status_backfill = loo_class$loo_status,
      loo_warning_reason_backfill = loo_class$loo_warning_reason,
      stringsAsFactors = FALSE
    )
  })
  loo_fields_df <- bind_rows(loo_fields)

  diagnostics_df$loo_status <- loo_fields_df$loo_status_backfill
  diagnostics_df$loo_warning_reason <- loo_fields_df$loo_warning_reason_backfill

  formula_order <- diagnostic_key(formulas_df)
  diagnostics_df %>%
    mutate(.diagnostic_key = diagnostic_key(.)) %>%
    arrange(match(.diagnostic_key, formula_order)) %>%
    select(-.diagnostic_key)
}

if (file.exists(diag_path)) {
  diagnostics_df <- read.csv(diag_path, stringsAsFactors = FALSE)
  diagnostics_df <- ensure_diag_columns(diagnostics_df)
  message("Resuming from existing winsor diagnostics table with ", nrow(diagnostics_df), " entries.")
} else {
  diagnostics_df <- ensure_diag_columns(data.frame(stringsAsFactors = FALSE))
}

for (nm in names(metadata_columns())) {
  if (!nm %in% names(diagnostics_df)) diagnostics_df[[nm]] <- metadata_columns()[[nm]]
}
if (!"save_pars_all" %in% names(diagnostics_df)) {
  diagnostics_df$save_pars_all <- FALSE
}

empty_ma07_artifact_audit <- function() {
  data.frame(
    Model_ID = character(),
    Model_Name = character(),
    Target_Space = character(),
    Sample_Group = character(),
    Heterogeneity_Variant = character(),
    diagnostic_key = character(),
    Main_Stack_Inclusion = logical(),
    Secondary_Robustness = logical(),
    fit_path = character(),
    draws_path = character(),
    fit_exists_before = logical(),
    draws_exists_before = logical(),
    fit_exists_after = logical(),
    draws_exists_after = logical(),
    Fit_Status = character(),
    max_rhat = numeric(),
    divergences = numeric(),
    treedepth_warnings = numeric(),
    min_bulk_ess = numeric(),
    min_tail_ess = numeric(),
    converged = logical(),
    stacking_eligible = logical(),
    draw_generation_attempted = logical(),
    draw_generation_status = character(),
    draw_generation_skip_reason = character(),
    hard_gate_status = character(),
    hard_gate_reason = character(),
    recommended_remediation_key = character(),
    timestamp = character(),
    stringsAsFactors = FALSE
  )
}

empty_ma07_hard_gate_failures <- function() {
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

ensure_columns_from_template <- function(df, template) {
  for (nm in names(template)) {
    if (!nm %in% names(df)) {
      n_rows <- nrow(df)
      proto <- template[[nm]]
      if (is.logical(proto)) {
        df[[nm]] <- rep(NA, n_rows)
      } else if (is.numeric(proto)) {
        df[[nm]] <- rep(NA_real_, n_rows)
      } else {
        df[[nm]] <- rep(NA_character_, n_rows)
      }
    }
  }
  df[names(template)]
}

ma07_artifact_audit <- if (file.exists(ma07_artifact_audit_path)) {
  read.csv(ma07_artifact_audit_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  empty_ma07_artifact_audit()
}
ma07_artifact_audit <- ensure_columns_from_template(ma07_artifact_audit, empty_ma07_artifact_audit())
ma07_hard_gate_failures <- if (file.exists(ma07_hard_gate_failures_path)) {
  read.csv(ma07_hard_gate_failures_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  empty_ma07_hard_gate_failures()
}
ma07_hard_gate_failures <- ensure_columns_from_template(ma07_hard_gate_failures, empty_ma07_hard_gate_failures())

write_ma07_artifact_audit <- function() {
  write.csv(ma07_artifact_audit, ma07_artifact_audit_path, row.names = FALSE)
}

write_ma07_remediation_helper <- function() {
  failed_main_keys <- unique(ma07_hard_gate_failures$recommended_remediation_key[
    ma07_hard_gate_failures$Main_Stack_Inclusion %in% TRUE
  ])
  failed_main_keys <- failed_main_keys[!is.na(failed_main_keys) & nzchar(failed_main_keys)]
  lines <- c(
    "# Suggested ma07 remediation targets generated from hard-gate failures.",
    paste0("$env:ACCRUAL_MCMC_REMEDIATION_TARGETS = \"", paste(failed_main_keys, collapse = ";"), "\""),
    "Remove-Item Env:\\ACCRUAL_FORCE_REFIT -ErrorAction SilentlyContinue",
    "Rscript .\\scripts\\ma07_fit_brms_named_models.R",
    "Rscript .\\scripts\\ma08_mcmc_diagnostics.R"
  )
  writeLines(lines, con = ma07_remediation_helper_path, useBytes = TRUE)
}

write_ma07_hard_gate_failures <- function() {
  write.csv(ma07_hard_gate_failures, ma07_hard_gate_failures_path, row.names = FALSE)
  if (nrow(ma07_hard_gate_failures) > 0) write_ma07_remediation_helper()
}

upsert_ma07_artifact_audit <- function(audit_row) {
  key <- audit_row$diagnostic_key[[1]]
  ma07_artifact_audit <<- ma07_artifact_audit %>%
    filter(.data$diagnostic_key != key) %>%
    bind_rows(audit_row)
  write_ma07_artifact_audit()
}

upsert_ma07_hard_gate_failure <- function(failure_row) {
  key <- failure_row$diagnostic_key[[1]]
  ma07_hard_gate_failures <<- ma07_hard_gate_failures %>%
    filter(.data$diagnostic_key != key) %>%
    bind_rows(failure_row)
  write_ma07_hard_gate_failures()
}

make_ma07_artifact_audit_row <- function(row, model_filename, draws_filename,
                                         fit_exists_before, draws_exists_before,
                                         fit_exists_after, draws_exists_after,
                                         fit_status = NA_character_,
                                         max_rhat = NA_real_, divergences = NA_real_,
                                         treedepth_warnings = NA_real_,
                                         min_bulk_ess = NA_real_, min_tail_ess = NA_real_,
                                         converged = NA, stacking_eligible = NA,
                                         draw_generation_attempted = FALSE,
                                         draw_generation_status = NA_character_,
                                         draw_generation_skip_reason = NA_character_,
                                         hard_gate_status = NA_character_,
                                         hard_gate_reason = NA_character_) {
  data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    diagnostic_key = diagnostic_key_for_row(row),
    Main_Stack_Inclusion = isTRUE(row$Main_Stack_Inclusion),
    Secondary_Robustness = isTRUE(row$Secondary_Robustness),
    fit_path = model_filename,
    draws_path = draws_filename,
    fit_exists_before = isTRUE(fit_exists_before),
    draws_exists_before = isTRUE(draws_exists_before),
    fit_exists_after = isTRUE(fit_exists_after),
    draws_exists_after = isTRUE(draws_exists_after),
    Fit_Status = fit_status,
    max_rhat = max_rhat,
    divergences = divergences,
    treedepth_warnings = treedepth_warnings,
    min_bulk_ess = min_bulk_ess,
    min_tail_ess = min_tail_ess,
    converged = converged,
    stacking_eligible = stacking_eligible,
    draw_generation_attempted = isTRUE(draw_generation_attempted),
    draw_generation_status = draw_generation_status,
    draw_generation_skip_reason = ifelse(is.na(draw_generation_skip_reason), "", draw_generation_skip_reason),
    hard_gate_status = hard_gate_status,
    hard_gate_reason = hard_gate_reason,
    recommended_remediation_key = diagnostic_key_for_row(row),
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
}

record_ma07_failure <- function(row, audit_row) {
  failure_row <- data.frame(
    diagnostic_key = audit_row$diagnostic_key,
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    Main_Stack_Inclusion = isTRUE(row$Main_Stack_Inclusion),
    hard_gate_status = audit_row$hard_gate_status,
    hard_gate_reason = audit_row$hard_gate_reason,
    max_rhat = audit_row$max_rhat,
    divergences = audit_row$divergences,
    min_bulk_ess = audit_row$min_bulk_ess,
    min_tail_ess = audit_row$min_tail_ess,
    fit_path = audit_row$fit_path,
    draws_path = audit_row$draws_path,
    fit_exists_after = audit_row$fit_exists_after,
    draws_exists_after = audit_row$draws_exists_after,
    recommended_remediation_key = audit_row$recommended_remediation_key,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  upsert_ma07_hard_gate_failure(failure_row)
}

sampler_cfg <- accrual_sampler_config("baseline", varying_slopes = run_varying_slope_models)
chains <- sampler_cfg$chains
iter <- sampler_cfg$iter
warmup <- sampler_cfg$warmup
adapt_delta <- sampler_cfg$adapt_delta
max_treedepth <- sampler_cfg$max_treedepth
baseline_sampler_controls <- sampler_cfg[c("chains", "iter", "warmup", "adapt_delta", "max_treedepth")]
remediation_cfg <- accrual_sampler_config("baseline_remediation")
remediation_sampler_controls <- remediation_cfg[c("chains", "iter", "warmup", "adapt_delta", "max_treedepth")]
baseline_rng_meta <- accrual_rng_metadata_list("baseline_fit_brms_named_models")
remediation_rng_meta <- accrual_rng_metadata_list("baseline_fit_brms_named_models_remediation")

main_ex_post_ids <- main_model_ids_for_space("ex_post")
main_no_lookahead_ids <- main_model_ids_for_space("real_time")

formulas_df <- formulas_df %>%
  mutate(
    order_key = case_when(
      Model_ID %in% c("M01", "M02", "M03", "M04", "M05", "M06") ~ 1,
      Model_ID %in% c("M07", "M09", "M10") ~ 2,
      Model_ID == "M08" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  arrange(order_key, Model_ID, Target_Space, Heterogeneity_Variant)

remediation_targets <- parse_remediation_targets(remediation_targets_raw)
if (length(remediation_targets) > 0) {
  formula_keys <- diagnostic_key(formulas_df)
  unmatched_targets <- setdiff(remediation_targets, formula_keys)
  if (length(unmatched_targets) > 0) {
    stop(
      "[BLOCKER] ACCRUAL_MCMC_REMEDIATION_TARGETS contains key(s) that do not match any Step 7 formula row: ",
      paste(unmatched_targets, collapse = "; ")
    )
  }

  remediation_log_path <- file.path(phase_root, "logs", "ma08_mcmc_remediation_log.txt")
  remediation_log_lines <- c(
    "ma08 one-time MCMC remediation log",
    paste0("Target keys: ", paste(remediation_targets, collapse = "; ")),
    paste0(
      "Baseline sampler controls: chains=", baseline_sampler_controls$chains,
      "; iter=", baseline_sampler_controls$iter,
      "; warmup=", baseline_sampler_controls$warmup,
      "; adapt_delta=", baseline_sampler_controls$adapt_delta,
      "; max_treedepth=", baseline_sampler_controls$max_treedepth,
      "; canonical_seed=", baseline_rng_meta$Canonical_Seed,
      "; effective_seed=", baseline_rng_meta$Effective_Seed
    ),
    paste0(
      "Remediation sampler controls: chains=", remediation_sampler_controls$chains,
      "; iter=", remediation_sampler_controls$iter,
      "; warmup=", remediation_sampler_controls$warmup,
      "; adapt_delta=", remediation_sampler_controls$adapt_delta,
      "; max_treedepth=", remediation_sampler_controls$max_treedepth,
      "; canonical_seed=", remediation_rng_meta$Canonical_Seed,
      "; effective_seed=", remediation_rng_meta$Effective_Seed
    ),
    sprintf("Canonical pipeline seed retained: %d.", baseline_rng_meta$Canonical_Seed),
    "No seed search was performed.",
    "Formulas, priors, likelihood, model structure, and samples were unchanged."
  )
  writeLines(remediation_log_lines, con = remediation_log_path)
}

write_failure_diag <- function(row, err, n_obs_fit = NA_integer_, n_firms_fit = NA_integer_, loo_warning_reason = NULL) {
  if (is.null(loo_warning_reason) || !nzchar(loo_warning_reason)) {
    loo_warning_reason <- paste0("model fitting failed or LOO unavailable: ", err)
  }
  fail_row <- data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Main_Stack_Inclusion = row$Main_Stack_Inclusion,
    Secondary_Robustness = row$Secondary_Robustness,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    N_Obs = as.integer(n_obs_fit),
    N_Firms = as.integer(n_firms_fit),
    Fit_Status = "FAILED",
    Rhat_Max = NA_real_,
    ESS_Min = NA_real_,
    Divergences = NA_integer_,
    converged = FALSE,
    stacking_eligible = FALSE,
    max_rhat = NA_real_,
    divergences = NA_integer_,
    treedepth_warnings = NA_integer_,
    pareto_k_above_07 = NA_integer_,
    loo_status = "LOO_FAILED",
    loo_warning_reason = loo_warning_reason,
    random_intercept_sd = NA_real_,
    elpd_loo = NA_real_,
    error_message = err,
    Notes = row$Reason,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = phase_root,
    save_pars_all = FALSE,
    stringsAsFactors = FALSE
  )
  diagnostics_df <<- diagnostics_df %>%
    filter(!(Model_ID == row$Model_ID &
               Target_Space == row$Target_Space &
               Sample_Group == row$Sample_Group &
               Heterogeneity_Variant == row$Heterogeneity_Variant)) %>%
    bind_rows(fail_row)
  write.csv(diagnostics_df, diag_path, row.names = FALSE)
}

total_runs <- nrow(formulas_df)
message("Total winsorized configurations to fit/evaluate: ", total_runs)
if (backfill_diagnostics_only) {
  message("Step 7 diagnostics-only backfill mode is enabled. Existing fit .rds objects will be reused and brm() will not be called.")
}
if (length(remediation_targets) > 0) {
  message("Step 7 one-time MCMC remediation mode is enabled for ", length(remediation_targets), " target row(s).")
}

for (i in seq_len(total_runs)) {
  row <- formulas_df[i, ]
  model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
  model_filename <- file.path(phase_root, "models", paste0("fit_", model_key, ".rds"))
  draws_filename <- file.path(phase_root, "draws", paste0("draws_", model_key, ".rds"))
  existing_diag_row <- lookup_existing_diag_row(diagnostics_df, row)
  row_target_key <- diagnostic_key(row)
  row_is_remediation_target <- row_target_key %in% remediation_targets
  active_sampler_controls <- if (row_is_remediation_target) remediation_sampler_controls else baseline_sampler_controls
  fit_exists_before <- file.exists(model_filename)
  draws_exists_before <- file.exists(draws_filename)

  message(sprintf("\n=== [%d/%d] Winsor model %s (%s) - %s ===",
                  i, total_runs, row$Model_Name, row$Target_Space, row$Heterogeneity_Variant))
  upsert_ma07_artifact_audit(make_ma07_artifact_audit_row(
    row, model_filename, draws_filename,
    fit_exists_before = fit_exists_before,
    draws_exists_before = draws_exists_before,
    fit_exists_after = fit_exists_before,
    draws_exists_after = draws_exists_before,
    fit_status = "PENDING",
    draw_generation_status = "PENDING",
    draw_generation_skip_reason = "model not yet evaluated"
  ))

  fit <- NULL
  sample_info <- tryCatch(load_step7_sample_info(row), error = function(e) {
    write_failure_diag(row, e$message, loo_warning_reason = paste0("diagnostics backfill failed before LOO: ", e$message))
    upsert_ma07_artifact_audit(make_ma07_artifact_audit_row(
      row, model_filename, draws_filename,
      fit_exists_before = fit_exists_before,
      draws_exists_before = draws_exists_before,
      fit_exists_after = file.exists(model_filename),
      draws_exists_after = file.exists(draws_filename),
      fit_status = "FAILED",
      draw_generation_status = "SKIPPED",
      draw_generation_skip_reason = "sample loading failed before draw generation",
      hard_gate_status = "FAIL",
      hard_gate_reason = e$message
    ))
    stop(e)
  })
  df_scaled <- sample_info$df_scaled
  n_obs_fit <- sample_info$n_obs_fit
  n_firms_fit <- sample_info$n_firms_fit

  if (file.exists(model_filename) && !force_refit && !row_is_remediation_target) {
    message("Loading pre-existing winsor model fit from: ", model_filename)
    fit <- tryCatch(readRDS(model_filename), error = function(e) {
      message("[ERROR] Could not read existing fit: ", e$message)
      NULL
    })
    if (file.exists(draws_filename)) {
      message("Draw file already exists; ma07 will not regenerate it unless ACCRUAL_FORCE_REFIT='TRUE': ", draws_filename)
    } else if (!is.null(fit)) {
      message("[MA07 ARTIFACT WARNING] Existing fit has no matching draws file: ", row_target_key, " -> ", draws_filename)
    }
  }

  if (remediation_mode && !row_is_remediation_target) {
    if (!file.exists(model_filename)) {
      upsert_ma07_artifact_audit(make_ma07_artifact_audit_row(
        row, model_filename, draws_filename,
        fit_exists_before = fit_exists_before,
        draws_exists_before = draws_exists_before,
        fit_exists_after = file.exists(model_filename),
        draws_exists_after = file.exists(draws_filename),
        fit_status = "FAILED",
        draw_generation_status = "SKIPPED",
        draw_generation_skip_reason = "remediation mode blocks non-target regeneration",
        hard_gate_status = "BLOCKED",
        hard_gate_reason = "non-target remediation row is missing required fit"
      ))
      stop(
        "[BLOCKER] Non-target remediation row is missing required fit .rds file: ",
        row_target_key, " -> ", model_filename,
        ". Current ACCRUAL_MCMC_REMEDIATION_TARGETS: ", paste(remediation_targets, collapse = "; "),
        ". Either backfill missing artifacts without remediation mode, or add this exact key to ACCRUAL_MCMC_REMEDIATION_TARGETS: ",
        row_target_key
      )
    }
    if (!file.exists(draws_filename)) {
      upsert_ma07_artifact_audit(make_ma07_artifact_audit_row(
        row, model_filename, draws_filename,
        fit_exists_before = fit_exists_before,
        draws_exists_before = draws_exists_before,
        fit_exists_after = file.exists(model_filename),
        draws_exists_after = file.exists(draws_filename),
        fit_status = "SUCCESS",
        draw_generation_status = "SKIPPED",
        draw_generation_skip_reason = "remediation mode blocks non-target regeneration",
        hard_gate_status = "BLOCKED",
        hard_gate_reason = "non-target remediation row is missing required draws"
      ))
      stop(
        "[BLOCKER] Non-target remediation row is missing required draws .rds file: ",
        row_target_key, " -> ", draws_filename,
        ". Current ACCRUAL_MCMC_REMEDIATION_TARGETS: ", paste(remediation_targets, collapse = "; "),
        ". Either backfill missing draws without remediation mode, or add this exact key to ACCRUAL_MCMC_REMEDIATION_TARGETS: ",
        row_target_key
      )
    }
  }

  if (backfill_diagnostics_only && is.null(fit)) {
    err_msg <- paste0(
      "[BLOCKER] ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE requires an existing fit object for ",
      model_key, ": ", model_filename
    )
    write_failure_diag(
      row,
      err_msg,
      n_obs_fit = n_obs_fit,
      n_firms_fit = n_firms_fit,
      loo_warning_reason = paste0("diagnostics backfill failed: missing or unreadable fit object at ", model_filename)
    )
    upsert_ma07_artifact_audit(make_ma07_artifact_audit_row(
      row, model_filename, draws_filename,
      fit_exists_before = fit_exists_before,
      draws_exists_before = draws_exists_before,
      fit_exists_after = file.exists(model_filename),
      draws_exists_after = file.exists(draws_filename),
      fit_status = "FAILED",
      draw_generation_status = "SKIPPED",
      draw_generation_skip_reason = "diagnostics-only/backfill mode",
      hard_gate_status = "FAIL",
      hard_gate_reason = "missing or unreadable fit object in backfill mode"
    ))
    stop(err_msg)
  }

  if (is.null(fit)) {
    if (row_is_remediation_target) {
      message(
        "Refitting remediation target with stronger sampler controls and configured baseline seed: ",
        row_target_key
      )
    }
    if (run_varying_slope_models) {
      df_scaled <- prepare_varying_slope_data(df_scaled)
    }

    formula_str <- fix_formula(row$brms_Formula)
    brms_form <- bf(as.formula(formula_str))

    prior_list <- default_prior_list(row$Heterogeneity_Variant, model_structure = model_structure)

    fit <- tryCatch({
      brm(
        formula = brms_form,
        data = df_scaled,
        family = brms_family(),
        prior = prior_list,
        chains = active_sampler_controls$chains,
        iter = active_sampler_controls$iter,
        warmup = active_sampler_controls$warmup,
        control = list(adapt_delta = active_sampler_controls$adapt_delta, max_treedepth = active_sampler_controls$max_treedepth),
        seed = accrual_seed_for(
          if (row_is_remediation_target) {
            paste0("baseline_fit_brms_named_models_remediation_", row_target_key)
          } else {
            paste0("baseline_fit_brms_named_models_", row_target_key)
          },
          offset = i
        ),
        save_pars = save_pars(all = TRUE),
        refresh = 500
      )
    }, error = function(e) {
      message("[ERROR] Winsor model fitting crashed: ", e$message)
      write_failure_diag(row, e$message, n_obs_fit = n_obs_fit, n_firms_fit = n_firms_fit)
      NULL
    })

    if (is.null(fit)) {
      audit_row <- make_ma07_artifact_audit_row(
        row, model_filename, draws_filename,
        fit_exists_before = fit_exists_before,
        draws_exists_before = draws_exists_before,
        fit_exists_after = file.exists(model_filename),
        draws_exists_after = file.exists(draws_filename),
        fit_status = "FAILED",
        draw_generation_status = "SKIPPED",
        draw_generation_skip_reason = "fit failed before draw generation",
        hard_gate_status = "FAIL",
        hard_gate_reason = "fit object unavailable after brm() error"
      )
      upsert_ma07_artifact_audit(audit_row)
      if (isTRUE(row$Main_Stack_Inclusion)) {
        record_ma07_failure(row, audit_row)
        stop("[BLOCKER] Required main-stack model failed: ", model_key)
      }
      next
    }

    saveRDS(fit, model_filename)
    message("Saved winsor fit to: ", model_filename)
  }

  if (is.null(fit) && file.exists(model_filename)) {
    fit <- readRDS(model_filename)
  }
  if (is.null(fit)) next

  post_summary <- summary(fit)
  rhats <- post_summary$fixed[, "Rhat"]
  bulk_esses <- post_summary$fixed[, "Bulk_ESS"]
  tail_esses <- if ("Tail_ESS" %in% colnames(post_summary$fixed)) post_summary$fixed[, "Tail_ESS"] else rep(NA_real_, length(bulk_esses))
  if ("random" %in% names(post_summary) && !is.null(post_summary$random)) {
    for (group in names(post_summary$random)) {
      rhats <- c(rhats, post_summary$random[[group]][, "Rhat"])
      bulk_esses <- c(bulk_esses, post_summary$random[[group]][, "Bulk_ESS"])
      if ("Tail_ESS" %in% colnames(post_summary$random[[group]])) {
        tail_esses <- c(tail_esses, post_summary$random[[group]][, "Tail_ESS"])
      } else {
        tail_esses <- c(tail_esses, rep(NA_real_, nrow(post_summary$random[[group]])))
      }
    }
  }
  max_rhat <- suppressWarnings(max(rhats, na.rm = TRUE))
  min_ess <- suppressWarnings(min(bulk_esses, na.rm = TRUE))
  min_tail_ess <- suppressWarnings(min(tail_esses, na.rm = TRUE))
  if (!is.finite(max_rhat)) max_rhat <- NA_real_
  if (!is.finite(min_ess)) min_ess <- NA_real_
  if (!is.finite(min_tail_ess)) min_tail_ess <- NA_real_

  np <- nuts_params(fit)
  divergences <- sum(subset(np, Parameter == "divergent__")$Value)
  treedepths <- subset(np, Parameter == "treedepth__")$Value
  treedepth_warnings <- sum(treedepths >= max_treedepth)

  hard_gate_status <- classify_ma07_mcmc_gate(max_rhat, divergences, min_ess, min_tail_ess)
  hard_gate_reason <- ma07_mcmc_gate_reason(max_rhat, divergences, min_ess, min_tail_ess)
  converged <- hard_gate_status %in% c("PASS", "REVIEW")
  stacking_eligible <- converged
  if (identical(hard_gate_status, "REVIEW")) {
    message("[MA07 MCMC REVIEW] Model passed minimum MCMC thresholds but did not pass strict ESS marker: ",
            row_target_key, ". Reason: ", hard_gate_reason)
  }

  random_intercept_sd <- NA_real_
  if (grepl("Firm RE", row$Heterogeneity_Variant) && !is.null(post_summary$random$company)) {
    random_intercept_sd <- post_summary$random$company["sd(Intercept)", "Estimate"]
  }

  loo_diag <- reuse_existing_loo_diag(existing_diag_row)
  if (is.null(loo_diag)) {
    loo_res <- tryCatch(loo(fit), error = function(e) {
      message("[ERROR] LOO failed: ", e$message)
      NULL
    })
    loo_diag <- classify_loo_result(loo_res)
  } else if (backfill_diagnostics_only) {
    message("Reusing existing LOO metrics from diagnostics table for backfill: ", model_key)
  }

  diag_row <- data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Main_Stack_Inclusion = row$Main_Stack_Inclusion,
    Secondary_Robustness = row$Secondary_Robustness,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    N_Obs = as.integer(n_obs_fit),
    N_Firms = as.integer(n_firms_fit),
    Fit_Status = "SUCCESS",
    Rhat_Max = max_rhat,
    ESS_Min = min_ess,
    Min_Tail_ESS = min_tail_ess,
    Divergences = divergences,
    converged = converged,
    stacking_eligible = stacking_eligible,
    max_rhat = max_rhat,
    divergences = divergences,
    treedepth_warnings = treedepth_warnings,
    pareto_k_above_07 = loo_diag$pareto_k_above_07,
    loo_status = loo_diag$loo_status,
    loo_warning_reason = loo_diag$loo_warning_reason,
    random_intercept_sd = random_intercept_sd,
    elpd_loo = loo_diag$elpd_loo,
    error_message = NA_character_,
    Notes = row$Reason,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = phase_root,
    save_pars_all = isTRUE(fit$save_pars$all) || (is.list(fit$save_pars) && "all" %in% names(fit$save_pars) && isTRUE(fit$save_pars$all)),
    stringsAsFactors = FALSE
  )

  diagnostics_df <- diagnostics_df %>%
    filter(!(Model_ID == row$Model_ID &
               Target_Space == row$Target_Space &
               Sample_Group == row$Sample_Group &
               Heterogeneity_Variant == row$Heterogeneity_Variant)) %>%
    bind_rows(diag_row)
  write.csv(diagnostics_df, diag_path, row.names = FALSE)

  blocker_for_mcmc <- isTRUE(row$Main_Stack_Inclusion) &&
    (identical(hard_gate_status, "FAIL") ||
       (identical(hard_gate_status, "REVIEW") && isTRUE(ma07_strict_review_blocker)))
  if (blocker_for_mcmc) {
    audit_row <- make_ma07_artifact_audit_row(
      row, model_filename, draws_filename,
      fit_exists_before = fit_exists_before,
      draws_exists_before = draws_exists_before,
      fit_exists_after = file.exists(model_filename),
      draws_exists_after = file.exists(draws_filename),
      fit_status = "SUCCESS",
      max_rhat = max_rhat,
      divergences = divergences,
      treedepth_warnings = treedepth_warnings,
      min_bulk_ess = min_ess,
      min_tail_ess = min_tail_ess,
      converged = converged,
      stacking_eligible = stacking_eligible,
      draw_generation_attempted = FALSE,
      draw_generation_status = "SKIPPED",
      draw_generation_skip_reason = ifelse(identical(hard_gate_status, "FAIL"), "model is not stacking eligible", "strict review blocker enabled"),
      hard_gate_status = hard_gate_status,
      hard_gate_reason = hard_gate_reason
    )
    upsert_ma07_artifact_audit(audit_row)
    record_ma07_failure(row, audit_row)
    stop(
      if (identical(hard_gate_status, "REVIEW")) "[MA07 HARD GATE BLOCKER] Main-stack model failed strict REVIEW diagnostics: " else "[MA07 HARD GATE BLOCKER] Main-stack model failed minimum MCMC diagnostics: ",
      row_target_key,
      ". max_rhat=", sprintf("%.6f", max_rhat),
      ", divergences=", divergences,
      ", min_bulk_ess=", sprintf("%.2f", min_ess),
      ", min_tail_ess=", sprintf("%.2f", min_tail_ess),
      ". fit_exists_after=", file.exists(model_filename),
      ", draws_exists_after=", file.exists(draws_filename),
      ". Add this exact key to ACCRUAL_MCMC_REMEDIATION_TARGETS and rerun ma07: ",
      row_target_key
    )
  }

  regenerate_draws <- !backfill_diagnostics_only && stacking_eligible && (row_is_remediation_target || !file.exists(draws_filename) || force_refit)
  draw_generation_attempted <- FALSE
  draw_generation_status <- NA_character_
  draw_generation_skip_reason <- NA_character_
  if (regenerate_draws) {
    draw_generation_attempted <- TRUE
    message("Generating winsor posterior_epred and posterior_predict draws...")
    draw_error <- tryCatch({
      ep_draws <- posterior_epred(fit)
      pp_draws <- posterior_predict(fit)
      saveRDS(list(epred = ep_draws, predict = pp_draws), draws_filename)
      NULL
    }, error = function(e) conditionMessage(e))
    if (is.null(draw_error) && file.exists(draws_filename)) {
      message("Saved winsor draws to: ", draws_filename)
      draw_generation_status <- "GENERATED"
    } else {
      draw_generation_status <- "FAILED"
      draw_generation_skip_reason <- paste0("draw generation failed: ", draw_error)
      message("[MA07 DRAW ERROR] ", row_target_key, ": ", draw_generation_skip_reason)
    }
  } else {
    draw_generation_status <- "SKIPPED"
    draw_generation_skip_reason <- if (file.exists(draws_filename)) {
      "existing draws already present"
    } else if (!stacking_eligible) {
      "model is not stacking eligible"
    } else if (backfill_diagnostics_only) {
      "diagnostics-only/backfill mode"
    } else if (!isTRUE(row$Main_Stack_Inclusion)) {
      "model is not in active stacking space"
    } else if (remediation_mode && !row_is_remediation_target) {
      "remediation mode blocks non-target regeneration"
    } else {
      "draw generation not required by current mode"
    }
    message("[MA07 DRAW SKIP] ", row_target_key, ": ", draw_generation_skip_reason)
  }

  audit_row <- make_ma07_artifact_audit_row(
    row, model_filename, draws_filename,
    fit_exists_before = fit_exists_before,
    draws_exists_before = draws_exists_before,
    fit_exists_after = file.exists(model_filename),
    draws_exists_after = file.exists(draws_filename),
    fit_status = "SUCCESS",
    max_rhat = max_rhat,
    divergences = divergences,
    treedepth_warnings = treedepth_warnings,
    min_bulk_ess = min_ess,
    min_tail_ess = min_tail_ess,
    converged = converged,
    stacking_eligible = stacking_eligible,
    draw_generation_attempted = draw_generation_attempted,
    draw_generation_status = draw_generation_status,
    draw_generation_skip_reason = draw_generation_skip_reason,
    hard_gate_status = hard_gate_status,
    hard_gate_reason = hard_gate_reason
  )
  upsert_ma07_artifact_audit(audit_row)

  missing_required_draws <- isTRUE(row$Main_Stack_Inclusion) &&
    file.exists(model_filename) &&
    hard_gate_status %in% c("PASS", "REVIEW") &&
    !file.exists(draws_filename)
  if (missing_required_draws) {
    record_ma07_failure(row, audit_row)
    stop(
      "[MA07 DRAW ARTIFACT BLOCKER] Main-stack fit passed MCMC gate but expected draws are missing: ",
      row_target_key,
      ". fit_exists_after=", file.exists(model_filename),
      ", draws_exists_after=", file.exists(draws_filename),
      ", draws_path=", draws_filename,
      ". Add this exact key to ACCRUAL_MCMC_REMEDIATION_TARGETS and rerun ma07: ",
      row_target_key
    )
  }
}

diagnostics_df <- reconcile_step7_diagnostics(diagnostics_df, formulas_df)
write.csv(diagnostics_df, diag_path, row.names = FALSE)

message("\nSaving winsor coefficient table...")
coeff_df <- data.frame(
  Model_ID = character(),
  Model_Name = character(),
  Target_Space = character(),
  Heterogeneity_Variant = character(),
  Parameter = character(),
  Estimate = double(),
  Est_Error = double(),
  CI_Lower = double(),
  CI_Upper = double(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(formulas_df))) {
  row <- formulas_df[i, ]
  model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
  model_filename <- file.path(phase_root, "models", paste0("fit_", model_key, ".rds"))
  if (!file.exists(model_filename)) next
  fit <- readRDS(model_filename)
  fix_effects <- as.data.frame(fixef(fit))
  for (pname in rownames(fix_effects)) {
    coeff_df <- rbind(coeff_df, data.frame(
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Main_Stack_Inclusion = row$Main_Stack_Inclusion,
      Secondary_Robustness = row$Secondary_Robustness,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Parameter = pname,
      Estimate = fix_effects[pname, "Estimate"],
      Est_Error = fix_effects[pname, "Est.Error"],
      CI_Lower = fix_effects[pname, "Q2.5"],
      CI_Upper = fix_effects[pname, "Q97.5"],
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      Output_Root = phase_root,
      stringsAsFactors = FALSE
    ))
  }
}
write.csv(coeff_df, coeff_path, row.names = FALSE)

phase3_notes <- sprintf(
  paste0(
    "ma07 winsorized BRMS fit notes\n",
    "Winsorized samples are read from %s/tables/.\n",
    "Outputs are written to %s/.\n",
    "Diagnostics-only backfill mode: %s.\n",
    "Targeted MCMC remediation mode: %s.\n",
    "Predictors are z-standardized after winsorization using winsorized sample moments.\n",
    "Sampling settings: chains=%d, iter=%d, warmup=%d, adapt_delta=%.2f, max_treedepth=%d, canonical_seed=%d, effective_seed=%d.\n",
    "Prior_Set_ID: %s.\n",
    "Likelihood_Family: %s.\n",
    "Model_Structure: %s.\n",
    "Varying slopes are written separately under ACCRUAL_OUTPUT_ROOT/varyslopes and are not mixed into baseline stacking weights.\n",
    "Stacking eligibility requires ma07 hard gate PASS or REVIEW: max Rhat <= 1.01, divergences == 0, min bulk ESS >= 400, and min tail ESS >= 400.\n",
    "Step 7 diagnostics are computed from the winsorized input samples plus fitted .rds objects.\n",
    "N_Firms is intentionally computed from the input sample rather than fit$data so pooled models retain correct firm counts.\n",
    "Pareto-k warnings do not fail Step 7; they are recorded as loo_status='PSIS_REVIEW_REQUIRED'.\n",
    "Step 9 or grouped K-fold must review models flagged PSIS_REVIEW_REQUIRED before relying on PSIS-LOO.\n"
  ),
  input_winsor_root, phase_root, ifelse(backfill_diagnostics_only, "TRUE", "FALSE"),
  ifelse(length(remediation_targets) > 0, "TRUE", "FALSE"),
  chains, iter, warmup, adapt_delta, max_treedepth,
  baseline_rng_meta$Canonical_Seed, baseline_rng_meta$Effective_Seed,
  prior_set_id, likelihood_family, model_structure
)
notes_file <- if (run_varying_slope_models) {
  file.path(phase_root, "logs", "varyslopes_notes.txt")
} else {
  file.path(phase_root, "logs", "ma07_fit_notes_winsor.txt")
}
writeLines(phase3_notes, con = notes_file)

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

# Write baseline run manifest
manifest_notes <- character()
if (prior_pred_override_used) {
  manifest_notes <- c(manifest_notes, "prior predictive check FAIL bypassed")
}
if (backfill_diagnostics_only) {
  manifest_notes <- c(manifest_notes, "diagnostics backfill only; reused existing fit .rds objects without refitting")
}
if (length(remediation_targets) > 0) {
  manifest_notes <- c(manifest_notes, "one-time MCMC remediation; same seed as baseline; stronger sampler controls only; no model/prior/sample/formula changes")
}
is_deviant_config <- !identical(prior_set_id, "scale_aware_student_baseline_v1") || 
                     !identical(likelihood_family, "student") || 
                     !identical(model_structure, "pooled_random_intercept")
if (is_deviant_config && env_flag("ACCRUAL_ALLOW_DIAGNOSTIC_CONFIG", "FALSE")) {
  manifest_notes <- c(manifest_notes, sprintf("diagnostic config override used: actual prior_set_id=%s, actual family=%s, actual model_structure=%s", prior_set_id, likelihood_family, model_structure))
}
notes_str <- if (length(manifest_notes) > 0) paste(manifest_notes, collapse = "; ") else "normal baseline run"

manifest_path <- file.path(phase_root, "manifests", "baseline_manifest.csv")
write_run_manifest(
  path = manifest_path,
  scenario = "baseline",
  prior_set_id = prior_set_id,
  family = likelihood_family,
  model_structure = model_structure,
  model_list = unique(formulas_df$Model_ID),
  seed = baseline_rng_meta$Effective_Seed,
  sampling_config = if (length(remediation_targets) > 0) {
    sprintf(
      "baseline_chains=%d;baseline_iter=%d;baseline_warmup=%d;remediation_chains=%d;remediation_iter=%d;remediation_warmup=%d",
      baseline_sampler_controls$chains,
      baseline_sampler_controls$iter,
      baseline_sampler_controls$warmup,
      remediation_sampler_controls$chains,
      remediation_sampler_controls$iter,
      remediation_sampler_controls$warmup
    )
  } else {
    sprintf("chains=%d;iter=%d;warmup=%d", chains, iter, warmup)
  },
  status = "SUCCESS",
  notes = notes_str,
  input_paths = c(formulas_path, gate_csv_path),
  rng_context = baseline_rng_meta$RNG_Context,
  rng_offset = baseline_rng_meta$RNG_Offset
)
message("Saved baseline manifest to: ", manifest_path)

ma07_artifact_audit <- if (file.exists(ma07_artifact_audit_path)) {
  read.csv(ma07_artifact_audit_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  ma07_artifact_audit
}
processed_audit <- ma07_artifact_audit[!is.na(ma07_artifact_audit$Fit_Status) & ma07_artifact_audit$Fit_Status != "PENDING", , drop = FALSE]
pass_count <- sum(processed_audit$hard_gate_status == "PASS", na.rm = TRUE)
review_count <- sum(processed_audit$hard_gate_status == "REVIEW", na.rm = TRUE)
fail_count <- sum(processed_audit$hard_gate_status == "FAIL", na.rm = TRUE)
existing_fit_missing_draws <- sum(processed_audit$fit_exists_before %in% TRUE & processed_audit$draws_exists_before %in% FALSE, na.rm = TRUE)
draws_generated <- sum(processed_audit$draw_generation_status == "GENERATED", na.rm = TRUE)
draws_skipped <- sum(processed_audit$draw_generation_status == "SKIPPED", na.rm = TRUE)

cat("\n[MA07 SUMMARY]\n")
cat("Processed models: ", nrow(processed_audit), "\n", sep = "")
cat("MCMC gate PASS: ", pass_count, " REVIEW: ", review_count, " FAIL: ", fail_count, "\n", sep = "")
cat("Existing fits with missing draws before ma07: ", existing_fit_missing_draws, "\n", sep = "")
cat("Draws generated: ", draws_generated, "\n", sep = "")
cat("Draw generation skipped: ", draws_skipped, "\n", sep = "")
cat("Artifact audit: ", ma07_artifact_audit_path, "\n", sep = "")
cat("Hard-gate failures: ", ma07_hard_gate_failures_path, "\n", sep = "")
cat("Suggested remediation helper: ", ma07_remediation_helper_path, "\n", sep = "")

cat("\n[SUCCESS] ma07 winsorized model fitting completed.\n")
phase_end("ma07", "Fit baseline brms models")
