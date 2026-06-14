# -----------------------------------------------------------------------------
# Script: 00_v3_winsor_helpers.R
# Purpose: Shared helpers for the v3 winsorized robustness pipeline.
# -----------------------------------------------------------------------------

env_value_v3 <- function(name, default) {
  val <- Sys.getenv(name, unset = default)
  if (!nzchar(val)) default else val
}

env_flag_v3 <- function(name, default = "FALSE") {
  toupper(env_value_v3(name, default)) %in% c("TRUE", "1", "YES", "Y")
}

v3_data_path <- env_value_v3("V3_DATA_PATH", file.path("data", "raw", "data.xlsx"))
v3_original_root <- env_value_v3("V3_BASELINE_ROOT", file.path("out", "interim", "baseline"))
v3_output_root <- env_value_v3("V3_OUTPUT_ROOT", file.path("out", "interim", "winsor"))
v3_input_winsor_root <- env_value_v3("V3_INPUT_WINSOR_ROOT", file.path("out", "interim", "winsor"))
v3_reports_root <- env_value_v3("V3_REPORTS_ROOT", "reports")
v3_accruals_root <- env_value_v3("V3_ACCRUALS_ROOT", "accruals")
v3_method_design_root <- env_value_v3("V3_METHOD_DESIGN_ROOT", file.path("out", "manifests", "method_design"))
v3_prior_set_id <- env_value_v3("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1")
v3_likelihood_family <- tolower(env_value_v3("V3_FAMILY", "student"))
v3_model_structure <- env_value_v3("V3_MODEL_STRUCTURE", "pooled_random_intercept")
v3_run_varying_slopes <- env_flag_v3("V3_RUN_VARYING_SLOPES", "FALSE")
v3_varyslope_scope <- toupper(env_value_v3("V3_VARYSLOPE_SCOPE", "LEADING_ONLY"))
v3_varyslope_group <- env_value_v3("V3_VARYSLOPE_GROUP", "industry_year")
v3_force_refit <- env_flag_v3("V3_FORCE_REFIT", "FALSE")
v3_prior_predictive_mode <- toupper(env_value_v3("V3_PRIOR_PREDICTIVE_MODE", "REPRESENTATIVE"))
v3_prior_pred_n_draws <- as.integer(env_value_v3("V3_PRIOR_PRED_N_DRAWS", "1000"))
v3_stacking_mixture_draws <- as.integer(env_value_v3("V3_STACKING_MIXTURE_DRAWS", "12000"))

if (is.na(v3_prior_pred_n_draws) || v3_prior_pred_n_draws <= 0) v3_prior_pred_n_draws <- 1000L
if (is.na(v3_stacking_mixture_draws) || v3_stacking_mixture_draws <= 0) v3_stacking_mixture_draws <- 12000L
if (!v3_likelihood_family %in% c("gaussian", "student")) {
  stop("[BLOCKER] V3_FAMILY must be 'gaussian' or 'student'.")
}

v3_winsor_root <- v3_output_root
v3_varyslopes_root <- file.path(v3_output_root, "varyslopes")

v3_baseline_dirs <- file.path(
  v3_original_root,
  c("", "tables", "models", "draws", "figures", "logs", "validation", "appendix")
)

v3_winsor_dirs <- file.path(
  v3_winsor_root,
  c(
    "",
    "tables",
    "models",
    "draws",
    "draws/loo_cache",
    "figures",
    "logs",
    "validation",
    "lofo",
    "lofo/tables",
    "lofo/logs",
    "lofo/figures",
    "lofo/cache",
    "kfold_firm",
    "sensitivity",
    "sensitivity/tables",
    "sensitivity/logs",
    "sensitivity/manifests",
    "sensitivity/reports",
    "sensitivity/cache",
    "varyslopes",
    "varyslopes/tables",
    "varyslopes/models",
    "varyslopes/draws",
    "varyslopes/logs",
    "varyslopes/figures",
    "varyslopes/cache"
  )
)

v3_sensitivity_scenario_ids <- c("baseline", "tight", "wide")

v3_main_model_ids_for_space <- function(target_space) {
  if (identical(target_space, "ex_post")) return(c("M01", "M02", "M03", "M04", "M05", "M06", "M07"))
  if (identical(target_space, "real_time")) return(c("M01", "M02", "M03", "M07", "M09"))
  character()
}

v3_sensitivity_scenarios <- function() {
  data.frame(
    Scenario = v3_sensitivity_scenario_ids,
    Prior_Set_ID = c(
      "scale_aware_student_baseline_v1",
      "scale_aware_student_tight_v1",
      "scale_aware_student_wide_v1"
    ),
    Likelihood_Family = "student",
    Model_Structure = "pooled_random_intercept",
    Manuscript_Use = "final_prior_sensitivity",
    Scenario_Description = c(
      "Baseline scale-aware weakly informative Student-t prior.",
      "Tight scale-aware Student-t prior with about half-scale slope and intercept shrinkage.",
      "Wide but still plausible scale-aware Student-t prior for final sensitivity."
    ),
    stringsAsFactors = FALSE
  )
}

v3_sensitivity_root <- function(scenario = NULL, root = v3_output_root) {
  base <- file.path(root, "sensitivity")
  if (is.null(scenario) || !nzchar(scenario)) return(base)
  file.path(base, scenario)
}

ensure_v3_sensitivity_dirs <- function(scenario = NULL, root = v3_output_root) {
  base_dirs <- file.path(v3_sensitivity_root(NULL, root), c("", "tables", "logs", "manifests", "reports", "cache"))
  for (d in base_dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  scenarios <- if (is.null(scenario) || !nzchar(scenario)) v3_sensitivity_scenario_ids else scenario
  subdirs <- c("", "prior_predictive", "fits", "models", "draws", "diagnostics", "stacking", "DA", "validation", "logs", "manifests", "cache", "tables")
  for (sc in scenarios) {
    for (d in file.path(v3_sensitivity_root(sc, root), subdirs)) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(TRUE)
}

ensure_v3_baseline_dirs <- function() {
  for (d in v3_baseline_dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

v3_baseline_table_path <- function(file_name) {
  file.path(v3_original_root, "tables", file_name)
}

v3_baseline_log_path <- function(file_name) {
  file.path(v3_original_root, "logs", file_name)
}

v3_reports_path <- function(...) {
  file.path(v3_reports_root, ...)
}

v3_baseline_accruals_path <- function(file_name = "final_v3_uncertainty_adjusted_accruals_winsor.csv") {
  file.path(v3_accruals_root, "baseline", file_name)
}

v3_sensitivity_accruals_path <- function(scenario, file_name = NULL) {
  if (is.null(file_name) || !nzchar(file_name)) {
    file_name <- paste0("final_v3_sensitivity_uncertainty_adjusted_accruals_", scenario, ".csv")
  }
  file.path(v3_accruals_root, "sensitivity", scenario, file_name)
}

continuous_vars_to_winsor <- c(
  "TA_scaled",
  "inv_A_lag",
  "dREV_scaled",
  "dREC_scaled",
  "dREV_dREC_scaled",
  "PPE_scaled",
  "ROA_lag",
  "ROA_curr",
  "CFO_lag_scaled",
  "CFO_curr_scaled",
  "CFO_lead_scaled",
  "Size",
  "operating_cycle",
  "sales_growth",
  "revenue_growth",
  "sd_REV",
  "sd_CFO"
)

binary_vars_do_not_winsor <- c("NEG_CFO", "NEG_EARN")

pred_vars_v3 <- c(
  "inv_A_lag", "dREV_scaled", "dREC_scaled", "dREV_dREC_scaled", "PPE_scaled",
  "ROA_lag", "CFO_lag_scaled", "CFO_curr_scaled", "CFO_lead_scaled", "Size",
  "operating_cycle", "sales_growth", "sd_REV", "sd_CFO"
)

appendix1_vars_v3 <- c(
  "TA_scaled",
  "dREV_scaled",
  "dREC_scaled",
  "dREV_dREC_scaled",
  "PPE_scaled",
  "CFO_lag_scaled",
  "CFO_curr_scaled",
  "CFO_lead_scaled",
  "ROA_lag",
  "Size",
  "operating_cycle",
  "sales_growth"
)

ensure_v3_winsor_dirs <- function() {
  for (d in v3_winsor_dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

prior_registry_v3 <- function() {
  row <- function(id, cls, dist, loc, scale, applies, family, role, use, notes) {
    data.frame(
      Prior_Set_ID = id,
      Parameter_Class = cls,
      Prior_Distribution = dist,
      Location = loc,
      Scale_or_Rate = as.character(scale),
      Applies_To = applies,
      Likelihood_Family = family,
      Prior_Set_Role = role,
      Manuscript_Use = use,
      Notes = notes,
      stringsAsFactors = FALSE
    )
  }

  bind_rows_if_available <- function(rows) {
    if (requireNamespace("dplyr", quietly = TRUE)) {
      return(dplyr::bind_rows(rows))
    }
    do.call(rbind, rows)
  }

  bind_rows_if_available(list(
    row("wide_original", "b", "normal(0, 2.5)", 0, 2.5, "All slope coefficients", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Old Gaussian wide-prior result preserved as diagnostic only."),
    row("wide_original", "Intercept", "normal(0, 2.5)", 0, 2.5, "Model intercept", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Old Gaussian wide-prior result preserved as diagnostic only."),
    row("wide_original", "sigma", "exponential(1)", NA, 1, "Residual standard deviation", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Old Gaussian wide-prior result preserved as diagnostic only."),
    row("wide_original", "sd", "exponential(1)", NA, 1, "Group-level standard deviations", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Applied to random-effect variants."),
    row("scale_aware_student_baseline_v1", "b", "normal(0, 0.10)", 0, 0.10, "All slope coefficients", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Scale-aware prior for accruals scaled by lagged assets."),
    row("scale_aware_student_baseline_v1", "Intercept", "normal(0, 0.10)", 0, 0.10, "Model intercept", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Scale-aware prior for centered accrual level."),
    row("scale_aware_student_baseline_v1", "sigma", "exponential(10)", NA, 10, "Residual standard deviation", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Favors residual scale plausible for TA scaled by lagged assets."),
    row("scale_aware_student_baseline_v1", "sd", "exponential(10)", NA, 10, "Group-level standard deviations", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Applied to random intercepts or varying slopes when present."),
    row("scale_aware_student_baseline_v1", "nu", "gamma(2, 0.1)", NA, "shape=2; rate=0.1", "Student-t degrees of freedom", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Lets the likelihood absorb heavier tails without making priors diffuse."),
    row("scale_aware_student_baseline_v1", "cor", "lkj(2)", NA, 2, "Correlated random effects", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Used for optional Breuer-like varying-slope robustness."),
    row("scale_aware_student_tight_v1", "b", "normal(0, 0.05)", 0, 0.05, "All slope coefficients", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter slope shrinkage."),
    row("scale_aware_student_tight_v1", "Intercept", "normal(0, 0.05)", 0, 0.05, "Model intercept", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter intercept shrinkage."),
    row("scale_aware_student_tight_v1", "sigma", "exponential(20)", NA, 20, "Residual standard deviation", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter residual scale."),
    row("scale_aware_student_tight_v1", "sd", "exponential(20)", NA, 20, "Group-level standard deviations", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter group-level scale."),
    row("scale_aware_student_tight_v1", "nu", "gamma(2, 0.1)", NA, "shape=2; rate=0.1", "Student-t degrees of freedom", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Same Student-t tail prior as baseline."),
    row("scale_aware_student_tight_v1", "cor", "lkj(2)", NA, 2, "Correlated random effects", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Used for optional varying slopes."),
    row("scale_aware_student_wide_v1", "b", "normal(0, 0.25)", 0, 0.25, "All slope coefficients", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider but still scale-aware slope prior."),
    row("scale_aware_student_wide_v1", "Intercept", "normal(0, 0.25)", 0, 0.25, "Model intercept", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider but still scale-aware intercept prior."),
    row("scale_aware_student_wide_v1", "sigma", "exponential(5)", NA, 5, "Residual standard deviation", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider residual scale."),
    row("scale_aware_student_wide_v1", "sd", "exponential(5)", NA, 5, "Group-level standard deviations", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider group-level scale."),
    row("scale_aware_student_wide_v1", "nu", "gamma(2, 0.1)", NA, "shape=2; rate=0.1", "Student-t degrees of freedom", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Same Student-t tail prior as baseline."),
    row("scale_aware_student_wide_v1", "cor", "lkj(2)", NA, 2, "Correlated random effects", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Used for optional varying slopes.")
  ))
}

default_prior_specification_v3 <- function() {
  prior_registry_v3()
}

write_prior_registry_v3 <- function(root = v3_output_root) {
  ensure_v3_winsor_dirs()
  out <- file.path(root, "tables", "table_v3_prior_sets.csv")
  write.csv(prior_registry_v3(), out, row.names = FALSE)
  out
}

prior_set_rows_v3 <- function(prior_set_id = v3_prior_set_id) {
  rows <- prior_registry_v3()
  rows <- rows[rows$Prior_Set_ID == prior_set_id, , drop = FALSE]
  if (nrow(rows) == 0) stop("[BLOCKER] Unknown V3_PRIOR_SET_ID: ", prior_set_id)
  rows
}

default_prior_list_v3 <- function(heterogeneity_variant = "", model_structure = v3_model_structure,
                                  prior_set_id = v3_prior_set_id, family = v3_likelihood_family) {
  rows <- prior_set_rows_v3(prior_set_id)
  prior_for <- function(cls) rows$Prior_Distribution[rows$Parameter_Class == cls][1]
  prior_list <- c(
    brms::prior(prior_for("b"), class = "b"),
    brms::prior(prior_for("Intercept"), class = "Intercept"),
    brms::prior(prior_for("sigma"), class = "sigma")
  )
  has_group_effect <- grepl("Firm RE", heterogeneity_variant, fixed = TRUE) ||
    identical(model_structure, "breuer_varying_slopes")
  if (has_group_effect && any(rows$Parameter_Class == "sd")) {
    prior_list <- c(prior_list, brms::prior(prior_for("sd"), class = "sd"))
  }
  if (identical(tolower(family), "student") && any(rows$Parameter_Class == "nu")) {
    prior_list <- c(prior_list, brms::prior(prior_for("nu"), class = "nu"))
  }
  if (identical(model_structure, "breuer_varying_slopes") && any(rows$Parameter_Class == "cor")) {
    prior_list <- c(prior_list, brms::prior(prior_for("cor"), class = "cor"))
  }
  prior_list
}

brms_family_v3 <- function(family = v3_likelihood_family) {
  if (identical(tolower(family), "student")) brms::student() else brms::gaussian()
}

metadata_columns_v3 <- function() {
  data.frame(
    Prior_Set_ID = v3_prior_set_id,
    Likelihood_Family = v3_likelihood_family,
    Model_Structure = v3_model_structure,
    Output_Root = v3_output_root,
    stringsAsFactors = FALSE
  )
}

validate_v3_final_analysis_config <- function(context = "final analysis", final_mode = TRUE) {
  is_invalid <- !identical(v3_prior_set_id, "scale_aware_student_baseline_v1") || 
                !identical(v3_likelihood_family, "student") || 
                !identical(v3_model_structure, "pooled_random_intercept")
  if (!is_invalid) return(invisible(TRUE))

  msg <- paste0(
    "[CONFIG WARNING] ", context, " has a deviant/diagnostic configuration: ",
    "(V3_PRIOR_SET_ID=", v3_prior_set_id, ", V3_FAMILY=", v3_likelihood_family, ", V3_MODEL_STRUCTURE=", v3_model_structure, "). ",
    "The standard final-analysis config should have V3_PRIOR_SET_ID='scale_aware_student_baseline_v1', ",
    "V3_FAMILY='student', and V3_MODEL_STRUCTURE='pooled_random_intercept'."
  )
  if (final_mode && !env_flag_v3("V3_ALLOW_DIAGNOSTIC_CONFIG", "FALSE")) {
    stop(msg, " Set V3_ALLOW_DIAGNOSTIC_CONFIG=TRUE only for an intentional diagnostic run.")
  }
  warning(msg, call. = FALSE)
  invisible(FALSE)
}

selected_sensitivity_scenarios_v3 <- function() {
  requested <- env_value_v3("V3_SENS_SCENARIO", "")
  scenarios <- v3_sensitivity_scenarios()
  if (!nzchar(requested) || toupper(requested) == "ALL") return(scenarios)
  keep <- trimws(unlist(strsplit(requested, ",", fixed = TRUE)))
  unknown <- setdiff(keep, scenarios$Scenario)
  if (length(unknown) > 0) stop("[BLOCKER] Unknown V3_SENS_SCENARIO: ", paste(unknown, collapse = ", "))
  scenarios[scenarios$Scenario %in% keep, , drop = FALSE]
}

v3_package_versions <- function(pkgs = c("brms", "rstan", "cmdstanr", "posterior", "loo", "bayesplot", "dplyr", "readr", "tibble", "ggplot2")) {
  vals <- vapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return("NOT_INSTALLED")
    as.character(utils::packageVersion(pkg))
  }, character(1))
  paste(paste(names(vals), vals, sep = "="), collapse = "; ")
}

v3_file_fingerprint <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (requireNamespace("digest", quietly = TRUE)) {
    return(paste0("sha256:", digest::digest(path, algo = "sha256", file = TRUE)))
  }
  info <- file.info(path)
  paste0("mtime:", format(info$mtime[1], "%Y-%m-%d %H:%M:%S %z"), ";size:", as.numeric(info$size[1]))
}

v3_session_info_string <- function() {
  paste(capture.output(sessionInfo()), collapse = "\n")
}

v3_metadata_matches <- function(path, expected) {
  if (!file.exists(path)) return(FALSE)
  old <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(old) == 0) return(FALSE)
  for (nm in names(expected)) {
    if (!nm %in% names(old)) return(FALSE)
    old_val <- as.character(old[[nm]][1])
    new_val <- as.character(expected[[nm]][1])
    if (!identical(old_val, new_val)) return(FALSE)
  }
  TRUE
}

write_v3_run_manifest <- function(path, scenario, prior_set_id, family, model_structure,
                                  model_list, seed, sampling_config, status,
                                  notes = "", input_paths = character()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  input_hash <- if (length(input_paths) > 0) {
    paste(paste(input_paths, vapply(input_paths, v3_file_fingerprint, character(1)), sep = "="), collapse = "; ")
  } else {
    NA_character_
  }
  manifest <- data.frame(
    Scenario = scenario,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = family,
    Model_Structure = model_structure,
    Model_List = paste(model_list, collapse = ","),
    Seed = seed,
    Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    Input_Hash = input_hash,
    Package_Versions = v3_package_versions(),
    R_Version = paste(R.version$major, R.version$minor, sep = "."),
    Sampling_Config = sampling_config,
    Status = status,
    Notes = notes,
    Session_Info = v3_session_info_string(),
    stringsAsFactors = FALSE
  )
  write.csv(manifest, path, row.names = FALSE)
  path
}

write_v3_pipeline_index <- function() {
  dir.create(v3_method_design_root, recursive = TRUE, showWarnings = FALSE)
  pipeline <- data.frame(
    Order = sprintf("%02d", 0:22),
    Script = c(
      "00_v3_winsor_helpers.R",
      "01_v3_setup_and_registry.R",
      "02_v3_build_common_sample.R",
      "03_v3_audit_cogs_inv_operating_cycle_after_fix.R",
      "04_v3_define_named_models.R",
      "05_v3_winsorize_common_samples.R",
      "06_v3_prior_predictive_checks_winsor.R",
      "07_v3_fit_brms_named_models_winsor.R",
      "08_v3_mcmc_diagnostics_winsor.R",
      "09_v3_loo_stacking_winsor.R",
      "10_v3_construct_uncertainty_adjusted_DA_winsor.R",
      "11_v3_posterior_predictive_checks_winsor.R",
      "12_v3_lofo_stacking_winsor.R",
      "13_v3_grouped_kfold_firm_winsor.R",
      "14_v3_sensitivity_prior_predictive_winsor.R",
      "15_v3_sensitivity_refit_prior_scenarios_winsor.R",
      "16_v3_sensitivity_mcmc_diagnostics_winsor.R",
      "17_v3_sensitivity_stacking_winsor.R",
      "18_v3_sensitivity_construct_DA_winsor.R",
      "19_v3_sensitivity_validation_winsor.R",
      "20_v3_sensitivity_report_winsor.R",
      "21_v3_validation_on_scaleaware_student_DA.R",
      "22_reset_and_rerun_after_cogs_inv_fix.R"
    ),
    Role = c(
      "Shared helpers and registries",
      "Setup and model registry",
      "Build common samples",
      "COGS/INV audit",
      "Define model formulas",
      "Winsorize common samples",
      "Baseline prior predictive checks",
      "Baseline brms fits",
      "Baseline MCMC diagnostics",
      "Baseline LOO stacking",
      "Baseline uncertainty-adjusted DA",
      "Baseline posterior predictive checks",
      "Baseline grouped PSIS-LOFO",
      "Baseline exact grouped K-fold",
      "Sensitivity prior predictive gate",
      "Sensitivity full refits by prior scenario",
      "Sensitivity MCMC diagnostics gate",
      "Sensitivity LOO/stacking by scenario",
      "Sensitivity DA reconstruction",
      "Sensitivity validation/outcome tests",
      "Sensitivity report",
      "Baseline validation",
      "Reset/orchestrator"
    ),
    Active = TRUE,
    stringsAsFactors = FALSE
  )
  write.csv(pipeline, file.path(v3_method_design_root, "pipeline_index_v3.csv"), row.names = FALSE)

  mapping <- data.frame(
    Old_Script = c(
      "14_v3_sensitivity_analysis_winsor.R",
      "15_v3_validation_on_scaleaware_student_DA.R",
      "16_reset_and_rerun_after_cogs_inv_fix.R"
    ),
    New_Script = c(
      "14_v3_sensitivity_prior_predictive_winsor.R; 15_v3_sensitivity_refit_prior_scenarios_winsor.R; 16_v3_sensitivity_mcmc_diagnostics_winsor.R; 17_v3_sensitivity_stacking_winsor.R; 18_v3_sensitivity_construct_DA_winsor.R; 19_v3_sensitivity_validation_winsor.R; 20_v3_sensitivity_report_winsor.R",
      "21_v3_validation_on_scaleaware_student_DA.R",
      "22_reset_and_rerun_after_cogs_inv_fix.R"
    ),
    Status = c("replaced_by_full_refit_sensitivity_workflow", "renumbered", "renumbered"),
    Deprecated_But_Kept = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  write.csv(mapping, file.path(v3_method_design_root, "script_renaming_mapping.csv"), row.names = FALSE)

  readme_lines <- c(
    "# v3 pipeline index",
    "",
    "Active scripts use numeric prefixes only. No letter suffixes are used in script numbers.",
    "",
    "| Order | Script | Role |",
    "|---|---|---|",
    sprintf("| %s | `%s` | %s |", pipeline$Order, pipeline$Script, pipeline$Role),
    "",
    "Sensitivity phases 14-20 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `V3_DRY_RUN=FALSE` and the relevant phase is launched intentionally.",
    "",
    paste0("Old-to-new mapping is written to `", file.path(v3_method_design_root, "script_renaming_mapping.csv"), "`.")
  )
  writeLines(readme_lines, "scripts/v3/README_pipeline_index.md")
  invisible(pipeline)
}

winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  qs <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  pmin(pmax(x, qs[1]), qs[2])
}

safe_variant_name <- function(x) {
  gsub(" ", "_", gsub("[()|]", "", x))
}

model_key_v3 <- function(model_id, target_space, heterogeneity_variant, suffix = NULL) {
  key <- sprintf("%s_%s_%s", model_id, target_space, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

model_key_v3_sampled <- function(model_id, target_space, sample_group, heterogeneity_variant, suffix = NULL) {
  if (is.null(sample_group) || is.na(sample_group) || !nzchar(sample_group)) sample_group <- "main_common"
  key <- sprintf("%s_%s_%s_%s", model_id, target_space, sample_group, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

standardize_predictors_v3 <- function(df, pred_vars = pred_vars_v3) {
  for (v in pred_vars) {
    if (v %in% colnames(df)) {
      m <- mean(df[[v]], na.rm = TRUE)
      s <- sd(df[[v]], na.rm = TRUE)
      df[[paste0(v, "_std")]] <- if (!is.na(s) && s > 0) (df[[v]] - m) / s else 0
    }
  }
  df
}

fix_formula_v3 <- function(formula_str, pred_vars = pred_vars_v3, prefactor = FALSE) {
  if (prefactor) {
    formula_str <- gsub("factor\\(industry\\)", "industry_f", formula_str)
    formula_str <- gsub("factor\\(year\\)", "year_f", formula_str)
  }
  for (v in pred_vars) {
    formula_str <- gsub(paste0("\\b", v, "\\b"), paste0(v, "_std"), formula_str)
  }
  formula_str
}

read_winsor_sample <- function(sample_file, prefactor = FALSE, root = v3_input_winsor_root) {
  path <- file.path(root, "tables", sample_file)
  if (!file.exists(path)) stop("[BLOCKER] Winsorized sample file missing: ", path)
  df <- read.csv(path, stringsAsFactors = FALSE)
  df <- standardize_predictors_v3(df)
  if (prefactor) {
    df$industry_f <- factor(df$industry)
    df$year_f <- factor(df$year)
  }
  df
}

prepare_varying_slope_data_v3 <- function(df, group = v3_varyslope_group) {
  if (identical(group, "industry_year")) {
    if (!all(c("industry", "year") %in% names(df))) {
      stop("[BLOCKER] industry_year varying slopes require industry and year columns.")
    }
    df$industry_year_id <- interaction(df$industry, df$year, drop = TRUE)
  }
  df
}

varying_slope_formula_v3 <- function(formula_str, group = v3_varyslope_group) {
  parts <- strsplit(formula_str, "~", fixed = TRUE)[[1]]
  if (length(parts) != 2) stop("[BLOCKER] Cannot parse formula for varying slopes: ", formula_str)
  rhs <- trimws(parts[2])
  rhs <- gsub("\\+\\s*factor\\(industry\\)", "", rhs)
  rhs <- gsub("\\+\\s*factor\\(year\\)", "", rhs)
  rhs <- gsub("\\+\\s*\\(1\\s*\\|\\s*company\\)", "", rhs)
  rhs <- trimws(gsub("\\s+", " ", rhs))
  group_var <- if (identical(group, "firm")) "company" else "industry_year_id"
  sprintf("TA_scaled ~ 1 + %s + (1 + %s | %s)", rhs, rhs, group_var)
}

varying_slope_candidate_v3 <- function(model_id, target_space) {
  if (identical(v3_varyslope_scope, "FULL")) return(TRUE)
  paste(model_id, target_space) %in% c(
    "M06 ex_post",
    "M07 ex_post",
    "M07 real_time",
    "M09 real_time",
    "M01 ex_post",
    "M01 real_time"
  )
}

describe_numeric <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(c(N = 0, Mean = NA, SD = NA, Min = NA, P01 = NA, P05 = NA, P25 = NA,
             Median = NA, P75 = NA, P95 = NA, P99 = NA, Max = NA))
  }
  qs <- quantile(x, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99),
                 na.rm = TRUE, names = FALSE, type = 7)
  c(
    N = length(x),
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    P01 = qs[1],
    P05 = qs[2],
    P25 = qs[3],
    Median = qs[4],
    P75 = qs[5],
    P95 = qs[6],
    P99 = qs[7],
    Max = max(x)
  )
}

extract_weight_variant <- function(model_name, heterogeneity_variant = NULL) {
  if (!is.null(heterogeneity_variant) && !is.na(heterogeneity_variant) && nzchar(heterogeneity_variant)) {
    return(heterogeneity_variant)
  }
  if (grepl("Firm RE", model_name)) return("Firm RE (Random Intercept + Year FE)")
  if (grepl("Pooled", model_name)) return("Pooled (Industry + Year FE)")
  NA_character_
}

extract_base_model_name <- function(model_name) {
  sub(" \\((Firm RE|Pooled).*$", "", model_name)
}

read_original_weight_file <- function(space) {
  if (space == "ex_post") {
    candidates <- c(
      file.path(v3_original_root, "tables", "table_v3_stacking_weights_ex_post_corrected.csv"),
      file.path(v3_original_root, "tables", "table_v3_stacking_weights_ex_post.csv")
    )
  } else {
    candidates <- c(
      file.path(v3_original_root, "tables", "table_v3_stacking_weights_real_time_corrected.csv"),
      file.path(v3_original_root, "tables", "table_v3_stacking_weights_real_time.csv")
    )
  }
  source_path <- candidates[file.exists(candidates)][1]
  if (is.na(source_path)) stop("[BLOCKER] No original weight file found for space: ", space)
  df <- read.csv(source_path, stringsAsFactors = FALSE)
  df$Original_Weight_Source <- source_path
  df
}

classify_model_family <- function(model_id, model_name) {
  if (model_id %in% c("M01", "M02", "M03")) return("Jones-family")
  if (model_id %in% c("M04", "M05", "M06")) return("Cash-flow/McNichols-family")
  if (model_id == "M07") return("Ball-Shivakumar/asymmetry")
  if (model_id == "M09") return("No-lookahead/real-time")
  if (model_id == "M08") return("Secondary volatility")
  if (model_id == "M10") return("Secondary operating-cycle")
  model_name
}

write_method_design_files_v3 <- function() {
  design_root <- v3_method_design_root
  dir.create(design_root, recursive = TRUE, showWarnings = FALSE)
  differences <- data.frame(
    Dimension = c(
      "Scaling",
      "Model structure",
      "Outlier handling",
      "Model space",
      "Output",
      "Cross-validation",
      "Priors"
    ),
    Original_AccForUncertaintyCode = c(
      "Firm-demeaned, truncated, standardized variables.",
      "Hierarchical varying-coefficient / varying-slope model by group.",
      "Truncation after firm demeaning.",
      "Original AccForUncertaintyCode model set.",
      "NDA posterior mean and posterior SD.",
      "Observation-level PSIS-LOO stacking.",
      "Diffuse Gaussian prior used in the original implementation."
    ),
    This_Project = c(
      "Winsorized variables and scale-aware priors on accruals scaled by lagged assets.",
      "brms extension with pooled, random-intercept, and optional Breuer-like varying-slope variants.",
      "1/99 winsorization to preserve sample size in Vietnam.",
      "Vietnam-feasible two-tier model space, with M10/M08 secondary.",
      "NDA mean/estimation uncertainty plus posterior-predictive tail flags as an extension.",
      "Row-level LOO plus grouped PSIS-LOFO and optional exact grouped K-fold.",
      "Original diffuse prior retained only as diagnostic; manuscript baseline uses scale-aware priors after prior predictive checks."
    ),
    Implication = c(
      "The scale is tailored to Vietnamese listed-firm accrual variables.",
      "The Breuer-style structure is available as robustness, not default baseline.",
      "The corrected design avoids losing already limited Vietnam observations.",
      "The main stack remains feasible without imposing M08/M10 restrictions on all models.",
      "The output keeps the original NDA mean/uncertainty concept and adds predictive-tail diagnostics.",
      "Panel dependence is handled through grouped diagnostics and optional exact refits.",
      "Wide-prior Gaussian outputs are preserved as diagnostic only."
    ),
    stringsAsFactors = FALSE
  )
  write.csv(differences, file.path(design_root, "differences_from_AccForUncertaintyCode.csv"), row.names = FALSE)
  writeLines(c(
    "This study adapts the Bayesian model-averaging framework of AccForUncertaintyCode to the Vietnamese listed-firm setting. It differs from the original implementation in sample construction, scaling, outlier handling, model space, posterior predictive abnormality classification, and panel-dependence robustness checks.",
    "",
    "The analysis is therefore positioned as an extension/adaptation, not a replication. The corrected v3 design preserves corrected COGS/INV data, the two-tier sample design, the exclusion of M08 and M10 from main stacks, and the treatment of existing wide-prior Gaussian outputs as diagnostic only."
  ), file.path(design_root, "method_note_adaptation_not_replication.txt"))
  write_v3_pipeline_index()
}
