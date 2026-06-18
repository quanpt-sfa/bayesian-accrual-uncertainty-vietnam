# -----------------------------------------------------------------------------
# Script: 00_helpers.R
# Purpose: Shared helpers for the accrual uncertainty pipeline.
# -----------------------------------------------------------------------------

env_value <- function(name, default) {
  val <- Sys.getenv(name, unset = default)
  if (!nzchar(val)) default else val
}

env_flag <- function(name, default = "FALSE") {
  toupper(env_value(name, default)) %in% c("TRUE", "1", "YES", "Y")
}

data_path <- env_value("ACCRUAL_DATA_PATH", file.path("data", "raw", "data.xlsx"))
baseline_root <- env_value("ACCRUAL_BASELINE_ROOT", file.path("out", "interim", "baseline"))
output_root <- env_value("ACCRUAL_OUTPUT_ROOT", file.path("out", "interim", "winsor"))
input_winsor_root <- env_value("ACCRUAL_INPUT_WINSOR_ROOT", file.path("out", "interim", "winsor"))
reports_root <- env_value("ACCRUAL_REPORTS_ROOT", "reports")
accruals_root <- env_value("ACCRUAL_ACCRUALS_ROOT", "accruals")
method_design_root <- env_value("ACCRUAL_METHOD_DESIGN_ROOT", file.path("out", "manifests", "method_design"))
prior_set_id <- env_value("ACCRUAL_PRIOR_SET_ID", "scale_aware_student_baseline_v1")
likelihood_family <- tolower(env_value("ACCRUAL_FAMILY", "student"))
model_structure <- env_value("ACCRUAL_MODEL_STRUCTURE", "pooled_random_intercept")
run_varying_slopes <- env_flag("ACCRUAL_RUN_VARYING_SLOPES", "FALSE")
varyslope_scope <- toupper(env_value("ACCRUAL_VARYSLOPE_SCOPE", "LEADING_ONLY"))
varyslope_group <- env_value("ACCRUAL_VARYSLOPE_GROUP", "industry_year")
force_refit <- env_flag("ACCRUAL_FORCE_REFIT", "FALSE")
prior_predictive_mode <- toupper(env_value("ACCRUAL_PRIOR_PREDICTIVE_MODE", "REPRESENTATIVE"))
prior_pred_n_draws <- as.integer(env_value("ACCRUAL_PRIOR_PRED_N_DRAWS", "1000"))
stacking_mixture_draws <- as.integer(env_value("ACCRUAL_STACKING_MIXTURE_DRAWS", "12000"))

if (is.na(prior_pred_n_draws) || prior_pred_n_draws <= 0) prior_pred_n_draws <- 1000L
if (is.na(stacking_mixture_draws) || stacking_mixture_draws <= 0) stacking_mixture_draws <- 12000L
if (!likelihood_family %in% c("gaussian", "student")) {
  stop("[BLOCKER] ACCRUAL_FAMILY must be 'gaussian' or 'student'.")
}

winsor_root <- output_root
varyslopes_root <- file.path(output_root, "varyslopes")

baseline_dirs <- file.path(
  baseline_root,
  c("", "tables", "models", "draws", "figures", "logs", "validation", "appendix")
)

winsor_dirs <- file.path(
  winsor_root,
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

sensitivity_scenario_ids <- c("baseline", "tight", "wide")

main_model_ids_for_space <- function(target_space) {
  if (identical(target_space, "ex_post")) return(c("M01", "M02", "M03", "M04", "M05", "M06", "M07"))
  if (identical(target_space, "real_time")) return(c("M01", "M02", "M03", "M07", "M09"))
  character()
}

sensitivity_scenarios <- function() {
  data.frame(
    Scenario = sensitivity_scenario_ids,
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

sensitivity_root <- function(scenario = NULL, root = output_root) {
  base <- file.path(root, "sensitivity")
  if (is.null(scenario) || !nzchar(scenario)) return(base)
  file.path(base, scenario)
}

ensure_sensitivity_dirs <- function(scenario = NULL, root = output_root) {
  base_dirs <- file.path(sensitivity_root(NULL, root), c("", "tables", "logs", "manifests", "reports", "cache"))
  for (d in base_dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  scenarios <- if (is.null(scenario) || !nzchar(scenario)) sensitivity_scenario_ids else scenario
  subdirs <- c("", "prior_predictive", "fits", "models", "draws", "diagnostics", "stacking", "DA", "validation", "logs", "manifests", "cache", "tables")
  for (sc in scenarios) {
    for (d in file.path(sensitivity_root(sc, root), subdirs)) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(TRUE)
}

ensure_baseline_dirs <- function() {
  for (d in baseline_dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

baseline_table_path <- function(file_name) {
  file.path(baseline_root, "tables", file_name)
}

baseline_log_path <- function(file_name) {
  file.path(baseline_root, "logs", file_name)
}

normalize_column_names_safely <- function(column_names) {
  text <- as.character(column_names)
  text <- enc2utf8(text)
  text[is.na(text)] <- ""
  transliterated <- suppressWarnings(iconv(text, from = "", to = "ASCII//TRANSLIT"))
  transliterated[is.na(transliterated)] <- text[is.na(transliterated)]
  normalized <- tolower(trimws(transliterated))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("_+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized
}

format_available_columns <- function(column_names) {
  paste(as.character(column_names), collapse = ", ")
}

normalize_join_key_values <- function(values) {
  normalized <- trimws(as.character(values))
  normalized[normalized == ""] <- NA_character_
  normalized
}

detect_column_from_candidates <- function(column_names, candidates, context_label = "required column") {
  available_columns <- as.character(column_names)
  available_normalized <- normalize_column_names_safely(available_columns)
  candidate_normalized <- normalize_column_names_safely(candidates)

  for (candidate in candidates) {
    exact_hits <- which(trimws(available_columns) == trimws(candidate))
    if (length(exact_hits) == 1) {
      return(available_columns[[exact_hits]])
    }
    if (length(exact_hits) > 1) {
      stop(
        "[BLOCKER] Ambiguous ", context_label, ": multiple exact matches for '", candidate,
        "'. Available columns: ", format_available_columns(available_columns)
      )
    }
  }

  for (i in seq_along(candidates)) {
    hits <- which(available_normalized == candidate_normalized[[i]])
    if (length(hits) == 1) {
      return(available_columns[[hits]])
    }
    if (length(hits) > 1) {
      stop(
        "[BLOCKER] Ambiguous ", context_label, ": candidate '", candidates[[i]],
        "' matched multiple columns after normalization. Available columns: ",
        format_available_columns(available_columns)
      )
    }
  }

  stop(
    "[BLOCKER] Could not identify ", context_label, ". Available columns: ",
    format_available_columns(available_columns)
  )
}

detect_metadata_company_column <- function(column_names) {
  candidates <- c(
    "company", "Company", "ticker", "Ticker", "symbol", "Symbol", "code", "Code",
    "Ma", "Mã", "Mã CK", "Ma CK", "StockCode", "Stock_Code"
  )
  detect_column_from_candidates(
    column_names = column_names,
    candidates = candidates,
    context_label = "metadata company-code column"
  )
}

reports_path <- function(...) {
  file.path(reports_root, ...)
}

baseline_accruals_path <- function(file_name = "final_uncertainty_adjusted_accruals_winsor.csv") {
  file.path(accruals_root, "baseline", file_name)
}

sensitivity_accruals_path <- function(scenario, file_name = NULL) {
  if (is.null(file_name) || !nzchar(file_name)) {
    file_name <- paste0("final_sensitivity_uncertainty_adjusted_accruals_", scenario, ".csv")
  }
  file.path(accruals_root, "sensitivity", scenario, file_name)
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

pred_vars <- c(
  "inv_A_lag", "dREV_scaled", "dREC_scaled", "dREV_dREC_scaled", "PPE_scaled",
  "ROA_lag", "CFO_lag_scaled", "CFO_curr_scaled", "CFO_lead_scaled", "Size",
  "operating_cycle", "sales_growth", "sd_REV", "sd_CFO"
)

appendix1_vars <- c(
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

ensure_analysis_dirs <- function() {
  for (d in winsor_dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

prior_registry <- function() {
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

default_prior_specification <- function() {
  prior_registry()
}

write_prior_registry <- function(root = output_root) {
  ensure_analysis_dirs()
  out <- file.path(root, "tables", "table_prior_sets.csv")
  write.csv(prior_registry(), out, row.names = FALSE)
  out
}

prior_set_rows <- function(selected_prior_set_id = prior_set_id) {
  rows <- prior_registry()
  rows <- rows[rows$Prior_Set_ID == selected_prior_set_id, , drop = FALSE]
  if (nrow(rows) == 0) stop("[BLOCKER] Unknown ACCRUAL_PRIOR_SET_ID: ", selected_prior_set_id)
  rows
}

default_prior_list <- function(heterogeneity_variant = "", selected_model_structure = NULL,
                                  model_structure = NULL, selected_prior_set_id = NULL,
                                  prior_set_id = NULL, family = NULL) {
  helper_env <- environment(default_prior_list)
  if (is.null(selected_model_structure)) {
    selected_model_structure <- if (!is.null(model_structure)) model_structure else helper_env$model_structure
  }
  if (is.null(selected_prior_set_id)) {
    selected_prior_set_id <- if (!is.null(prior_set_id)) prior_set_id else helper_env$prior_set_id
  }
  if (is.null(family)) {
    family <- helper_env$likelihood_family
  }

  rows <- prior_set_rows(selected_prior_set_id)
  prior_for <- function(cls) rows$Prior_Distribution[rows$Parameter_Class == cls][1]
  prior_list <- c(
    brms::prior_string(prior_for("b"), class = "b"),
    brms::prior_string(prior_for("Intercept"), class = "Intercept"),
    brms::prior_string(prior_for("sigma"), class = "sigma")
  )
  has_group_effect <- grepl("Firm RE", heterogeneity_variant, fixed = TRUE) ||
    identical(selected_model_structure, "breuer_varying_slopes")
  if (has_group_effect && any(rows$Parameter_Class == "sd")) {
    prior_list <- c(prior_list, brms::prior_string(prior_for("sd"), class = "sd"))
  }
  if (identical(tolower(family), "student") && any(rows$Parameter_Class == "nu")) {
    prior_list <- c(prior_list, brms::prior_string(prior_for("nu"), class = "nu"))
  }
  if (identical(selected_model_structure, "breuer_varying_slopes") && any(rows$Parameter_Class == "cor")) {
    prior_list <- c(prior_list, brms::prior_string(prior_for("cor"), class = "cor"))
  }
  prior_list
}

brms_family <- function(family = likelihood_family) {
  if (identical(tolower(family), "student")) brms::student() else brms::gaussian()
}

metadata_columns <- function() {
  data.frame(
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
}

validate_final_analysis_config <- function(context = "final analysis", final_mode = TRUE) {
  is_invalid <- !identical(prior_set_id, "scale_aware_student_baseline_v1") || 
                !identical(likelihood_family, "student") || 
                !identical(model_structure, "pooled_random_intercept")
  if (!is_invalid) return(invisible(TRUE))

  msg <- paste0(
    "[CONFIG WARNING] ", context, " has a deviant/diagnostic configuration: ",
    "(ACCRUAL_PRIOR_SET_ID=", prior_set_id, ", ACCRUAL_FAMILY=", likelihood_family, ", ACCRUAL_MODEL_STRUCTURE=", model_structure, "). ",
    "The standard final-analysis config should have ACCRUAL_PRIOR_SET_ID='scale_aware_student_baseline_v1', ",
    "ACCRUAL_FAMILY='student', and ACCRUAL_MODEL_STRUCTURE='pooled_random_intercept'."
  )
  if (final_mode && !env_flag("ACCRUAL_ALLOW_DIAGNOSTIC_CONFIG", "FALSE")) {
    stop(msg, " Set ACCRUAL_ALLOW_DIAGNOSTIC_CONFIG=TRUE only for an intentional diagnostic run.")
  }
  warning(msg, call. = FALSE)
  invisible(FALSE)
}

selected_sensitivity_scenarios <- function() {
  requested <- env_value("ACCRUAL_SENS_SCENARIO", "")
  scenarios <- sensitivity_scenarios()
  if (!nzchar(requested) || toupper(requested) == "ALL") return(scenarios)
  keep <- trimws(unlist(strsplit(requested, ",", fixed = TRUE)))
  unknown <- setdiff(keep, scenarios$Scenario)
  if (length(unknown) > 0) stop("[BLOCKER] Unknown ACCRUAL_SENS_SCENARIO: ", paste(unknown, collapse = ", "))
  scenarios[scenarios$Scenario %in% keep, , drop = FALSE]
}

package_versions <- function(pkgs = c("brms", "rstan", "cmdstanr", "posterior", "loo", "bayesplot", "dplyr", "readr", "tibble", "ggplot2")) {
  vals <- vapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return("NOT_INSTALLED")
    as.character(utils::packageVersion(pkg))
  }, character(1))
  paste(paste(names(vals), vals, sep = "="), collapse = "; ")
}

file_fingerprint <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (requireNamespace("digest", quietly = TRUE)) {
    return(paste0("sha256:", digest::digest(path, algo = "sha256", file = TRUE)))
  }
  info <- file.info(path)
  paste0("mtime:", format(info$mtime[1], "%Y-%m-%d %H:%M:%S %z"), ";size:", as.numeric(info$size[1]))
}

session_info_string <- function() {
  paste(capture.output(sessionInfo()), collapse = "\n")
}

metadata_matches <- function(path, expected) {
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

write_run_manifest <- function(path, scenario, prior_set_id, family, model_structure,
                                  model_list, seed, sampling_config, status,
                                  notes = "", input_paths = character()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  input_hash <- if (length(input_paths) > 0) {
    paste(paste(input_paths, vapply(input_paths, file_fingerprint, character(1)), sep = "="), collapse = "; ")
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
    Package_Versions = package_versions(),
    R_Version = paste(R.version$major, R.version$minor, sep = "."),
    Sampling_Config = sampling_config,
    Status = status,
    Notes = notes,
    Session_Info = session_info_string(),
    stringsAsFactors = FALSE
  )
  write.csv(manifest, path, row.names = FALSE)
  path
}

write_pipeline_index <- function() {
  dir.create(method_design_root, recursive = TRUE, showWarnings = FALSE)
  pipeline <- data.frame(
    Order = sprintf("%02d", 0:22),
    Script = c(
      "00_helpers.R",
      "01_setup_and_registry.R",
      "02_build_common_sample.R",
      "03_audit_cogs_inv_operating_cycle.R",
      "04_define_named_models.R",
      "05_winsorize_common_samples.R",
      "06_prior_predictive_checks.R",
      "07_fit_brms_named_models.R",
      "08_mcmc_diagnostics.R",
      "09_loo_stacking.R",
      "10_construct_uncertainty_adjusted_DA.R",
      "11_posterior_predictive_checks.R",
      "12_lofo_stacking.R",
      "13_grouped_kfold_firm.R",
      "14_sensitivity_prior_predictive.R",
      "15_sensitivity_refit_prior_scenarios.R",
      "16_sensitivity_mcmc_diagnostics.R",
      "17_sensitivity_stacking.R",
      "18_sensitivity_construct_DA.R",
      "19_sensitivity_validation.R",
      "20_sensitivity_report.R",
      "21_validation_on_scaleaware_student_DA.R",
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
  write.csv(pipeline, file.path(method_design_root, "pipeline_index.csv"), row.names = FALSE)

  readme_lines <- c(
    "# accrual uncertainty pipeline index",
    "",
    "Active scripts use numeric prefixes only. No letter suffixes are used in script numbers.",
    "",
    "| Order | Script | Role |",
    "|---|---|---|",
    sprintf("| %s | `%s` | %s |", pipeline$Order, pipeline$Script, pipeline$Role),
    "",
    "Sensitivity phases 14-20 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `ACCRUAL_DRY_RUN=FALSE` and the relevant phase is launched intentionally.",
    "",
    "Sampler protocol: full-sample baseline `brms` fits use 4 chains, 4000 iterations, and 1000 warmup iterations; exact K-fold refits use 4 chains, 3000 iterations, and 1000 warmup iterations because they are repeated across validation folds and are used for method-matched validation comparisons; FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference. The baseline 4000/1000 setting is intentional, while 3000/1000 is the primary validation-refit protocol. Manifests should record actual sampler settings.",
    "",
    paste0("The machine-readable pipeline index is written to `", file.path(method_design_root, "pipeline_index.csv"), "`.")
  )
  writeLines(readme_lines, file.path("doc", "pipeline_index.md"))
  invisible(pipeline)
}

winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  qs <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  pmin(pmax(x, qs[1]), qs[2])
}

safe_variant_name <- function(x) {
  gsub(" ", "_", gsub("[()|]", "", x))
}

model_key <- function(model_id, target_space, heterogeneity_variant, suffix = NULL) {
  key <- sprintf("%s_%s_%s", model_id, target_space, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

model_key_sampled <- function(model_id, target_space, sample_group, heterogeneity_variant, suffix = NULL) {
  if (is.null(sample_group) || is.na(sample_group) || !nzchar(sample_group)) sample_group <- "main_common"
  key <- sprintf("%s_%s_%s_%s", model_id, target_space, sample_group, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

standardize_predictors <- function(df, predictor_vars = pred_vars) {
  for (v in predictor_vars) {
    if (v %in% colnames(df)) {
      m <- mean(df[[v]], na.rm = TRUE)
      s <- sd(df[[v]], na.rm = TRUE)
      df[[paste0(v, "_std")]] <- if (!is.na(s) && s > 0) (df[[v]] - m) / s else 0
    }
  }
  df
}

fix_formula <- function(formula_str, predictor_vars = pred_vars, prefactor = FALSE) {
  if (prefactor) {
    formula_str <- gsub("factor\\(industry\\)", "industry_f", formula_str)
    formula_str <- gsub("factor\\(year\\)", "year_f", formula_str)
  }
  for (v in predictor_vars) {
    formula_str <- gsub(paste0("\\b", v, "\\b"), paste0(v, "_std"), formula_str)
  }
  formula_str
}

read_winsor_sample <- function(sample_file, prefactor = FALSE, root = input_winsor_root) {
  path <- file.path(root, "tables", sample_file)
  if (!file.exists(path)) stop("[BLOCKER] Winsorized sample file missing: ", path)
  df <- read.csv(path, stringsAsFactors = FALSE)
  df <- standardize_predictors(df)
  if (prefactor) {
    df$industry_f <- factor(df$industry)
    df$year_f <- factor(df$year)
  }
  df
}

prepare_varying_slope_data <- function(df, group = varyslope_group) {
  if (identical(group, "industry_year")) {
    if (!all(c("industry", "year") %in% names(df))) {
      stop("[BLOCKER] industry_year varying slopes require industry and year columns.")
    }
    df$industry_year_id <- interaction(df$industry, df$year, drop = TRUE)
  }
  df
}

varying_slope_formula <- function(formula_str, group = varyslope_group) {
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

varying_slope_candidate <- function(model_id, target_space) {
  if (identical(varyslope_scope, "FULL")) return(TRUE)
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
      file.path(baseline_root, "tables", "table_stacking_weights_ex_post_corrected.csv"),
      file.path(baseline_root, "tables", "table_stacking_weights_ex_post.csv")
    )
  } else {
    candidates <- c(
      file.path(baseline_root, "tables", "table_stacking_weights_real_time_corrected.csv"),
      file.path(baseline_root, "tables", "table_stacking_weights_real_time.csv")
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

write_method_design_files <- function() {
  design_root <- method_design_root
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
    "The analysis is therefore positioned as an extension/adaptation, not a replication. The corrected design preserves corrected COGS/INV data, the two-tier sample design, the exclusion of M08 and M10 from main stacks, and the treatment of existing wide-prior Gaussian outputs as diagnostic only."
  ), file.path(design_root, "method_note_adaptation_not_replication.txt"))
  write_pipeline_index()
}
