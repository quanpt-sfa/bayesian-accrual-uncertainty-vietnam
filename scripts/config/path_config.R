# -----------------------------------------------------------------------------
# Path globals, directory helpers, and data-shape constants
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

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
stacking_mixture_draws <- as.integer(env_value("ACCRUAL_STACKING_MIXTURE_DRAWS", "8000"))

if (is.na(prior_pred_n_draws) || prior_pred_n_draws <= 0) prior_pred_n_draws <- 1000L
if (is.na(stacking_mixture_draws) || stacking_mixture_draws <= 0) stacking_mixture_draws <- 8000L
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
    "Ma", "MÃƒÂ£", "MÃƒÂ£ CK", "Ma CK", "StockCode", "Stock_Code"
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

