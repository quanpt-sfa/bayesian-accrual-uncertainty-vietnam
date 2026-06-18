# -----------------------------------------------------------------------------
# Script: 20_sensitivity_report.R
# Purpose: Build sensitivity-analysis report and reproducibility bundle.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/00_helpers.R")
ensure_analysis_dirs()
ensure_sensitivity_dirs()
write_method_design_files()

sens_root <- sensitivity_root()
tables_root <- file.path(sens_root, "tables")
reports_root <- reports_path("sensitivity")
dir.create(reports_root, recursive = TRUE, showWarnings = FALSE)

# Read parameter for partial report from environment
allow_partial_report <- as.logical(env_value("ACCRUAL_ALLOW_PARTIAL_REPORT", "FALSE"))

missing_inputs_log <- data.frame(
  Path = character(),
  Label = character(),
  Reason = character(),
  stringsAsFactors = FALSE
)

read_sens_input <- function(path, label) {
  if (file.exists(path)) {
    existing <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(existing) && ncol(existing) > 0) return(existing)
  }
  
  missing_inputs_log <<- rbind(missing_inputs_log, data.frame(
    Path = path,
    Label = label,
    Reason = paste0("Missing required sensitivity analysis input file: ", basename(path)),
    stringsAsFactors = FALSE
  ))
  
  if (allow_partial_report) {
    return(data.frame(Status = "NOT_AVAILABLE_PARTIAL", stringsAsFactors = FALSE))
  } else {
    return(NULL)
  }
}

prior_pp <- read_sens_input(file.path(tables_root, "sensitivity_prior_predictive_summary.csv"), "prior_predictive")
mcmc <- read_sens_input(file.path(tables_root, "sensitivity_mcmc_diagnostics_summary.csv"), "mcmc_diagnostics")
weights <- read_sens_input(file.path(tables_root, "sensitivity_stacking_weights_by_scenario.csv"), "stacking_weights")
top_models <- read_sens_input(file.path(tables_root, "sensitivity_top_models_comparison.csv"), "top_models_comparison")
da_stability <- read_sens_input(file.path(tables_root, "sensitivity_DA_stability_summary.csv"), "DA_stability")
validation <- read_sens_input(file.path(tables_root, "sensitivity_validation_summary.csv"), "validation")

if (nrow(missing_inputs_log) > 0) {
  logs_root <- file.path(sens_root, "logs")
  dir.create(logs_root, recursive = TRUE, showWarnings = FALSE)
  write.csv(missing_inputs_log, file.path(logs_root, "sensitivity_missing_inputs_log.csv"), row.names = FALSE)
  
  if (!allow_partial_report) {
    stop("[BLOCKER] Sensitivity report was stopped due to missing input files. Details written to '", 
         file.path(logs_root, "sensitivity_missing_inputs_log.csv"), "'. Set 'ACCRUAL_ALLOW_PARTIAL_REPORT=TRUE' if you want a partial report.")
  }
}

prior_registry <- prior_registry() %>%
  filter(Prior_Set_ID %in% sensitivity_scenarios()$Prior_Set_ID) %>%
  select(Prior_Set_ID, Parameter_Class, Prior_Distribution, Likelihood_Family, Notes)
write.csv(prior_registry, file.path(tables_root, "sensitivity_prior_scenario_registry.csv"), row.names = FALSE)

repro <- data.frame(
  Item = c(
    "R version",
    "Package versions",
    "Session info path",
    "Default seed",
    "Default chains",
    "Default iterations",
    "Default warmup",
    "Default adapt_delta",
    "Default max_treedepth",
    "Backend",
    "Input fingerprint data/raw/data.xlsx",
    "Input fingerprint formulas"
  ),
  Value = c(
    paste(R.version$major, R.version$minor, sep = "."),
    package_versions(),
    file.path(reports_root, "sensitivity_sessionInfo.txt"),
    as.character(accrual_seed("sensitivity")),
    env_value("ACCRUAL_SENS_CHAINS", "4"),
    env_value("ACCRUAL_SENS_ITER", "4000"),
    env_value("ACCRUAL_SENS_WARMUP", "1000"),
    env_value("ACCRUAL_SENS_ADAPT_DELTA", "0.95"),
    env_value("ACCRUAL_SENS_MAX_TREEDEPTH", "12"),
    env_value("ACCRUAL_BACKEND", "brms default backend"),
    file_fingerprint(data_path),
    file_fingerprint(file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv"))
  ),
  stringsAsFactors = FALSE
)
write.csv(repro, file.path(tables_root, "sensitivity_reproducibility_info.csv"), row.names = FALSE)
writeLines(session_info_string(), file.path(reports_root, "sensitivity_sessionInfo.txt"))

robustness_interpretation <- "Sensitivity results are prepared but not yet fully evaluated because at least one full-refit phase has not produced non-dry-run outputs."
if ("stability_flag" %in% names(da_stability) && nrow(da_stability) > 0) {
  if (any(da_stability$stability_flag == "REVIEW_PRIOR_SENSITIVITY", na.rm = TRUE)) {
    robustness_interpretation <- "The sensitivity analysis indicates prior-sensitive DA rankings or tail flags; conclusions should be reported as prior-sensitive and reviewed before manuscript claims."
  } else if (all(da_stability$stability_flag == "STABLE", na.rm = TRUE)) {
    robustness_interpretation <- "The sensitivity analysis provides evidence that the main conclusions are not materially driven by reasonable changes in prior scale."
  }
}

scenario_lines <- prior_registry %>%
  mutate(Line = sprintf("- `%s`, class `%s`: `%s`", Prior_Set_ID, Parameter_Class, Prior_Distribution)) %>%
  pull(Line)

count_status <- function(df, col) {
  if (!col %in% names(df) || nrow(df) == 0) return("not available")
  paste(capture.output(print(table(df[[col]], useNA = "ifany"))), collapse = "; ")
}

available_n <- function(df) {
  if (is.null(df)) return(0L)
  if ("Status" %in% names(df) && all(df$Status %in% c("NOT_AVAILABLE", "NOT_AVAILABLE_PARTIAL"))) return(0L)
  nrow(df)
}

report_lines <- c(
  "# sensitivity analysis report",
  "",
  "## 1. Purpose and design",
  "This sensitivity workflow evaluates whether Bayesian accrual conclusions are materially driven by reasonable changes in prior scale. The design uses separate prior predictive checks, full scenario-specific MCMC refits, diagnostics gating, scenario-specific stacking, DA reconstruction, and validation.",
  "",
  "## 2. Prior scenarios",
  scenario_lines,
  "",
  "The old wide Gaussian configuration is diagnostic-only and is not used as a final manuscript sensitivity scenario unless explicitly overridden for diagnostics.",
  "",
  "## 3. Prior predictive checks",
  sprintf("Prior predictive status counts: %s", count_status(prior_pp, "status")),
  "Scenarios with FAIL status are blocked from full refit unless `ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE` is set intentionally.",
  "",
  "## 4. MCMC diagnostics",
  sprintf("MCMC diagnostic status counts: %s", count_status(mcmc, "diagnostics_status")),
  "Only models with diagnostics status PASS are allowed into stacking. REVIEW and FAIL models are excluded from scenario stacking.",
  "",
  "## 5. Stacking weights stability",
  sprintf("Stacking rows available: %d", available_n(weights)),
  "Weights are recomputed separately by scenario from scenario posterior fits. Row-level LOO is treated as diagnostic/supplementary if grouped validation is not run.",
  "",
  "## 6. DA distribution and rank stability",
  sprintf("DA stability rows available: %d", available_n(da_stability)),
  if ("stability_flag" %in% names(da_stability)) paste("DA stability flags:", paste(unique(da_stability$stability_flag), collapse = ", ")) else "DA stability flags: not available",
  "",
  "## 7. Validation/outcome test stability",
  sprintf("Validation rows available: %d", available_n(validation)),
  "Validation is scenario-specific and flags ex-post DA tests against future CFO/earnings for look-ahead or circularity risk.",
  "",
  "## 8. Interpretation",
  robustness_interpretation,
  "",
  "## 9. Limitations and manuscript wording",
  "Do not describe the findings as mathematically proven robust or definitely robust. Use conservative wording such as: \"The sensitivity analysis provides evidence that the main conclusions are not materially driven by reasonable changes in prior scale.\" If scenario outputs are unstable, report that directly and describe the conclusions as prior-sensitive.",
  "",
  "## 10. Reproducibility",
  sprintf("- R version: %s", repro$Value[repro$Item == "R version"]),
  sprintf("- Package versions: %s", repro$Value[repro$Item == "Package versions"]),
  sprintf("- Session info: `%s`", repro$Value[repro$Item == "Session info path"]),
  sprintf("- Canonical seed (ACCRUAL_SEED): %s", accrual_seed("sensitivity")),
  sprintf("- Chains/iter/warmup: %s/%s/%s", env_value("ACCRUAL_SENS_CHAINS", "4"), env_value("ACCRUAL_SENS_ITER", "4000"), env_value("ACCRUAL_SENS_WARMUP", "1000")),
  sprintf("- adapt_delta/max_treedepth: %s/%s", env_value("ACCRUAL_SENS_ADAPT_DELTA", "0.95"), env_value("ACCRUAL_SENS_MAX_TREEDEPTH", "12")),
  "",
  "Optional renv snapshot guidance: if this project uses renv, run `renv::snapshot()` manually after confirming package state. This workflow does not force renv initialization."
)

writeLines(report_lines, file.path(reports_root, "sensitivity_report.md"))
file.copy(file.path(reports_root, "sensitivity_report.md"),
          file.path(tables_root, "sensitivity_report.md"),
          overwrite = TRUE)

cat("\n[SUCCESS] Sensitivity report completed.\n")
