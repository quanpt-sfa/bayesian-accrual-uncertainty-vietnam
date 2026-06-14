formulas_path <- file.path("out", "interim", "winsor", "tables", "table_named_model_formulas_winsor.csv")
diag_path <- file.path("out", "interim", "winsor", "tables", "table_brms_diagnostics_winsor.csv")
log_path <- file.path("out", "interim", "winsor", "logs", "step7_diagnostics_validation.txt")

if (!file.exists(formulas_path)) {
  stop("[BLOCKER] Missing Step 7 formulas table: ", formulas_path)
}
if (!file.exists(diag_path)) {
  stop("[BLOCKER] Missing Step 7 diagnostics table: ", diag_path)
}

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
diag_df <- read.csv(diag_path, stringsAsFactors = FALSE)

required_columns <- c(
  "Model_ID", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
  "Fit_Status", "N_Obs", "N_Firms", "Rhat_Max", "Divergences",
  "treedepth_warnings", "pareto_k_above_07", "loo_status"
)
missing_columns <- setdiff(required_columns, names(diag_df))
if (length(missing_columns) > 0) {
  stop("[BLOCKER] Step 7 diagnostics table is missing required columns: ", paste(missing_columns, collapse = ", "))
}

key_for <- function(df) {
  paste(df$Model_ID, df$Target_Space, df$Sample_Group, df$Heterogeneity_Variant, sep = "||")
}

expected_keys <- unique(key_for(formulas_df))
actual_keys <- unique(key_for(diag_df))
missing_keys <- setdiff(expected_keys, actual_keys)
unexpected_keys <- setdiff(actual_keys, expected_keys)

bad_status <- unique(diag_df$Fit_Status[diag_df$Fit_Status != "SUCCESS"])
bad_loo <- sort(unique(diag_df$loo_status[!diag_df$loo_status %in% c("PSIS_OK", "PSIS_REVIEW_REQUIRED", "LOO_FAILED")]))
bad_pareto_flags <- diag_df$pareto_k_above_07 > 0 & diag_df$loo_status != "PSIS_REVIEW_REQUIRED"
bad_pareto_flags[is.na(bad_pareto_flags)] <- FALSE

checks <- c(
  sprintf("Expected rows: %d", length(expected_keys)),
  sprintf("Actual rows: %d", nrow(diag_df)),
  sprintf("Missing expected rows: %d", length(missing_keys)),
  sprintf("Unexpected rows: %d", length(unexpected_keys)),
  sprintf("Non-success Fit_Status rows: %d", sum(diag_df$Fit_Status != "SUCCESS", na.rm = TRUE)),
  sprintf("Rows with N_Obs <= 0: %d", sum(is.na(diag_df$N_Obs) | diag_df$N_Obs <= 0)),
  sprintf("Rows with N_Firms <= 0: %d", sum(is.na(diag_df$N_Firms) | diag_df$N_Firms <= 0)),
  sprintf("Rows with Rhat_Max > 1.01: %d", sum(is.na(diag_df$Rhat_Max) | diag_df$Rhat_Max > 1.01)),
  sprintf("Rows with Divergences != 0: %d", sum(is.na(diag_df$Divergences) | diag_df$Divergences != 0)),
  sprintf("Rows with treedepth_warnings != 0: %d", sum(is.na(diag_df$treedepth_warnings) | diag_df$treedepth_warnings != 0)),
  sprintf("Rows with invalid loo_status: %d", length(bad_loo)),
  sprintf("Rows with pareto_k_above_07 > 0 but not PSIS_REVIEW_REQUIRED: %d", sum(bad_pareto_flags))
)

dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
writeLines(checks, con = log_path)

failures <- character()
if (length(missing_keys) > 0) failures <- c(failures, paste("Missing expected diagnostics rows:", paste(missing_keys, collapse = "; ")))
if (length(unexpected_keys) > 0) failures <- c(failures, paste("Unexpected diagnostics rows:", paste(unexpected_keys, collapse = "; ")))
if (length(bad_status) > 0) failures <- c(failures, paste("Unexpected Fit_Status values:", paste(bad_status, collapse = ", ")))
if (any(is.na(diag_df$N_Obs) | diag_df$N_Obs <= 0)) failures <- c(failures, "Some Step 7 rows have N_Obs <= 0 or missing.")
if (any(is.na(diag_df$N_Firms) | diag_df$N_Firms <= 0)) failures <- c(failures, "Some Step 7 rows have N_Firms <= 0 or missing.")
if (any(is.na(diag_df$Rhat_Max) | diag_df$Rhat_Max > 1.01)) failures <- c(failures, "Some Step 7 rows have Rhat_Max > 1.01 or missing.")
if (any(is.na(diag_df$Divergences) | diag_df$Divergences != 0)) failures <- c(failures, "Some Step 7 rows have non-zero or missing Divergences.")
if (any(is.na(diag_df$treedepth_warnings) | diag_df$treedepth_warnings != 0)) failures <- c(failures, "Some Step 7 rows have non-zero or missing treedepth_warnings.")
if (length(bad_loo) > 0) failures <- c(failures, paste("Invalid loo_status values:", paste(bad_loo, collapse = ", ")))
if (any(bad_pareto_flags)) failures <- c(failures, "Models with pareto_k_above_07 > 0 were not flagged as PSIS_REVIEW_REQUIRED.")

if (length(failures) > 0) {
  stop(paste(failures, collapse = "\n"))
}

cat("test_step7_diagnostics_schema.R passed\n")