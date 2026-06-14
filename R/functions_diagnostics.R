source("scripts/v3/00_v3_winsor_helpers.R")

v3_baseline_diagnostics_path <- function() {
  file.path(v3_output_root, "tables", "table_v3_brms_diagnostics_winsor.csv")
}

v3_mcmc_summary_path <- function() {
  file.path(v3_output_root, "tables", "table_v3_mcmc_diagnostics_model_summary.csv")
}

v3_validation_summary_path <- function() {
  file.path(v3_output_root, "validation", "table_v3_validation_comparison_summary_scaleaware_student.csv")
}
