source("scripts/00_helpers.R")

baseline_diagnostics_path <- function() {
  file.path(output_root, "tables", "table_brms_diagnostics_winsor.csv")
}

mcmc_summary_path <- function() {
  file.path(output_root, "tables", "table_mcmc_diagnostics_model_summary.csv")
}

validation_summary_path <- function() {
  file.path(output_root, "validation", "table_validation_comparison_summary_scaleaware_student.csv")
}
