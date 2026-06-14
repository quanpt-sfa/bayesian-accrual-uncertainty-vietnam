source("scripts/v3/00_v3_winsor_helpers.R")

v3_baseline_accruals_file <- function() {
  v3_baseline_accruals_path()
}

v3_sensitivity_accruals_file <- function(scenario) {
  v3_sensitivity_accruals_path(scenario)
}

v3_baseline_da_summary_path <- function() {
  file.path(v3_output_root, "tables", "table_v3_DA_distribution_summary_winsor.csv")
}
