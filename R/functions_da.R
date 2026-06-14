source("scripts/00_helpers.R")

baseline_accruals_file <- function() {
  baseline_accruals_path()
}

sensitivity_accruals_file <- function(scenario) {
  sensitivity_accruals_path(scenario)
}

baseline_da_summary_path <- function() {
  file.path(output_root, "tables", "table_DA_distribution_summary_winsor.csv")
}
