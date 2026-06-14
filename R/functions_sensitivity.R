source("scripts/00_helpers.R")

scenarios <- function() {
  sensitivity_scenarios()
}

selected_scenarios <- function() {
  selected_sensitivity_scenarios()
}

sensitivity_table_path <- function(file_name) {
  file.path(sensitivity_root(), "tables", file_name)
}
