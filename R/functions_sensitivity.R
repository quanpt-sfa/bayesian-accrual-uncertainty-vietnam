source("scripts/v3/00_v3_winsor_helpers.R")

v3_scenarios <- function() {
  v3_sensitivity_scenarios()
}

v3_selected_scenarios <- function() {
  selected_sensitivity_scenarios_v3()
}

v3_sensitivity_table_path <- function(file_name) {
  file.path(v3_sensitivity_root(), "tables", file_name)
}
