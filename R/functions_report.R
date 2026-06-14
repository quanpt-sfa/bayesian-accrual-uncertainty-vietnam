source("scripts/v3/00_v3_winsor_helpers.R")

v3_reports_dir <- function(...) {
  v3_reports_path(...)
}

v3_sensitivity_report_path <- function() {
  v3_reports_path("sensitivity", "sensitivity_report_v3.md")
}

v3_method_design_dir <- function() {
  v3_method_design_root
}
