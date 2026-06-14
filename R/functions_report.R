source("scripts/00_helpers.R")

reports_dir <- function(...) {
  reports_path(...)
}

sensitivity_report_path <- function() {
  reports_path("sensitivity", "sensitivity_report.md")
}

method_design_dir <- function() {
  method_design_root
}
