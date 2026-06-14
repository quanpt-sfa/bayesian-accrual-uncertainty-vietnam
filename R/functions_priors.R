source("scripts/00_helpers.R")

prior_registry_table <- function() {
  prior_registry()
}

prior_rows <- function(prior_set_id = prior_set_id) {
  prior_set_rows(prior_set_id)
}

prior_gate_path <- function() {
  file.path(output_root, "prior_predictive_gate_status.csv")
}
