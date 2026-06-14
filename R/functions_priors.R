source("scripts/v3/00_v3_winsor_helpers.R")

v3_prior_registry <- function() {
  prior_registry_v3()
}

v3_prior_rows <- function(prior_set_id = v3_prior_set_id) {
  prior_set_rows_v3(prior_set_id)
}

v3_prior_gate_path <- function() {
  file.path(v3_output_root, "prior_predictive_gate_status.csv")
}
