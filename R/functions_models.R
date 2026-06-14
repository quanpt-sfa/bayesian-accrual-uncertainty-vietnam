source("scripts/v3/00_v3_winsor_helpers.R")

v3_model_registry_path <- function() {
  v3_baseline_table_path("table_v3_model_registry.csv")
}

v3_named_formula_path <- function(winsor = TRUE) {
  if (winsor) {
    file.path(v3_input_winsor_root, "tables", "table_v3_named_model_formulas_winsor.csv")
  } else {
    v3_baseline_table_path("table_v3_named_model_formulas.csv")
  }
}

v3_main_models_for_space <- function(target_space) {
  v3_main_model_ids_for_space(target_space)
}
