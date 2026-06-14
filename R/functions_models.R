source("scripts/00_helpers.R")

model_registry_path <- function() {
  baseline_table_path("table_model_registry.csv")
}

named_formula_path <- function(winsor = TRUE) {
  if (winsor) {
    file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
  } else {
    baseline_table_path("table_named_model_formulas.csv")
  }
}

main_models_for_space <- function(target_space) {
  main_model_ids_for_space(target_space)
}
