root <- Sys.getenv("ACCRUAL_ROW_KFOLD_CHECK_ROOT", file.path("out", "interim", "winsor", "row_exact_kfold"))
tables <- file.path(root, "tables")

if (!dir.exists(tables)) {
  cat("Skipping row exact K-fold schema test; no output root found:", root, "\n")
  quit(save = "no", status = 0)
}

check_cols <- function(file, required) {
  path <- file.path(tables, file)
  if (!file.exists(path)) {
    cat("Skipping missing optional row exact K-fold file:", path, "\n")
    return(invisible(TRUE))
  }
  x <- read.csv(path, nrows = 1, stringsAsFactors = FALSE)
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) stop("Missing columns in ", path, ": ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

check_cols(
  "table_winsor_row_exact_kfold_fold_assignment.csv",
  c("observation_id", "row_id", "company", "year", "target_space", "fold", "K", "seed", "fold_assignment_unit")
)
check_cols(
  "table_winsor_row_exact_kfold_balance.csv",
  c("target_space", "fold", "n_obs", "n_firms", "n_industries", "n_years", "min_year", "max_year")
)
check_cols(
  "table_winsor_row_exact_kfold_observation_scores.csv",
  c("target_space", "model_id", "model_name", "heterogeneity_variant", "sample_group", "fold",
    "company", "year", "row_id", "observation_id", "observed_TA_scaled",
    "log_predictive_density", "prediction_rule", "refit_type", "validation_unit",
    "prior_set_id", "likelihood_family", "model_structure", "output_root")
)
check_cols(
  "table_winsor_row_exact_kfold_weights_ex_post.csv",
  c("target_space", "sample_group", "model_id", "model_name", "heterogeneity_variant",
    "model_key_row_exact_kfold", "weight_row_exact_kfold", "rank_row_exact_kfold")
)
check_cols(
  "table_winsor_row_vs_firm_kfold_weight_comparison.csv",
  c("target_space", "model_id", "heterogeneity_variant", "row_exact_kfold_weight",
    "firm_grouped_kfold_weight", "difference", "firmRE_family_indicator")
)

cat("test_row_exact_kfold_schema.R passed\n")
