# Script: ma13c_collect_row_level_exact_kfold_scores.R
# Purpose: Collect row-level exact K-fold scores and shared outputs.

source("scripts/ma00_setup.R")
phase_begin("ma13c", "Collect row-level exact K-fold scores")
tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma13_row_kfold_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma13_row_kfold_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) stop("[BLOCKER] ma13c requires ma13a manifest and ma13b task status.")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "ma13c row K-fold collect")
if (!"result_path" %in% names(manifest)) manifest$result_path <- manifest$prediction_path
results <- lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] ma13c missing row K-fold task result: ", path)
  readRDS(path)
})
fold_diagnostics <- do.call(rbind, lapply(results, `[[`, "fold_diag"))
obs_scores <- do.call(rbind, lapply(results, `[[`, "obs_scores"))
write.csv(fold_diagnostics, file.path(tables_dir, "table_winsor_row_exact_kfold_refit_diagnostics.csv"), row.names = FALSE)
write.csv(obs_scores, file.path(tables_dir, "table_winsor_row_exact_kfold_observation_scores.csv"), row.names = FALSE)
included <- obs_scores[obs_scores$primary_row_target_inclusion %in% c(TRUE, "TRUE", 1L), , drop = FALSE]
model_scores <- aggregate(log_predictive_density ~ target_space + model_id + model_name + heterogeneity_variant, included, mean)
names(model_scores)[names(model_scores) == "log_predictive_density"] <- "mean_log_predictive_density"
write.csv(model_scores, file.path(tables_dir, "table_winsor_row_exact_kfold_model_scores.csv"), row.names = FALSE)
make_weights <- function(space) {
  score_rows <- included[included$target_space == space, , drop = FALSE]
  if (!nrow(score_rows)) return(data.frame())
  score_rows$Model_Key <- paste(score_rows$model_id, score_rows$heterogeneity_variant, sep = "::")
  wide <- stats::reshape(
    score_rows[, c("observation_id", "Model_Key", "log_predictive_density")],
    idvar = "observation_id",
    timevar = "Model_Key",
    direction = "wide"
  )
  lpd_matrix <- as.matrix(wide[, setdiff(names(wide), "observation_id"), drop = FALSE])
  colnames(lpd_matrix) <- sub("^log_predictive_density\\.", "", colnames(lpd_matrix))
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  meta <- unique(score_rows[, c("Model_Key", "model_id", "model_name", "target_space", "heterogeneity_variant"), drop = FALSE])
  meta <- meta[match(names(weights), meta$Model_Key), , drop = FALSE]
  data.frame(meta[, c("model_id", "model_name", "target_space", "heterogeneity_variant"), drop = FALSE],
             stacking_weight = as.numeric(weights),
             Primary_Secondary = "primary_exact_row_kfold",
             stringsAsFactors = FALSE)
}
write.csv(make_weights("ex_post"), file.path(tables_dir, "table_winsor_row_exact_kfold_weights_ex_post.csv"), row.names = FALSE)
write.csv(make_weights("real_time"), file.path(tables_dir, "table_winsor_row_exact_kfold_weights_no_lookahead.csv"), row.names = FALSE)
message("ma13c collected row K-fold task results and wrote shared outputs.")
phase_end("ma13c", "Collect row-level exact K-fold scores")
