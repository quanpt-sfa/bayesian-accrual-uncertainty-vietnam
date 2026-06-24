# Script: ma12c_collect_grouped_kfold_firm_scores.R
# Purpose: Collect grouped-firm exact K-fold scores and shared outputs.

source("scripts/ma00_setup.R")
phase_begin("ma12c", "Collect grouped-firm exact K-fold scores")

tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma12_grouped_kfold_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma12_grouped_kfold_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) stop("[BLOCKER] ma12c requires ma12a manifest and ma12b task status.")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "ma12c grouped K-fold collect")
if (!"result_path" %in% names(manifest)) manifest$result_path <- manifest$prediction_path
results <- lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] ma12c missing grouped K-fold task result: ", path)
  readRDS(path)
})
fold_diagnostics <- do.call(rbind, lapply(results, `[[`, "fold_diag"))
obs_scores <- do.call(rbind, lapply(results, `[[`, "obs_scores"))
write.csv(fold_diagnostics, file.path(tables_dir, "table_winsor_kfold_refit_diagnostics.csv"), row.names = FALSE)
write.csv(obs_scores, file.path(tables_dir, "table_winsor_kfold_observation_scores.csv"), row.names = FALSE)
model_scores <- aggregate(lpd_obs ~ Target_Space + Model_ID + Model_Name + Heterogeneity_Variant, obs_scores, mean)
names(model_scores)[names(model_scores) == "lpd_obs"] <- "mean_lpd_obs"
write.csv(model_scores, file.path(tables_dir, "table_winsor_kfold_model_scores.csv"), row.names = FALSE)
make_weights <- function(space) {
  score_rows <- obs_scores[obs_scores$Target_Space == space, , drop = FALSE]
  if (!nrow(score_rows)) return(data.frame())
  score_rows$Obs_Key <- paste(score_rows$company, score_rows$year, sep = "::")
  score_rows$Model_Key <- paste(score_rows$Model_ID, score_rows$Heterogeneity_Variant, sep = "::")
  wide <- stats::reshape(
    score_rows[, c("Obs_Key", "Model_Key", "lpd_obs")],
    idvar = "Obs_Key",
    timevar = "Model_Key",
    direction = "wide"
  )
  lpd_matrix <- as.matrix(wide[, setdiff(names(wide), "Obs_Key"), drop = FALSE])
  colnames(lpd_matrix) <- sub("^lpd_obs\\.", "", colnames(lpd_matrix))
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  meta <- unique(score_rows[, c("Model_Key", "Model_ID", "Model_Name", "Target_Space", "Heterogeneity_Variant"), drop = FALSE])
  meta <- meta[match(names(weights), meta$Model_Key), , drop = FALSE]
  data.frame(meta[, c("Model_ID", "Model_Name", "Target_Space", "Heterogeneity_Variant"), drop = FALSE],
             stacking_weight = as.numeric(weights),
             Primary_Secondary = "primary_exact_grouped_firm_kfold",
             stringsAsFactors = FALSE)
}
write.csv(make_weights("ex_post"), file.path(tables_dir, "table_winsor_kfold_weights_ex_post.csv"), row.names = FALSE)
write.csv(make_weights("real_time"), file.path(tables_dir, "table_winsor_kfold_weights_no_lookahead.csv"), row.names = FALSE)
message("ma12c collected grouped K-fold task results and wrote shared outputs.")
phase_end("ma12c", "Collect grouped-firm exact K-fold scores")
