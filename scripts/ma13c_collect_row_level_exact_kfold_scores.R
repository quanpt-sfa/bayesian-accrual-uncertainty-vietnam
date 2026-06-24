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
collector_contract <- data.frame(
  output = c("table_winsor_row_exact_kfold_model_scores.csv", "table_winsor_row_exact_kfold_weights_ex_post.csv",
             "table_winsor_row_exact_kfold_weights_no_lookahead.csv", "LATEST_COMPLETED_RUN.txt"),
  owner = "ma13c_collect_row_level_exact_kfold_scores.R",
  evidence_role = "primary_row_exact_kfold",
  task_manifest_rows = nrow(manifest),
  stringsAsFactors = FALSE
)
write.csv(collector_contract, file.path(tables_dir, "table_ma13_collect_contract.csv"), row.names = FALSE)
message("ma13c collector owns row exact K-fold shared outputs and completed-run pins.")
phase_end("ma13c", "Collect row-level exact K-fold scores")
