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
collector_contract <- data.frame(
  output = c("table_winsor_kfold_model_scores.csv", "table_winsor_kfold_weights_ex_post.csv",
             "table_winsor_kfold_weights_no_lookahead.csv", "LATEST_COMPLETED_RUN.txt"),
  owner = "ma12c_collect_grouped_kfold_firm_scores.R",
  evidence_role = "primary_grouped_firm_exact_kfold",
  task_manifest_rows = nrow(manifest),
  stringsAsFactors = FALSE
)
write.csv(collector_contract, file.path(tables_dir, "table_ma12_collect_contract.csv"), row.names = FALSE)
message("ma12c collector owns grouped K-fold shared outputs and completed-run pins.")
phase_end("ma12c", "Collect grouped-firm exact K-fold scores")
