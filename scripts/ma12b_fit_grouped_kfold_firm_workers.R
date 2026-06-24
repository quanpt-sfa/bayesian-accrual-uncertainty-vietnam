# Script: ma12b_fit_grouped_kfold_firm_workers.R
# Purpose: Run grouped-firm exact K-fold fit tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("ma12b", "Fit grouped-firm exact K-fold workers")

tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma12_grouped_kfold_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma12_grouped_kfold_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing ma12a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)

fit_ma12b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("ma12b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  status <- "BLOCKED_PENDING_SPLIT_IMPLEMENTATION"
  reason <- "ma12b worker contract is in place; fold-specific brms::brm body must preserve existing grouped K-fold semantics before heavy execution."
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason, backend = "rstan",
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, prediction_path = task$prediction_path, stringsAsFactors = FALSE)
}

parallel_cfg <- accrual_fit_worker_config("grouped_kfold", max(as.integer(tasks$cores), na.rm = TRUE), "ma12b grouped K-fold workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_ma12b_task_worker, parallel_cfg,
                                 export_names = "fit_ma12b_task_worker", context = "ma12b grouped K-fold workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "ma12b grouped K-fold workers")
phase_end("ma12b", "Fit grouped-firm exact K-fold workers")
