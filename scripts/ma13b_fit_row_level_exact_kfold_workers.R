# Script: ma13b_fit_row_level_exact_kfold_workers.R
# Purpose: Run row-level exact K-fold fit tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("ma13b", "Fit row-level exact K-fold workers")
tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma13_row_kfold_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma13_row_kfold_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing ma13a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_ma13b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("ma13b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  status <- "BLOCKED_PENDING_SPLIT_IMPLEMENTATION"
  reason <- "ma13b worker contract is in place; row-fold brms::brm body must preserve existing row K-fold semantics before heavy execution."
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason, backend = "rstan",
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, prediction_path = task$prediction_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("row_kfold", max(as.integer(tasks$cores), na.rm = TRUE), "ma13b row K-fold workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_ma13b_task_worker, parallel_cfg,
                                 export_names = "fit_ma13b_task_worker", context = "ma13b row K-fold workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "ma13b row K-fold workers")
phase_end("ma13b", "Fit row-level exact K-fold workers")
