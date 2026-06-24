# Script: si03b_fit_brms_leakage_confirmation_workers.R
# Purpose: Fit BRMS leakage confirmation simulation tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("si03b", "Fit BRMS leakage confirmation workers")
root <- file.path(output_root, "simulation", "brms_leakage")
manifest_path <- file.path(root, "tables", "table_si03_brms_leakage_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si03_brms_leakage_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing si03a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_si03b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("si03b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  status <- "BLOCKED_PENDING_SPLIT_IMPLEMENTATION"
  reason <- "si03b worker contract is in place; brms::brm simulation replicate body must preserve DGP semantics before heavy execution."
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason,
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, result_path = task$result_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("simulation", max(as.integer(tasks$cores), na.rm = TRUE), "si03b brms leakage workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_si03b_task_worker, parallel_cfg,
                                 export_names = "fit_si03b_task_worker", context = "si03b brms leakage workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "si03b brms leakage workers")
phase_end("si03b", "Fit BRMS leakage confirmation workers")
