# Script: di08b_fit_mcmc_sampler_calibration_workers.R
# Purpose: Fit diagnostic MCMC sampler calibration tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("di08b", "Fit MCMC sampler calibration workers")
root <- file.path(output_root, "diagnostics", "mcmc_sampler_calibration")
manifest_path <- file.path(root, "tables", "table_di08_sampler_calibration_task_manifest.csv")
status_path <- file.path(root, "tables", "table_di08_sampler_calibration_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing di08a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_di08b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("di08b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  status <- "BLOCKED_PENDING_SPLIT_IMPLEMENTATION"
  reason <- "di08b worker contract is in place; diagnostic brms::brm calibration body must remain non-production before heavy execution."
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason,
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, diagnostic_path = task$diagnostic_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("diagnostic_calibration", max(as.integer(tasks$cores), na.rm = TRUE), "di08b calibration workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_di08b_task_worker, parallel_cfg,
                                 export_names = "fit_di08b_task_worker", context = "di08b calibration workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "di08b calibration workers")
phase_end("di08b", "Fit MCMC sampler calibration workers")
