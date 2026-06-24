# Script: ma09b_fit_loo_savepars_refits.R
# Purpose: Fit secondary PSIS/LOO save_pars refit tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("ma09b", "Fit LOO save_pars refits")

tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma09_savepars_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma09_savepars_refit_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing ma09a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)

fit_ma09b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("ma09b task log", paste("Task_Key:", task$Task_Key)), task$task_log_path)
  status <- "BLOCKED_PENDING_SPLIT_IMPLEMENTATION"
  reason <- "ma09b worker contract is in place; model-specific brms refit body must preserve existing ma09 statistical semantics before heavy execution."
  # brms::brm(...) is intentionally not called unless the task-specific refit body is completed.
  metadata <- data.frame(
    Task_Key = task$Task_Key,
    status = status,
    reason = reason,
    RNG_Context = task$RNG_Context,
    Effective_Seed = task$Effective_Seed,
    backend = "rstan",
    stringsAsFactors = FALSE
  )
  write.csv(metadata, task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, metadata_path = task$metadata_path, stringsAsFactors = FALSE)
}

parallel_cfg <- accrual_fit_worker_config(
  "loo_savepars",
  cores_per_fit = if (nrow(tasks)) max(as.integer(tasks$cores), na.rm = TRUE) else 1L,
  context = "ma09b loo save_pars refits"
)
results <- accrual_run_task_pool(
  split(tasks, seq_len(nrow(tasks))),
  fit_ma09b_task_worker,
  parallel_cfg,
  export_names = "fit_ma09b_task_worker",
  context = "ma09b loo save_pars refits"
)
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "ma09b loo save_pars refits")
phase_end("ma09b", "Fit LOO save_pars refits")
