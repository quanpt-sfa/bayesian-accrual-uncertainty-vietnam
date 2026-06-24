# Script: se02b_fit_prior_scenario_workers.R
# Purpose: Fit sensitivity prior-scenario tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("se02b", "Fit prior-scenario workers")
tables_dir <- file.path(output_root, "sensitivity", "tables")
manifest_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing se02a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_se02b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("se02b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  status <- "BLOCKED_PENDING_SPLIT_IMPLEMENTATION"
  reason <- "se02b worker contract is in place; brms::brm sensitivity refit body must preserve existing scenario semantics before heavy execution."
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason, backend = "rstan",
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, draw_path = task$draw_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("sensitivity", max(as.integer(tasks$cores), na.rm = TRUE), "se02b prior-scenario workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_se02b_task_worker, parallel_cfg,
                                 export_names = "fit_se02b_task_worker", context = "se02b prior-scenario workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "se02b prior-scenario workers")
phase_end("se02b", "Fit prior-scenario workers")
