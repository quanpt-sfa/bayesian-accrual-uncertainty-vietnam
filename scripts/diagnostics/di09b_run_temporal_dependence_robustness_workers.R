# -----------------------------------------------------------------------------
# Script: scripts/diagnostics/di09b_run_temporal_dependence_robustness_workers.R
# Purpose: Run DI09 split-worker tasks in one R session using the repo worker pool.
#
# Important environment variables:
#   ACCRUAL_RUN_TEMPORAL_ROBUSTNESS=TRUE     # required gate for this heavy diagnostic
#   ACCRUAL_ENABLE_MODEL_PARALLEL=TRUE/FALSE
#   ACCRUAL_MODEL_PARALLEL_WORKERS=<n>
#   ACCRUAL_TOTAL_CORE_BUDGET=<n>
#   ACCRUAL_DI09_FORCE_RERUN=TRUE/FALSE
#   ACCRUAL_DI09_TASK_KEYS=<optional comma-separated task keys>
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di09b", "Run temporal-dependence robustness split-worker tasks")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()
source("scripts/diagnostics/di09_temporal_dependence_helpers.R")

if (!env_flag("ACCRUAL_RUN_TEMPORAL_ROBUSTNESS", "FALSE")) {
  stop("[BLOCKER] Temporal-dependence robustness is gated because it can run many lmer fits. ",
       "Set ACCRUAL_RUN_TEMPORAL_ROBUSTNESS=TRUE to run this diagnostic intentionally.")
}

stage_start_time <- Sys.time()
dirs <- di09_temporal_dirs()
cfg <- di09_runtime_config()

manifest_path <- file.path(dirs$tables, "table_di09_temporal_dependence_task_manifest.csv")
status_path <- file.path(dirs$tables, "table_di09_temporal_dependence_task_status.csv")
if (!file.exists(manifest_path)) {
  stop("[BLOCKER] Missing DI09 task manifest. Run di09a first: ", manifest_path)
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!nrow(manifest)) stop("[BLOCKER] DI09 task manifest has zero rows.")

di09_force_rerun_enabled <- function() {
  # env_flag()/env_value in this repo expects a single env-name string.
  local_value <- Sys.getenv("ACCRUAL_DI09_FORCE_RERUN", unset = "")
  if (nzchar(local_value)) {
    return(toupper(trimws(local_value)) %in% c("TRUE", "T", "1", "YES", "Y"))
  }
  env_flag("ACCRUAL_FORCE_REFIT", "FALSE")
}

force_rerun <- di09_force_rerun_enabled()
task_key_filter <- env_list("ACCRUAL_DI09_TASK_KEYS")
if (length(task_key_filter)) {
  manifest <- manifest[manifest$Task_Key %in% task_key_filter, , drop = FALSE]
  if (!nrow(manifest)) stop("[BLOCKER] ACCRUAL_DI09_TASK_KEYS did not match any task in manifest.")
}

parallel_cfg <- accrual_fit_worker_config(
  kind = "diagnostic",
  cores_per_fit = 1L,
  context = "di09b temporal-dependence robustness diagnostic"
)

task_list <- lapply(seq_len(nrow(manifest)), function(i) as.list(manifest[i, ]))

di09_write_task_status <- function(task, status, start_time, end_time,
                                   n_rows = NA_integer_, n_success_pairs = NA_integer_,
                                   n_failed_rows = NA_integer_, error = NA_character_) {
  row <- data.frame(
    Task_ID = as.integer(task$Task_ID),
    Task_Key = as.character(task$Task_Key),
    T = as.integer(task$T),
    rho = as.numeric(task$rho),
    sigma_firm = as.numeric(task$sigma_firm),
    Replications = as.integer(task$Replications),
    status = status,
    worker_pid = Sys.getpid(),
    start_time = as.character(start_time),
    end_time = as.character(end_time),
    runtime_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    n_rows = as.integer(n_rows),
    n_successful_replication_pairs = as.integer(n_success_pairs),
    n_failed_rows = as.integer(n_failed_rows),
    result_path = as.character(task$result_path),
    status_path = as.character(task$status_path),
    error = error,
    stringsAsFactors = FALSE
  )
  dir.create(dirname(as.character(task$status_path)), recursive = TRUE, showWarnings = FALSE)
  write_csv_safely(row, as.character(task$status_path), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(row)
}

di09b_task_worker <- function(task) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  source("scripts/ma00_setup.R")

  task_start <- Sys.time()
  result_path <- as.character(task$result_path)
  log_path <- as.character(task$task_log_path)
  dir.create(dirname(result_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  force_rerun_local <- di09_force_rerun_enabled()
  existing_state <- di09_existing_result_state(result_path, task)

  writeLines(
    c(
      paste("Task:", as.character(task$Task_Key)),
      paste("Task_ID:", as.integer(task$Task_ID)),
      paste("Started:", as.character(task_start)),
      paste("T:", as.integer(task$T)),
      paste("rho:", as.numeric(task$rho)),
      paste("sigma_firm:", as.numeric(task$sigma_firm)),
      paste("Rep_Start:", as.integer(task$Rep_Start)),
      paste("Rep_End:", as.integer(task$Rep_End)),
      paste("K:", as.integer(task$K)),
      paste("n_firms:", as.integer(task$n_firms)),
      paste("n_industries:", as.integer(task$n_industries)),
      paste("sigma_eps:", as.numeric(task$sigma_eps)),
      paste("base_seed:", as.integer(task$base_seed)),
      paste("result_path:", result_path),
      paste("existing_result_reusable:", existing_state$reusable),
      paste("existing_result_reason:", existing_state$reason),
      paste("force_rerun:", force_rerun_local)
    ),
    log_path,
    useBytes = TRUE
  )

  if (isTRUE(existing_state$reusable) && !force_rerun_local) {
    status_row <- di09_write_task_status(
      task = task,
      status = "SKIPPED_EXISTING_COMPLETE",
      start_time = task_start,
      end_time = Sys.time(),
      n_rows = existing_state$n_rows,
      n_success_pairs = existing_state$n_success_pairs,
      n_failed_rows = existing_state$n_failed_rows,
      error = NA_character_
    )
    writeLines(
      c(readLines(log_path, warn = FALSE), paste("status:", "SKIPPED_EXISTING_COMPLETE")),
      log_path,
      useBytes = TRUE
    )
    return(status_row)
  }

  if (file.exists(result_path) && !isTRUE(existing_state$reusable)) {
    message("[DI09B] Existing result is stale/incomplete and will be overwritten: ", result_path,
            " | reason=", existing_state$reason)
  }

  lme4_available <- requireNamespace("lme4", quietly = TRUE)
  rep_ids <- seq.int(as.integer(task$Rep_Start), as.integer(task$Rep_End))
  out <- vector("list", length(rep_ids))

  for (jj in seq_along(rep_ids)) {
    rep_id <- rep_ids[[jj]]
    out[[jj]] <- tryCatch(
      di09_run_replication(
        T = as.integer(task$T),
        rho = as.numeric(task$rho),
        sigma_firm = as.numeric(task$sigma_firm),
        replication = rep_id,
        K = as.integer(task$K),
        n_firms = as.integer(task$n_firms),
        n_industries = as.integer(task$n_industries),
        sigma_eps = as.numeric(task$sigma_eps),
        base_seed = as.integer(task$base_seed),
        lme4_available = lme4_available
      ),
      error = function(e) {
        di09_failure_rows(
          T = as.integer(task$T),
          rho = as.numeric(task$rho),
          sigma_firm = as.numeric(task$sigma_firm),
          replication = rep_id,
          K = as.integer(task$K),
          n_firms = as.integer(task$n_firms),
          e = e
        )
      }
    )

    # Task-local checkpoint only. The final shared CSV is written by di09c.
    if (jj %% 10L == 0L || jj == length(rep_ids)) {
      partial <- dplyr::bind_rows(out[seq_len(jj)])
      partial$Task_ID <- as.integer(task$Task_ID)
      partial$Task_Key <- as.character(task$Task_Key)
      partial$Worker_PID <- Sys.getpid()
      write_csv_safely(partial, result_path, row.names = FALSE, fileEncoding = "UTF-8")
    }
  }

  result <- dplyr::bind_rows(out)
  result$Task_ID <- as.integer(task$Task_ID)
  result$Task_Key <- as.character(task$Task_Key)
  result$Worker_PID <- Sys.getpid()
  write_csv_safely(result, result_path, row.names = FALSE, fileEncoding = "UTF-8")

  n_success_pairs <- di09_successful_replication_pairs(result)
  n_failed_rows <- sum(result$fit_status != "SUCCESS", na.rm = TRUE)
  n_insufficient_rows <- sum(result$fit_status == "INSUFFICIENT_DEPENDENCY", na.rm = TRUE)
  expected_pairs <- length(rep_ids)

  status_value <- if (n_success_pairs == expected_pairs && n_failed_rows == 0L) {
    "SUCCESS"
  } else if (n_insufficient_rows > 0L && n_success_pairs == 0L) {
    "INSUFFICIENT_DEPENDENCY"
  } else if (n_success_pairs > 0L) {
    "PARTIAL_SUCCESS"
  } else {
    "FAILED"
  }

  status_error <- if (n_failed_rows > 0L) paste(unique(na.omit(result$warning)), collapse = " | ") else NA_character_
  status_row <- di09_write_task_status(
    task = task,
    status = status_value,
    start_time = task_start,
    end_time = Sys.time(),
    n_rows = nrow(result),
    n_success_pairs = n_success_pairs,
    n_failed_rows = n_failed_rows,
    error = status_error
  )

  writeLines(
    c(
      readLines(log_path, warn = FALSE),
      paste("Ended:", as.character(Sys.time())),
      paste("status:", status_value),
      paste("n_rows:", nrow(result)),
      paste("n_successful_replication_pairs:", n_success_pairs),
      paste("n_failed_rows:", n_failed_rows)
    ),
    log_path,
    useBytes = TRUE
  )

  status_row
}

statuses <- accrual_run_task_pool(
  tasks = task_list,
  worker_fun = di09b_task_worker,
  parallel_cfg = parallel_cfg,
  export_names = c(
    "di09_force_rerun_enabled",
    "di09_write_task_status",
    "di09_existing_result_state",
    "di09_successful_replication_pairs",
    "di09_expected_target_pair_ok",
    "di09_run_replication",
    "di09_failure_rows",
    "di09_simulate_panel_ar1",
    "di09_safe_seed",
    "di09_score_cv",
    "di09_score_fold",
    "di09_normal_lpd",
    "di09_make_row_folds",
    "di09_make_grouped_folds"
  ),
  packages = c("dplyr"),
  context = "di09b temporal-dependence robustness diagnostic"
)

status_df <- bind_rows(statuses) %>% arrange(Task_ID)
write_csv_safely(status_df, status_path, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(dirs$logs, "sessionInfo_di09b.txt"))

cat("[SUCCESS] DI09B worker-pool stage completed.\n")
cat("Tasks processed: ", nrow(status_df), "\n", sep = "")
cat("Status: ", status_path, "\n", sep = "")
cat("Runtime seconds: ", as.numeric(difftime(Sys.time(), stage_start_time, units = "secs")), "\n", sep = "")

phase_end("di09b", "Run temporal-dependence robustness split-worker tasks")
