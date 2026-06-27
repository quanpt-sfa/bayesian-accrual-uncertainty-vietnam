# -----------------------------------------------------------------------------
# Script: scripts/simulation/si05b_run_lmer_temporal_dependence_workers.R
# Purpose: Run SI05 split-worker tasks in one R session using the repo worker pool.
#
# This version follows the ma07 pattern:
#   - one command launches the stage;
#   - if ACCRUAL_ENABLE_MODEL_PARALLEL=TRUE, tasks are distributed to PSOCK
#     workers via accrual_run_task_pool();
#   - each task writes only task-local result/status/log files;
#   - final combined CSVs are written only by si05c.
#
# Important environment variables:
#   ACCRUAL_ENABLE_MODEL_PARALLEL=TRUE/FALSE
#   ACCRUAL_MODEL_PARALLEL_WORKERS=<n>
#   ACCRUAL_TOTAL_CORE_BUDGET=<n>
#   ACCRUAL_SI05_FORCE_RERUN=TRUE/FALSE
#   ACCRUAL_SI05_TASK_KEYS=<optional comma-separated task keys>
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("si05b", "Run LMER temporal-dependence split-worker tasks")
source("scripts/simulation/si00_helpers.R")
source("scripts/simulation/si05_lmer_temporal_dependence_helpers.R")

check_sim_packages(c("lme4", "dplyr", "ggplot2"))
suppressPackageStartupMessages({
  library(lme4)
})

stage_start_time <- Sys.time()
dirs <- si05_ensure_temporal_dirs()
cfg <- si05_runtime_config()

manifest_path <- file.path(dirs$tables, "table_si05_lmer_temporal_task_manifest.csv")
status_path <- file.path(dirs$tables, "table_si05_lmer_temporal_task_status.csv")
if (!file.exists(manifest_path)) {
  stop("[BLOCKER] Missing SI05 task manifest. Run si05a first: ", manifest_path)
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!nrow(manifest)) stop("[BLOCKER] SI05 task manifest has zero rows.")

si05_force_rerun_enabled <- function() {
  # env_flag()/env_value in this repo expects a single env-name string.
  # Do not pass c("ACCRUAL_SI05_FORCE_RERUN", "ACCRUAL_FORCE_REFIT") here.
  local_value <- Sys.getenv("ACCRUAL_SI05_FORCE_RERUN", unset = "")
  if (nzchar(local_value)) {
    return(toupper(trimws(local_value)) %in% c("TRUE", "T", "1", "YES", "Y"))
  }
  env_flag("ACCRUAL_FORCE_REFIT", "FALSE")
}

force_rerun <- si05_force_rerun_enabled()
task_key_filter <- env_list("ACCRUAL_SI05_TASK_KEYS")
if (length(task_key_filter)) {
  manifest <- manifest[manifest$Task_Key %in% task_key_filter, , drop = FALSE]
  if (!nrow(manifest)) {
    stop("[BLOCKER] ACCRUAL_SI05_TASK_KEYS did not match any task in manifest.")
  }
}

# SI05 lmer tasks are single-core fits. Parallelization is across design-cell tasks.
# This mirrors the MA07 worker-pool pattern, but with cores_per_fit = 1 to avoid
# oversubscription and Windows nested parallelism problems.
parallel_cfg <- accrual_fit_worker_config(
  kind = "simulation",
  cores_per_fit = 1L,
  context = "si05b lmer temporal-dependence simulation"
)

task_list <- lapply(seq_len(nrow(manifest)), function(i) as.list(manifest[i, ]))

si05_write_task_status <- function(task, status, start_time, end_time,
                                   n_rows = NA_integer_, n_success = NA_integer_,
                                   n_failed = NA_integer_, error = NA_character_) {
  row <- data.frame(
    Task_ID = as.integer(task$Task_ID),
    Task_Key = as.character(task$Task_Key),
    T = as.integer(task$T),
    sigma_firm = as.numeric(task$sigma_firm),
    rho = as.numeric(task$rho),
    shock_duration = as.integer(task$shock_duration),
    Replications = as.integer(task$Replications),
    status = status,
    worker_pid = Sys.getpid(),
    start_time = as.character(start_time),
    end_time = as.character(end_time),
    runtime_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    n_rows = as.integer(n_rows),
    n_successful_replications = as.integer(n_success),
    n_failed_replications = as.integer(n_failed),
    result_path = as.character(task$result_path),
    status_path = as.character(task$status_path),
    error = error,
    stringsAsFactors = FALSE
  )
  dir.create(dirname(as.character(task$status_path)), recursive = TRUE, showWarnings = FALSE)
  write_csv_safely(row, as.character(task$status_path), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(row)
}

si05b_task_worker <- function(task) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(lme4)
  })
  source("scripts/ma00_setup.R")
  source("scripts/simulation/si00_helpers.R")
  source("scripts/simulation/si05_lmer_temporal_dependence_helpers.R")

  # Re-read runtime config inside each worker to avoid exporting large/fragile
  # parent-frame objects across PSOCK workers.
  cfg <- si05_runtime_config()
  force_rerun_local <- si05_force_rerun_enabled()

  task_start <- Sys.time()
  result_path <- as.character(task$result_path)
  log_path <- as.character(task$task_log_path)
  dir.create(dirname(result_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  task_header <- sprintf(
    "[SI05B pid=%s] Task_ID=%d Task_Key=%s T=%d sigma=%.4f rho=%.4f duration=%d reps=%d:%d",
    Sys.getpid(), as.integer(task$Task_ID), as.character(task$Task_Key),
    as.integer(task$T), as.numeric(task$sigma_firm), as.numeric(task$rho),
    as.integer(task$shock_duration), as.integer(task$Rep_Start), as.integer(task$Rep_End)
  )
  message(task_header)
  writeLines(c(task_header, paste("start_time:", as.character(task_start))), log_path, useBytes = TRUE)

  if (file.exists(result_path) && !force_rerun_local) {
    existing <- tryCatch(read.csv(result_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    n_rows <- if (is.null(existing)) NA_integer_ else nrow(existing)
    n_success <- if (is.null(existing) || !"error" %in% names(existing)) NA_integer_ else sum(is.na(existing$error) | existing$error == "")
    n_failed <- if (is.null(existing) || !"error" %in% names(existing)) NA_integer_ else sum(!(is.na(existing$error) | existing$error == ""))
    status_row <- si05_write_task_status(
      task, "SKIPPED_EXISTING", task_start, Sys.time(), n_rows, n_success, n_failed, NA_character_
    )
    return(status_row)
  }

  rep_ids <- seq.int(as.integer(task$Rep_Start), as.integer(task$Rep_End))
  out <- vector("list", length(rep_ids))

  for (jj in seq_along(rep_ids)) {
    rep_id <- rep_ids[[jj]]
    out[[jj]] <- tryCatch(
      si05_run_temporal_replication(
        cfg = cfg,
        T = as.integer(task$T),
        sigma_firm = as.numeric(task$sigma_firm),
        rho = as.numeric(task$rho),
        shock_duration = as.integer(task$shock_duration),
        rep_id = rep_id
      ),
      error = function(e) {
        si05_error_replication_row(
          cfg = cfg,
          T = as.integer(task$T),
          sigma_firm = as.numeric(task$sigma_firm),
          rho = as.numeric(task$rho),
          shock_duration = as.integer(task$shock_duration),
          rep_id = rep_id,
          e = e
        )
      }
    )

    # Task-local checkpoint only. No final shared CSV is written here.
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

  n_success <- sum(is.na(result$error) | result$error == "")
  n_failed <- sum(!(is.na(result$error) | result$error == ""))
  status_value <- if (n_success == nrow(result)) {
    "SUCCESS"
  } else if (n_success > 0) {
    "PARTIAL_SUCCESS"
  } else {
    "FAILED"
  }

  status_row <- si05_write_task_status(
    task = task,
    status = status_value,
    start_time = task_start,
    end_time = Sys.time(),
    n_rows = nrow(result),
    n_success = n_success,
    n_failed = n_failed,
    error = if (n_failed > 0) paste(unique(na.omit(result$error)), collapse = " | ") else NA_character_
  )

  writeLines(
    c(
      readLines(log_path, warn = FALSE),
      paste("end_time:", as.character(Sys.time())),
      paste("status:", status_value),
      paste("n_success:", n_success),
      paste("n_failed:", n_failed)
    ),
    log_path,
    useBytes = TRUE
  )

  status_row
}

statuses <- accrual_run_task_pool(
  tasks = task_list,
  worker_fun = si05b_task_worker,
  parallel_cfg = parallel_cfg,
  export_names = c("si05_write_task_status", "si05_force_rerun_enabled"),
  packages = c("dplyr", "lme4"),
  context = "si05b lmer temporal-dependence simulation"
)

status_df <- bind_rows(statuses) %>% arrange(Task_ID)
write_csv_safely(status_df, status_path, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(dirs$logs, "sessionInfo_si05b.txt"))

cat("[SUCCESS] SI05B worker-pool stage completed.\n")
cat("Tasks processed: ", nrow(status_df), "\n", sep = "")
cat("Status: ", status_path, "\n", sep = "")
cat("Runtime seconds: ", as.numeric(difftime(Sys.time(), stage_start_time, units = "secs")), "\n", sep = "")

phase_end("si05b", "Run LMER temporal-dependence split-worker tasks")
