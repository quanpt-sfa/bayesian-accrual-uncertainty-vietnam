# -----------------------------------------------------------------------------
# Script: scripts/simulation/si05b_run_lmer_temporal_dependence_workers.R
# Purpose: Run SI05 split-worker tasks.
#
# Worker selection:
#   ACCRUAL_SI05_WORKER_ID = 1..N
#   ACCRUAL_SI05_N_WORKERS = N
#
# Each worker runs tasks where ((Task_ID - 1) %% N) + 1 == WORKER_ID.
# Results are written to task_artifacts/results/*.csv, never directly to the
# final combined table. This avoids race conditions.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si05b", "Run LMER temporal-dependence split-worker tasks")
source("scripts/simulation/si00_helpers.R")
source("scripts/simulation/si05_lmer_temporal_dependence_helpers.R")

check_sim_packages(c("lme4", "dplyr", "ggplot2"))
suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
})

worker_start_time <- Sys.time()
dirs <- si05_ensure_temporal_dirs()
cfg <- si05_runtime_config()

manifest_path <- file.path(dirs$tables, "table_si05_lmer_temporal_task_manifest.csv")
if (!file.exists(manifest_path)) {
  stop("[BLOCKER] Missing SI05 task manifest. Run si05a first: ", manifest_path)
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!nrow(manifest)) stop("[BLOCKER] SI05 task manifest has zero rows.")

worker_id <- env_int("ACCRUAL_SI05_WORKER_ID", 1L, min = 1L)
n_workers <- env_int("ACCRUAL_SI05_N_WORKERS", 1L, min = 1L)
if (worker_id > n_workers) stop("[BLOCKER] ACCRUAL_SI05_WORKER_ID cannot exceed ACCRUAL_SI05_N_WORKERS.")

force_rerun <- env_flag(c("ACCRUAL_SI05_FORCE_RERUN", "ACCRUAL_FORCE_REFIT"), "FALSE")

task_key_filter <- env_list("ACCRUAL_SI05_TASK_KEYS")
if (length(task_key_filter)) {
  todo <- manifest[manifest$Task_Key %in% task_key_filter, , drop = FALSE]
} else {
  assigned_worker <- ((as.integer(manifest$Task_ID) - 1L) %% n_workers) + 1L
  todo <- manifest[assigned_worker == worker_id, , drop = FALSE]
}

if (!nrow(todo)) {
  cat("[INFO] SI05 worker ", worker_id, "/", n_workers, " has no assigned tasks.\n", sep = "")
  phase_end("si05b", "Run LMER temporal-dependence split-worker tasks")
  quit(save = "no", status = 0)
}

write_one_status <- function(task, status, start_time, end_time, n_rows = NA_integer_,
                             n_success = NA_integer_, n_failed = NA_integer_,
                             error = NA_character_) {
  row <- data.frame(
    Task_ID = as.integer(task$Task_ID),
    Task_Key = as.character(task$Task_Key),
    T = as.integer(task$T),
    sigma_firm = as.numeric(task$sigma_firm),
    rho = as.numeric(task$rho),
    shock_duration = as.integer(task$shock_duration),
    Replications = as.integer(task$Replications),
    status = status,
    worker_id = worker_id,
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
  write_csv_safely(row, as.character(task$status_path), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(row)
}

cat("[INFO] SI05 worker ", worker_id, "/", n_workers, " assigned tasks: ", nrow(todo), "\n", sep = "")

worker_status_rows <- list()
for (ii in seq_len(nrow(todo))) {
  task <- todo[ii, , drop = FALSE]
  task_start <- Sys.time()
  result_path <- as.character(task$result_path)
  log_path <- as.character(task$task_log_path)

  if (file.exists(result_path) && !force_rerun) {
    existing <- tryCatch(read.csv(result_path, stringsAsFactors = FALSE), error = function(e) NULL)
    n_rows <- if (is.null(existing)) NA_integer_ else nrow(existing)
    n_success <- if (is.null(existing) || !"error" %in% names(existing)) NA_integer_ else sum(is.na(existing$error) | existing$error == "")
    n_failed <- if (is.null(existing) || !"error" %in% names(existing)) NA_integer_ else sum(!(is.na(existing$error) | existing$error == ""))
    worker_status_rows[[length(worker_status_rows) + 1L]] <- write_one_status(
      task, "SKIPPED_EXISTING", task_start, Sys.time(), n_rows, n_success, n_failed, NA_character_
    )
    next
  }

  msg <- sprintf(
    "[SI05B worker %d/%d] Task %d/%d: %s | T=%d sigma=%.4f rho=%.4f duration=%d reps=%d:%d",
    worker_id, n_workers, ii, nrow(todo), task$Task_Key,
    as.integer(task$T), as.numeric(task$sigma_firm), as.numeric(task$rho),
    as.integer(task$shock_duration), as.integer(task$Rep_Start), as.integer(task$Rep_End)
  )
  message(msg)
  writeLines(c(msg, paste("start_time:", as.character(task_start))), log_path, useBytes = TRUE)

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
    if (jj %% 10 == 0L || jj == length(rep_ids)) {
      partial <- bind_rows(out[seq_len(jj)])
      partial$Task_ID <- as.integer(task$Task_ID)
      partial$Task_Key <- as.character(task$Task_Key)
      partial$Worker_ID <- worker_id
      write_csv_safely(partial, result_path, row.names = FALSE, fileEncoding = "UTF-8")
    }
  }

  result <- bind_rows(out)
  result$Task_ID <- as.integer(task$Task_ID)
  result$Task_Key <- as.character(task$Task_Key)
  result$Worker_ID <- worker_id
  write_csv_safely(result, result_path, row.names = FALSE, fileEncoding = "UTF-8")

  n_success <- sum(is.na(result$error) | result$error == "")
  n_failed <- sum(!(is.na(result$error) | result$error == ""))
  status_value <- if (n_success == nrow(result)) "SUCCESS" else if (n_success > 0) "PARTIAL_SUCCESS" else "FAILED"

  status_row <- write_one_status(
    task = task,
    status = status_value,
    start_time = task_start,
    end_time = Sys.time(),
    n_rows = nrow(result),
    n_success = n_success,
    n_failed = n_failed,
    error = if (n_failed > 0) paste(unique(na.omit(result$error)), collapse = " | ") else NA_character_
  )
  worker_status_rows[[length(worker_status_rows) + 1L]] <- status_row

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
}

worker_status <- bind_rows(worker_status_rows)
worker_status_path <- file.path(
  dirs$logs,
  sprintf("si05_worker_%03d_of_%03d_status.csv", worker_id, n_workers)
)
write_csv_safely(worker_status, worker_status_path, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(dirs$logs, sprintf("sessionInfo_si05b_worker_%03d.txt", worker_id)))

cat("[SUCCESS] SI05 worker completed.\n")
cat("Worker status: ", worker_status_path, "\n", sep = "")
cat("Assigned tasks: ", nrow(todo), "\n", sep = "")
cat("Runtime seconds: ", as.numeric(difftime(Sys.time(), worker_start_time, units = "secs")), "\n", sep = "")

phase_end("si05b", "Run LMER temporal-dependence split-worker tasks")
