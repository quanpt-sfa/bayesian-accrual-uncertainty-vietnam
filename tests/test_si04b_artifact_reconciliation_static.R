txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

source("scripts/ma00_setup.R")

si04b_path <- "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R"
si04b <- txt(si04b_path)

for (fragment in c(
  "reconcile_si04b_task_artifacts <- function",
  "reconcile_si04b_status_table <- function",
  "merge_si04b_status_rows <- function",
  "ACCRUAL_TASK_FILTER",
  "ACCRUAL_TASK_STATUS_FILTER",
  "ACCRUAL_RECONCILE_ONLY",
  "inherits(fit_read$value, \"brmsfit\")",
  "is.data.frame(result_read$value)",
  "all(internal == \"SUCCESS\")",
  "RECOVERED_FROM_VALID_ARTIFACTS",
  "valid_fit_and_success_result_artifacts",
  "artifact_reconciled",
  "reconciliation_reason",
  "fit_exists",
  "result_exists",
  "fit_readable",
  "result_readable",
  "fit_class",
  "result_internal_status",
  "metadata_path",
  "task_log_path"
)) {
  if (!grepl(fragment, si04b, fixed = TRUE)) {
    stop("si04b missing artifact reconciliation contract fragment: ", fragment)
  }
}

for (fragment in c(
  "replace rows for Task_Key values present in new_status",
  "keep prior rows",
  "MISSING_STATUS_ROW"
)) {
  if (!grepl(fragment, si04b, fixed = TRUE)) {
    stop("si04b missing filtered status merge fragment: ", fragment)
  }
}

if (!grepl("env_list(\"ACCRUAL_TASK_FILTER\")", si04b, fixed = TRUE)) {
  stop("si04b must parse ACCRUAL_TASK_FILTER through ma00 env_list().")
}
if (!grepl("env_choice(\"ACCRUAL_TASK_STATUS_FILTER\"", si04b, fixed = TRUE)) {
  stop("si04b must parse ACCRUAL_TASK_STATUS_FILTER through ma00 env_choice().")
}
if (!grepl("env_flag(\"ACCRUAL_RECONCILE_ONLY\"", si04b, fixed = TRUE)) {
  stop("si04b must parse ACCRUAL_RECONCILE_ONLY through ma00 env_flag().")
}

worker_tail_start <- regexpr("new_status <- do.call(rbind, results)", si04b, fixed = TRUE)[1]
if (worker_tail_start < 0) stop("si04b worker path must bind new_status before merging status rows.")
worker_tail <- substr(si04b, worker_tail_start, nchar(si04b))
merge_pos <- regexpr("status <- merge_si04b_status_rows(new_status, prior_status, manifest_tasks)", worker_tail, fixed = TRUE)[1]
status_reconcile_pos <- regexpr("status <- reconcile_si04b_status_table(status, manifest_tasks)", worker_tail, fixed = TRUE)[1]
blocker_pos <- regexpr("accrual_task_status_blocker(status, required_col = \"Required\", context = \"si04b brms recovery workers\")", worker_tail, fixed = TRUE)[1]
if (merge_pos < 0 || status_reconcile_pos < 0 || blocker_pos < 0 ||
    merge_pos > status_reconcile_pos || status_reconcile_pos > blocker_pos) {
  stop("si04b must merge, reconcile the full status table, and then call accrual_task_status_blocker().")
}
reconcile_only_tail_start <- regexpr("ACCRUAL_RECONCILE_ONLY=TRUE", si04b, fixed = TRUE)[1]
reconcile_only_tail <- substr(si04b, reconcile_only_tail_start, worker_tail_start - 1L)
if (!grepl("status <- merge_si04b_status_rows(do.call(rbind, rows), prior, manifest_tasks)", reconcile_only_tail, fixed = TRUE) ||
    !grepl("status <- reconcile_si04b_status_table(status, manifest_tasks)", reconcile_only_tail, fixed = TRUE) ||
    !grepl("accrual_task_status_blocker(status, required_col = \"Required\", context = \"si04b brms recovery reconcile-only\")", reconcile_only_tail, fixed = TRUE)) {
  stop("si04b reconcile-only path must merge and reconcile the full manifest status table before blocking.")
}

reconcile_only_pos <- regexpr("ACCRUAL_RECONCILE_ONLY=TRUE", si04b, fixed = TRUE)[1]
pool_pos <- regexpr("accrual_run_task_pool", si04b, fixed = TRUE)[1]
if (reconcile_only_pos < 0 || pool_pos < 0 || reconcile_only_pos > pool_pos) {
  stop("si04b reconcile-only mode must be handled before worker-pool dispatch.")
}

if (grepl("status <<-", si04b, fixed = TRUE)) {
  stop("si04b must not use status superassignment.")
}

tmp_root <- file.path(tempdir(), paste0("si04b_reconcile_test_", Sys.getpid()))
root <- file.path(tmp_root, "simulation", "brms_parameter_recovery")
dir.create(file.path(root, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "artifacts"), recursive = TRUE, showWarnings = FALSE)
manifest_path <- file.path(root, "tables", "table_si04_brms_recovery_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si04_brms_recovery_task_status.csv")

task1_fit <- file.path(root, "artifacts", "task1_fit.rds")
task1_result <- file.path(root, "artifacts", "task1_result.rds")
task2_fit <- file.path(root, "artifacts", "task2_fit.rds")
task2_result <- file.path(root, "artifacts", "task2_result.rds")
task2_fit_object <- list(note = "fake readable fit for artifact reconciliation test")
class(task2_fit_object) <- "brmsfit"
saveRDS(task2_fit_object, task2_fit)
saveRDS(data.frame(status = "SUCCESS", value = 1, stringsAsFactors = FALSE), task2_result)

manifest <- data.frame(
  Task_Key = c("si04_test_success_prior", "si04_test_failed_recovered"),
  fit_path = c(task1_fit, task2_fit),
  result_path = c(task1_result, task2_result),
  metadata_path = file.path(root, "artifacts", c("task1_meta.csv", "task2_meta.csv")),
  task_log_path = file.path(root, "artifacts", c("task1.log", "task2.log")),
  Required = c(TRUE, TRUE),
  stringsAsFactors = FALSE
)
write.csv(manifest, manifest_path, row.names = FALSE)
prior_status <- data.frame(
  Task_Key = manifest$Task_Key,
  status = c("SUCCESS", "FAILED"),
  reason = c(NA_character_, "cannot open the connection"),
  Required = c(TRUE, TRUE),
  fit_path = manifest$fit_path,
  result_path = manifest$result_path,
  metadata_path = manifest$metadata_path,
  task_log_path = manifest$task_log_path,
  stringsAsFactors = FALSE
)
write.csv(prior_status, status_path, row.names = FALSE)

subprocess_script <- tempfile(fileext = ".R")
writeLines(c(
  sprintf("Sys.setenv(ACCRUAL_OUTPUT_ROOT = %s)", dQuote(normalizePath(tmp_root, winslash = "/", mustWork = FALSE))),
  "Sys.setenv(ACCRUAL_RECONCILE_ONLY = 'TRUE')",
  "Sys.setenv(ACCRUAL_TASK_FILTER = 'si04_test_failed_recovered')",
  "source('scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R')"
), subprocess_script, useBytes = TRUE)
cmd_out <- system2("Rscript", subprocess_script, stdout = TRUE, stderr = TRUE)
exit_status <- attr(cmd_out, "status")
if (!is.null(exit_status) && exit_status != 0L) {
  stop("si04b reconcile-only subprocess failed: ", paste(cmd_out, collapse = "\n"))
}

final_status <- read.csv(status_path, stringsAsFactors = FALSE)
if (!identical(sort(final_status$Task_Key), sort(manifest$Task_Key))) {
  stop("Filtered reconcile-only status table must retain prior unrelated task rows.")
}
task1 <- final_status[final_status$Task_Key == "si04_test_success_prior", , drop = FALSE]
task2 <- final_status[final_status$Task_Key == "si04_test_failed_recovered", , drop = FALSE]
if (nrow(task1) != 1L || nrow(task2) != 1L) stop("Expected exactly one final status row per task.")
if (!identical(task1$status[[1]], "SUCCESS")) stop("Unrelated prior SUCCESS row was not preserved.")
if (!identical(task2$status[[1]], "SUCCESS") ||
    !isTRUE(task2$artifact_reconciled[[1]]) ||
    !identical(task2$reconciliation_reason[[1]], "valid_fit_and_success_result_artifacts")) {
  stop("Filtered failed task was not recovered from valid artifacts.")
}

cat("test_si04b_artifact_reconciliation_static.R passed\n")
