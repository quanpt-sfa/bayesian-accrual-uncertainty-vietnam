txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

source("scripts/ma00_setup.R")

si04b_path <- "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R"
si04b <- txt(si04b_path)

for (fragment in c(
  "reconcile_si04b_task_artifacts <- function",
  "reconcile_si04b_status_table <- function",
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

if (!grepl("env_list(\"ACCRUAL_TASK_FILTER\")", si04b, fixed = TRUE)) {
  stop("si04b must parse ACCRUAL_TASK_FILTER through ma00 env_list().")
}
if (!grepl("env_choice(\"ACCRUAL_TASK_STATUS_FILTER\"", si04b, fixed = TRUE)) {
  stop("si04b must parse ACCRUAL_TASK_STATUS_FILTER through ma00 env_choice().")
}
if (!grepl("env_flag(\"ACCRUAL_RECONCILE_ONLY\"", si04b, fixed = TRUE)) {
  stop("si04b must parse ACCRUAL_RECONCILE_ONLY through ma00 env_flag().")
}

status_reconcile_pos <- regexpr("status <- reconcile_si04b_status_table(status, tasks)", si04b, fixed = TRUE)[1]
blocker_pos <- regexpr("accrual_task_status_blocker(status, required_col = \"Required\", context = \"si04b brms recovery workers\")", si04b, fixed = TRUE)[1]
if (status_reconcile_pos < 0 || blocker_pos < 0 || status_reconcile_pos > blocker_pos) {
  stop("si04b must reconcile the final status table before accrual_task_status_blocker().")
}

reconcile_only_pos <- regexpr("ACCRUAL_RECONCILE_ONLY=TRUE", si04b, fixed = TRUE)[1]
pool_pos <- regexpr("accrual_run_task_pool", si04b, fixed = TRUE)[1]
if (reconcile_only_pos < 0 || pool_pos < 0 || reconcile_only_pos > pool_pos) {
  stop("si04b reconcile-only mode must be handled before worker-pool dispatch.")
}

if (grepl("status <<-", si04b, fixed = TRUE)) {
  stop("si04b must not use status superassignment.")
}

cat("test_si04b_artifact_reconciliation_static.R passed\n")
