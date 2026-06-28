# Lightweight behavioral guard for SE08C's pre-setup lock.

lock_path <- file.path("out", "interim", "winsor", "sensitivity", "fold_local_preprocessing", "logs", "se08c_collect.lock")
dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "PID=999999",
    paste0("start_time=", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste0("commandArgs=", paste(commandArgs(), collapse = " ")),
    paste0("working_directory=", getwd())
  ),
  lock_path,
  useBytes = TRUE
)
on.exit(unlink(lock_path, force = TRUE), add = TRUE)

blocked <- FALSE
tryCatch(
  source("scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R"),
  error = function(e) {
    blocked <<- grepl("[BLOCKER] se08c lock exists; refusing to start another collector", conditionMessage(e), fixed = TRUE)
  }
)

if (!blocked) {
  stop("SE08C must block immediately whenever se08c_collect.lock already exists.")
}
if (!file.exists(lock_path)) {
  stop("SE08C must not remove an existing lock unless ACCRUAL_SE08C_CLEAR_STALE_LOCK=TRUE.")
}

old_clear <- Sys.getenv("ACCRUAL_SE08C_CLEAR_STALE_LOCK", unset = NA_character_)
old_disable <- Sys.getenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG", unset = NA_character_)
old_root <- Sys.getenv("ACCRUAL_OUTPUT_ROOT", unset = NA_character_)
old_input <- Sys.getenv("ACCRUAL_INPUT_WINSOR_ROOT", unset = NA_character_)
on.exit({
  if (is.na(old_clear)) Sys.unsetenv("ACCRUAL_SE08C_CLEAR_STALE_LOCK") else Sys.setenv(ACCRUAL_SE08C_CLEAR_STALE_LOCK = old_clear)
  if (is.na(old_disable)) Sys.unsetenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG") else Sys.setenv(ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = old_disable)
  if (is.na(old_root)) Sys.unsetenv("ACCRUAL_OUTPUT_ROOT") else Sys.setenv(ACCRUAL_OUTPUT_ROOT = old_root)
  if (is.na(old_input)) Sys.unsetenv("ACCRUAL_INPUT_WINSOR_ROOT") else Sys.setenv(ACCRUAL_INPUT_WINSOR_ROOT = old_input)
}, add = TRUE)
tmp_root <- normalizePath(file.path(tempdir(), paste0("se08c_lock_", Sys.getpid())), winslash = "/", mustWork = FALSE)
Sys.setenv(
  ACCRUAL_SE08C_CLEAR_STALE_LOCK = "TRUE",
  ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = "TRUE",
  ACCRUAL_OUTPUT_ROOT = tmp_root,
  ACCRUAL_INPUT_WINSOR_ROOT = tmp_root
)

cleared_and_reached_setup <- FALSE
tryCatch(
  source("scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R"),
  error = function(e) {
    cleared_and_reached_setup <<- grepl("[BLOCKER] se08c requires se08a manifest", conditionMessage(e), fixed = TRUE)
  }
)
if (!cleared_and_reached_setup) {
  stop("SE08C stale-lock clear mode must remove the lock and proceed to normal setup blockers.")
}

cat("test_se08c_lock_guard_behavioral.R passed\n")
