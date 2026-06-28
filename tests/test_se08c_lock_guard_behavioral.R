# Lightweight behavioral guard for SE08C's pre-setup lock.

lock_path <- file.path("out", "interim", "winsor", "sensitivity", "fold_local_preprocessing", "logs", "se08c_collect.lock")
dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    paste0("PID=", Sys.getpid()),
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
    blocked <<- grepl("[BLOCKER] se08c is already running", conditionMessage(e), fixed = TRUE)
  }
)

if (!blocked) {
  stop("SE08C must block immediately when se08c_collect.lock contains a live PID.")
}

cat("test_se08c_lock_guard_behavioral.R passed\n")
