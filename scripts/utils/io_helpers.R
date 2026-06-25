# -----------------------------------------------------------------------------
# CSV and task table IO helpers
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

write_csv_safely <- function(x, file, row.names = FALSE, ...) {
  if (missing(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("[BLOCKER] write_csv_safely requires a single non-empty file path.")
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  write.csv(x, file = file, row.names = row.names, ...)
  invisible(file)
}

write_task_manifest <- function(path, tasks) {
  write_csv_safely(as.data.frame(tasks, stringsAsFactors = FALSE), path, row.names = FALSE)
}

write_task_status <- function(path, status_rows) {
  write_csv_safely(as.data.frame(status_rows, stringsAsFactors = FALSE), path, row.names = FALSE)
}

