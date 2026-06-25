# -----------------------------------------------------------------------------
# Baseline completion marker helpers
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}

baseline_ma17_marker_path <- function(root = output_root) {
  file.path(root, "BASELINE_MA17_COMPLETE.txt")
}

write_baseline_ma17_complete_marker <- function(root = output_root, context = "main pipeline") {
  marker <- baseline_ma17_marker_path(root)
  dir.create(dirname(marker), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "BASELINE_MA17_COMPLETE",
    paste0("context=", context),
    paste0("timestamp=", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste0("output_root=", root),
    paste0("git_commit=", git_commit_or_na())
  )
  writeLines(lines, marker, useBytes = TRUE)
  invisible(marker)
}

assert_baseline_ma17_complete <- function(root = output_root, context = "downstream branch") {
  marker <- baseline_ma17_marker_path(root)
  if (!file.exists(marker)) {
    stop(
      "[BASELINE COMPLETION BLOCKER] ",
      context,
      " requires successful completion of the main pipeline through ma17. Missing marker: ",
      marker,
      ". Run `Rscript run.R main` first."
    )
  }
  invisible(TRUE)
}
