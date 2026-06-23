txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

files <- list.files("scripts", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
for (path in files) {
  lines <- readLines(path, warn = FALSE)
  header <- grep("^# Script:", lines, value = TRUE)
  if (length(header)) {
    if (!grepl(basename(path), header[[1]], fixed = TRUE)) {
      stop("Script header does not contain actual basename for ", path, ": ", header[[1]], call. = FALSE)
    }
  }
}

antigravity_hits <- files[vapply(files, function(path) {
  grepl("Author: Antigravity", txt(path), fixed = TRUE)
}, logical(1))]
if (length(antigravity_hits)) {
  stop("Tool-name authorship remains in script headers/body: ", paste(antigravity_hits, collapse = ", "), call. = FALSE)
}

cat("test_script_header_filename_consistency.R passed\n")
