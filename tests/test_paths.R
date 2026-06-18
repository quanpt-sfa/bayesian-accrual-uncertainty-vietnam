required_dirs <- c(
  "scripts",
  "data/raw",
  "out",
  "accruals",
  "reports",
  "doc",
  "tests"
)

missing_dirs <- required_dirs[!dir.exists(required_dirs)]
if (length(missing_dirs) > 0) {
  stop("Missing required directories: ", paste(missing_dirs, collapse = ", "))
}

cat("test_paths.R passed\n")
