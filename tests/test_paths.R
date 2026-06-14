required_dirs <- c(
  "scripts/v3",
  "data/raw",
  "R",
  "out/interim",
  "out/fits",
  "out/diagnostics",
  "out/loo",
  "out/lofo",
  "out/kfold",
  "out/sensitivity",
  "out/logs",
  "out/manifests",
  "accruals/baseline",
  "accruals/sensitivity/baseline",
  "accruals/sensitivity/tight",
  "accruals/sensitivity/wide",
  "reports",
  "doc",
  "tests"
)

missing_dirs <- required_dirs[!dir.exists(required_dirs)]
if (length(missing_dirs) > 0) {
  stop("Missing required directories: ", paste(missing_dirs, collapse = ", "))
}

cat("test_paths.R passed\n")
