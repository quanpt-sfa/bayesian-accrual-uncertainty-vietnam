txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

di02_path <- "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R"
if (!file.exists(di02_path)) stop("Missing di02 script: ", di02_path)
di02 <- txt(di02_path)

required_fragments <- c(
  "exact_grouped_kfold_worker",
  "exact_grouped_kfold_collector",
  "exact_row_kfold_worker",
  "exact_row_kfold_collector",
  "exact_kfold_DA_constructor",
  "scripts\", \"ma12b_fit_grouped_kfold_firm_workers.R",
  "scripts\", \"ma12c_collect_grouped_kfold_firm_scores.R",
  "scripts\", \"ma13b_fit_row_level_exact_kfold_workers.R",
  "scripts\", \"ma13c_collect_row_level_exact_kfold_scores.R",
  "scripts\", \"ma14_construct_exact_kfold_DA.R"
)

for (fragment in required_fragments) {
  if (!grepl(fragment, di02, fixed = TRUE)) {
    stop("di02 missing active split source-audit fragment: ", fragment)
  }
}

forbidden_active_fragments <- c(
  "file.path(\"scripts\", \"ma12_grouped_kfold_firm.R\")",
  "file.path(\"scripts\", \"ma13_row_level_exact_kfold.R\")"
)

for (fragment in forbidden_active_fragments) {
  if (grepl(fragment, di02, fixed = TRUE)) {
    stop("di02 still uses monolithic exact-KFold script as active source evidence: ", fragment)
  }
}

cat("test_di02_active_split_source_static.R passed\n")
