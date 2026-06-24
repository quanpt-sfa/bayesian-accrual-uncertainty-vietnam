txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

ma17 <- txt("scripts/ma17_export_tables_figures.R")

required_fragments <- c(
  'report_dir <- file.path(reports_root, "chapter3_methods_tables")',
  'path_baseline_table <- function(file) file.path(baseline_root, "tables", file)',
  'audit_prediction("scripts/ma12b_fit_grouped_kfold_firm_workers.R", "grouped_kfold")',
  'script = "scripts/ma12c_collect_grouped_kfold_firm_scores.R"',
  'Run split ma12a/ma12b/ma12c stages to produce train-standardization audit.'
)

for (fragment in required_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 missing split exact-KFold export contract fragment: ", fragment)
  }
}

forbidden_fragments <- c(
  'report_dir <- file.path("reports", "chapter3_methods_tables")',
  'file.path("out", "interim", "baseline", "tables", file)',
  'audit_prediction("scripts/ma12_grouped_kfold_firm.R", "grouped_kfold")',
  'script = "scripts/ma12_grouped_kfold_firm.R"',
  'Run scripts/ma12_grouped_kfold_firm.R'
)

for (fragment in forbidden_fragments) {
  if (grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 still contains stale/hard-coded export contract fragment: ", fragment)
  }
}

if (!grepl("reports_root", ma17, fixed = TRUE)) {
  stop("ma17 must use reports_root from ma00_setup.R for Chapter 3 exports.")
}

if (!grepl("baseline_root", ma17, fixed = TRUE)) {
  stop("ma17 must use baseline_root from ma00_setup.R for baseline fallbacks.")
}

cat("test_ma17_export_contract_static.R passed\n")
