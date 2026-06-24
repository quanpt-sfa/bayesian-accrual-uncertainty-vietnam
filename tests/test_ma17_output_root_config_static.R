txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

ma17 <- txt("scripts/ma17_export_tables_figures.R")

required_fragments <- c(
  'report_dir <- file.path(reports_root, "chapter3_methods_tables")',
  'path_baseline_table <- function(file) file.path(baseline_root, "tables", file)',
  'audit_prediction("scripts/ma12b_fit_grouped_kfold_firm_workers.R", "grouped_kfold")',
  'Run split ma12a/ma12b/ma12c stages to produce train-standardization audit.'
)

for (fragment in required_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 missing run-root/config-aware fragment: ", fragment)
  }
}

forbidden_fragments <- c(
  'report_dir <- file.path("reports", "chapter3_methods_tables")',
  'path_baseline_table <- function(file) file.path("out", "interim", "baseline", "tables", file)',
  'audit_prediction("scripts/ma12_grouped_kfold_firm.R", "grouped_kfold")',
  'Run scripts/ma12_grouped_kfold_firm.R to produce train-standardization audit.'
)

for (fragment in forbidden_fragments) {
  if (grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 still contains hard-coded or monolithic-output fragment: ", fragment)
  }
}

cat("test_ma17_output_root_config_static.R passed\n")
