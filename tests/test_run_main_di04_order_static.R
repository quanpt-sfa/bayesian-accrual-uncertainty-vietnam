run_text <- paste(readLines("run.R", warn = FALSE), collapse = "\n")

di03_pos <- regexpr("scripts/diagnostics/di03_exact_kfold_reclassification_audit.R", run_text, fixed = TRUE)[1]
di04_pos <- regexpr("scripts/diagnostics/di04_denominator_diagnostics.R", run_text, fixed = TRUE)[1]
ma17_pos <- regexpr("scripts/ma17_export_tables_figures.R", run_text, fixed = TRUE)[1]

if (di03_pos < 0) stop("run.R missing di03 exact-KFold reclassification stage.")
if (di04_pos < 0) stop("run.R missing di04 denominator diagnostics stage.")
if (ma17_pos < 0) stop("run.R missing ma17 export stage.")

if (!(di03_pos < di04_pos && di04_pos < ma17_pos)) {
  stop("run.R main order must be di03 -> di04_denominator_diagnostics -> ma17.")
}

if (!grepl("final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv", run_text, fixed = TRUE) ||
    !grepl("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv", run_text, fixed = TRUE)) {
  stop("run.R di04 stage must require grouped and row exact-KFold DA outputs.")
}

cat("test_run_main_di04_order_static.R passed\n")
