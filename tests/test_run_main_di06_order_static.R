run_lines <- readLines("run.R", warn = FALSE)
run_text <- paste(run_lines, collapse = "\n")

main_start <- regexpr("main_steps <- list", run_text, fixed = TRUE)[1]
robustness_start <- regexpr("robustness_steps <- list", run_text, fixed = TRUE)[1]
if (main_start < 0 || robustness_start < 0 || robustness_start <= main_start) {
  stop("Could not isolate run.R main_steps block.")
}
main_text <- substr(run_text, main_start, robustness_start - 1L)

pos <- function(fragment, text = main_text) regexpr(fragment, text, fixed = TRUE)[1]

di03_pos <- pos("scripts/diagnostics/di03_exact_kfold_reclassification_audit.R")
di04_pos <- pos("scripts/diagnostics/di04_denominator_diagnostics.R")
di05_pos <- pos("scripts/diagnostics/di05_economic_validity_top_tail.R")
temporal_pos <- pos("scripts/diagnostics/di09_temporal_dependence_robustness.R")
ma17_pos <- pos("scripts/ma17_export_tables_figures.R")

if (di03_pos < 0) stop("run.R main missing di03.")
if (di04_pos < 0) stop("run.R main missing di04 denominator diagnostics.")
if (di05_pos < 0) stop("run.R main missing di05 economic-validity diagnostics.")
if (ma17_pos < 0) stop("run.R main missing ma17.")

if (!(di03_pos < di04_pos && di04_pos < di05_pos && di05_pos < ma17_pos)) {
  stop("run.R main order must be di03 -> di04_denominator -> di05_economic_validity -> ma17.")
}

if (temporal_pos > 0) {
  stop("Temporal-dependence robustness must not run as a default main step.")
}

cat("test_run_main_di06_order_static.R passed\n")
