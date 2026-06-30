# Static contract for MA17 optional MA12D and SE10 exports.

txt <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

path <- "scripts/ma17_export_tables_figures.R"
ma17 <- txt(path)

for (fragment in c(
  "table_grouped_population_vs_marginal_new_firm_weight_comparison.csv",
  "table_grouped_marginal_new_firm_decision.csv",
  "table_winsor_kfold_weights_ex_post_marginal_new_firm.csv",
  "table_winsor_kfold_weights_no_lookahead_marginal_new_firm.csv",
  "table_se10_pooled_only_row_vs_grouped_family_shift.csv",
  "table_se10_pooled_only_decision.csv",
  "pooled_only_substacking",
  "ACCRUAL_REQUIRE_SE10_POOLED_ONLY"
)) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("MA17 missing MA12D/SE10 export fragment: ", fragment)
  }
}

if (!grepl("export_optional_se10", ma17, fixed = TRUE)) {
  stop("MA17 must export SE10 through an optional helper.")
}
if (!grepl("REQUIRE_SE10_POOLED_ONLY", ma17, fixed = TRUE)) {
  stop("MA17 must recognize strict SE10 env gate.")
}
if (!grepl("if \\(isTRUE\\(REQUIRE_SE10_POOLED_ONLY\\)\\) stop", ma17, perl = TRUE)) {
  stop("MA17 must block missing SE10 only when ACCRUAL_REQUIRE_SE10_POOLED_ONLY is true.")
}
if (!grepl("add_warning(msg)", ma17, fixed = TRUE)) {
  stop("MA17 must warn, not block, when optional SE10 output is missing by default.")
}

invisible(parse(path))
cat("test_ma17_ma12d_se10_export_static.R passed\n")
