ma17_path <- "scripts/ma17_export_tables_figures.R"
if (!file.exists(ma17_path)) {
  stop("Missing ma17 export script: ", ma17_path)
}

ma17 <- paste(readLines(ma17_path, warn = FALSE), collapse = "\n")

if (!grepl("reports_root", ma17, fixed = TRUE)) {
  stop("ma17 must use reports_root from ma00_setup.R for Chapter 3 exports.")
}

if (!grepl("baseline_root", ma17, fixed = TRUE)) {
  stop("ma17 must use baseline_root from ma00_setup.R for baseline fallbacks.")
}

if (!grepl("scripts/ma12b_fit_grouped_kfold_firm_workers.R", ma17, fixed = TRUE)) {
  stop("ma17 must audit the active split grouped-KFold worker script.")
}

for (fragment in c(
  "table_denominator_diagnostics_decision.csv",
  "table_denominator_capped_jaccard.csv",
  "table_da_z_est_vs_z_pred_comparison.csv",
  "table_3_13_denominator_diagnostics_summary",
  "table_top_tail_group_economic_validity.csv",
  "table_top_tail_group_outcome_means.csv",
  "table_top_tail_group_economic_validity_decision.csv",
  "table_3_14_top_tail_economic_validity_summary",
  "table_temporal_dependence_firmre_premium.csv",
  "table_temporal_dependence_decision.csv",
  "table_3_15_temporal_dependence_robustness_summary"
)) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 must consume/export diagnostic robustness fragment: ", fragment)
  }
}

if (grepl('file.path("reports", "chapter3_methods_tables")', ma17, fixed = TRUE)) {
  stop("ma17 must not hard-code ", paste0("reports", "/"), "chapter3_methods_tables.")
}

if (grepl('file.path("out", "interim", "baseline", "tables", file)', ma17, fixed = TRUE)) {
  stop("ma17 must not hard-code out/interim/baseline/tables.")
}

if (grepl('audit_prediction("scripts/ma12_grouped_kfold_firm.R", "grouped_kfold")', ma17, fixed = TRUE) ||
    grepl('script = "scripts/ma12_grouped_kfold_firm.R"', ma17, fixed = TRUE)) {
  stop("ma17 must not use ma12_grouped_kfold_firm.R as the active grouped-KFold audit source.")
}

cat("test_ma17_export_contract_static.R passed\n")
