ma17_path <- "scripts/ma17_export_tables_figures.R"
if (!file.exists(ma17_path)) {
  stop("Missing ma17 export script: ", ma17_path)
}

ma17 <- paste(readLines(ma17_path, warn = FALSE), collapse = "\n")

required_fragments <- c(
  "discover_si05_si06_temporal_bundle",
  "score_temporal_weight_bundle",
  "temporal_coverage_alignment",
  "si05_si06_temporal_bundle <- discover_si05_si06_temporal_bundle(output_root)",
  "coverage_match_rate",
  "coverage_complete_all",
  "Selected SI05/SI06 temporal evidence bundle has incomplete or unmatched coverage",
  "si05_temporal_grid_summary_path",
  "table_lmer_temporal_dependence_grid_summary.csv",
  "si05_temporal_rep_results_path",
  "table_lmer_temporal_dependence_rep_results.csv",
  "si05_temporal_cell_coverage_path",
  "table_si05_lmer_temporal_cell_coverage.csv",
  "si05_temporal_decision_path",
  "table_si05_lmer_temporal_decision.csv",
  "si06_temporal_mechanism_summary_path",
  "table_temporal_dependence_mechanism_summary.csv",
  "di09_temporal_elpd_premium_path",
  "di09_temporal_elpd_decision_path",
  "ACCRUAL_EXPORT_DI09_TEMPORAL_ELPD_DIAGNOSTIC",
  "simulation_temporal_weight_rows",
  "build_temporal_weight_appendix",
  "sigma0_weight_premium_near_zero_not_leakage",
  "PASS_SIGMA0_ANCHOR_NEAR_ZERO",
  "positive_row_minus_grouped_firmre_weight_premium_under_same_firm_heterogeneity",
  "secondary_elpd_diagnostic_not_leakage_weight_evidence",
  "paper_appendix_A6b_temporal_dependence_elpd_diagnostic",
  "SI05/SI06 LMER temporal-dependence weight-premium simulation",
  "brms_confirmation_pilot_sampler_review_required",
  "diagnostic_only_pilot",
  "rq1_weight_reallocation_artifacts_available",
  "rq1_top_model_weight_artifacts_available"
)

for (fragment in required_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 temporal evidence contract missing fragment: ", fragment)
  }
}

if (grepl("temporal_decision_value", ma17, fixed = TRUE)) {
  stop("ma17 must not attach DI09 temporal decision labels to Appendix A6 rows.")
}

if (grepl("WARN_ROW_PREMIUM_INCREASES_WITH_TEMPORAL_DEPENDENCE", ma17, fixed = TRUE)) {
  stop("ma17 must not use DI09 global row-premium warning as SI05/SI06 leakage-weight evidence.")
}

if (grepl("lmer_temporal_root <- file.path\\(output_root, \"simulation\", \"lmer_temporal_dependence\"\\)", ma17)) {
  stop("ma17 must discover the current SI05/SI06 temporal bundle instead of hard-coding one lmer_temporal_dependence path.")
}

if (!grepl("paper_appendix_A6_temporal_dependence_robustness", ma17, fixed = TRUE) ||
    !grepl("build_temporal_weight_appendix(", ma17, fixed = TRUE)) {
  stop("Appendix A6 must be built from the SI05/SI06 temporal weight-premium helper.")
}

if (grepl("paste\\(na.omit\\(c\\(Exact_KFold_Reclassification_Decision,[[:space:]]*Primary_Magnitude_Reclassification_Decision\\)\\)", ma17)) {
  stop("RQ1 Table 3/Table 4 source mapping must not reuse RQ2 DI03 reclassification decisions.")
}

cat("test_ma17_temporal_evidence_contract_static.R passed\n")
