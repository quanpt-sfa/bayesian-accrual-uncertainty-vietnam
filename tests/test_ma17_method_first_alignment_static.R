# Static contract for ma17 method-first paper alignment.

ma17_path <- "scripts/ma17_export_tables_figures.R"
patch_path <- "scripts/ma17_economic_validity_and_appendix_patch.R"

if (!file.exists(ma17_path)) stop("Missing ma17 export script: ", ma17_path)
if (file.exists(patch_path)) stop("ma17 must be a single-script exporter; remove patch script: ", patch_path)
if (file.exists("tests/test_ma17_economic_validity_patch_static.R")) {
  stop("ma17 patch integration test is obsolete; keep ma17 coverage in this alignment test.")
}

ma17 <- paste(readLines(ma17_path, warn = FALSE), collapse = "\n")
if (grepl("ma17_economic_validity_and_appendix_patch", ma17, fixed = TRUE) ||
    grepl("economic_validity_patch_path", ma17, fixed = TRUE) ||
    grepl("source\\(economic_validity_patch_path\\)", ma17)) {
  stop("ma17 must not source or reference a separate economic-validity patch script.")
}

required_stems <- c(
  "paper_table_1_sample_and_provenance_summary",
  "paper_table_2_model_space_bayesian_config",
  "paper_table_3_rq1_firmre_weight_reallocation",
  "paper_table_4_rq1_top_model_weights_by_validation_target",
  "paper_table_5_simulation_mechanism_evidence",
  "paper_table_6_rq2_reclassification_jaccard_spearman",
  "paper_appendix_A1_panel_coverage_industry_year_cells",
  "paper_appendix_A2_fold_balance_prediction_rule_audit",
  "paper_appendix_A3_preprocessing_prior_predictive_diagnostics",
  "paper_appendix_A4_denominator_diagnostics",
  "paper_appendix_A5_supplementary_economic_validity_diagnostics",
  "paper_appendix_A6_temporal_dependence_robustness",
  "paper_appendix_result_source_mapping"
)
for (stem in required_stems) {
  if (!grepl(stem, ma17, fixed = TRUE)) {
    stop("ma17 method-first exporter missing required paper/appendix stem: ", stem)
  }
}

provenance_fragments <- c(
  "FiinPro/FiinPro-X",
  "HOSE and HNX",
  "non-financial listed firms",
  "audited annual financial statement figures",
  "2015-2024 extraction window",
  "proprietary licensed dataset",
  "raw data are not redistributed",
  "no U.S.-style restatement-vintage distinction",
  "build_sheet2_provenance_audit",
  "Sheet1 HOSE/HNX exchange-only check",
  "Sheet2 non-financial industry screen",
  "Sheet2 listing/delisting status audit",
  "metadata_column_missing"
)
for (fragment in provenance_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 data-provenance table missing required fragment: ", fragment)
  }
}

source_fragments <- c(
  "table_winsor_exact_kfold_weight_comparison_row_vs_firm.csv",
  "table_winsor_kfold_weights_ex_post.csv",
  "table_winsor_kfold_weights_no_lookahead.csv",
  "table_winsor_row_exact_kfold_weights_ex_post.csv",
  "table_winsor_row_exact_kfold_weights_no_lookahead.csv",
  "table_lmer_leakage_pilot_grid_summary.csv",
  "table_brms_leakage_confirmation_grid_summary.csv",
  "table_si14_brms_recovery_n_sensitivity_summary.csv",
  "table_si14_brms_recovery_n_sensitivity_diagnostics.csv",
  "table_exact_kfold_reclassification_jaccard.csv"
)
for (fragment in source_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 method-first exporter missing required source artifact: ", fragment)
  }
}

logic_fragments <- c(
  "build_rq1_weight_reallocation_table",
  "row_minus_grouped_firm_re_shift",
  "row_over_grouped_firm_re_ratio",
  "build_rq1_top_model_table",
  "latest_file_by_name",
  "latest_file_by_names",
  "simulation_leakage_rows",
  "simulation_recovery_rows",
  "mean_weight_premium",
  "prob_positive_weight_premium",
  "grouped_firmre_weight_T_slope",
  "max_rhat",
  "total_divergent",
  "min_ess_bulk",
  "max_rhat_max",
  "min_ess_bulk_min",
  "n_divergent",
  "n_replications",
  "mean_error",
  "mean_abs_error",
  "summary_available_diagnostics_missing",
  "summary_and_diagnostics_available",
  "table5_metric_available",
  "QC20",
  "sigma0_premium_near_zero",
  "result_source_mapping",
  "source_md5",
  "ACCRUAL_EXPORT_SUPPLEMENTARY_ECON_VALIDITY",
  "q_value_BH_score_family",
  "q_value_BH_global",
  "expected_sign",
  "sign_consistent",
  "table_3_14_economic_validity_signed",
  "add_note",
  "notes_for_author",
  "suppressed_by_default",
  "PASS_SUPPRESSED_BY_DESIGN"
)
for (fragment in logic_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 method-first exporter missing required logic fragment: ", fragment)
  }
}

if (!grepl("if \\(!isTRUE\\(EXPORT_SUPPLEMENTARY_ECON_VALIDITY\\)", ma17)) {
  stop("Economic-validity exports must be suppressed by default in ma17.")
}

if (grepl("Supplementary economic-validity diagnostics are suppressed by default", ma17, fixed = TRUE)) {
  stop("Intentional economic-validity suppression must be a non-warning note, not an author-review warning.")
}

if (!grepl("paper_table_6_rq2_reclassification_jaccard_spearman", ma17, fixed = TRUE) ||
    !grepl("table_3_12_exact_kfold_reclassification_jaccard", ma17, fixed = TRUE)) {
  stop("ma17 must keep the existing RQ2 table_3_12 and add the paper Table 6 alias.")
}

if (grepl("brms_parameter_recovery_summary_path", ma17, fixed = TRUE) ||
    grepl("table_brms_parameter_recovery_summary.csv\",", ma17, fixed = TRUE)) {
  stop("ma17 Table 5 must not treat legacy brms_parameter_recovery as the official SI14 evidence path.")
}

cat("test_ma17_method_first_alignment_static.R passed\n")
