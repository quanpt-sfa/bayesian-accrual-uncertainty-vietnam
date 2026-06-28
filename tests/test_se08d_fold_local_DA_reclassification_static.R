# Static contract for SE08D fold-local DA and RQ2 reclassification sensitivity.

paths <- c(
  se08b = "scripts/sensitivity/se08b_fit_fold_local_preprocessing_workers.R",
  se08c = "scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R",
  se08d = "scripts/sensitivity/se08d_construct_fold_local_DA_reclassification.R",
  helpers = "scripts/utils/analysis_helpers.R",
  ma17 = "scripts/ma17_export_tables_figures.R",
  run = "run.R"
)
for (p in paths) {
  if (!file.exists(p)) stop("Missing required file: ", p)
}

txt <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")
se08b <- txt(paths[["se08b"]])
se08c <- txt(paths[["se08c"]])
se08d <- txt(paths[["se08d"]])
helpers <- txt(paths[["helpers"]])
se08d_contract <- paste(se08d, helpers, sep = "\n")
ma17 <- txt(paths[["ma17"]])
run <- txt(paths[["run"]])

for (fragment in c(
  "pred_mean",
  "pred_sd",
  "posterior_epred",
  "posterior_predict",
  "score_brms_fold_local",
  "same_firm_history",
  "sample_new_levels = \"uncertainty\""
)) {
  if (!grepl(fragment, se08b, fixed = TRUE)) stop("se08b must emit predictive quantities for SE08D: ", fragment)
}

if (grepl("write_csv_safely\\(data.frame\\(\\).*table_se08_fold_local_reclassification_jaccard", se08c, perl = TRUE) ||
    grepl("write_csv_safely\\(data.frame\\(\\).*table_se08_fold_local_vs_global_reclassification_comparison", se08c, perl = TRUE)) {
  stop("se08c must not write empty RQ2 placeholders.")
}

for (fragment in c(
  "table_se08_grouped_fold_local_observation_scores.csv",
  "table_se08_row_fold_local_observation_scores.csv",
  "table_se08_grouped_fold_local_weights_ex_post.csv",
  "table_se08_grouped_fold_local_weights_no_lookahead.csv",
  "table_se08_row_fold_local_weights_ex_post.csv",
  "table_se08_row_fold_local_weights_no_lookahead.csv",
  "final_se08_fold_local_uncertainty_adjusted_accruals_grouped.csv",
  "final_se08_fold_local_uncertainty_adjusted_accruals_row.csv",
  "table_se08_fold_local_DA_source_manifest.csv",
  "table_se08_fold_local_DA_finite_gate.csv",
  "table_se08_fold_local_reclassification_jaccard.csv",
  "table_se08_fold_local_vs_global_reclassification_comparison.csv",
  "table_se08_fold_local_RQ2_decision.csv",
  "table_exact_kfold_reclassification_jaccard.csv",
  "table_se08_fold_local_top_tail_membership_sets.csv",
  "table_se08_fold_local_spearman_rank_correlation.csv",
  "stacked_pred_mean",
  "stacked_pred_sd",
  "DA_raw",
  "DA_z_estimation",
  "jaccard",
  "Spearman",
  "top_n",
  "intersection",
  "union",
  "only_row",
  "only_grouped",
  "build_se08d_rq2_global_fold_local_comparison",
  "decide_se08d_rq2_global_fold_local",
  "[BLOCKER] se08d global-vs-fold-local RQ2 comparison is incomplete",
  "global_jaccard",
  "fold_local_jaccard",
  "abs_absolute_difference",
  "global_material_turnover",
  "fold_local_material_turnover",
  "materiality_conclusion_unchanged",
  "global_spearman_rank_correlation",
  "fold_local_spearman_rank_correlation",
  "0.80",
  "0.60"
)) {
  if (!grepl(fragment, se08d_contract, fixed = TRUE)) stop("se08d missing DA/RQ2 contract fragment: ", fragment)
}

if (grepl("global_jaccard\\s*=\\s*NA_real_", se08d, perl = TRUE) ||
    grepl("global_value\\s*=\\s*comparison\\$global_jaccard\\[match", se08d, perl = TRUE)) {
  stop("se08d must not allow a decision table with unmatched/NA global Jaccard values.")
}

for (fragment in c(
  "se08d",
  "scripts/sensitivity/se08d_construct_fold_local_DA_reclassification.R",
  "table_se08_grouped_fold_local_observation_scores.csv",
  "table_se08_row_fold_local_observation_scores.csv"
)) {
  if (!grepl(fragment, run, fixed = TRUE)) stop("run.R missing se08d contract fragment: ", fragment)
}

for (fragment in c(
  "table_3_17_fold_local_RQ2_reclassification_sensitivity",
  "table_se08_fold_local_reclassification_jaccard.csv",
  "table_se08_fold_local_vs_global_reclassification_comparison.csv",
  "table_se08_fold_local_RQ2_decision.csv",
  "QC21",
  "fold_local_RQ2_reclassification_sensitivity_not_yet_available",
  "do not claim RQ2 fold-local preprocessing robustness"
)) {
  if (!grepl(fragment, ma17, fixed = TRUE)) stop("ma17 missing SE08D/RQ2 integration fragment: ", fragment)
}

cat("test_se08d_fold_local_DA_reclassification_static.R passed\n")
