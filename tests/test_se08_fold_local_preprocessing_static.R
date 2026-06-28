# Static contract for fold-local preprocessing sensitivity B-1.

paths <- c(
  plan = "scripts/sensitivity/se08a_plan_fold_local_preprocessing_kfold.R",
  worker = "scripts/sensitivity/se08b_fit_fold_local_preprocessing_workers.R",
  collect = "scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R",
  da = "scripts/sensitivity/se08d_construct_fold_local_DA_reclassification.R",
  ma17 = "scripts/ma17_export_tables_figures.R",
  run = "run.R"
)
for (p in paths) {
  if (!file.exists(p)) stop("Missing required se08 contract file: ", p)
}

txt <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")
plan <- txt(paths[["plan"]])
worker <- txt(paths[["worker"]])
collect <- txt(paths[["collect"]])
ma17 <- txt(paths[["ma17"]])
run <- txt(paths[["run"]])

required_plan_fragments <- c(
  "table_se08_fold_local_preprocessing_task_manifest.csv",
  "table_se08_fold_local_preprocessing_task_status.csv",
  "LATEST_COMPLETED_RUN.txt",
  "[BLOCKER] se08 requires the completed",
  "final_common_ex_post_sample.csv",
  "final_common_realtime_sample.csv",
  "M08",
  "M10",
  "grouped_firm_kfold",
  "row_exact_kfold",
  "table_ma12_grouped_kfold_fold_assignment.csv",
  "table_ma13_row_kfold_fold_assignment.csv"
)
for (fragment in required_plan_fragments) {
  if (!grepl(fragment, plan, fixed = TRUE)) stop("se08 planner missing fragment: ", fragment)
}

required_worker_fragments <- c(
  "compute_train_winsor_cutoffs(train_raw",
  "compute_train_standardization_params(train_win",
  "apply_winsor_cutoffs(test_raw, cutoffs)",
  "apply_standardization_params(test_win, params)",
  "audit_fold_local_preprocessing",
  "re_formula = NA",
  "allow_new_levels = TRUE",
  "re_formula = NULL",
  "sample_new_levels = \"uncertainty\"",
  "same_firm_history_available",
  "new_company_in_row_fold",
  "primary_row_target_inclusion",
  "pred_mean",
  "pred_sd",
  "posterior_epred",
  "posterior_predict",
  "score_brms_fold_local",
  "default_prior_list",
  "brms_family",
  "Effective_Seed"
)
for (fragment in required_worker_fragments) {
  if (!grepl(fragment, worker, fixed = TRUE)) stop("se08 worker missing fragment: ", fragment)
}

if (grepl("read_winsor_sample\\s*\\(", worker, perl = TRUE)) {
  stop("se08 worker must not call read_winsor_sample(); it must build fold-local preprocessing from raw baseline samples.")
}

required_collect_fragments <- c(
  "table_se08_fold_local_preprocessing_audit.csv",
  "table_se08_fold_local_cutoff_summary.csv",
  "table_se08_fold_local_standardization_summary.csv",
  "table_se08_grouped_fold_local_observation_scores.csv",
  "table_se08_grouped_fold_local_model_scores.csv",
  "table_se08_grouped_fold_local_weights_ex_post.csv",
  "table_se08_grouped_fold_local_weights_no_lookahead.csv",
  "table_se08_row_fold_local_observation_scores.csv",
  "table_se08_row_fold_local_model_scores.csv",
  "table_se08_row_fold_local_weights_ex_post.csv",
  "table_se08_row_fold_local_weights_no_lookahead.csv",
  "table_se08_fold_local_vs_global_weight_comparison.csv",
  "table_se08_fold_local_vs_global_firmre_shift_summary.csv",
  "table_se08_fold_local_vs_global_top_model_comparison.csv",
  "table_se08_fold_local_sensitivity_decision.csv",
  "row_minus_grouped_firmre_shift",
  "row_over_grouped_firmre_ratio",
  "optimize_stacking_from_lpd",
  "LATEST_COMPLETED_RUN.txt"
)
for (fragment in required_collect_fragments) {
  if (!grepl(fragment, collect, fixed = TRUE)) stop("se08 collector missing fragment: ", fragment)
}

if (grepl("table_se08_fold_local_reclassification_jaccard.csv", collect, fixed = TRUE) ||
    grepl("table_se08_fold_local_vs_global_reclassification_comparison.csv", collect, fixed = TRUE)) {
  stop("se08c must not write empty RQ2 reclassification placeholders; se08d owns DA/reclassification outputs.")
}

required_ma17_fragments <- c(
  "paper_appendix_A7_fold_local_preprocessing_sensitivity",
  "table_3_16_fold_local_preprocessing_sensitivity_summary",
  "fold_local_preprocessing_sensitivity_available",
  "fold_local_preprocessing_sensitivity_not_yet_available",
  "fold_local_preprocessing_sensitivity_failed",
  "se08_fold_local_overall",
  "QC11"
)
for (fragment in required_ma17_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) stop("ma17 missing se08 integration fragment: ", fragment)
}

for (fragment in c(
  "scripts/sensitivity/se08a_plan_fold_local_preprocessing_kfold.R",
  "scripts/sensitivity/se08b_fit_fold_local_preprocessing_workers.R",
  "scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R",
  "scripts/sensitivity/se08d_construct_fold_local_DA_reclassification.R"
)) {
  if (!grepl(fragment, run, fixed = TRUE)) stop("run.R sensitivity target missing se08 step: ", fragment)
}

cat("test_se08_fold_local_preprocessing_static.R passed\n")
