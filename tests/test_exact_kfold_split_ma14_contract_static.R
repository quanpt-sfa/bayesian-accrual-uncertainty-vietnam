txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

source("scripts/ma00_setup.R")
grouped_cfg <- accrual_kfold_config("grouped_firm")
row_cfg <- accrual_kfold_config("row")
if (!all(c("chains", "cores", "iter", "warmup", "adapt_delta", "max_treedepth") %in% names(grouped_cfg))) {
  stop("grouped K-fold expected runtime config must come from ma00_setup.R.")
}
if (!all(c("chains", "cores", "iter", "warmup", "adapt_delta", "max_treedepth") %in% names(row_cfg))) {
  stop("row K-fold expected runtime config must come from ma00_setup.R.")
}

run_body <- txt("run.R")
for (fragment in c(
  'scripts/ma12a_plan_grouped_kfold_firm.R',
  'scripts/ma12b_fit_grouped_kfold_firm_workers.R',
  'scripts/ma12c_collect_grouped_kfold_firm_scores.R',
  'scripts/ma13a_plan_row_level_exact_kfold.R',
  'scripts/ma13b_fit_row_level_exact_kfold_workers.R',
  'scripts/ma13c_collect_row_level_exact_kfold_scores.R'
)) {
  if (!grepl(fragment, run_body, fixed = TRUE)) stop("run.R missing split exact K-fold stage: ", fragment)
}
for (fragment in c(
  'step("ma12", "scripts/ma12_grouped_kfold_firm.R"',
  'step("ma13", "scripts/ma13_row_level_exact_kfold.R"'
)) {
  if (grepl(fragment, run_body, fixed = TRUE)) stop("run.R must not call monolithic exact K-fold script: ", fragment)
}

ma12a <- txt("scripts/ma12a_plan_grouped_kfold_firm.R")
for (fragment in c("accrual_exact_kfold_run_context(\"grouped_firm\"", "Kfold_Run_Root", "Config_Tag", "compat_manifest_path")) {
  if (!grepl(fragment, ma12a, fixed = TRUE)) stop("ma12a must plan grouped split tasks into a run-root contract: ", fragment)
}

ma13a <- txt("scripts/ma13a_plan_row_level_exact_kfold.R")
for (fragment in c("accrual_exact_kfold_run_context(\"row\"", "Row_KFold_Root", "Config_Tag", "compat_manifest_path")) {
  if (!grepl(fragment, ma13a, fixed = TRUE)) stop("ma13a must plan row split tasks into a run-root contract: ", fragment)
}

ma12b <- txt("scripts/ma12b_fit_grouped_kfold_firm_workers.R")
for (fragment in c("accrual_extract_brms_mcmc_diagnostics", "Max_Rhat", "Min_ESS_Bulk", "Min_ESS_Tail", "Divergences", "Treedepth_Warnings", "run_status_path")) {
  if (!grepl(fragment, ma12b, fixed = TRUE)) stop("ma12b must write fold-level diagnostics and run-root status: ", fragment)
}

ma13b <- txt("scripts/ma13b_fit_row_level_exact_kfold_workers.R")
for (fragment in c("accrual_extract_brms_mcmc_diagnostics", "Max_Rhat", "Min_ESS_Bulk", "Min_ESS_Tail", "Divergences", "Treedepth_Warnings", "run_status_path")) {
  if (!grepl(fragment, ma13b, fixed = TRUE)) stop("ma13b must write fold-level diagnostics and run-root status: ", fragment)
}

ma12c <- txt("scripts/ma12c_collect_grouped_kfold_firm_scores.R")
for (fragment in c(
  "run_config_manifest.csv",
  "LATEST_RUN.txt",
  "LATEST_COMPLETED_RUN.txt",
  "Completed_Run_Pin_Eligible",
  "Kfold_Run_Root",
  "Weight_KFold",
  "Rank_KFold",
  "Model_Key_KFold",
  "Singleton_ELPD",
  "reliability_flag",
  "table_winsor_kfold_weights_ex_post.csv",
  "table_winsor_kfold_weights_no_lookahead.csv"
)) {
  if (!grepl(fragment, ma12c, fixed = TRUE)) stop("ma12c missing ma14 grouped contract fragment: ", fragment)
}

ma13c <- txt("scripts/ma13c_collect_row_level_exact_kfold_scores.R")
for (fragment in c(
  "row_exact_kfold_run_manifest.csv",
  "LATEST_RUN.txt",
  "LATEST_COMPLETED_RUN.txt",
  "Primary_Inference_Allowed",
  "Completed_Run_Pin_Eligible",
  "Row_KFold_Root",
  "weight_row_exact_kfold",
  "rank_row_exact_kfold",
  "model_key_row_exact_kfold",
  "reliability_flag",
  "table_winsor_row_exact_kfold_weights_ex_post.csv",
  "table_winsor_row_exact_kfold_weights_no_lookahead.csv"
)) {
  if (!grepl(fragment, ma13c, fixed = TRUE)) stop("ma13c missing ma14 row contract fragment: ", fragment)
}

ma14 <- txt("scripts/ma14_construct_exact_kfold_DA.R")
for (fragment in c("validate_grouped_run", "validate_row_run", "Weight_KFold", "weight_row_exact_kfold", "LATEST_COMPLETED_RUN.txt")) {
  if (!grepl(fragment, ma14, fixed = TRUE)) stop("ma14 must retain exact K-fold completed-run contract fragment: ", fragment)
}

cat("test_exact_kfold_split_ma14_contract_static.R passed\n")
