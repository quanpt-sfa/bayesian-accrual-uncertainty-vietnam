txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

worker_scripts <- c(
  "scripts/ma09b_fit_loo_savepars_refits.R",
  "scripts/ma12b_fit_grouped_kfold_firm_workers.R",
  "scripts/ma13b_fit_row_level_exact_kfold_workers.R",
  "scripts/sensitivity/se02b_fit_prior_scenario_workers.R",
  "scripts/simulation/si03b_fit_brms_leakage_confirmation_workers.R",
  "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R",
  "scripts/diagnostics/di08b_fit_mcmc_sampler_calibration_workers.R"
)

collector_scripts <- c(
  "scripts/ma09c_collect_loo_stacking.R",
  "scripts/ma12c_collect_grouped_kfold_firm_scores.R",
  "scripts/ma13c_collect_row_level_exact_kfold_scores.R",
  "scripts/sensitivity/se02c_collect_prior_scenario_outputs.R",
  "scripts/simulation/si03c_collect_brms_leakage_confirmation.R",
  "scripts/simulation/si04c_collect_brms_parameter_recovery.R",
  "scripts/diagnostics/di08c_collect_mcmc_sampler_calibration.R"
)

shared_output_fragments <- c(
  "table_stacking_weights",
  "final_uncertainty_adjusted",
  "LATEST_COMPLETED_RUN",
  "manuscript",
  "table_winsor_kfold_weights",
  "table_winsor_row_exact_kfold_weights",
  "table_loo_comparison_winsor_corrected.csv"
)

for (path in worker_scripts) {
  if (!file.exists(path)) stop("Missing worker split script: ", path)
  body <- txt(path)
  if (grepl("BLOCKED_PENDING_SPLIT_IMPLEMENTATION", body, fixed = TRUE)) {
    stop(path, " still contains the forbidden split-stage placeholder status.")
  }
  if (grepl("contract is in place", body, fixed = TRUE)) {
    stop(path, " still contains worker-contract placeholder prose.")
  }
  if (!grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", body, perl = TRUE)) {
    stop(path, " must contain the task-specific brms fit body.")
  }
  for (fragment in c("accrual_run_task_pool(", "accrual_fit_worker_config(", "write_task_status(")) {
    if (!grepl(fragment, body, fixed = TRUE)) stop(path, " is not a worker fit script; missing ", fragment)
  }
  hits <- shared_output_fragments[vapply(shared_output_fragments, grepl, logical(1), x = body, fixed = TRUE)]
  if (length(hits)) stop(path, " names collector-owned shared output(s): ", paste(hits, collapse = ", "))
}

for (path in c("scripts/ma12a_plan_grouped_kfold_firm.R", "scripts/ma13a_plan_row_level_exact_kfold.R")) {
  body <- txt(path)
  for (fragment in c("Target_Sample", "Fold_Assignment_Path", "write.csv")) {
    if (!grepl(fragment, body, fixed = TRUE)) stop(path, " must plan fixed fold assignments and carry target samples; missing ", fragment)
  }
}

for (path in c("scripts/ma12b_fit_grouped_kfold_firm_workers.R", "scripts/ma13b_fit_row_level_exact_kfold_workers.R")) {
  body <- txt(path)
  if (!grepl("Fold_Assignment_Path", body, fixed = TRUE)) stop(path, " must read the planner-owned fold assignment artifact.")
  if (grepl("\\bsample\\s*\\(", body, perl = TRUE) || grepl("\\bset\\.seed\\s*\\(", body, perl = TRUE)) {
    stop(path, " must not create or randomize K-fold assignments inside workers.")
  }
}

ma09a_body <- txt("scripts/ma09a_plan_loo_savepars_refits.R")
for (fragment in c("Main_Stack_Inclusion", "Secondary_Robustness", "original_elpd", "original_k_above_07")) {
  if (!grepl(fragment, ma09a_body, fixed = TRUE)) stop("ma09a manifest must preserve old ma09 guard metadata: ", fragment)
}
ma09b_body <- txt("scripts/ma09b_fit_loo_savepars_refits.R")
for (fragment in c("ELPD shifted materially", "Coefficient shift", "table_coefficient_summary_winsor.csv")) {
  if (!grepl(fragment, ma09b_body, fixed = TRUE)) stop("ma09b must preserve old ma09 refit guard: ", fragment)
}
ma09c_body <- txt("scripts/ma09c_collect_loo_stacking.R")
for (fragment in c("Main_Stack_Inclusion", "Sample_Group", "main_common", "N mismatch", "loo_model_weights")) {
  if (!grepl(fragment, ma09c_body, fixed = TRUE)) stop("ma09c must preserve old ma09 stacking guard: ", fragment)
}

for (path in collector_scripts) {
  if (!file.exists(path)) stop("Missing collector split script: ", path)
  body <- txt(path)
  if (grepl("BLOCKED_PENDING_SPLIT_IMPLEMENTATION", body, fixed = TRUE)) {
    stop(path, " still contains the forbidden split-stage placeholder status.")
  }
  if (grepl("collect_contract|contract is in place", body, perl = TRUE)) {
    stop(path, " still contains collector-contract placeholder output.")
  }
  if (grepl("accrual_run_task_pool\\(", body, perl = TRUE)) stop(path, " collector must not run model-level worker pools.")
  if (grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", body, perl = TRUE)) stop(path, " collector must not fit brms models.")
  if (!grepl("accrual_task_status_blocker(", body, fixed = TRUE)) stop(path, " collector must block failed required tasks.")
}

cat("test_split_fit_collect_contract_static.R passed\n")
