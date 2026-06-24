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
  for (fragment in c("accrual_run_task_pool(", "accrual_fit_worker_config(", "write_task_status(")) {
    if (!grepl(fragment, body, fixed = TRUE)) stop(path, " is not a worker fit script; missing ", fragment)
  }
  hits <- shared_output_fragments[vapply(shared_output_fragments, grepl, logical(1), x = body, fixed = TRUE)]
  if (length(hits)) stop(path, " names collector-owned shared output(s): ", paste(hits, collapse = ", "))
}

for (path in collector_scripts) {
  if (!file.exists(path)) stop("Missing collector split script: ", path)
  body <- txt(path)
  if (grepl("accrual_run_task_pool\\(", body, perl = TRUE)) stop(path, " collector must not run model-level worker pools.")
  if (grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", body, perl = TRUE)) stop(path, " collector must not fit brms models.")
  if (!grepl("accrual_task_status_blocker(", body, fixed = TRUE)) stop(path, " collector must block failed required tasks.")
}

cat("test_split_fit_collect_contract_static.R passed\n")
