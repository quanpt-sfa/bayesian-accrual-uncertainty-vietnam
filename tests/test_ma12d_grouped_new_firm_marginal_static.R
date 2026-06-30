# Static contract for MA12D v1.1 grouped-firm marginal new-firm rescoring.

txt <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

prepare_path <- "scripts/ma12d_prepare_grouped_new_firm_marginal_tasks.R"
worker_path <- "scripts/ma12e_compute_grouped_new_firm_marginal_workers.R"
collector_path <- "scripts/ma12f_collect_grouped_new_firm_marginal_scores.R"
wrapper_path <- "scripts/ma12d_compute_grouped_new_firm_marginal_scores.R"
di02_path <- "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R"
ma17_path <- "scripts/ma17_export_tables_figures.R"

prepare <- txt(prepare_path)
worker <- txt(worker_path)
collector <- txt(collector_path)
wrapper <- txt(wrapper_path)
di02 <- txt(di02_path)
ma17 <- txt(ma17_path)
all_ma12d <- paste(prepare, worker, collector, wrapper, sep = "\n")

for (path in c(prepare_path, worker_path, collector_path, wrapper_path)) {
  if (!file.exists(path)) stop("MA12D workerized structure missing: ", path)
}

for (fragment in c(
  "ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT",
  "ACCRUAL_MA12D_OUTPUT_RUN_ROOT",
  "ACCRUAL_MA12D_ALLOW_RESTACK_EXCLUDED",
  "ACCRUAL_MA12D_NEW_FIRM_DRAWS",
  "ACCRUAL_MA12D_MAX_POSTERIOR_DRAWS",
  "ACCRUAL_MA12D_SEED",
  "ACCRUAL_MA12D_FORCE_RECOMPUTE"
)) {
  if (!grepl(fragment, all_ma12d, fixed = TRUE)) {
    stop("MA12D/MA12E missing required environment-variable fragment: ", fragment)
  }
}

for (fragment in c(
  "accrual_run_task_pool",
  "accrual_fit_worker_config",
  "write_task_status",
  "accrual_task_status_blocker"
)) {
  if (!grepl(fragment, worker, fixed = TRUE)) {
    stop("MA12E worker missing worker-pool fragment: ", fragment)
  }
}

for (fragment in c(
  "Source_Reliability_Flag",
  "Source_Included_In_Stack",
  "MA12D_Primary_Stack_Eligible",
  "ACCRUAL_MA12D_ALLOW_RESTACK_EXCLUDED",
  "included_in_stack"
)) {
  if (!grepl(fragment, all_ma12d, fixed = TRUE)) {
    stop("MA12D missing source-gate inheritance fragment: ", fragment)
  }
}
if (!grepl("included_in_stack\\s*=\\s*\\.data\\$MA12D_Primary_Stack_Eligible", collector, perl = TRUE) &&
    !grepl("included_in_stack\\s*=\\s*MA12D_Primary_Stack_Eligible", collector, perl = TRUE)) {
  stop("MA12F must inherit included_in_stack from MA12D_Primary_Stack_Eligible.")
}

bad_completion_gate <- grepl(
  "reliability_flag\\s*=\\s*ifelse\\s*\\([^\\)]*N_Folds_Completed[^\\)]*\"OK\"",
  collector,
  perl = TRUE
) || grepl(
  "reliability_flag\\s*=\\s*case_when\\s*\\([^\\)]*N_Folds_Completed[^\\)]*~\\s*\"OK\"",
  collector,
  perl = TRUE
)
if (bad_completion_gate) {
  stop("MA12F must not self-generate OK reliability from MA12D completion status.")
}
if (grepl("reliability_flag\\s*=\\s*\"OK\"", collector, perl = TRUE)) {
  stop("MA12F must not hard-code OK reliability for MA12D scores.")
}

for (fragment in c(
  "u_new",
  "sigma_u",
  "rnorm",
  "log_mean_exp",
  "student_log_density_matrix",
  "posterior_linpred",
  "grouped_firm_marginal_new_firm_integrated"
)) {
  if (!grepl(fragment, worker, fixed = TRUE)) {
    stop("MA12E worker missing custom scoring fragment: ", fragment)
  }
}

if (grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", worker, perl = TRUE)) {
  stop("MA12E must be a rescoring module and must not refit with brm().")
}

if (grepl("brms::log_lik\\s*\\(", worker, perl = TRUE)) {
  stop("MA12E must not use brms::log_lik(re_formula = NA) as the Firm-RE marginal score.")
}

if (!grepl("re_formula\\s*=\\s*NA", worker, perl = TRUE) ||
    !grepl("posterior_linpred\\s*\\(", worker, perl = TRUE)) {
  stop("MA12E should use re_formula = NA only to extract the population-level linear predictor.")
}

for (fragment in c(
  "exact_grouped_kfold_marginal_new_firm_rescoring",
  "ma12e_compute_grouped_new_firm_marginal_workers.R",
  "has_custom_u_new_draw_logic",
  "custom_u_new_draw_logic"
)) {
  if (!grepl(fragment, di02, fixed = TRUE)) {
    stop("DI02 missing MA12E custom u_new audit fragment: ", fragment)
  }
}

for (fragment in c(
  "table_grouped_population_vs_marginal_new_firm_weight_comparison.csv",
  "table_grouped_marginal_new_firm_decision.csv",
  "table_winsor_kfold_weights_ex_post_marginal_new_firm.csv",
  "table_winsor_kfold_weights_no_lookahead_marginal_new_firm.csv",
  "grouped_new_firm_marginal",
  "LATEST_COMPLETED_RUN.txt",
  "ma12e_compute_grouped_new_firm_marginal_workers.R",
  "marginal_new_firm_prediction",
  "custom_u_new"
)) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("MA17 missing MA12D export/audit fragment: ", fragment)
  }
}

invisible(parse(prepare_path))
invisible(parse(worker_path))
invisible(parse(collector_path))
invisible(parse(wrapper_path))
invisible(parse(di02_path))
invisible(parse(ma17_path))

cat("test_ma12d_grouped_new_firm_marginal_static.R passed\n")
