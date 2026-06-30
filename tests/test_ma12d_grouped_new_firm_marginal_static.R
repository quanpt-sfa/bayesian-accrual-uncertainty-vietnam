# Static contract for MA12D grouped-firm marginal new-firm rescoring.

txt <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

ma12d_path <- "scripts/ma12d_compute_grouped_new_firm_marginal_scores.R"
di02_path <- "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R"
ma17_path <- "scripts/ma17_export_tables_figures.R"

ma12d <- txt(ma12d_path)
di02 <- txt(di02_path)
ma17 <- txt(ma17_path)

for (fragment in c(
  "ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT",
  "ACCRUAL_MA12D_SOURCE_ROW_KFOLD_RUN_ROOT",
  "ACCRUAL_MA12D_OUTPUT_RUN_ROOT",
  "ACCRUAL_MA12D_NEW_FIRM_DRAWS",
  "ACCRUAL_MA12D_MAX_POSTERIOR_DRAWS",
  "ACCRUAL_MA12D_SEED",
  "ACCRUAL_MA12D_FORCE_RECOMPUTE",
  "ACCRUAL_MA12D_WEIGHT_CHANGE_MATERIAL"
)) {
  if (!grepl(fragment, ma12d, fixed = TRUE)) {
    stop("MA12D missing required environment-variable fragment: ", fragment)
  }
}

for (fragment in c(
  "u_new",
  "rnorm",
  "sigma_u",
  "extract_firm_intercept_sd_draws",
  "^sd_.*__Intercept$",
  "log_mean_exp",
  "posterior_linpred",
  "student_log_density_matrix",
  "grouped_firm_marginal_new_firm_integrated",
  "table_grouped_marginal_new_firm_decision.csv",
  "BLOCKED_MISSING_FITS",
  "BLOCKED_UNVERIFIED_FIRMRE_SD",
  "table_grouped_population_vs_marginal_new_firm_weight_comparison.csv",
  "table_winsor_kfold_observation_scores_marginal_new_firm.csv",
  "Source_KFold_Run_Root",
  "Source_KFold_Run_Root_Resolution",
  "Source_KFold_Manifest_Path",
  "Source_KFold_Status_Path",
  "Source_KFold_Fold_Assignment_Path"
)) {
  if (!grepl(fragment, ma12d, fixed = TRUE)) {
    stop("MA12D missing marginal-new-firm contract fragment: ", fragment)
  }
}

if (grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", ma12d, perl = TRUE)) {
  stop("MA12D must be a rescoring module and must not refit with brm().")
}

if (grepl("brms::log_lik\\s*\\(", ma12d, perl = TRUE)) {
  stop("MA12D must not use brms::log_lik(re_formula = NA) as the Firm-RE marginal score.")
}

if (!grepl("re_formula\\s*=\\s*NA", ma12d, perl = TRUE) ||
    !grepl("posterior_linpred\\s*\\(", ma12d, perl = TRUE)) {
  stop("MA12D should use re_formula = NA only to extract the population-level linear predictor.")
}

if (!grepl("lpd_obs\\s*=\\s*lpd_obs", ma12d, perl = TRUE) ||
    !grepl("lpd_obs_marginal_new_firm\\s*=\\s*if \\(is_firm_re\\) lpd_obs", ma12d, perl = TRUE)) {
  stop("MA12D must export Firm-RE lpd_obs from the marginal new-firm score.")
}

for (fragment in c(
  "exact_grouped_kfold_marginal_new_firm_rescoring",
  "ma12d_compute_grouped_new_firm_marginal_scores.R",
  "has_custom_u_new_draw_logic",
  "custom_u_new_draw_logic"
)) {
  if (!grepl(fragment, di02, fixed = TRUE)) {
    stop("DI02 missing MA12D custom u_new audit fragment: ", fragment)
  }
}

for (fragment in c(
  "table_grouped_population_vs_marginal_new_firm_weight_comparison.csv",
  "table_grouped_marginal_new_firm_decision.csv",
  "table_winsor_kfold_weights_ex_post_marginal_new_firm.csv",
  "table_winsor_kfold_weights_no_lookahead_marginal_new_firm.csv",
  "scripts/ma12d_compute_grouped_new_firm_marginal_scores.R",
  "marginal_new_firm_prediction",
  "custom_u_new"
)) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("MA17 missing MA12D export/audit fragment: ", fragment)
  }
}

invisible(parse(ma12d_path))
invisible(parse(di02_path))
invisible(parse(ma17_path))

cat("test_ma12d_grouped_new_firm_marginal_static.R passed\n")
