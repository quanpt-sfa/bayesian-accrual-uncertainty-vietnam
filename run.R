args <- commandArgs(trailingOnly = TRUE)

flag_from_env <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, if (default) "TRUE" else "FALSE")
  toupper(raw) %in% c("TRUE", "1", "YES", "Y")
}

selection <- if (length(args) == 0) "full" else tolower(args[[1]])
if (!selection %in% c("baseline", "sensitivity", "simulation", "method_matching", "reviewer_final", "full")) {
  stop("Usage: Rscript run.R [baseline|sensitivity|simulation|method_matching|reviewer_final|full]")
}

baseline_scripts <- c(
  "scripts/01_setup_and_registry.R",
  "scripts/02_build_common_sample.R",
  "scripts/03_audit_cogs_inv_operating_cycle.R",
  "scripts/04_define_named_models.R",
  "scripts/05_winsorize_common_samples.R",
  "scripts/06_prior_predictive_checks.R",
  "scripts/07_fit_brms_named_models.R",
  "scripts/08_mcmc_diagnostics.R",
  "scripts/09_loo_stacking.R",
  "scripts/10_construct_uncertainty_adjusted_DA.R",
  "scripts/11_posterior_predictive_checks.R",
  "scripts/12_lofo_stacking.R",
  "scripts/13_grouped_kfold_firm.R",
  "scripts/21_validation_on_scaleaware_student_DA.R"
)

sensitivity_scripts <- c(
  "scripts/14_sensitivity_prior_predictive.R",
  "scripts/15_sensitivity_refit_prior_scenarios.R",
  "scripts/16_sensitivity_mcmc_diagnostics.R",
  "scripts/17_sensitivity_stacking.R",
  "scripts/18_sensitivity_construct_DA.R",
  "scripts/19_sensitivity_validation.R",
  "scripts/20_sensitivity_report.R"
)

simulation_scripts <- c(
  "scripts/24_sim_lmer_leakage_pilot_run.R",
  "scripts/25_sim_lmer_leakage_pilot_report.R",
  "scripts/26_sim_brms_leakage_confirmation.R",
  "scripts/27_sim_brms_parameter_recovery.R"
)

method_matching_scripts <- c(
  "scripts/28_row_level_exact_kfold.R",
  "scripts/29_psis_reliability_gate.R"
)

reviewer_final_scripts <- method_matching_scripts

heavy_scripts <- c(
  "scripts/07_fit_brms_named_models.R",
  "scripts/13_grouped_kfold_firm.R",
  "scripts/15_sensitivity_refit_prior_scenarios.R",
  "scripts/26_sim_brms_leakage_confirmation.R",
  "scripts/27_sim_brms_parameter_recovery.R",
  "scripts/28_row_level_exact_kfold.R"
)

run_reviewer_final <- flag_from_env("ACCRUAL_RUN_REVIEWER_FINAL", FALSE)

selected_scripts <- switch(
  selection,
  baseline = baseline_scripts,
  sensitivity = sensitivity_scripts,
  simulation = simulation_scripts,
  method_matching = method_matching_scripts,
  reviewer_final = reviewer_final_scripts,
  full = c(baseline_scripts, sensitivity_scripts, if (run_reviewer_final) reviewer_final_scripts else character())
)

run_heavy <- flag_from_env("ACCRUAL_RUN_HEAVY", FALSE)
dry_run <- flag_from_env("ACCRUAL_DRY_RUN", TRUE)
Sys.setenv(ACCRUAL_RUN_HEAVY = if (run_heavy) "TRUE" else "FALSE")
Sys.setenv(ACCRUAL_DRY_RUN = if (dry_run) "TRUE" else "FALSE")
Sys.setenv(ACCRUAL_RUN_REVIEWER_FINAL = if (run_reviewer_final) "TRUE" else "FALSE")

print_header <- function() {
  cat("Bayesian Accrual Uncertainty Vietnam\n")
  cat("Selection :", selection, "\n")
  cat("Dry run   :", dry_run, "\n")
  cat("Run heavy :", run_heavy, "\n")
  cat("Reviewer-final in full :", run_reviewer_final, "\n")
  cat("Data path :", Sys.getenv("ACCRUAL_DATA_PATH", "data/raw/data.xlsx"), "\n\n")
}

print_manual_heavy_commands <- function() {
  cat("Heavy steps were skipped. Run them manually when needed:\n")
  cat("  Rscript scripts/07_fit_brms_named_models.R\n")
  cat("  Rscript scripts/13_grouped_kfold_firm.R\n")
  cat("  Rscript scripts/15_sensitivity_refit_prior_scenarios.R\n")
  cat("  Rscript scripts/26_sim_brms_leakage_confirmation.R\n")
  cat("  Rscript scripts/27_sim_brms_parameter_recovery.R\n")
  cat("  Rscript scripts/28_row_level_exact_kfold.R\n")
  cat("Simulation / leakage mechanism checks:\n")
  cat("  Rscript run.R simulation\n")
  cat("Reviewer-final method-matching checks:\n")
  cat("  $env:ACCRUAL_ROW_KFOLD_PREFLIGHT_ONLY='TRUE'; Rscript scripts/28_row_level_exact_kfold.R\n")
  cat("  $env:ACCRUAL_RUN_HEAVY='TRUE'; Rscript run.R method_matching\n")
  cat("  Rscript scripts/29_psis_reliability_gate.R\n")
  cat("Optional Chapter 3 methods reporting tables:\n")
  cat("  Rscript -e \"source('scripts/22_chapter3_methods_tables.R')\"\n")
  cat("PowerShell example:\n")
  cat("  $env:ACCRUAL_DRY_RUN='FALSE'; $env:ACCRUAL_RUN_HEAVY='TRUE'; Rscript run.R full\n\n")
}

run_script <- function(path) {
  if (!file.exists(path)) {
    stop("Missing script: ", path)
  }
  if (!run_heavy && path %in% heavy_scripts) {
    message("[SKIP HEAVY] ", path)
    return(invisible(FALSE))
  }
  message("[RUN] ", path)
  sys.source(path, envir = new.env(parent = globalenv()))
  invisible(TRUE)
}

print_header()
invisible(lapply(selected_scripts, run_script))

if (!run_heavy) {
  print_manual_heavy_commands()
}

cat("Pipeline entrypoint completed.\n")
