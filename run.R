args <- commandArgs(trailingOnly = TRUE)

flag_from_env <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, if (default) "TRUE" else "FALSE")
  toupper(raw) %in% c("TRUE", "1", "YES", "Y")
}

selection <- if (length(args) == 0) "full" else tolower(args[[1]])
if (!selection %in% c("baseline", "sensitivity", "full")) {
  stop("Usage: Rscript run.R [baseline|sensitivity|full]")
}

baseline_scripts <- c(
  "scripts/v3/01_v3_setup_and_registry.R",
  "scripts/v3/02_v3_build_common_sample.R",
  "scripts/v3/03_v3_audit_cogs_inv_operating_cycle_after_fix.R",
  "scripts/v3/04_v3_define_named_models.R",
  "scripts/v3/05_v3_winsorize_common_samples.R",
  "scripts/v3/06_v3_prior_predictive_checks_winsor.R",
  "scripts/v3/07_v3_fit_brms_named_models_winsor.R",
  "scripts/v3/08_v3_mcmc_diagnostics_winsor.R",
  "scripts/v3/09_v3_loo_stacking_winsor.R",
  "scripts/v3/10_v3_construct_uncertainty_adjusted_DA_winsor.R",
  "scripts/v3/11_v3_posterior_predictive_checks_winsor.R",
  "scripts/v3/12_v3_lofo_stacking_winsor.R",
  "scripts/v3/13_v3_grouped_kfold_firm_winsor.R",
  "scripts/v3/21_v3_validation_on_scaleaware_student_DA.R"
)

sensitivity_scripts <- c(
  "scripts/v3/14_v3_sensitivity_prior_predictive_winsor.R",
  "scripts/v3/15_v3_sensitivity_refit_prior_scenarios_winsor.R",
  "scripts/v3/16_v3_sensitivity_mcmc_diagnostics_winsor.R",
  "scripts/v3/17_v3_sensitivity_stacking_winsor.R",
  "scripts/v3/18_v3_sensitivity_construct_DA_winsor.R",
  "scripts/v3/19_v3_sensitivity_validation_winsor.R",
  "scripts/v3/20_v3_sensitivity_report_winsor.R"
)

heavy_scripts <- c(
  "scripts/v3/07_v3_fit_brms_named_models_winsor.R",
  "scripts/v3/13_v3_grouped_kfold_firm_winsor.R",
  "scripts/v3/15_v3_sensitivity_refit_prior_scenarios_winsor.R"
)

selected_scripts <- switch(
  selection,
  baseline = baseline_scripts,
  sensitivity = sensitivity_scripts,
  full = c(baseline_scripts, sensitivity_scripts)
)

run_heavy <- flag_from_env("V3_RUN_HEAVY", FALSE)
dry_run <- flag_from_env("V3_DRY_RUN", TRUE)
Sys.setenv(V3_RUN_HEAVY = if (run_heavy) "TRUE" else "FALSE")
Sys.setenv(V3_DRY_RUN = if (dry_run) "TRUE" else "FALSE")

print_header <- function() {
  cat("Bayesian Accrual Uncertainty Vietnam\n")
  cat("Selection :", selection, "\n")
  cat("Dry run   :", dry_run, "\n")
  cat("Run heavy :", run_heavy, "\n")
  cat("Data path :", Sys.getenv("V3_DATA_PATH", "data/raw/data.xlsx"), "\n\n")
}

print_manual_heavy_commands <- function() {
  cat("Heavy steps were skipped. Run them manually when needed:\n")
  cat("  Rscript scripts/v3/07_v3_fit_brms_named_models_winsor.R\n")
  cat("  Rscript scripts/v3/13_v3_grouped_kfold_firm_winsor.R\n")
  cat("  Rscript scripts/v3/15_v3_sensitivity_refit_prior_scenarios_winsor.R\n")
  cat("PowerShell example:\n")
  cat("  $env:V3_DRY_RUN='FALSE'; $env:V3_RUN_HEAVY='TRUE'; Rscript run.R full\n\n")
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
