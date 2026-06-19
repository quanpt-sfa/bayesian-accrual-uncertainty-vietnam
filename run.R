args <- commandArgs(trailingOnly = TRUE)

flag_from_env <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, if (default) "TRUE" else "FALSE")
  toupper(raw) %in% c("TRUE", "1", "YES", "Y")
}

dry_run <- "--dry-run" %in% args || flag_from_env("ACCRUAL_DRY_RUN", FALSE)
args <- args[args != "--dry-run"]

# --- Root-directory + raw-data safety assertions ---
if (!file.exists(file.path("scripts", "ma00_setup.R")) || !dir.exists(file.path("data", "raw"))) {
  stop("[ROOT BLOCKER] run.R must be executed from the project root ",
       "(missing scripts/ma00_setup.R or data/raw/). Current working directory: ", getwd())
}
.raw_dir <- file.path("data", "raw")
.raw_snapshot <- function() {
  fs <- sort(list.files(.raw_dir, recursive = TRUE, full.names = TRUE))
  if (!length(fs)) return(character())
  vapply(fs, function(p) {
    info <- file.info(p)
    paste0(p, "|", info$size, "|", as.numeric(info$mtime))
  }, character(1))
}
.raw_before <- .raw_snapshot()

target <- if (length(args) == 0) "main" else tolower(args[[1]])
valid_targets <- c("main", "robustness", "sensitivity", "simulation", "diagnostics", "all")
if (!target %in% valid_targets) {
  stop("Usage: Rscript run.R [main|robustness|sensitivity|simulation|diagnostics|all] [--dry-run]")
}

run_heavy <- flag_from_env("ACCRUAL_RUN_HEAVY", FALSE)
allow_suppressed_tail_flags <- flag_from_env("ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS", FALSE)
output_root <- Sys.getenv("ACCRUAL_OUTPUT_ROOT", file.path("out", "interim", "winsor"))
tables_root <- file.path(output_root, "tables")
accruals_root <- Sys.getenv("ACCRUAL_ACCRUALS_ROOT", "accruals")

Sys.setenv(ACCRUAL_RUN_HEAVY = if (run_heavy) "TRUE" else "FALSE")
Sys.setenv(ACCRUAL_DRY_RUN = if (dry_run) "TRUE" else "FALSE")

artifact <- function(...) file.path(...)
table_artifact <- function(file) file.path(tables_root, file)
baseline_da_path <- function(file = "final_uncertainty_adjusted_accruals_winsor.csv") {
  file.path(accruals_root, "baseline", file)
}

step <- function(id, path, role, heavy = FALSE, gate = NULL, requires = character(), require_reason = NULL) {
  list(id = id, path = path, role = role, heavy = heavy, gate = gate,
       requires = requires, require_reason = require_reason)
}

main_steps <- list(
  step("00", "scripts/ma00_setup.R", "Setup helpers and shared registries"),
  step("01", "scripts/ma01_setup_and_registry.R", "Setup and ten-model registry"),
  step("02", "scripts/ma02_build_common_sample.R", "Build common samples"),
  step("03", "scripts/ma03_audit_data_integrity.R", "Data integrity audit"),
  step("04", "scripts/ma04_define_named_models.R", "Define named model formulas"),
  step("05", "scripts/ma05_winsorize_common_samples.R", "Winsorize common samples"),
  step("06", "scripts/ma06_prior_predictive_checks.R", "Prior predictive gate"),
  step("07", "scripts/ma07_fit_brms_named_models.R", "Full-sample baseline brms fits", heavy = TRUE),
  step("08", "scripts/ma08_mcmc_diagnostics.R", "MCMC diagnostics for baseline fits",
       requires = c(table_artifact("table_brms_diagnostics_winsor.csv"), file.path(output_root, "models")),
       require_reason = "baseline fit diagnostics and model files from script 07"),
  step("09", "scripts/ma09_loo_stacking.R", "LOO stacking evidence, secondary to exact K-fold",
       requires = c(
         table_artifact("table_brms_diagnostics_winsor.csv"),
         table_artifact("table_coefficient_summary_winsor.csv"),
         table_artifact("table_mcmc_diagnostics_gate_winsor.csv")
       ),
       require_reason = "baseline MCMC diagnostics gate and coefficient summaries"),
  step("10", "scripts/ma10_construct_psis_loo_DA.R", "Construct PSIS/LOO secondary uncertainty-adjusted DA",
       requires = c(
         table_artifact("table_stacking_weights_ex_post_winsor_corrected.csv"),
         table_artifact("table_stacking_weights_no_lookahead_winsor_corrected.csv")
       ),
       require_reason = "secondary PSIS/LOO stacking weights from script 09"),
  step("11", "scripts/ma11_posterior_predictive_checks.R", "Posterior predictive checks for secondary PSIS/LOO DA",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_winsor.csv"),
         table_artifact("table_stacking_weights_ex_post_winsor_corrected.csv"),
         table_artifact("table_stacking_weights_no_lookahead_winsor_corrected.csv")
       ),
       require_reason = "secondary PSIS/LOO DA and weights"),
  step("13", "scripts/ma12_grouped_kfold_firm.R", "Grouped exact firm K-fold, primary RQ1 evidence", heavy = TRUE),
  step("28", "scripts/ma13_row_level_exact_kfold.R", "Row-level exact K-fold, primary RQ1 evidence", heavy = TRUE),
  step("31", "scripts/ma14_construct_exact_kfold_DA.R", "Primary exact-KFoldW DA construction",
       requires = c(
         table_artifact("table_mcmc_diagnostics_gate_winsor.csv"),
         if (!nzchar(Sys.getenv("ACCRUAL_GROUPED_KFOLD_RUN_ROOT", ""))) file.path(output_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt") else character(),
         if (!nzchar(Sys.getenv("ACCRUAL_ROW_KFOLD_RUN_ROOT", ""))) file.path(output_root, "row_exact_kfold", "LATEST_COMPLETED_RUN.txt") else character()
       ),
       require_reason = "MCMC diagnostics gate and completed exact K-fold run pins from scripts 13 and 28"),
  step("32", "scripts/ma15_audit_DA_finite_outputs.R", "Finite-output gate for exact-KFold DA", gate = "da_finite",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv"),
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
       ),
       require_reason = "exact-KFold DA outputs from script 31"),
  step("21", "scripts/ma16_validate_outcomes.R", "Outcome validation on primary exact row-KFold DA",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv"),
         table_artifact("table_DA_finite_gate_decision.csv"),
         table_artifact("table_model_primary_inclusion_gate.csv")
       ),
       require_reason = "primary exact row-KFold DA plus finite DA and model inclusion gate decisions"),
  step("30", "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R", "New-firm predictive integration reporting gate", gate = "new_firm_predictive",
       requires = c(
         table_artifact("table_DA_finite_gate_decision.csv"),
         table_artifact("table_DA_exact_kfold_source_manifest.csv")
       ),
       require_reason = "finite DA gate and exact-KFold DA provenance manifest"),
  step("C3", "scripts/ma17_export_tables_figures.R", "Chapter 3 manuscript table export",
       requires = c(
         table_artifact("table_DA_finite_gate_decision.csv"),
         table_artifact("table_model_primary_inclusion_gate.csv"),
         file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv")
       ),
       require_reason = "finite DA, model inclusion, and new-firm predictive gate decisions")
)

robustness_steps <- list(
  step("12", "scripts/robustness/ro01_lofo_stacking.R", "Grouped PSIS-LOFO robustness evidence")
)

sensitivity_steps <- list(
  step("14", "scripts/sensitivity/se01_prior_predictive.R", "Sensitivity prior predictive gate"),
  step("15", "scripts/sensitivity/se02_refit_prior_scenarios.R", "Sensitivity full refits by prior scenario", heavy = TRUE),
  step("16", "scripts/sensitivity/se03_mcmc_diagnostics.R", "Sensitivity MCMC diagnostics"),
  step("17", "scripts/sensitivity/se04_stacking.R", "Sensitivity stacking"),
  step("18", "scripts/sensitivity/se05_construct_DA.R", "Sensitivity DA reconstruction"),
  step("19", "scripts/sensitivity/se06_validation.R", "Sensitivity validation"),
  step("20", "scripts/sensitivity/se07_report.R", "Sensitivity report")
)

simulation_steps <- list(
  step("24", "scripts/simulation/si01_lmer_pilot_run.R", "LMER leakage pilot simulation"),
  step("25", "scripts/simulation/si02_lmer_pilot_report.R", "LMER leakage pilot report"),
  step("26", "scripts/simulation/si03_brms_leakage_confirmation.R", "BRMS leakage confirmation simulation", heavy = TRUE),
  step("27", "scripts/simulation/si04_brms_parameter_recovery.R", "BRMS parameter recovery simulation", heavy = TRUE)
)

diagnostics_steps <- list(
  step("29", "scripts/diagnostics/di01_psis_reliability_gate.R", "Secondary PSIS reliability diagnostics"),
  step("30", "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R", "New-firm predictive integration diagnostics", gate = "new_firm_predictive")
)

diagnostics_steps_for_all <- list(
  step("29", "scripts/diagnostics/di01_psis_reliability_gate.R", "Secondary PSIS reliability diagnostics")
)

steps_for_target <- function(x) {
  switch(
    x,
    main = main_steps,
    robustness = robustness_steps,
    sensitivity = sensitivity_steps,
    simulation = simulation_steps,
    diagnostics = diagnostics_steps,
    all = c(main_steps, diagnostics_steps_for_all, robustness_steps, sensitivity_steps, simulation_steps)
  )
}

write_config_registry_if_available <- function() {
  helper_env <- new.env(parent = globalenv())
  sys.source("scripts/ma00_setup.R", envir = helper_env)
  if (exists("write_execution_config_registry", envir = helper_env, inherits = FALSE)) {
    helper_env$write_execution_config_registry()
  }
}

print_header <- function(steps) {
  cat("Bayesian Accrual Uncertainty Vietnam\n")
  cat("Target    :", target, "\n")
  cat("Dry run   :", dry_run, "\n")
  cat("Run heavy :", run_heavy, "\n")
  cat("Data path :", Sys.getenv("ACCRUAL_DATA_PATH", "data/raw/data.xlsx"), "\n")
  cat("Plan:\n")
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    flags <- c(
      if (isTRUE(s$heavy)) "heavy" else character(),
      if (!is.null(s$gate)) "gate" else character(),
      if (length(s$requires)) "requires artifacts" else character()
    )
    flag_text <- if (length(flags)) paste0(" [", paste(flags, collapse = ", "), "]") else ""
    cat(sprintf("  %02d. %s %s - %s%s\n", i, s$id, s$path, s$role, flag_text))
  }
  cat("\n")
}

require_artifacts <- function(step_id, paths, reason = NULL) {
  if (!length(paths)) return(invisible(TRUE))
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      "[DEPENDENCY BLOCKER] Step ", step_id, " cannot run because required artifact(s) are missing",
      if (!is.null(reason) && nzchar(reason)) paste0(" for ", reason) else "",
      ": ", paste(missing, collapse = "; ")
    )
  }
  invisible(TRUE)
}

check_da_finite_gate <- function() {
  decision_path <- table_artifact("table_DA_finite_gate_decision.csv")
  if (!file.exists(decision_path)) {
    stop("[GATE BLOCKER] DA finite gate decision table was not created: ", decision_path)
  }
  decision <- tryCatch(read.csv(decision_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(decision) || nrow(decision) == 0 || !"gate_decision" %in% names(decision)) {
    stop("[GATE BLOCKER] Cannot read gate_decision from: ", decision_path)
  }
  gate_decision <- as.character(decision$gate_decision[[1]])
  message("[GATE] DA finite output decision: ", gate_decision)
  if (!gate_decision %in% c("PASS", "PASS_WITH_STRUCTURAL_NA_ONLY", "WARN_SECONDARY_NONFINITE_ONLY")) {
    stop("[GATE BLOCKER] DA finite gate is not passable for primary RQ2/export: ", gate_decision)
  }
  invisible(TRUE)
}

check_new_firm_predictive_gate <- function() {
  decision_path <- file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv")
  if (!file.exists(decision_path)) {
    stop(
      "[GATE BLOCKER] New-firm predictive integration decision table was not created: ",
      decision_path,
      ". Script 30 must write this table before Chapter 3 table export can proceed."
    )
  }
  decision <- tryCatch(read.csv(decision_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(decision) || nrow(decision) == 0 || !"audit_decision" %in% names(decision)) {
    stop("[GATE BLOCKER] Cannot read audit_decision from: ", decision_path)
  }
  audit_decision <- as.character(decision$audit_decision[[1]])
  message("[GATE] New-firm predictive integration decision: ", audit_decision)
  if (identical(audit_decision, "PASS_FOR_AVAILABLE_FIRMRE_OUT_OF_FIRM_QUANTITIES") ||
      identical(audit_decision, "NO_FIRMRE_OUT_OF_FIRM_PRIMARY_QUANTITIES_DETECTED")) {
    return(invisible(TRUE))
  }
  if (identical(audit_decision, "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES")) {
    msg <- paste0(
      "[GATE BLOCKER] Firm-RE out-of-firm posterior predictive tail flags require suppression/non-primary treatment. ",
      "Set ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS=TRUE only if downstream Chapter 3 export/reporting will preserve that suppression."
    )
    if (!allow_suppressed_tail_flags) stop(msg)
    warning(msg, call. = FALSE)
    return(invisible(TRUE))
  }
  stop("[GATE BLOCKER] Unknown new-firm predictive audit decision '", audit_decision, "' in: ", decision_path)
}

run_step <- function(s) {
  if (!file.exists(s$path)) stop("Missing script: ", s$path)
  if (isTRUE(s$heavy) && !run_heavy) {
    warning("[SKIP HEAVY] ", s$path, " requires ACCRUAL_RUN_HEAVY=TRUE; skipped explicitly.", call. = FALSE)
    return(invisible(FALSE))
  }
  require_artifacts(s$id, s$requires, s$require_reason)
  if (identical(s$gate, "da_finite")) Sys.setenv(ACCRUAL_DA_FINITE_GATE_STRICT = "TRUE")
  message("[RUN] ", s$path)
  sys.source(s$path, envir = new.env(parent = globalenv()))
  if (identical(s$gate, "da_finite")) check_da_finite_gate()
  if (identical(s$gate, "new_firm_predictive")) check_new_firm_predictive_gate()
  invisible(TRUE)
}

selected_steps <- steps_for_target(target)
write_config_registry_if_available()
print_header(selected_steps)

if (dry_run) {
  cat("Dry-run only: no scripts were executed.\n")
  quit(save = "no", status = 0)
}

invisible(lapply(selected_steps, run_step))

# --- Raw-data read-only verification ---
if (!identical(.raw_before, .raw_snapshot())) {
  warning("[RAW WRITE WARNING] Files under data/raw/ changed during this run; ",
          "raw inputs must remain read-only. Inspect which step wrote to data/raw/.", call. = FALSE)
}

cat("Pipeline entrypoint completed.\n")
