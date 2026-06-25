args <- commandArgs(trailingOnly = TRUE)

cli_dry_run <- "--dry-run" %in% args
args <- args[args != "--dry-run"]

# --- Root-directory + raw-data safety assertions ---
if (!file.exists(file.path("scripts", "ma00_setup.R")) || !dir.exists(file.path("data", "raw"))) {
  stop("[ROOT BLOCKER] run.R must be executed from the project root ",
       "(missing scripts/ma00_setup.R or data/raw/). Current working directory: ", getwd())
}
source("scripts/ma00_setup.R")
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
valid_targets <- c("main", "robustness", "sensitivity", "simulation", "diagnostics", "reviewer", "all")
if (!target %in% valid_targets) {
  stop("Usage: Rscript run.R [main|robustness|sensitivity|simulation|diagnostics|reviewer|all] [--dry-run]")
}

orch_cfg <- accrual_orchestrator_config()
dry_run <- cli_dry_run || isTRUE(orch_cfg$dry_run)
run_heavy <- isTRUE(orch_cfg$run_heavy)
allow_suppressed_tail_flags <- isTRUE(orch_cfg$allow_suppressed_tail_flags)
output_root <- orch_cfg$output_root
tables_root <- file.path(output_root, "tables")
accruals_root <- orch_cfg$accruals_root

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
  step("ma00", "scripts/ma00_setup.R", "Setup helpers and shared registries"),
  step("ma01", "scripts/ma01_setup_and_registry.R", "Setup and ten-model registry"),
  step("ma02", "scripts/ma02_build_common_sample.R", "Build common samples"),
  step("ma03", "scripts/ma03_audit_data_integrity.R", "Data integrity audit"),
  step("ma04", "scripts/ma04_define_named_models.R", "Define named model formulas"),
  step("ma05", "scripts/ma05_winsorize_common_samples.R", "Winsorize common samples"),
  step("ma06", "scripts/ma06_prior_predictive_checks.R", "Prior predictive gate"),
  step("ma07a", "scripts/ma07a_fit_brms_named_models.R", "Full-sample baseline brms fit worker stage", heavy = TRUE),
  step("ma07b", "scripts/ma07b_extract_brms_fit_outputs_workers.R", "Extract baseline brms fit outputs with workers", heavy = TRUE,
       requires = c(
         table_artifact("table_ma07_fit_task_manifest.csv"),
         table_artifact("table_ma07_fit_task_status.csv"),
         file.path(output_root, "models")
       ),
       require_reason = "ma07a fit task manifest, task status, and fit artifacts"),
  step("ma07c", "scripts/ma07c_collect_brms_fit_outputs.R", "Collect extracted baseline brms fit outputs",
       requires = c(
         table_artifact("table_ma07_collect_task_manifest.csv"),
         table_artifact("table_ma07_collect_task_status.csv")
       ),
       require_reason = "ma07b extract task manifest, task status, and task-local artifacts"),
  step("ma08", "scripts/ma08_mcmc_diagnostics.R", "MCMC diagnostics for baseline fits",
       requires = c(table_artifact("table_brms_diagnostics_winsor.csv"), file.path(output_root, "models")),
       require_reason = "baseline fit diagnostics from ma07c and model files from ma07a"),
  step("ma09a", "scripts/ma09a_plan_loo_savepars_refits.R", "Plan PSIS/LOO save_pars refits, secondary to exact K-fold",
       requires = c(
         table_artifact("table_brms_diagnostics_winsor.csv"),
         table_artifact("table_coefficient_summary_winsor.csv"),
         table_artifact("table_mcmc_diagnostics_gate_winsor.csv")
       ),
       require_reason = "baseline MCMC diagnostics gate and coefficient summaries"),
  step("ma09b", "scripts/ma09b_fit_loo_savepars_refits.R", "Fit PSIS/LOO save_pars refits with workers", heavy = TRUE,
       requires = c(table_artifact("table_ma09_savepars_refit_task_manifest.csv")),
       require_reason = "ma09a save_pars refit task manifest"),
  step("ma09c", "scripts/ma09c_collect_loo_stacking.R", "Collect PSIS/LOO stacking evidence, secondary to exact K-fold",
       requires = c(
         table_artifact("table_ma09_savepars_refit_task_manifest.csv"),
         table_artifact("table_ma09_savepars_refit_task_status.csv")
       ),
       require_reason = "ma09a manifest and ma09b task status"),
  step("ma10", "scripts/ma10_construct_psis_loo_DA.R", "Construct PSIS/LOO secondary uncertainty-adjusted DA",
       requires = c(
         table_artifact("table_stacking_weights_ex_post_winsor_corrected.csv"),
         table_artifact("table_stacking_weights_no_lookahead_winsor_corrected.csv")
       ),
       require_reason = "secondary PSIS/LOO stacking weights from ma09"),
  step("ma11", "scripts/ma11_posterior_predictive_checks.R", "Posterior predictive checks for secondary PSIS/LOO DA",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_winsor.csv"),
         table_artifact("table_stacking_weights_ex_post_winsor_corrected.csv"),
         table_artifact("table_stacking_weights_no_lookahead_winsor_corrected.csv")
       ),
       require_reason = "secondary PSIS/LOO DA and weights"),
  step("ma12a", "scripts/ma12a_plan_grouped_kfold_firm.R", "Plan grouped exact firm K-fold, primary RQ1 evidence"),
  step("ma12b", "scripts/ma12b_fit_grouped_kfold_firm_workers.R", "Fit grouped exact firm K-fold with workers, primary RQ1 evidence", heavy = TRUE,
       requires = c(table_artifact("table_ma12_grouped_kfold_task_manifest.csv")),
       require_reason = "ma12a grouped K-fold task manifest"),
  step("ma12c", "scripts/ma12c_collect_grouped_kfold_firm_scores.R", "Collect grouped exact firm K-fold scores, primary RQ1 evidence",
       requires = c(
         table_artifact("table_ma12_grouped_kfold_task_manifest.csv"),
         table_artifact("table_ma12_grouped_kfold_task_status.csv")
       ),
       require_reason = "ma12a manifest and ma12b task status"),
  step("ma13a", "scripts/ma13a_plan_row_level_exact_kfold.R", "Plan row-level exact K-fold, primary RQ1 evidence"),
  step("ma13b", "scripts/ma13b_fit_row_level_exact_kfold_workers.R", "Fit row-level exact K-fold with workers, primary RQ1 evidence", heavy = TRUE,
       requires = c(table_artifact("table_ma13_row_kfold_task_manifest.csv")),
       require_reason = "ma13a row K-fold task manifest"),
  step("ma13c", "scripts/ma13c_collect_row_level_exact_kfold_scores.R", "Collect row-level exact K-fold scores, primary RQ1 evidence",
       requires = c(
         table_artifact("table_ma13_row_kfold_task_manifest.csv"),
         table_artifact("table_ma13_row_kfold_task_status.csv")
       ),
       require_reason = "ma13a manifest and ma13b task status"),
  step("ma14", "scripts/ma14_construct_exact_kfold_DA.R", "Primary exact-KFoldW DA construction",
       requires = c(
         table_artifact("table_mcmc_diagnostics_gate_winsor.csv"),
         if (!nzchar(orch_cfg$grouped_kfold_run_root)) file.path(output_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt") else character(),
         if (!nzchar(orch_cfg$row_kfold_run_root)) file.path(output_root, "row_exact_kfold", "LATEST_COMPLETED_RUN.txt") else character()
       ),
       require_reason = "MCMC diagnostics gate and completed exact K-fold run pins from ma12 and ma13"),
  step("ma15", "scripts/ma15_audit_DA_finite_outputs.R", "Finite-output gate for exact-KFold DA", gate = "da_finite",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv"),
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
       ),
       require_reason = "exact-KFold DA outputs from ma14"),
  step("ma16", "scripts/ma16_validate_outcomes.R", "Outcome validation on primary exact row-KFold DA",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv"),
         table_artifact("table_DA_finite_gate_decision.csv"),
         table_artifact("table_model_primary_inclusion_gate.csv")
       ),
       require_reason = "primary exact row-KFold DA plus finite DA and model inclusion gate decisions"),
  step("di02", "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R", "New-firm predictive integration reporting gate", gate = "new_firm_predictive",
       requires = c(
          table_artifact("table_DA_finite_gate_decision.csv"),
          table_artifact("table_DA_exact_kfold_source_manifest.csv")
       ),
       require_reason = "finite DA gate and exact-KFold DA provenance manifest"),
  step("di03", "scripts/diagnostics/di03_exact_kfold_reclassification_audit.R", "Exact K-fold reclassification/Jaccard audit",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv"),
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv"),
         file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv")
       ),
       require_reason = "exact-KFold grouped/row DA outputs and new-firm predictive gate decision"),
  step("di04", "scripts/diagnostics/di04_denominator_diagnostics.R", "Denominator diagnostics for estimation-scaled exact-KFold DA",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv"),
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
       ),
       require_reason = "exact-KFold grouped/row DA outputs"),
  step("di05", "scripts/diagnostics/di05_economic_validity_top_tail.R", "Economic-validity check for exact-KFold top-tail groups",
       requires = c(
         file.path(output_root, "diagnostics", "table_exact_kfold_reclassification_sets.csv"),
         file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
       ),
       require_reason = "di03 membership sets and winsor no-lookahead sample"),
  step("ma17", "scripts/ma17_export_tables_figures.R", "Chapter 3 manuscript table export",
       requires = c(
          table_artifact("table_DA_finite_gate_decision.csv"),
          table_artifact("table_model_primary_inclusion_gate.csv"),
          file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv"),
          file.path(output_root, "diagnostics", "table_exact_kfold_reclassification_decision.csv")
       ),
       require_reason = "finite DA, model inclusion, new-firm predictive, and exact-KFold reclassification decisions")
)

robustness_steps <- list(
  step("ro01", "scripts/robustness/ro01_lofo_stacking.R", "Grouped PSIS-LOFO robustness evidence")
)

sensitivity_steps <- list(
  step("se01", "scripts/sensitivity/se01_prior_predictive.R", "Sensitivity prior predictive gate"),
  step("se02a", "scripts/sensitivity/se02a_plan_prior_scenario_refits.R", "Plan sensitivity prior-scenario refits"),
  step("se02b", "scripts/sensitivity/se02b_fit_prior_scenario_workers.R", "Fit sensitivity prior-scenario refits with workers", heavy = TRUE,
       requires = c(file.path(output_root, "sensitivity", "tables", "table_se02_prior_scenario_refit_task_manifest.csv")),
       require_reason = "se02a sensitivity refit task manifest"),
  step("se02c", "scripts/sensitivity/se02c_collect_prior_scenario_outputs.R", "Collect sensitivity prior-scenario outputs",
       requires = c(
         file.path(output_root, "sensitivity", "tables", "table_se02_prior_scenario_refit_task_manifest.csv"),
         file.path(output_root, "sensitivity", "tables", "table_se02_prior_scenario_refit_task_status.csv")
       ),
       require_reason = "se02a manifest and se02b task status"),
  step("se03", "scripts/sensitivity/se03_mcmc_diagnostics.R", "Sensitivity MCMC diagnostics"),
  step("se04", "scripts/sensitivity/se04_stacking.R", "Sensitivity stacking"),
  step("se05", "scripts/sensitivity/se05_construct_DA.R", "Sensitivity DA reconstruction"),
  step("se06", "scripts/sensitivity/se06_validation.R", "Sensitivity validation"),
  step("se07", "scripts/sensitivity/se07_report.R", "Sensitivity report")
)

simulation_steps <- list(
  step("si01", "scripts/simulation/si01_lmer_pilot_run.R", "LMER leakage pilot simulation"),
  step("si02", "scripts/simulation/si02_lmer_pilot_report.R", "LMER leakage pilot report"),
  step("si03a", "scripts/simulation/si03a_plan_brms_leakage_confirmation.R", "Plan BRMS leakage confirmation simulation"),
  step("si03b", "scripts/simulation/si03b_fit_brms_leakage_confirmation_workers.R", "Fit BRMS leakage confirmation simulation with workers", heavy = TRUE,
       requires = c(file.path(output_root, "simulation", "brms_leakage", "tables", "table_si03_brms_leakage_task_manifest.csv")),
       require_reason = "si03a BRMS leakage task manifest"),
  step("si03c", "scripts/simulation/si03c_collect_brms_leakage_confirmation.R", "Collect BRMS leakage confirmation simulation",
       requires = c(
         file.path(output_root, "simulation", "brms_leakage", "tables", "table_si03_brms_leakage_task_manifest.csv"),
         file.path(output_root, "simulation", "brms_leakage", "tables", "table_si03_brms_leakage_task_status.csv")
       ),
       require_reason = "si03a manifest and si03b task status"),
  step("si04a", "scripts/simulation/si04a_plan_brms_parameter_recovery.R", "Plan BRMS parameter recovery simulation"),
  step("si04b", "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R", "Fit BRMS parameter recovery simulation with workers", heavy = TRUE,
       requires = c(file.path(output_root, "simulation", "brms_parameter_recovery", "tables", "table_si04_brms_recovery_task_manifest.csv")),
       require_reason = "si04a BRMS recovery task manifest"),
  step("si04c", "scripts/simulation/si04c_collect_brms_parameter_recovery.R", "Collect BRMS parameter recovery simulation",
       requires = c(
         file.path(output_root, "simulation", "brms_parameter_recovery", "tables", "table_si04_brms_recovery_task_manifest.csv"),
         file.path(output_root, "simulation", "brms_parameter_recovery", "tables", "table_si04_brms_recovery_task_status.csv")
       ),
       require_reason = "si04a manifest and si04b task status"),
  step("si05", "scripts/simulation/si05_lmer_temporal_dependence_run.R",
       "LMER temporal-dependence persistent-shock simulation",
       heavy = TRUE),
  step("si06", "scripts/simulation/si06_lmer_temporal_dependence_report.R",
       "Report temporal-dependence mechanism simulation",
       requires = c(
         file.path(output_root, "simulation", "lmer_temporal_dependence", "tables",
                   "table_lmer_temporal_dependence_rep_results.csv")
       ),
       require_reason = "si05 temporal-dependence replication results")
)

reviewer_steps <- list(
  step("di04", "scripts/diagnostics/di04_denominator_diagnostics.R",
       "Canonical denominator diagnostics for exact-KFold DA",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv"),
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
       ),
       require_reason = "exact-KFold grouped/row DA outputs"),
  step("di05", "scripts/diagnostics/di05_economic_validity_top_tail.R",
       "Canonical economic-validity check for exact-KFold top-tail groups",
       requires = c(
         file.path(output_root, "diagnostics", "table_exact_kfold_reclassification_sets.csv"),
         file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
       ),
       require_reason = "di03 membership sets and winsor no-lookahead sample"),
  step("di09", "scripts/diagnostics/di09_temporal_dependence_robustness.R",
       "Temporal-dependence robustness for row-minus-grouped Firm-RE premium",
       heavy = TRUE),
  step("di07", "scripts/diagnostics/di07_section4_7_reviewer_package.R",
       "Assemble Section 4.7 reviewer-required evidence package",
       requires = accrual_section47_required_artifacts(output_root),
       require_reason = "canonical denominator and economic-validity reviewer artifacts")
)

diagnostics_steps <- list(
  step("di01", "scripts/diagnostics/di01_psis_reliability_gate.R", "Secondary PSIS reliability diagnostics"),
  step("di02", "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R", "New-firm predictive integration diagnostics", gate = "new_firm_predictive"),
  step("di03", "scripts/diagnostics/di03_exact_kfold_reclassification_audit.R", "Exact K-fold reclassification/Jaccard diagnostics"),
  step("di08a", "scripts/diagnostics/di08a_plan_mcmc_sampler_calibration.R", "Plan diagnostic MCMC sampler calibration"),
  step("di08b", "scripts/diagnostics/di08b_fit_mcmc_sampler_calibration_workers.R", "Fit diagnostic MCMC sampler calibration with workers", heavy = TRUE,
       requires = c(file.path(output_root, "diagnostics", "mcmc_sampler_calibration", "tables", "table_di08_sampler_calibration_task_manifest.csv")),
       require_reason = "di08a sampler calibration task manifest"),
  step("di08c", "scripts/diagnostics/di08c_collect_mcmc_sampler_calibration.R", "Collect diagnostic MCMC sampler calibration",
       requires = c(
         file.path(output_root, "diagnostics", "mcmc_sampler_calibration", "tables", "table_di08_sampler_calibration_task_manifest.csv"),
         file.path(output_root, "diagnostics", "mcmc_sampler_calibration", "tables", "table_di08_sampler_calibration_task_status.csv")
       ),
       require_reason = "di08a manifest and di08b task status")
)

diagnostics_steps_for_all <- list(
  step("di01", "scripts/diagnostics/di01_psis_reliability_gate.R", "Secondary PSIS reliability diagnostics"),
  step("di03", "scripts/diagnostics/di03_exact_kfold_reclassification_audit.R", "Exact K-fold reclassification/Jaccard diagnostics",
       requires = c(
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv"),
         table_artifact("final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv"),
         file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv")
       ),
       require_reason = "exact-KFold grouped/row DA outputs and new-firm predictive gate decision"),
  step("di08a", "scripts/diagnostics/di08a_plan_mcmc_sampler_calibration.R", "Plan diagnostic MCMC sampler calibration"),
  step("di08b", "scripts/diagnostics/di08b_fit_mcmc_sampler_calibration_workers.R", "Fit diagnostic MCMC sampler calibration with workers", heavy = TRUE,
       requires = c(file.path(output_root, "diagnostics", "mcmc_sampler_calibration", "tables", "table_di08_sampler_calibration_task_manifest.csv")),
       require_reason = "di08a sampler calibration task manifest"),
  step("di08c", "scripts/diagnostics/di08c_collect_mcmc_sampler_calibration.R", "Collect diagnostic MCMC sampler calibration",
       requires = c(
         file.path(output_root, "diagnostics", "mcmc_sampler_calibration", "tables", "table_di08_sampler_calibration_task_manifest.csv"),
         file.path(output_root, "diagnostics", "mcmc_sampler_calibration", "tables", "table_di08_sampler_calibration_task_status.csv")
       ),
       require_reason = "di08a manifest and di08b task status")
)

steps_for_target <- function(x) {
  switch(
    x,
    main = main_steps,
    robustness = robustness_steps,
    sensitivity = sensitivity_steps,
    simulation = simulation_steps,
    diagnostics = diagnostics_steps,
    reviewer = reviewer_steps,
    all = c(main_steps, diagnostics_steps_for_all, robustness_steps, sensitivity_steps, simulation_steps, reviewer_steps)
  )
}

write_config_registry_if_available <- function() {
  if (exists("write_execution_config_registry", mode = "function")) {
    write_execution_config_registry()
  }
}

print_header <- function(steps) {
  cat("Bayesian Accrual Uncertainty Vietnam\n")
  cat("Target    :", target, "\n")
  cat("Dry run   :", dry_run, "\n")
  cat("Run heavy :", run_heavy, "\n")
  cat("Data path :", orch_cfg$data_path, "\n")
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
