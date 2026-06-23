chapter3_path <- "reports/chapter_3_method_only_reviewer_final_journal_style_transitions.md"
if (!file.exists(chapter3_path)) stop("Missing Chapter 3 authority file: ", chapter3_path)

source("scripts/ma00_setup.R")

chapter3 <- readLines(chapter3_path, warn = FALSE, encoding = "UTF-8")
chapter3_text <- paste(chapter3, collapse = "\n")
if (!grepl("mass outside \\(|TA|>2\\) should not exceed 1%", chapter3_text, fixed = TRUE)) {
  stop("Chapter 3 authority text no longer contains the expected |TA|>2 <= 1% rule.")
}

thr <- chapter3_prior_predictive_thresholds()
if (!identical(thr$abs_gt_1_pass, 0.05)) stop("Chapter 3 |TA|>1 PASS threshold must be 0.05.")
if (!identical(thr$abs_gt_2_pass, 0.01)) stop("Chapter 3 |TA|>2 PASS threshold must be 0.01.")
if (!identical(thr$range_ratio_pass, 3.00)) stop("Chapter 3 prior predictive range ratio PASS threshold must be 3.00.")

baseline_cfg <- accrual_sampler_config("baseline")
if (!identical(baseline_cfg$chains, 4L) ||
    !identical(baseline_cfg$cores, 4L) ||
    !identical(baseline_cfg$iter, 3000L) ||
    !identical(baseline_cfg$warmup, 1000L) ||
    !isTRUE(all.equal(baseline_cfg$adapt_delta, 0.95)) ||
    !identical(baseline_cfg$max_treedepth, 12L)) {
  stop("Baseline sampler defaults do not match Chapter 3 4/3000/1000/adapt_delta=.95/max_treedepth=12.")
}

remediation_cfg <- accrual_sampler_config("baseline_remediation")
if (!"cores" %in% names(remediation_cfg) || !identical(remediation_cfg$cores, remediation_cfg$chains)) {
  stop("Baseline remediation sampler config must expose cores defaulting to chains.")
}

grouped_cfg <- accrual_kfold_config("grouped_firm")
row_cfg <- accrual_kfold_config("row")
for (cfg_name in c("grouped_cfg", "row_cfg")) {
  cfg <- get(cfg_name)
  if (!identical(cfg$K, 5L) ||
      !identical(cfg$seed, 42L) ||
      !identical(cfg$chains, 4L) ||
      !identical(cfg$cores, 4L) ||
      !identical(cfg$iter, 3000L) ||
      !identical(cfg$warmup, 1000L)) {
    stop(cfg_name, " does not match Chapter 3 exact K-fold defaults.")
  }
}

fast_cfg <- accrual_sampler_config("row_kfold", run_mode = "FAST_MODE")
if (!identical(fast_cfg$chains, 2L) || !identical(fast_cfg$cores, 2L) ||
    !identical(fast_cfg$iter, 1000L) || !identical(fast_cfg$warmup, 500L)) {
  stop("FAST_MODE defaults must remain 2 chains, 1000 iter, 500 warmup.")
}

sensitivity_cfg <- accrual_sampler_config("sensitivity")
if (!"cores" %in% names(sensitivity_cfg) || !identical(sensitivity_cfg$cores, sensitivity_cfg$chains)) {
  stop("Sensitivity sampler config must expose cores defaulting to chains.")
}

registry_path <- tempfile(fileext = ".csv")
write_execution_config_registry(registry_path)
registry <- read.csv(registry_path, stringsAsFactors = FALSE)
required_core_scopes <- c(
  "baseline",
  "baseline_remediation",
  "grouped_kfold_FULL_MODE",
  "grouped_kfold_FAST_MODE",
  "row_kfold_FULL_MODE",
  "row_kfold_FAST_MODE",
  "sensitivity"
)
missing_core_scopes <- required_core_scopes[
  !vapply(required_core_scopes, function(scope) {
    any(registry$Scope == scope & registry$Parameter == "cores")
  }, logical(1))
]
if (length(missing_core_scopes)) {
  stop("Execution config registry missing sampler cores rows for: ", paste(missing_core_scopes, collapse = ", "))
}
core_notes <- registry$Notes[registry$Parameter == "cores"]
if (!any(grepl("rstan between-chain parallelization only", core_notes, fixed = TRUE))) {
  stop("Execution config registry cores rows must document rstan between-chain parallelization.")
}

script_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
ma07_text <- script_text("scripts/ma07_fit_brms_named_models.R")
if (!grepl("cores\\s*=\\s*active_sampler_controls\\$cores", ma07_text, perl = TRUE)) {
  stop("ma07 brm() call must pass cores = active_sampler_controls$cores.")
}
ma12_text <- script_text("scripts/ma12_grouped_kfold_firm.R")
if (!grepl("cores\\s*=\\s*kfold_chain_cores", ma12_text, perl = TRUE) ||
    !grepl("kfold_chain_cores\\s*<-\\s*kfold_cfg\\$cores", ma12_text, perl = TRUE)) {
  stop("ma12 grouped K-fold brm() call must pass cores from accrual_kfold_config().")
}
for (fragment in c("Cores = kfold_chain_cores", "Backend = \"rstan\"", "sampler_provenance <- list(",
                   "cores = kfold_chain_cores", "backend = \"rstan\"")) {
  if (!grepl(fragment, ma12_text, fixed = TRUE)) {
    stop("ma12 grouped K-fold cache/manifest metadata missing sampler provenance fragment: ", fragment)
  }
}
ma13_text <- script_text("scripts/ma13_row_level_exact_kfold.R")
if (!grepl("cores\\s*=\\s*row_kfold_chain_cores", ma13_text, perl = TRUE) ||
    !grepl("row_kfold_chain_cores\\s*<-\\s*kfold_cfg\\$cores", ma13_text, perl = TRUE)) {
  stop("ma13 row K-fold brm() call must pass cores from accrual_kfold_config().")
}
for (fragment in c("Cores = row_kfold_chain_cores", "Backend = \"rstan\"", "sampler_provenance <- list(",
                   "cores = row_kfold_chain_cores", "backend = \"rstan\"")) {
  if (!grepl(fragment, ma13_text, fixed = TRUE)) {
    stop("ma13 row K-fold cache/manifest metadata missing sampler provenance fragment: ", fragment)
  }
}
se02_text <- script_text("scripts/sensitivity/se02_refit_prior_scenarios.R")
if (!grepl("cores\\s*=\\s*cores", se02_text, perl = TRUE) ||
    !grepl("cores\\s*<-\\s*sampler_cfg\\$cores", se02_text, perl = TRUE)) {
  stop("se02 sensitivity brm() call must pass cores from accrual_sampler_config().")
}
if (!grepl("backend = \"rstan\"", se02_text, fixed = TRUE)) {
  stop("se02 sensitivity metadata must record backend = rstan.")
}
ma06_text <- script_text("scripts/ma06_prior_predictive_checks.R")
if (!grepl("ACCRUAL_BASELINE_CORES", ma06_text, fixed = TRUE) ||
    !grepl("cores\\s*=\\s*cores", ma06_text, perl = TRUE)) {
  stop("ma06 prior predictive brm() call must pass explicit baseline cores.")
}
ma09_text <- script_text("scripts/ma09_loo_stacking.R")
if (!grepl("cores\\s*<-\\s*sampler_cfg\\$cores", ma09_text, perl = TRUE) ||
    !grepl("cores\\s*=\\s*cores", ma09_text, perl = TRUE)) {
  stop("ma09 LOO refit brm() call must pass cores from accrual_sampler_config().")
}
se01_text <- script_text("scripts/sensitivity/se01_prior_predictive.R")
if (!grepl("ACCRUAL_SENS_CORES", se01_text, fixed = TRUE) ||
    !grepl("cores\\s*=\\s*cores", se01_text, perl = TRUE)) {
  stop("se01 sensitivity prior predictive brm() call must pass explicit sensitivity cores.")
}
si03_text <- script_text("scripts/simulation/si03_brms_leakage_confirmation.R")
if (!grepl("ACCRUAL_SIM_CORES", si03_text, fixed = TRUE) ||
    !grepl("cores\\s*=\\s*cores", si03_text, perl = TRUE)) {
  stop("si03 simulation brm() call must pass explicit simulation cores.")
}
si04_text <- script_text("scripts/simulation/si04_brms_parameter_recovery.R")
if (!grepl("ACCRUAL_SIM_CORES", si04_text, fixed = TRUE) ||
    !grepl("cores\\s*=\\s*cores", si04_text, perl = TRUE)) {
  stop("si04 simulation brm() call must pass explicit simulation cores.")
}

di02 <- readLines("scripts/diagnostics/di02_new_firm_predictive_integration_audit.R", warn = FALSE)
di02_text <- paste(di02, collapse = "\n")
obsolete_di02_paths <- c(
  "scripts/10_construct_uncertainty_adjusted_DA.R",
  "scripts/12_lofo_stacking.R",
  "scripts/13_grouped_kfold_firm.R",
  "scripts/26_sim_brms_leakage_confirmation.R",
  "scripts/28_row_level_exact_kfold.R",
  "scripts/29_psis_reliability_gate.R"
)
hits <- obsolete_di02_paths[vapply(obsolete_di02_paths, grepl, logical(1), x = di02_text, fixed = TRUE)]
if (length(hits)) stop("di02 still references obsolete script path(s): ", paste(hits, collapse = ", "))

required_di02_paths <- c(
  "ma10_construct_psis_loo_DA.R",
  "robustness\", \"ro01_lofo_stacking.R",
  "ma12_grouped_kfold_firm.R",
  "simulation\", \"si03_brms_leakage_confirmation.R",
  "ma13_row_level_exact_kfold.R",
  "diagnostics\", \"di01_psis_reliability_gate.R"
)
missing <- required_di02_paths[vapply(required_di02_paths, function(x) !grepl(x, di02_text, fixed = TRUE), logical(1))]
if (length(missing)) stop("di02 is missing active source path fragment(s): ", paste(missing, collapse = ", "))

if (!grepl("source_role_specific_not_global", di02_text, fixed = TRUE) ||
    !grepl("does not verify posterior predictive tail draws", di02_text, fixed = TRUE)) {
  stop("di02 must preserve source-specific verification and reject global evidence transfer.")
}

ma14 <- paste(readLines("scripts/ma14_construct_exact_kfold_DA.R", warn = FALSE), collapse = "\n")
if (!grepl("Completed_Run_Pin_Eligible", ma14, fixed = TRUE) ||
    !grepl("LATEST_COMPLETED_RUN.txt", ma14, fixed = TRUE)) {
  stop("ma14 must retain completed-pin eligibility checks and use LATEST_COMPLETED_RUN pins.")
}
if (grepl('file.path\\([^\\n]*"LATEST_RUN.txt"', ma14)) {
  stop("ma14 must not use moving LATEST_RUN.txt as primary provenance.")
}

dry <- system2("Rscript", c("run.R", "--dry-run"), stdout = TRUE, stderr = TRUE)
dry_text <- paste(dry, collapse = "\n")
required_dry_paths <- c(
  "scripts/ma12_grouped_kfold_firm.R",
  "scripts/ma13_row_level_exact_kfold.R",
  "scripts/ma14_construct_exact_kfold_DA.R",
  "scripts/ma15_audit_DA_finite_outputs.R",
  "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R"
)
missing_dry <- required_dry_paths[vapply(required_dry_paths, function(x) !grepl(x, dry_text, fixed = TRUE), logical(1))]
if (length(missing_dry)) stop("run.R --dry-run missing required primary/gate path(s): ", paste(missing_dry, collapse = ", "))

cat("test_chapter3_method_alignment_static.R passed\n")
