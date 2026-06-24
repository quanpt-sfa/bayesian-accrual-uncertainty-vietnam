source("scripts/ma00_setup.R")

this_test <- paste(readLines("tests/test_chapter3_method_alignment_static.R", warn = FALSE), collapse = "\n")
forbidden_report_fragment <- paste0("reports", "/")
if (grepl(forbidden_report_fragment, this_test, fixed = TRUE)) {
  stop("test_chapter3_method_alignment_static.R must not depend on generated report paths.")
}

thr <- chapter3_prior_predictive_thresholds()
authority_path <- thr$source
if (grepl(forbidden_report_fragment, authority_path, fixed = TRUE)) {
  stop("chapter3_prior_predictive_thresholds() source must not point to generated reports.")
}
if (!grepl(paste0("^doc", .Platform$file.sep), authority_path) &&
    !grepl(paste0("^docs", .Platform$file.sep), authority_path)) {
  stop("Chapter 3 authority file must live under doc/ or docs/.")
}
if (!file.exists(authority_path)) stop("Missing Chapter 3 authority file: ", authority_path)

chapter3 <- readLines(authority_path, warn = FALSE, encoding = "UTF-8")
chapter3_text <- paste(chapter3, collapse = "\n")
if (!grepl("mass outside (|TA|>2) should not exceed 1%", chapter3_text, fixed = TRUE)) {
  stop("Chapter 3 authority text no longer contains the expected |TA|>2 <= 1% rule.")
}
fmt <- function(x) formatC(x, format = "f", digits = 2)
for (fragment in c(
  paste0("|TA| > 1 PASS threshold = ", fmt(thr$abs_gt_1_pass)),
  paste0("|TA| > 2 PASS threshold = ", fmt(thr$abs_gt_2_pass)),
  paste0("range-ratio PASS threshold = ", fmt(thr$range_ratio_pass))
)) {
  if (!grepl(fragment, chapter3_text, fixed = TRUE)) {
    stop("Chapter 3 authority file missing required threshold text: ", fragment)
  }
}

if (!identical(thr$source, authority_path)) stop("Chapter 3 threshold source must point to ", authority_path)

with_clean_env <- function(names, expr) {
  old_values <- Sys.getenv(names, unset = NA_character_)
  names(old_values) <- names
  on.exit({
    for (nm in names(old_values)) {
      if (is.na(old_values[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, as.list(stats::setNames(old_values[[nm]], nm)))
      }
    }
  }, add = TRUE)
  Sys.unsetenv(names)
  force(expr)
}

sampler_default_env <- c(
  "ACCRUAL_BASELINE_CHAINS",
  "ACCRUAL_BASELINE_CORES",
  "ACCRUAL_BASELINE_ITER",
  "ACCRUAL_BASELINE_WARMUP",
  "ACCRUAL_BASELINE_ADAPT_DELTA",
  "ACCRUAL_BASELINE_MAX_TREEDEPTH",
  "ACCRUAL_KFOLD_FIRM_CHAINS",
  "ACCRUAL_KFOLD_FIRM_CORES",
  "ACCRUAL_KFOLD_FIRM_ITER",
  "ACCRUAL_KFOLD_FIRM_WARMUP",
  "ACCRUAL_ROW_KFOLD_CHAINS",
  "ACCRUAL_ROW_KFOLD_CORES",
  "ACCRUAL_ROW_KFOLD_ITER",
  "ACCRUAL_ROW_KFOLD_WARMUP"
)

with_clean_env(sampler_default_env, {
  baseline_cfg <- accrual_sampler_config("baseline")
  if (baseline_cfg$chains < 1L || baseline_cfg$cores < 1L ||
      !(baseline_cfg$warmup < baseline_cfg$iter) ||
      !identical(baseline_cfg$backend, "rstan") ||
      !grepl("scripts/ma00_setup.R", baseline_cfg$config_source, fixed = TRUE)) {
    stop("Baseline sampler defaults must be structurally valid and sourced from ma00.")
  }

  grouped_cfg <- accrual_kfold_config("grouped_firm")
  row_cfg <- accrual_kfold_config("row")
  for (cfg_name in c("grouped_cfg", "row_cfg")) {
    cfg <- get(cfg_name)
    if (cfg$K < 1L || cfg$seed < 0L || cfg$chains < 1L ||
        cfg$cores < 1L || !(cfg$warmup < cfg$iter) ||
        !identical(cfg$backend, "rstan") ||
        !grepl("scripts/ma00_setup.R", cfg$config_source, fixed = TRUE)) {
      stop(cfg_name, " does not expose structurally valid ma00-owned exact K-fold config.")
    }
  }

  fast_cfg <- accrual_sampler_config("row_kfold", run_mode = "FAST_MODE")
  if (fast_cfg$chains < 1L || fast_cfg$cores < 1L ||
      !(fast_cfg$warmup < fast_cfg$iter) ||
      !identical(fast_cfg$backend, "rstan") ||
      !grepl("scripts/ma00_setup.R", fast_cfg$config_source, fixed = TRUE)) {
    stop("FAST_MODE defaults must be structurally valid and sourced from ma00.")
  }
})

remediation_cfg <- accrual_sampler_config("baseline_remediation")
if (!"cores" %in% names(remediation_cfg) || !identical(remediation_cfg$cores, remediation_cfg$chains)) {
  stop("Baseline remediation sampler config must expose cores defaulting to chains.")
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
ma07_text <- script_text("scripts/ma07a_fit_brms_named_models.R")
if (!grepl("cores\\s*=\\s*task\\$cores", ma07_text, perl = TRUE)) {
  stop("ma07a brms fit call must pass cores = task$cores.")
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
if (!grepl('accrual_sampler_config("prior_predictive")', ma06_text, fixed = TRUE) ||
    !grepl("cores\\s*=\\s*prior_cfg\\$cores", ma06_text, perl = TRUE)) {
  stop("ma06 prior predictive brm() call must use centralized prior_predictive sampler cores.")
}
ma09_text <- script_text("scripts/ma09_loo_stacking.R")
if (!grepl("loo_cfg\\s*<-\\s*accrual_loo_config\\(\\)", ma09_text, perl = TRUE) ||
    !grepl("cores\\s*<-\\s*loo_cfg\\$cores", ma09_text, perl = TRUE) ||
    !grepl("cores\\s*=\\s*cores", ma09_text, perl = TRUE)) {
  stop("ma09 LOO refit brm() call must pass cores from accrual_loo_config().")
}
se01_text <- script_text("scripts/sensitivity/se01_prior_predictive.R")
if (!grepl('accrual_sampler_config("prior_predictive")', se01_text, fixed = TRUE) ||
    !grepl("cores\\s*=\\s*prior_cfg\\$cores", se01_text, perl = TRUE)) {
  stop("se01 sensitivity prior predictive brm() call must use centralized prior_predictive sampler cores.")
}
si03_text <- script_text("scripts/simulation/si03_brms_leakage_confirmation.R")
if (!grepl('accrual_simulation_runtime_config("brms_leakage")', si03_text, fixed = TRUE) ||
    !grepl("cores\\s*<-\\s*sim_cfg\\$cores", si03_text, perl = TRUE) ||
    !grepl("cores\\s*=\\s*cores", si03_text, perl = TRUE)) {
  stop("si03 simulation brm() call must pass explicit simulation cores from accrual_simulation_runtime_config().")
}
si04_text <- script_text("scripts/simulation/si04_brms_parameter_recovery.R")
if (!grepl('accrual_simulation_runtime_config("brms_recovery")', si04_text, fixed = TRUE) ||
    !grepl("cores\\s*<-\\s*sim_cfg\\$cores", si04_text, perl = TRUE) ||
    !grepl("cores\\s*=\\s*cores", si04_text, perl = TRUE)) {
  stop("si04 simulation brm() call must pass explicit simulation cores from accrual_simulation_runtime_config().")
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
  "scripts/ma12a_plan_grouped_kfold_firm.R",
  "scripts/ma12b_fit_grouped_kfold_firm_workers.R",
  "scripts/ma12c_collect_grouped_kfold_firm_scores.R",
  "scripts/ma13a_plan_row_level_exact_kfold.R",
  "scripts/ma13b_fit_row_level_exact_kfold_workers.R",
  "scripts/ma13c_collect_row_level_exact_kfold_scores.R",
  "scripts/ma14_construct_exact_kfold_DA.R",
  "scripts/ma15_audit_DA_finite_outputs.R",
  "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R"
)
missing_dry <- required_dry_paths[vapply(required_dry_paths, function(x) !grepl(x, dry_text, fixed = TRUE), logical(1))]
if (length(missing_dry)) stop("run.R --dry-run missing required primary/gate path(s): ", paste(missing_dry, collapse = ", "))

cat("test_chapter3_method_alignment_static.R passed\n")
