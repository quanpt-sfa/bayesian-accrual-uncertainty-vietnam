txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
normalize_path <- function(path) gsub("\\\\", "/", path)

audit_path <- file.path("doc", "audits", "brms_worker_refactor_audit.md")
plan_path <- file.path("doc", "audits", "brms_worker_refactor_plan.md")
if (!file.exists(audit_path)) stop("Missing brms worker refactor audit report.")
if (!file.exists(plan_path)) stop("Missing brms worker refactor plan report.")
this_test <- txt("tests/test_brms_worker_refactor_static.R")
if (grepl(paste0("reports", "/"), this_test, fixed = TRUE)) {
  stop("test_brms_worker_refactor_static.R must not require generated reports.")
}

audit <- txt(audit_path)
plan <- txt(plan_path)
ma00 <- txt("scripts/ma00_setup.R")
ma06 <- txt("scripts/ma06_prior_predictive_checks.R")
ma07a <- txt("scripts/ma07a_fit_brms_named_models.R")
se01 <- txt("scripts/sensitivity/se01_prior_predictive.R")

scan_files <- c(
  "run.R",
  list.files("scripts", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
)
scan_files <- normalize_path(scan_files)
brm_files <- scan_files[vapply(scan_files, function(path) {
  body <- txt(path)
  grepl("brms::brm", body, fixed = TRUE) || grepl("\\bbrm\\s*\\(", body, perl = TRUE)
}, logical(1))]
missing <- brm_files[!vapply(brm_files, function(path) grepl(path, audit, fixed = TRUE), logical(1))]
if (length(missing)) {
  stop("Every file containing brm() must be listed in brms_worker_refactor_audit.md. Missing: ",
       paste(missing, collapse = ", "))
}

for (fragment in c(
  "accrual_fit_worker_config <- function",
  "accrual_run_task_pool <- function",
  "parallel::parLapplyLB",
  "parallel::makeCluster",
  "accrual_task_status_blocker <- function",
  "ACCRUAL_ALLOW_NESTED_RSTAN_CORES",
  "workers * cores_per_fit"
)) {
  if (!grepl(fragment, ma00, fixed = TRUE)) {
    stop("ma00 missing common worker helper or nested-rstan policy fragment: ", fragment)
  }
}

for (fragment in c(
  "table_ma06_prior_predictive_task_manifest.csv",
  "table_ma06_prior_predictive_task_status.csv",
  "fit_ma06_prior_task_worker <- function",
  "accrual_fit_worker_config(\"prior_predictive\"",
  "accrual_run_task_pool(",
  "stable_task_key(\"ma06_prior_predictive\"",
  "accrual_rng_metadata_list(\"baseline_prior_predictive_fit\", offset = i)",
  "seed = task$Effective_Seed",
  "sample_prior = \"only\"",
  "brms::posterior_predict(fit_prior)"
)) {
  if (!grepl(fragment, ma06, fixed = TRUE)) {
    stop("ma06 missing workerized prior predictive fragment: ", fragment)
  }
}
if (grepl("worker_id|cluster_id", ma06, perl = TRUE)) {
  stop("ma06 task seeds must not depend on worker identity.")
}

worker_body <- regmatches(
  ma06,
  regexpr("fit_ma06_prior_task_worker <- function\\(task\\) \\{[\\s\\S]*?\\n\\}\\n\\nnotes <-", ma06, perl = TRUE)
)
if (!length(worker_body) || !nzchar(worker_body)) {
  stop("Could not isolate ma06 worker body for static shared-write checks.")
}
for (shared_path in c(
  "table_prior_predictive_summary.csv",
  "table_prior_predictive_extreme_rates.csv",
  "prior_predictive_gate_status.csv",
  "phase3a_prior_predictive_notes.txt",
  "table_ma06_prior_predictive_task_manifest.csv",
  "table_ma06_prior_predictive_task_status.csv"
)) {
  if (grepl(shared_path, worker_body, fixed = TRUE)) {
    stop("ma06 worker body must not write or name shared output path: ", shared_path)
  }
}
if (grepl("write\\.csv|saveRDS", worker_body, perl = TRUE)) {
  stop("ma06 worker body must not write shared CSV/RDS outputs.")
}

for (fragment in c(
  "table_se01_prior_predictive_task_manifest.csv",
  "table_se01_prior_predictive_task_status.csv",
  "fit_se01_prior_task_worker <- function",
  "accrual_fit_worker_config(\"prior_predictive\"",
  "accrual_run_task_pool(",
  "stable_task_key(\"se01_prior_predictive\"",
  "accrual_rng_metadata_list(rng_context, offset = rng_offset)",
  "seed = task$Effective_Seed",
  "sample_prior = \"only\"",
  "brms::posterior_predict(fit, ndraws = task$n_draws)"
)) {
  if (!grepl(fragment, se01, fixed = TRUE)) {
    stop("se01 missing workerized sensitivity prior predictive fragment: ", fragment)
  }
}
if (grepl("worker_id|cluster_id", se01, perl = TRUE)) {
  stop("se01 task seeds must not depend on worker identity.")
}
se01_worker_body <- regmatches(
  se01,
  regexpr("fit_se01_prior_task_worker <- function\\(task\\) \\{[\\s\\S]*?\\n\\}\\n\\nfor \\(sidx", se01, perl = TRUE)
)
if (!length(se01_worker_body) || !nzchar(se01_worker_body)) {
  stop("Could not isolate se01 worker body for static shared-write checks.")
}
for (shared_path in c(
  "sensitivity_prior_predictive_summary.csv",
  "sensitivity_prior_predictive_gate.csv",
  "table_sensitivity_prior_predictive_",
  "sensitivity_prior_predictive_notes.txt",
  "table_se01_prior_predictive_task_manifest.csv",
  "table_se01_prior_predictive_task_status.csv"
)) {
  if (grepl(shared_path, se01_worker_body, fixed = TRUE)) {
    stop("se01 worker body must not write or name shared output path: ", shared_path)
  }
}
if (grepl("write\\.csv", se01_worker_body, perl = TRUE)) {
  stop("se01 worker body must not write shared CSV outputs.")
}

for (fragment in c(
  "accrual_fit_worker_config(\"baseline\"",
  "accrual_run_task_pool(",
  "metadata_state_file",
  "adopt_legacy_ma07_fits",
  "accrual_task_status_blocker("
)) {
  if (!grepl(fragment, ma07a, fixed = TRUE)) {
    stop("ma07a missing generalized worker-pool fragment: ", fragment)
  }
}

for (path in c(
  "scripts/ma09_loo_stacking.R",
  "scripts/ma12_grouped_kfold_firm.R",
  "scripts/ma13_row_level_exact_kfold.R",
  "scripts/sensitivity/se01_prior_predictive.R",
  "scripts/sensitivity/se02_refit_prior_scenarios.R"
)) {
  if (!grepl(path, audit, fixed = TRUE) || !grepl(path, plan, fixed = TRUE)) {
    stop("Production brms fit script must be explicitly audited and planned/deferred: ", path)
  }
}

for (path in c(
  "scripts/simulation/si03_brms_leakage_confirmation.R",
  "scripts/simulation/si04_brms_parameter_recovery.R",
  "scripts/diagnostics/di08_mcmc_sampler_calibration.R"
)) {
  if (!grepl(path, audit, fixed = TRUE)) {
    stop("Simulation/diagnostic brms fit script must be listed in audit: ", path)
  }
}
for (decision in c("already_workerized", "split_workerized")) {
  if (!grepl(decision, audit, fixed = TRUE)) {
    stop("Audit missing required worker_refactor_decision value: ", decision)
  }
}
for (forbidden in c("defer_diagnostic_only", "defer_simulation_only", "split_fit_collect_now")) {
  if (grepl(forbidden, audit, fixed = TRUE)) {
    stop("Audit still lists deferred/split-pending heavy brms stage: ", forbidden)
  }
}

production_worker_paths <- c(
  "scripts/ma06_prior_predictive_checks.R",
  "scripts/ma07a_fit_brms_named_models.R",
  "scripts/ma07b_extract_brms_fit_outputs_workers.R",
  "scripts/ma09b_fit_loo_savepars_refits.R",
  "scripts/ma12b_fit_grouped_kfold_firm_workers.R",
  "scripts/ma13b_fit_row_level_exact_kfold_workers.R",
  "scripts/sensitivity/se01_prior_predictive.R",
  "scripts/sensitivity/se02b_fit_prior_scenario_workers.R",
  "scripts/simulation/si03b_fit_brms_leakage_confirmation_workers.R",
  "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R",
  "scripts/diagnostics/di08b_fit_mcmc_sampler_calibration_workers.R"
)
worker_superassignment <- production_worker_paths[vapply(production_worker_paths, function(path) {
  grepl("<<-", txt(path), fixed = TRUE)
}, logical(1))]
if (length(worker_superassignment)) {
  stop("Production worker scripts must not use <<- superassignment in worker/refit code: ",
       paste(worker_superassignment, collapse = ", "))
}

cat("test_brms_worker_refactor_static.R passed\n")
