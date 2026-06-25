# -----------------------------------------------------------------------------
# Production run-profile registry
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

accrual_run_profile_registry <- function(current_only = TRUE) {
  baseline_defaults <- accrual_production_sampler_defaults("baseline")
  grouped_kfold_defaults <- accrual_production_sampler_defaults("grouped_kfold", "FULL_MODE")
  row_kfold_defaults <- accrual_production_sampler_defaults("row_kfold", "FULL_MODE")
  sensitivity_defaults <- accrual_production_sampler_defaults("sensitivity")
  common_static_env <- c(
    ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE",
    ACCRUAL_MODEL_PARALLEL_WORKERS = "10",
    ACCRUAL_TOTAL_CORE_BUDGET = "40",
    ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE",
    ACCRUAL_SEED = "42",
    ACCRUAL_PRIOR_SET_ID = "scale_aware_student_baseline_v1",
    ACCRUAL_FAMILY = "student",
    ACCRUAL_MODEL_STRUCTURE = "pooled_random_intercept",
    ACCRUAL_RUN_HEAVY = "TRUE"
  )
  main_env <- c(
    common_static_env,
    ACCRUAL_DRY_RUN = "FALSE",
    ACCRUAL_FORCE_REFIT = "TRUE",
    ACCRUAL_PRIOR_PRED_CHAINS = "4",
    ACCRUAL_PRIOR_PRED_CORES = "4",
    ACCRUAL_PRIOR_PRED_ITER = "1000",
    ACCRUAL_PRIOR_PRED_WARMUP = "500",
    ACCRUAL_PRIOR_PRED_REFRESH = "0",
    ACCRUAL_BASELINE_CHAINS = as.character(baseline_defaults$chains),
    ACCRUAL_BASELINE_CORES = as.character(baseline_defaults$cores),
    ACCRUAL_BASELINE_ITER = as.character(baseline_defaults$iter),
    ACCRUAL_BASELINE_WARMUP = as.character(baseline_defaults$warmup),
    ACCRUAL_BASELINE_ADAPT_DELTA = as.character(baseline_defaults$adapt_delta),
    ACCRUAL_BASELINE_MAX_TREEDEPTH = as.character(baseline_defaults$max_treedepth),
    ACCRUAL_BASELINE_REFRESH = as.character(baseline_defaults$refresh),
    ACCRUAL_REMEDIATION_CHAINS = "4",
    ACCRUAL_REMEDIATION_CORES = "4",
    ACCRUAL_REMEDIATION_ITER = "16000",
    ACCRUAL_REMEDIATION_WARMUP = "6000",
    ACCRUAL_REMEDIATION_ADAPT_DELTA = "0.99",
    ACCRUAL_REMEDIATION_MAX_TREEDEPTH = "15",
    ACCRUAL_REMEDIATION_REFRESH = "500",
    ACCRUAL_KFOLD_FIRM_MODE = "FULL_MODE",
    ACCRUAL_KFOLD_FIRM_K = "5",
    ACCRUAL_KFOLD_FIRM_CHAINS = as.character(grouped_kfold_defaults$chains),
    ACCRUAL_KFOLD_FIRM_CORES = as.character(grouped_kfold_defaults$cores),
    ACCRUAL_KFOLD_FIRM_ITER = as.character(grouped_kfold_defaults$iter),
    ACCRUAL_KFOLD_FIRM_WARMUP = as.character(grouped_kfold_defaults$warmup),
    ACCRUAL_KFOLD_FIRM_ADAPT_DELTA = as.character(grouped_kfold_defaults$adapt_delta),
    ACCRUAL_KFOLD_FIRM_MAX_TREEDEPTH = as.character(grouped_kfold_defaults$max_treedepth),
    ACCRUAL_KFOLD_FIRM_REFRESH = as.character(grouped_kfold_defaults$refresh),
    ACCRUAL_KFOLD_FIRM_OVERWRITE = "TRUE",
    ACCRUAL_ROW_KFOLD_MODE = "FULL_MODE",
    ACCRUAL_ROW_KFOLD_K = "5",
    ACCRUAL_ROW_KFOLD_CHAINS = as.character(row_kfold_defaults$chains),
    ACCRUAL_ROW_KFOLD_CORES = as.character(row_kfold_defaults$cores),
    ACCRUAL_ROW_KFOLD_ITER = as.character(row_kfold_defaults$iter),
    ACCRUAL_ROW_KFOLD_WARMUP = as.character(row_kfold_defaults$warmup),
    ACCRUAL_ROW_KFOLD_ADAPT_DELTA = as.character(row_kfold_defaults$adapt_delta),
    ACCRUAL_ROW_KFOLD_MAX_TREEDEPTH = as.character(row_kfold_defaults$max_treedepth),
    ACCRUAL_ROW_KFOLD_REFRESH = as.character(row_kfold_defaults$refresh),
    ACCRUAL_ROW_KFOLD_OVERWRITE = "TRUE"
  )
  sensitivity_env <- c(
    common_static_env,
    ACCRUAL_SENS_CHAINS = as.character(sensitivity_defaults$chains),
    ACCRUAL_SENS_CORES = as.character(sensitivity_defaults$cores),
    ACCRUAL_SENS_ITER = as.character(sensitivity_defaults$iter),
    ACCRUAL_SENS_WARMUP = as.character(sensitivity_defaults$warmup),
    ACCRUAL_SENS_ADAPT_DELTA = as.character(sensitivity_defaults$adapt_delta),
    ACCRUAL_SENS_MAX_TREEDEPTH = as.character(sensitivity_defaults$max_treedepth),
    ACCRUAL_SENS_REFRESH = as.character(sensitivity_defaults$refresh)
  )
  diagnostics_env <- c(
    common_static_env,
    ACCRUAL_DIAG_CHAINS = "4",
    ACCRUAL_DIAG_CORES = "4",
    ACCRUAL_DIAG_ITER = "4000",
    ACCRUAL_DIAG_WARMUP = "1500",
    ACCRUAL_DIAG_ADAPT_DELTA = "0.99",
    ACCRUAL_DIAG_MAX_TREEDEPTH = "15",
    ACCRUAL_DIAG_REFRESH = "0"
  )
  simulation_env <- c(
    common_static_env,
    ACCRUAL_SIM_REPLICATIONS = "500",
    ACCRUAL_SIM_TEMPORAL_REPLICATIONS = "500",
    ACCRUAL_SIM_BRMS_REPLICATIONS = "30",
    ACCRUAL_SIM_BRMS_CHAINS = "4",
    ACCRUAL_SIM_BRMS_CORES = "4",
    ACCRUAL_SIM_BRMS_ITER = "4000",
    ACCRUAL_SIM_BRMS_WARMUP = "1500",
    ACCRUAL_SIM_BRMS_ADAPT_DELTA = "0.99",
    ACCRUAL_SIM_BRMS_MAX_TREEDEPTH = "15",
    ACCRUAL_SIM_RECOVERY_REPLICATIONS = "30",
    ACCRUAL_SIM_RECOVERY_CHAINS = "4",
    ACCRUAL_SIM_RECOVERY_CORES = "4",
    ACCRUAL_SIM_RECOVERY_ITER = "4000",
    ACCRUAL_SIM_RECOVERY_WARMUP = "1500",
    ACCRUAL_SIM_RECOVERY_ADAPT_DELTA = "0.99",
    ACCRUAL_SIM_RECOVERY_MAX_TREEDEPTH = "15",
    ACCRUAL_SIM_REFRESH = "0",
    ACCRUAL_BRMS_BACKEND = "rstan",
    ACCRUAL_PARALLEL_BACKEND = "base_parallel"
  )
  make_entry <- function(profile_id, profile_path, target, env,
                         requires_baseline_marker, requires_latest_main_pointer,
                         writes_baseline_marker = FALSE, writes_latest_main_pointer = FALSE) {
    list(
      profile_id = profile_id,
      profile_path = profile_path,
      target = target,
      workers = as.integer(env[["ACCRUAL_MODEL_PARALLEL_WORKERS"]]),
      rstan_cores_per_fit = 4L,
      total_core_budget = as.integer(env[["ACCRUAL_TOTAL_CORE_BUDGET"]]),
      requires_baseline_marker = requires_baseline_marker,
      requires_latest_main_pointer = requires_latest_main_pointer,
      writes_baseline_marker = writes_baseline_marker,
      writes_latest_main_pointer = writes_latest_main_pointer,
      baseline_marker_file = "BASELINE_MA17_COMPLETE.txt",
      latest_main_pointer = "out/runs/LATEST_MAIN_10W4C_RUN_ROOT.txt",
      env = env
    )
  }
  registry <- list(
    run_01_main_production_10w4c = make_entry(
      profile_id = "run_01_main_production_10w4c",
      profile_path = "run_profiles/run_01_main_production_10w4c.ps1",
      target = "main",
      env = main_env,
      requires_baseline_marker = FALSE,
      requires_latest_main_pointer = FALSE,
      writes_baseline_marker = TRUE,
      writes_latest_main_pointer = TRUE
    ),
    run_02_sensitivity_after_main_10w4c = make_entry(
      profile_id = "run_02_sensitivity_after_main_10w4c",
      profile_path = "run_profiles/run_02_sensitivity_after_main_10w4c.ps1",
      target = "sensitivity",
      env = sensitivity_env,
      requires_baseline_marker = TRUE,
      requires_latest_main_pointer = TRUE
    ),
    run_03_diagnostics_after_main_10w4c = make_entry(
      profile_id = "run_03_diagnostics_after_main_10w4c",
      profile_path = "run_profiles/run_03_diagnostics_after_main_10w4c.ps1",
      target = "diagnostics",
      env = diagnostics_env,
      requires_baseline_marker = TRUE,
      requires_latest_main_pointer = TRUE
    ),
    run_04_simulation_after_main_10w4c = make_entry(
      profile_id = "run_04_simulation_after_main_10w4c",
      profile_path = "run_profiles/run_04_simulation_after_main_10w4c.ps1",
      target = "simulation",
      env = simulation_env,
      requires_baseline_marker = TRUE,
      requires_latest_main_pointer = TRUE
    )
  )
  registry
}

accrual_run_profile_names <- function(current_only = TRUE) {
  names(accrual_run_profile_registry(current_only = current_only))
}

accrual_run_profile_entry <- function(profile = accrual_run_profile_names()[1], current_only = TRUE) {
  registry <- accrual_run_profile_registry(current_only = current_only)
  if (!profile %in% names(registry)) {
    stop("[BLOCKER] Unknown run profile: ", profile, ". Current profiles: ", paste(names(registry), collapse = ", "))
  }
  registry[[profile]]
}

accrual_run_profile_config <- function(profile = accrual_run_profile_names()[1]) {
  accrual_run_profile_entry(profile)$env
}

