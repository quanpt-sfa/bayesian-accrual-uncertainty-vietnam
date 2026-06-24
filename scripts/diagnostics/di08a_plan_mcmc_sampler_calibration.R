# Script: di08a_plan_mcmc_sampler_calibration.R
# Purpose: Plan diagnostic MCMC sampler calibration tasks.

source("scripts/ma00_setup.R")
phase_begin("di08a", "Plan MCMC sampler calibration")
grid <- accrual_calibration_profile_grid()
root <- file.path(output_root, "diagnostics", "mcmc_sampler_calibration")
manifest_path <- file.path(root, "tables", "table_di08_sampler_calibration_task_manifest.csv")
task_root <- file.path(root, "task_artifacts")
rows <- lapply(seq_len(nrow(grid)), function(i) {
  row <- grid[i, , drop = FALSE]
  task_key <- accrual_task_cache_key("di08", i, row$sampler_profile)
  rng <- accrual_rng_metadata_list("mcmc_sampler_calibration", offset = i)
  data.frame(Task_Key = task_key, Profile_ID = row$sampler_profile,
             fit_path = safe_task_artifact_path(task_root, task_key, "_fit.rds"),
             diagnostic_path = safe_task_artifact_path(task_root, task_key, "_diagnostics.rds"),
             metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
             task_log_path = safe_task_log_path(file.path(task_root, "logs"), task_key),
             chains = row$chains, cores = row$cores, iter = row$iter, warmup = row$warmup,
             adapt_delta = row$adapt_delta, max_treedepth = row$max_treedepth, backend = "rstan",
             RNG_Context = rng$RNG_Context, RNG_Offset = rng$RNG_Offset, Canonical_Seed = rng$Canonical_Seed,
             Effective_Seed = rng$Effective_Seed, Required = TRUE, stringsAsFactors = FALSE)
})
write_task_manifest(manifest_path, do.call(rbind, rows))
phase_end("di08a", "Plan MCMC sampler calibration")
