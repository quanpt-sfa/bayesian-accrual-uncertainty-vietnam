# Script: si03a_plan_brms_leakage_confirmation.R
# Purpose: Plan BRMS leakage confirmation simulation tasks.

source("scripts/ma00_setup.R")
phase_begin("si03a", "Plan BRMS leakage confirmation")
sim_cfg <- accrual_simulation_runtime_config("brms_leakage")
root <- file.path(output_root, "simulation", "brms_leakage")
manifest_path <- file.path(root, "tables", "table_si03_brms_leakage_task_manifest.csv")
task_root <- file.path(root, "task_artifacts")
rows <- lapply(seq_len(sim_cfg$R), function(i) {
  task_key <- accrual_task_cache_key("si03", i)
  rng <- accrual_rng_metadata_list("brms_leakage_confirmation", offset = i)
  data.frame(Task_Key = task_key, Replication = i,
             fit_path = safe_task_artifact_path(task_root, task_key, "_fit.rds"),
             result_path = safe_task_artifact_path(task_root, task_key, "_result.rds"),
             metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
             task_log_path = safe_task_log_path(file.path(task_root, "logs"), task_key),
             chains = sim_cfg$chains, cores = sim_cfg$cores, iter = sim_cfg$iter, warmup = sim_cfg$warmup,
             adapt_delta = sim_cfg$adapt_delta, max_treedepth = sim_cfg$max_treedepth, backend = "rstan",
             RNG_Context = rng$RNG_Context, RNG_Offset = rng$RNG_Offset, Canonical_Seed = rng$Canonical_Seed,
             Effective_Seed = rng$Effective_Seed, Required = TRUE, stringsAsFactors = FALSE)
})
write_task_manifest(manifest_path, do.call(rbind, rows))
phase_end("si03a", "Plan BRMS leakage confirmation")
