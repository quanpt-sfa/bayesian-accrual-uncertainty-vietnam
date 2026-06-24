# Script: si04a_plan_brms_parameter_recovery.R
# Purpose: Plan BRMS parameter recovery simulation tasks.

source("scripts/ma00_setup.R")
phase_begin("si04a", "Plan BRMS parameter recovery")
sim_cfg <- accrual_simulation_runtime_config("brms_recovery")
root <- file.path(output_root, "simulation", "brms_parameter_recovery")
manifest_path <- file.path(root, "tables", "table_si04_brms_recovery_task_manifest.csv")
task_root <- file.path(root, "task_artifacts")
format_sigma_key <- function(x) gsub("\\.", "p", format(x, trim = TRUE, scientific = FALSE))
grid <- expand.grid(
  T = as.integer(sim_cfg$t_grid),
  sigma_firm = as.numeric(sim_cfg$sigma_grid),
  Replication = seq_len(sim_cfg$R),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
grid <- grid[order(grid$T, grid$sigma_firm, grid$Replication), , drop = FALSE]
rows <- lapply(seq_len(nrow(grid)), function(i) {
  row <- grid[i, ]
  task_key <- sprintf("si04_T%d_sigma%s_rep%03d", row$T, format_sigma_key(row$sigma_firm), row$Replication)
  rng <- accrual_rng_metadata_list(
    paste0("brms_parameter_recovery_T", row$T, "_sigma", format_sigma_key(row$sigma_firm)),
    offset = row$Replication
  )
  data.frame(
    T = row$T,
    sigma_firm = row$sigma_firm,
    Replication = row$Replication,
    Task_Key = task_key,
    fit_path = safe_task_artifact_path(task_root, task_key, "_fit.rds"),
    result_path = safe_task_artifact_path(task_root, task_key, "_result.rds"),
    metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
    task_log_path = safe_task_log_path(file.path(task_root, "logs"), task_key),
    chains = sim_cfg$chains,
    cores = sim_cfg$cores,
    iter = sim_cfg$iter,
    warmup = sim_cfg$warmup,
    adapt_delta = sim_cfg$adapt_delta,
    max_treedepth = sim_cfg$max_treedepth,
    backend = "rstan",
    RNG_Context = rng$RNG_Context,
    RNG_Offset = rng$RNG_Offset,
    Canonical_Seed = rng$Canonical_Seed,
    Effective_Seed = rng$Effective_Seed,
    RNG_Source = rng$RNG_Source,
    Required = TRUE,
    stringsAsFactors = FALSE
  )
})
write_task_manifest(manifest_path, do.call(rbind, rows))
phase_end("si04a", "Plan BRMS parameter recovery")
