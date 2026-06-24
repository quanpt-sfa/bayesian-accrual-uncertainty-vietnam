# Script: se02a_plan_prior_scenario_refits.R
# Purpose: Plan sensitivity prior-scenario refit tasks.

source("scripts/ma00_setup.R")
phase_begin("se02a", "Plan prior-scenario refits")
tables_dir <- file.path(output_root, "sensitivity", "tables")
task_root <- file.path(output_root, "sensitivity", "task_artifacts")
manifest_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv")
formula_path <- file.path(output_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formula_path)) stop("[BLOCKER] se02a requires named model formulas: ", formula_path)
formulas <- read.csv(formula_path, stringsAsFactors = FALSE)
sampler <- accrual_sampler_config("sensitivity")
scenarios <- sensitivity_scenario_ids
rows <- list()
idx <- 0L
for (scenario in scenarios) {
  for (i in seq_len(nrow(formulas))) {
    idx <- idx + 1L
    row <- formulas[i, , drop = FALSE]
    task_key <- accrual_task_cache_key("se02", scenario, row$Model_ID, row$Target_Space, row$Heterogeneity_Variant)
    rng <- accrual_rng_metadata_list("sensitivity_prior_scenario_refit", offset = idx)
    rows[[idx]] <- data.frame(
      Task_Key = task_key, Scenario = scenario, Model_ID = row$Model_ID,
      Model_Name = if ("Model_Name" %in% names(row)) row$Model_Name else NA_character_,
      Target_Space = row$Target_Space, Sample_Group = if ("Sample_Group" %in% names(row)) row$Sample_Group else "main_common",
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Target_Sample = if ("Target_Sample" %in% names(row)) row$Target_Sample else NA_character_,
      brms_Formula = row$brms_Formula,
      fit_path = safe_task_artifact_path(task_root, task_key, "_fit.rds"),
      draw_path = safe_task_artifact_path(task_root, task_key, "_draws.rds"),
      metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
      task_log_path = safe_task_log_path(file.path(task_root, "logs"), task_key),
      chains = sampler$chains, cores = sampler$cores, iter = sampler$iter, warmup = sampler$warmup,
      adapt_delta = sampler$adapt_delta, max_treedepth = sampler$max_treedepth, backend = "rstan",
      RNG_Context = rng$RNG_Context, RNG_Offset = rng$RNG_Offset, Canonical_Seed = rng$Canonical_Seed,
      Effective_Seed = rng$Effective_Seed, Required = TRUE, stringsAsFactors = FALSE
    )
  }
}
write_task_manifest(manifest_path, do.call(rbind, rows))
message("se02a wrote task manifest: ", manifest_path)
phase_end("se02a", "Plan prior-scenario refits")
