# Script: ma09a_plan_loo_savepars_refits.R
# Purpose: Plan secondary PSIS/LOO save_pars refit tasks.

source("scripts/ma00_setup.R")
phase_begin("ma09a", "Plan LOO save_pars refits")

tables_dir <- file.path(output_root, "tables")
task_root <- file.path(output_root, "loo_savepars_tasks")
manifest_path <- file.path(tables_dir, "table_ma09_savepars_refit_task_manifest.csv")
diag_path <- file.path(tables_dir, "table_mcmc_diagnostics_gate_winsor.csv")
formula_path <- file.path(tables_dir, "table_named_model_formulas_winsor.csv")
loo_cfg <- accrual_loo_config()

if (!file.exists(diag_path) || !file.exists(formula_path)) {
  stop("[BLOCKER] ma09a requires baseline diagnostics gate and model formula tables.")
}

gate <- read.csv(diag_path, stringsAsFactors = FALSE)
formulas <- read.csv(formula_path, stringsAsFactors = FALSE)
eligible <- gate[gate$diagnostics_status %in% c("PASS", "REVIEW"), , drop = FALSE]
if (!nrow(eligible)) stop("[BLOCKER] ma09a found no LOO-eligible models after diagnostics gate filtering.")

tasks <- merge(
  eligible,
  formulas,
  by.x = c("model_id", "Target_Space", "Heterogeneity_Variant"),
  by.y = c("Model_ID", "Target_Space", "Heterogeneity_Variant"),
  all.x = TRUE,
  suffixes = c("", "_formula")
)

rows <- lapply(seq_len(nrow(tasks)), function(i) {
  row <- tasks[i, , drop = FALSE]
  task_key <- accrual_task_cache_key("ma09", row$model_id, row$Target_Space, row$Heterogeneity_Variant, i)
  rng <- accrual_rng_metadata_list("ma09_loo_savepars_refit", offset = i)
  data.frame(
    Task_Key = task_key,
    Model_ID = row$model_id,
    Model_Name = if ("model_name" %in% names(row)) row$model_name else NA_character_,
    Target_Space = row$Target_Space,
    Sample_Group = if ("Sample_Group" %in% names(row)) row$Sample_Group else "main_common",
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    Target_Sample = if ("Target_Sample" %in% names(row)) row$Target_Sample else NA_character_,
    brms_Formula = if ("brms_Formula" %in% names(row)) row$brms_Formula else NA_character_,
    fit_path = safe_task_artifact_path(task_root, task_key, "_fit_sp.rds"),
    metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
    task_log_path = safe_task_log_path(file.path(task_root, "logs"), task_key),
    chains = loo_cfg$chains,
    cores = loo_cfg$cores,
    iter = loo_cfg$iter,
    warmup = loo_cfg$warmup,
    adapt_delta = loo_cfg$adapt_delta,
    max_treedepth = loo_cfg$max_treedepth,
    backend = "rstan",
    RNG_Context = rng$RNG_Context,
    RNG_Offset = rng$RNG_Offset,
    Canonical_Seed = rng$Canonical_Seed,
    Effective_Seed = rng$Effective_Seed,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Required = TRUE,
    stringsAsFactors = FALSE
  )
})

write_task_manifest(manifest_path, do.call(rbind, rows))
message("ma09a wrote task manifest: ", manifest_path)
phase_end("ma09a", "Plan LOO save_pars refits")
