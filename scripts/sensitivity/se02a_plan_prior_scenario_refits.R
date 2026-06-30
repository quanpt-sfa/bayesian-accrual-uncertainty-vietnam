# Script: se02a_plan_prior_scenario_refits.R
# Purpose: Plan scenario-specific sensitivity prior refit tasks.

source("scripts/ma00_setup.R")
phase_begin("se02a", "Plan prior-scenario refits")

ensure_analysis_dirs()
ensure_sensitivity_dirs()

tables_dir <- file.path(output_root, "sensitivity", "tables")
task_root <- file.path(output_root, "sensitivity", "task_artifacts")
logs_root <- file.path(task_root, "logs")
manifest_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv")
formula_path <- file.path(output_root, "tables", "table_named_model_formulas_winsor.csv")

if (!file.exists(formula_path)) {
  stop("[BLOCKER] se02a requires named model formulas: ", formula_path)
}

formulas <- read.csv(formula_path, stringsAsFactors = FALSE, check.names = FALSE)
scenarios <- selected_sensitivity_scenarios()
sampler <- accrual_sampler_config("sensitivity")

truthy <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  toupper(trimws(as.character(x))) %in% c("TRUE", "1", "YES", "Y")
}

required_formula_cols <- c("Model_ID", "Target_Space", "Heterogeneity_Variant", "Target_Sample", "brms_Formula")
missing_formula_cols <- setdiff(required_formula_cols, names(formulas))
if (length(missing_formula_cols)) {
  stop("[BLOCKER] se02a formula table missing required columns: ", paste(missing_formula_cols, collapse = ", "))
}

if (!"Sample_Group" %in% names(formulas)) formulas$Sample_Group <- "main_common"
if (!"Model_Name" %in% names(formulas)) formulas$Model_Name <- formulas$Model_ID
if (!"Main_Stack_Inclusion" %in% names(formulas)) formulas$Main_Stack_Inclusion <- TRUE

eligible <- formulas[
  formulas$Sample_Group == "main_common" &
    truthy(formulas$Main_Stack_Inclusion) &
    mapply(function(space, id) id %in% main_model_ids_for_space(space), formulas$Target_Space, formulas$Model_ID),
  ,
  drop = FALSE
]
eligible <- eligible[order(eligible$Target_Space, eligible$Model_ID, eligible$Heterogeneity_Variant), , drop = FALSE]
eligible <- eligible[!duplicated(eligible[, c("Model_ID", "Target_Space", "Sample_Group", "Heterogeneity_Variant", "Target_Sample", "brms_Formula")]), , drop = FALSE]

if (!nrow(eligible)) stop("[BLOCKER] se02a found no eligible main-stack formulas for sensitivity refits.")

rows <- list()
idx <- 0L
for (sidx in seq_len(nrow(scenarios))) {
  sc <- scenarios[sidx, , drop = FALSE]
  for (i in seq_len(nrow(eligible))) {
    idx <- idx + 1L
    row <- eligible[i, , drop = FALSE]
    task_key <- accrual_task_cache_key(
      "se02",
      sc$Scenario,
      sc$Prior_Set_ID,
      sc$Likelihood_Family,
      sc$Model_Structure,
      row$Model_ID,
      row$Target_Space,
      row$Sample_Group,
      row$Heterogeneity_Variant
    )
    rng <- accrual_rng_metadata_list(
      paste0("sensitivity_prior_scenario_refit_", sc$Scenario, "_", row$Target_Space, "_", row$Model_ID),
      offset = sidx * 1000L + i
    )
    rows[[idx]] <- data.frame(
      Task_Key = task_key,
      Scenario = sc$Scenario,
      Prior_Set_ID = sc$Prior_Set_ID,
      Likelihood_Family = sc$Likelihood_Family,
      Model_Structure = sc$Model_Structure,
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Target_Sample = row$Target_Sample,
      brms_Formula = row$brms_Formula,
      fit_path = safe_task_artifact_path(task_root, task_key, "_fit.rds"),
      draw_path = safe_task_artifact_path(task_root, task_key, "_draws.rds"),
      metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
      task_log_path = safe_task_log_path(logs_root, task_key),
      chains = sampler$chains,
      cores = sampler$cores,
      iter = sampler$iter,
      warmup = sampler$warmup,
      adapt_delta = sampler$adapt_delta,
      max_treedepth = sampler$max_treedepth,
      refresh = sampler$refresh,
      backend = sampler$backend,
      RNG_Context = rng$RNG_Context,
      RNG_Offset = rng$RNG_Offset,
      Canonical_Seed = rng$Canonical_Seed,
      Effective_Seed = rng$Effective_Seed,
      RNG_Source = rng$RNG_Source,
      Required = TRUE,
      stringsAsFactors = FALSE
    )
  }
}

manifest <- do.call(rbind, rows)
required_cols <- c(
  "Task_Key", "Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure",
  "Model_ID", "Model_Name", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
  "Target_Sample", "brms_Formula", "fit_path", "draw_path", "metadata_path",
  "task_log_path", "chains", "cores", "iter", "warmup", "adapt_delta",
  "max_treedepth", "refresh", "backend", "RNG_Context", "RNG_Offset",
  "Canonical_Seed", "Effective_Seed", "RNG_Source", "Required"
)
missing_cols <- setdiff(required_cols, names(manifest))
if (length(missing_cols)) stop("[BLOCKER] se02a manifest missing columns: ", paste(missing_cols, collapse = ", "))
for (col in c("Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure")) {
  if (any(is.na(manifest[[col]]) | !nzchar(as.character(manifest[[col]])))) {
    stop("[BLOCKER] se02a manifest has missing ", col, ".")
  }
}

scenario_pairs <- unique(manifest[, c("Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure"), drop = FALSE])
expected_pairs <- selected_sensitivity_scenarios()[, c("Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure"), drop = FALSE]
pair_key <- function(x) apply(x, 1L, paste, collapse = "|")
missing_pairs <- setdiff(pair_key(expected_pairs), pair_key(scenario_pairs))
if (length(missing_pairs)) {
  stop("[BLOCKER] se02a manifest does not contain all selected scenario-prior/family/model-structure pairs: ",
       paste(missing_pairs, collapse = ", "))
}
if (toupper(Sys.getenv("ACCRUAL_SENS_SCENARIO", unset = "ALL")) %in% c("", "ALL")) {
  required_scenarios <- sensitivity_scenarios()$Scenario
  missing_all <- setdiff(required_scenarios, unique(manifest$Scenario))
  if (length(missing_all)) {
    stop("[BLOCKER] se02a ACCRUAL_SENS_SCENARIO=ALL manifest missing scenario(s): ", paste(missing_all, collapse = ", "))
  }
}

write_task_manifest(manifest_path, manifest)
message("se02a wrote task manifest: ", manifest_path)
phase_end("se02a", "Plan prior-scenario refits")
