# Script: ma13a_plan_row_level_exact_kfold.R
# Purpose: Plan row-level exact K-fold worker tasks.

source("scripts/ma00_setup.R")
phase_begin("ma13a", "Plan row-level exact K-fold")

kfold_cfg <- accrual_kfold_config("row")
tables_dir <- file.path(output_root, "tables")
task_root <- file.path(output_root, "row_exact_kfold", "task_artifacts")
manifest_path <- file.path(tables_dir, "table_ma13_row_kfold_task_manifest.csv")
formula_path <- file.path(tables_dir, "table_named_model_formulas_winsor.csv")
if (!file.exists(formula_path)) stop("[BLOCKER] ma13a requires named model formulas: ", formula_path)
formulas <- read.csv(formula_path, stringsAsFactors = FALSE)
formulas <- formulas[formulas$Model_ID %in% unique(unlist(lapply(c("ex_post", "real_time"), exact_kfold_model_ids_for_space))), , drop = FALSE]
if (!nrow(formulas)) stop("[BLOCKER] ma13a found no row K-fold eligible formulas.")
fold_assignment_path <- file.path(tables_dir, "table_ma13_row_kfold_fold_assignment.csv")
sample_rows <- unique(formulas[, intersect(c("Target_Space", "Target_Sample"), names(formulas)), drop = FALSE])
fold_parts <- list()
for (i in seq_len(nrow(sample_rows))) {
  row <- sample_rows[i, , drop = FALSE]
  path <- file.path(input_winsor_root, "tables", row$Target_Sample)
  if (!file.exists(path)) stop("[BLOCKER] ma13a missing target sample for fold planning: ", path)
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$row_id <- seq_len(nrow(df))
  set_accrual_seed(
    paste0("row_kfold_fold_assignment_", row$Target_Space),
    offset = match(row$Target_Space, c("ex_post", "real_time"), nomatch = i)
  )
  fold_vec <- integer(nrow(df))
  for (cc in sort(unique(df$company))) {
    idx <- which(df$company == cc)
    idx <- sample(idx, length(idx))
    local_folds <- rep(seq_len(min(kfold_cfg$K, length(idx))), length.out = length(idx))
    fold_vec[idx] <- sample(local_folds, length(local_folds))
  }
  fold_parts[[i]] <- data.frame(
    Target_Space = row$Target_Space,
    Target_Sample = row$Target_Sample,
    row_id = df$row_id,
    company = df$company,
    year = df$year,
    Fold_ID = fold_vec,
    K = kfold_cfg$K,
    RNG_Context = paste0("row_kfold_fold_assignment_", row$Target_Space),
    Canonical_Seed = accrual_base_seed(),
    Effective_Seed = accrual_seed_for(
      paste0("row_kfold_fold_assignment_", row$Target_Space),
      offset = match(row$Target_Space, c("ex_post", "real_time"), nomatch = i)
    ),
    stringsAsFactors = FALSE
  )
}
write.csv(do.call(rbind, fold_parts), fold_assignment_path, row.names = FALSE)
rows <- list()
idx <- 0L
for (i in seq_len(nrow(formulas))) {
  for (fold_id in seq_len(kfold_cfg$K)) {
    idx <- idx + 1L
    row <- formulas[i, , drop = FALSE]
    task_key <- accrual_task_cache_key("ma13", row$Model_ID, row$Target_Space, row$Heterogeneity_Variant, fold_id)
    rng <- accrual_rng_metadata_list("row_kfold_fit", offset = idx)
    rows[[idx]] <- data.frame(
      Task_Key = task_key, Fold_ID = fold_id, Model_ID = row$Model_ID,
      Model_Name = if ("Model_Name" %in% names(row)) row$Model_Name else NA_character_,
      Target_Space = row$Target_Space, Sample_Group = if ("Sample_Group" %in% names(row)) row$Sample_Group else "main_common",
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Target_Sample = if ("Target_Sample" %in% names(row)) row$Target_Sample else NA_character_,
      brms_Formula = row$brms_Formula,
      Fold_Assignment_Path = fold_assignment_path,
      fit_path = safe_task_artifact_path(task_root, task_key, "_fit.rds"),
      prediction_path = safe_task_artifact_path(task_root, task_key, "_prediction.rds"),
      result_path = safe_task_artifact_path(task_root, task_key, "_result.rds"),
      metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
      task_log_path = safe_task_log_path(file.path(task_root, "logs"), task_key),
      chains = kfold_cfg$chains, cores = kfold_cfg$cores, iter = kfold_cfg$iter, warmup = kfold_cfg$warmup,
      adapt_delta = kfold_cfg$adapt_delta, max_treedepth = kfold_cfg$max_treedepth, refresh = kfold_cfg$refresh,
      backend = kfold_cfg$backend, sampler_profile = kfold_cfg$sampler_profile,
      run_mode = kfold_cfg$run_mode, config_source = kfold_cfg$config_source,
      RNG_Context = rng$RNG_Context, RNG_Offset = rng$RNG_Offset, Canonical_Seed = rng$Canonical_Seed,
      Effective_Seed = rng$Effective_Seed, Required = TRUE, stringsAsFactors = FALSE
    )
  }
}
write_task_manifest(manifest_path, do.call(rbind, rows))
message("ma13a wrote task manifest: ", manifest_path)
phase_end("ma13a", "Plan row-level exact K-fold")
