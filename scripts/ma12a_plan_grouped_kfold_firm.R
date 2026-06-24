# Script: ma12a_plan_grouped_kfold_firm.R
# Purpose: Plan grouped-firm exact K-fold worker tasks.

source("scripts/ma00_setup.R")
phase_begin("ma12a", "Plan grouped-firm exact K-fold")

kfold_cfg <- accrual_kfold_config("grouped_firm")
tables_dir <- file.path(output_root, "tables")
task_root <- file.path(output_root, "kfold_firm", "task_artifacts")
manifest_path <- file.path(tables_dir, "table_ma12_grouped_kfold_task_manifest.csv")
formula_path <- file.path(tables_dir, "table_named_model_formulas_winsor.csv")
if (!file.exists(formula_path)) stop("[BLOCKER] ma12a requires named model formulas: ", formula_path)
formulas <- read.csv(formula_path, stringsAsFactors = FALSE)
formulas <- formulas[formulas$Model_ID %in% unique(unlist(lapply(c("ex_post", "real_time"), exact_kfold_model_ids_for_space))), , drop = FALSE]
if (!nrow(formulas)) stop("[BLOCKER] ma12a found no grouped K-fold eligible formulas.")

fold_assignment_path <- file.path(tables_dir, "table_ma12_grouped_kfold_fold_assignment.csv")
sample_rows <- unique(formulas[, intersect(c("Target_Space", "Target_Sample"), names(formulas)), drop = FALSE])
sample_parts <- lapply(seq_len(nrow(sample_rows)), function(i) {
  row <- sample_rows[i, , drop = FALSE]
  path <- file.path(input_winsor_root, "tables", row$Target_Sample)
  if (!file.exists(path)) stop("[BLOCKER] ma12a missing target sample for fold planning: ", path)
  df <- read.csv(path, stringsAsFactors = FALSE)
  data.frame(
    Target_Space = row$Target_Space,
    company = df$company,
    year = df$year,
    industry = if ("industry" %in% names(df)) df$industry else NA_character_,
    stringsAsFactors = FALSE
  )
})
combined <- unique(do.call(rbind, sample_parts))
firm_rows <- aggregate(year ~ company, combined, length)
names(firm_rows)[names(firm_rows) == "year"] <- "N_Obs"
industry_mode <- aggregate(industry ~ company, combined, function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
})
firm_rows <- merge(firm_rows, industry_mode, by = "company", all.x = TRUE, sort = FALSE)
set_accrual_seed("grouped_kfold_fold_assignment")
firm_rows$Random_Order <- stats::runif(nrow(firm_rows))
firm_rows <- firm_rows[order(firm_rows$Random_Order, firm_rows$company), , drop = FALSE]
firm_rows$Fold_ID <- rep(seq_len(kfold_cfg$K), length.out = nrow(firm_rows))
fold_assignment <- firm_rows[order(firm_rows$company), c("company", "Fold_ID", "N_Obs", "industry")]
fold_assignment$K <- kfold_cfg$K
fold_assignment$RNG_Context <- "grouped_kfold_fold_assignment"
fold_assignment$Canonical_Seed <- accrual_base_seed()
fold_assignment$Effective_Seed <- accrual_seed_for("grouped_kfold_fold_assignment")
dir.create(dirname(fold_assignment_path), recursive = TRUE, showWarnings = FALSE)
write.csv(fold_assignment, fold_assignment_path, row.names = FALSE)

rows <- list()
idx <- 0L
for (i in seq_len(nrow(formulas))) {
  for (fold_id in seq_len(kfold_cfg$K)) {
    idx <- idx + 1L
    row <- formulas[i, , drop = FALSE]
    task_key <- accrual_task_cache_key("ma12", row$Model_ID, row$Target_Space, row$Heterogeneity_Variant, fold_id)
    rng <- accrual_rng_metadata_list("grouped_kfold_fit", offset = idx)
    rows[[idx]] <- data.frame(
      Task_Key = task_key,
      Fold_ID = fold_id,
      Model_ID = row$Model_ID,
      Model_Name = if ("Model_Name" %in% names(row)) row$Model_Name else NA_character_,
      Target_Space = row$Target_Space,
      Sample_Group = if ("Sample_Group" %in% names(row)) row$Sample_Group else "main_common",
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Target_Sample = if ("Target_Sample" %in% names(row)) row$Target_Sample else NA_character_,
      brms_Formula = row$brms_Formula,
      Fold_Assignment_Path = fold_assignment_path,
      fit_path = safe_task_artifact_path(task_root, task_key, "_fit.rds"),
      prediction_path = safe_task_artifact_path(task_root, task_key, "_prediction.rds"),
      result_path = safe_task_artifact_path(task_root, task_key, "_result.rds"),
      metadata_path = safe_task_artifact_path(task_root, task_key, "_metadata.csv"),
      task_log_path = safe_task_log_path(file.path(task_root, "logs"), task_key),
      chains = kfold_cfg$chains,
      cores = kfold_cfg$cores,
      iter = kfold_cfg$iter,
      warmup = kfold_cfg$warmup,
      adapt_delta = kfold_cfg$adapt_delta,
      max_treedepth = kfold_cfg$max_treedepth,
      refresh = kfold_cfg$refresh,
      backend = kfold_cfg$backend,
      sampler_profile = kfold_cfg$sampler_profile,
      run_mode = kfold_cfg$run_mode,
      config_source = kfold_cfg$config_source,
      RNG_Context = rng$RNG_Context,
      RNG_Offset = rng$RNG_Offset,
      Canonical_Seed = rng$Canonical_Seed,
      Effective_Seed = rng$Effective_Seed,
      Required = TRUE,
      stringsAsFactors = FALSE
    )
  }
}
write_task_manifest(manifest_path, do.call(rbind, rows))
message("ma12a wrote task manifest: ", manifest_path)
phase_end("ma12a", "Plan grouped-firm exact K-fold")
