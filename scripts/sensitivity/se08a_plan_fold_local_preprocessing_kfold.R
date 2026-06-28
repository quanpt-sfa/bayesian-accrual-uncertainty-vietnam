# Script: se08a_plan_fold_local_preprocessing_kfold.R
# Purpose: Plan fold-local preprocessing exact K-fold sensitivity tasks.

source("scripts/ma00_setup.R")
phase_begin("se08a", "Plan fold-local preprocessing exact K-fold sensitivity")

script_name <- "scripts/sensitivity/se08a_plan_fold_local_preprocessing_kfold.R"
script_version <- "fold-local-preprocessing-v1"

se08_root <- file.path(output_root, "sensitivity", "fold_local_preprocessing")
se08_dirs <- file.path(se08_root, c("", "tables", "models", "draws", "logs", "cache", "task_logs", "diagnostics"))
for (d in se08_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
tables_dir <- file.path(se08_root, "tables")
models_dir <- file.path(se08_root, "models")
cache_dir <- file.path(se08_root, "cache")
task_logs_dir <- file.path(se08_root, "task_logs")

resolve_completed_run_root <- function(kind) {
  pin <- file.path(output_root, if (identical(kind, "grouped_firm")) "kfold_firm" else "row_exact_kfold", "LATEST_COMPLETED_RUN.txt")
  if (!file.exists(pin)) {
    stop("[BLOCKER] se08 requires the completed ", kind, " exact K-fold run pin and will not generate new folds. Missing: ", pin)
  }
  run_root <- read_single_line_no_bom(pin, paste0("se08 completed ", kind, " exact K-fold run pin"))
  if (!dir.exists(run_root)) {
    stop(
      "[BLOCKER] se08 completed ", kind,
      " exact K-fold run pin points to a missing run root after BOM cleanup. Pin: ",
      pin,
      "; cleaned value: ",
      run_root
    )
  }
  normalizePath(run_root, winslash = "/", mustWork = TRUE)
}

resolve_fold_assignment_path <- function(run_root, kind) {
  candidates <- if (identical(kind, "grouped_firm")) {
    c(
      file.path(run_root, "tables", "table_ma12_grouped_kfold_fold_assignment.csv"),
      file.path(run_root, "tables", "table_winsor_firm_fold_assignment.csv")
    )
  } else {
    c(file.path(run_root, "tables", "table_ma13_row_kfold_fold_assignment.csv"))
  }
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit) || !nzchar(hit)) {
    manifest_name <- if (identical(kind, "grouped_firm")) "table_ma12_grouped_kfold_task_manifest.csv" else "table_ma13_row_kfold_task_manifest.csv"
    manifest_path <- file.path(run_root, "tables", manifest_name)
    if (file.exists(manifest_path)) {
      manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
      if ("Fold_Assignment_Path" %in% names(manifest)) {
        hits <- unique(trimws(as.character(manifest$Fold_Assignment_Path)))
        hits <- hits[nzchar(hits) & file.exists(hits)]
        if (length(hits) == 1L) return(normalizePath(hits, winslash = "/", mustWork = TRUE))
      }
    }
    stop("[BLOCKER] se08 cannot locate the completed ", kind, " fold assignment under: ", run_root)
  }
  normalizePath(hit, winslash = "/", mustWork = TRUE)
}

read_fold_ids <- function(path, target_space = NULL, target_sample = NULL) {
  x <- read.csv(path, stringsAsFactors = FALSE)
  if (!"Fold_ID" %in% names(x)) stop("[BLOCKER] Fold assignment lacks Fold_ID: ", path)
  if (!is.null(target_space) && "Target_Space" %in% names(x)) x <- x[x$Target_Space == target_space, , drop = FALSE]
  if (!is.null(target_sample) && "Target_Sample" %in% names(x)) x <- x[x$Target_Sample == target_sample, , drop = FALSE]
  ids <- sort(unique(as.integer(x$Fold_ID)))
  ids <- ids[!is.na(ids)]
  if (!length(ids)) stop("[BLOCKER] se08 found no fold IDs in: ", path)
  ids
}

sample_file_for_space <- function(target_space) {
  if (identical(target_space, "ex_post")) "final_common_ex_post_sample.csv" else "final_common_realtime_sample.csv"
}

formula_path <- file.path(output_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formula_path)) formula_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formula_path)) stop("[BLOCKER] se08 requires model formulas from the primary winsor pipeline: ", formula_path)
formulas <- read.csv(formula_path, stringsAsFactors = FALSE)

primary_model_ids <- list(
  ex_post = sprintf("M%02d", 1:7),
  real_time = c("M01", "M02", "M03", "M07", "M09")
)
primary_variants <- c("Pooled (Industry + Year FE)", "Firm RE (Random Intercept + Year FE)")
formulas <- formulas[
  formulas$Target_Space %in% names(primary_model_ids) &
    formulas$Heterogeneity_Variant %in% primary_variants,
  ,
  drop = FALSE
]
formulas <- do.call(rbind, lapply(names(primary_model_ids), function(space) {
  formulas[formulas$Target_Space == space & formulas$Model_ID %in% primary_model_ids[[space]], , drop = FALSE]
}))
if (!nrow(formulas)) stop("[BLOCKER] se08 found no full primary model-space formulas.")
if (any(formulas$Model_ID %in% c("M08", "M10"))) {
  stop("[BLOCKER] se08 full primary sensitivity must exclude secondary robustness models M08 and M10.")
}

grouped_run_root <- resolve_completed_run_root("grouped_firm")
row_run_root <- resolve_completed_run_root("row")
grouped_fold_assignment_path <- resolve_fold_assignment_path(grouped_run_root, "grouped_firm")
row_fold_assignment_path <- resolve_fold_assignment_path(row_run_root, "row")

global_cutoff_path <- file.path(input_winsor_root, "tables", "table_winsor_cutoffs.csv")
rows <- list()
idx <- 0L
for (scheme in c("grouped_firm_kfold", "row_exact_kfold")) {
  scheme_fold_path <- if (identical(scheme, "grouped_firm_kfold")) grouped_fold_assignment_path else row_fold_assignment_path
  scheme_run_root <- if (identical(scheme, "grouped_firm_kfold")) grouped_run_root else row_run_root
  for (i in seq_len(nrow(formulas))) {
    row <- formulas[i, , drop = FALSE]
    source_sample <- sample_file_for_space(row$Target_Space)
    source_sample_path <- file.path(baseline_root, "tables", source_sample)
    if (!file.exists(source_sample_path)) {
      stop("[BLOCKER] se08 must use un-winsorized baseline samples, but this file is missing: ", source_sample_path)
    }
    fold_ids <- if (identical(scheme, "row_exact_kfold")) {
      read_fold_ids(scheme_fold_path, target_space = row$Target_Space, target_sample = row$Target_Sample)
    } else {
      read_fold_ids(scheme_fold_path)
    }
    cfg <- if (identical(scheme, "grouped_firm_kfold")) accrual_kfold_config("grouped_firm") else accrual_kfold_config("row")
    for (fold_id in fold_ids) {
      idx <- idx + 1L
      task_key <- accrual_task_cache_key("se08", scheme, row$Target_Space, row$Model_ID, row$Heterogeneity_Variant, fold_id)
      rng <- accrual_rng_metadata_list("se08_fold_local_preprocessing_fit", offset = idx)
      rows[[idx]] <- data.frame(
        Task_Key = task_key,
        Sensitivity_Root = se08_root,
        Validation_Scheme = scheme,
        Primary_Run_Root = scheme_run_root,
        Fold_Assignment_Path = scheme_fold_path,
        K = length(fold_ids),
        Fold_ID = fold_id,
        Target_Space = row$Target_Space,
        Sample_Group = if ("Sample_Group" %in% names(row)) row$Sample_Group else "main_common",
        Model_ID = row$Model_ID,
        Model_Name = if ("Model_Name" %in% names(row)) row$Model_Name else NA_character_,
        Heterogeneity_Variant = row$Heterogeneity_Variant,
        Source_Target_Sample = source_sample,
        Source_Sample_Path = source_sample_path,
        Primary_Winsor_Target_Sample = if ("Target_Sample" %in% names(row)) row$Target_Sample else NA_character_,
        brms_Formula = row$brms_Formula,
        fit_path = safe_task_artifact_path(models_dir, task_key, "_fit.rds"),
        result_path = safe_task_artifact_path(cache_dir, task_key, "_result.rds"),
        metadata_path = safe_task_artifact_path(cache_dir, task_key, "_metadata.csv"),
        task_log_path = safe_task_log_path(task_logs_dir, task_key),
        Global_Winsor_Cutoff_Path = global_cutoff_path,
        chains = cfg$chains,
        cores = cfg$cores,
        iter = cfg$iter,
        warmup = cfg$warmup,
        adapt_delta = cfg$adapt_delta,
        max_treedepth = cfg$max_treedepth,
        refresh = cfg$refresh,
        backend = cfg$backend,
        sampler_profile = cfg$sampler_profile,
        run_mode = cfg$run_mode,
        config_source = cfg$config_source,
        RNG_Context = rng$RNG_Context,
        RNG_Offset = rng$RNG_Offset,
        Canonical_Seed = rng$Canonical_Seed,
        Effective_Seed = rng$Effective_Seed,
        Required = TRUE,
        Script_Name = script_name,
        Script_Version = script_version,
        stringsAsFactors = FALSE
      )
    }
  }
}
task_manifest <- do.call(rbind, rows)
manifest_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_status.csv")
write_task_manifest(manifest_path, task_manifest)
write_task_status(status_path, data.frame(
  Task_Key = task_manifest$Task_Key,
  status = "PENDING",
  reason = NA_character_,
  Required = task_manifest$Required,
  result_path = task_manifest$result_path,
  stringsAsFactors = FALSE
))
message("se08a wrote fold-local preprocessing task manifest: ", manifest_path)
phase_end("se08a", "Plan fold-local preprocessing exact K-fold sensitivity")
