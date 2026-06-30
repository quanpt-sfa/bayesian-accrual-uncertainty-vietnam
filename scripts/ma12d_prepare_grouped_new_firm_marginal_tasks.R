# Script: ma12d_prepare_grouped_new_firm_marginal_tasks.R
# Purpose: Prepare MA12D v1.1 grouped-firm marginal new-firm rescoring tasks.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma12d", "Prepare grouped-firm marginal new-firm rescoring tasks")

script_name <- "scripts/ma12d_prepare_grouped_new_firm_marginal_tasks.R"
script_version <- "marginal-new-firm-rescore-v1.1-prepare"
script_start_time <- Sys.time()

ma12d_env <- list(
  new_firm_draws = env_int("ACCRUAL_MA12D_NEW_FIRM_DRAWS", 20L, min = 1L),
  max_posterior_draws = env_int("ACCRUAL_MA12D_MAX_POSTERIOR_DRAWS", 2000L, min = 1L),
  seed = env_int("ACCRUAL_MA12D_SEED", accrual_seed_for("ma12d_grouped_new_firm_marginal"), min = 0L),
  force_recompute = env_flag("ACCRUAL_MA12D_FORCE_RECOMPUTE", "FALSE"),
  material_weight_change = env_num("ACCRUAL_MA12D_WEIGHT_CHANGE_MATERIAL", 0.05, min = 0),
  allow_restack_excluded = env_flag("ACCRUAL_MA12D_ALLOW_RESTACK_EXCLUDED", "FALSE"),
  source_kfold_run_root = trimws(env_value("ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT", "")),
  source_row_kfold_run_root = trimws(env_value("ACCRUAL_MA12D_SOURCE_ROW_KFOLD_RUN_ROOT", "")),
  output_run_root = trimws(env_value("ACCRUAL_MA12D_OUTPUT_RUN_ROOT", ""))
)

nonempty <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
}

single_line <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  x <- trimws(readLines(path, warn = FALSE))
  x <- x[nzchar(x)]
  if (!length(x)) NA_character_ else x[[1]]
}

file_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

required_columns <- function(df, cols, context) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop("[BLOCKER] ", context, " missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

firm_re_indicator <- function(variant) {
  grepl("Firm RE|Random Intercept|firm_RE|firmre", as.character(variant), ignore.case = TRUE)
}

normalize_task_paths <- function(tasks) {
  for (nm in c("fit_path", "result_path", "prediction_path", "Fold_Assignment_Path")) {
    if (nm %in% names(tasks)) tasks[[nm]] <- normalizePath(tasks[[nm]], winslash = "/", mustWork = FALSE)
  }
  if (!"result_path" %in% names(tasks) && "prediction_path" %in% names(tasks)) tasks$result_path <- tasks$prediction_path
  tasks
}

resolve_source_kfold_run_root <- function() {
  explicit <- ma12d_env$source_kfold_run_root
  if (nonempty(explicit)) {
    root <- normalizePath(explicit, winslash = "/", mustWork = FALSE)
    return(list(root = root, resolution = "ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT"))
  }

  completed_pin <- file.path(output_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt")
  pinned <- single_line(completed_pin)
  if (nonempty(pinned)) {
    root <- normalizePath(pinned, winslash = "/", mustWork = FALSE)
    return(list(root = root, resolution = completed_pin))
  }

  stop(
    "[BLOCKER] MA12D could not resolve a completed grouped K-fold source run. ",
    "Set ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT or create ",
    completed_pin, "."
  )
}

require_source_run_contract <- function(source) {
  root <- source$root
  if (!dir.exists(root)) {
    stop("[BLOCKER] MA12D source grouped K-fold run root does not exist: ", root)
  }
  manifest_path <- file.path(root, "tables", "table_ma12_grouped_kfold_task_manifest.csv")
  status_path <- file.path(root, "tables", "table_ma12_grouped_kfold_task_status.csv")
  obs_path <- file.path(root, "tables", "table_winsor_kfold_observation_scores.csv")
  model_path <- file.path(root, "tables", "table_winsor_kfold_model_scores.csv")
  weights_ep_path <- file.path(root, "tables", "table_winsor_kfold_weights_ex_post.csv")
  weights_rt_path <- file.path(root, "tables", "table_winsor_kfold_weights_no_lookahead.csv")
  fold_assignment_path <- file.path(root, "tables", "table_ma12_grouped_kfold_fold_assignment.csv")
  missing <- c(manifest_path, status_path, obs_path, model_path)
  missing <- missing[!file.exists(missing)]
  if (length(missing)) {
    stop("[BLOCKER] MA12D source grouped K-fold run root lacks required MA12C artifacts: ",
         paste(missing, collapse = "; "))
  }
  list(
    root = root,
    resolution = source$resolution,
    manifest_path = manifest_path,
    status_path = status_path,
    model_scores_path = model_path,
    observation_scores_path = obs_path,
    fold_assignment_path = fold_assignment_path,
    weights_ex_post_path = weights_ep_path,
    weights_no_lookahead_path = weights_rt_path
  )
}

make_output_run_root <- function() {
  if (nonempty(ma12d_env$output_run_root)) {
    return(normalizePath(ma12d_env$output_run_root, winslash = "/", mustWork = FALSE))
  }
  run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file.path(output_root, "grouped_new_firm_marginal", paste0("ma12d_", run_id))
}

write_dual_csv <- function(x, file_name, tables_dir, compat_tables_dir = file.path(output_root, "tables")) {
  write_csv_safely(x, file.path(tables_dir, file_name), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safely(x, file.path(compat_tables_dir, file_name), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(file.path(tables_dir, file_name))
}

canonicalize_columns <- function(df, canonical, context) {
  nms <- names(df)
  lower <- tolower(nms)
  for (col in canonical) {
    if (col %in% nms) next
    idx <- which(lower == tolower(col))
    if (length(idx) == 1L) names(df)[idx] <- col
  }
  if ("Reliability_Flag" %in% names(df) && !"reliability_flag" %in% names(df)) {
    names(df)[names(df) == "Reliability_Flag"] <- "reliability_flag"
  }
  if ("Included_In_Stack" %in% names(df) && !"included_in_stack" %in% names(df)) {
    names(df)[names(df) == "Included_In_Stack"] <- "included_in_stack"
  }
  df
}

as_bool <- function(x) {
  if (is.logical(x)) return(x)
  as.character(x) %in% c("TRUE", "true", "True", "1", "yes", "YES")
}

inherit_source_model_gate <- function(source_contract, manifest) {
  source_scores <- read.csv(source_contract$model_scores_path, stringsAsFactors = FALSE, check.names = FALSE)
  source_scores <- canonicalize_columns(
    source_scores,
    c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant",
      "reliability_flag", "included_in_stack", "N_Folds_Completed", "N_Test_Obs_Total",
      "elpd_kfold", "mean_lpd_obs", "RMSE", "MAE"),
    "MA12D source model scores"
  )

  gate_keys <- c("Target_Space", "Sample_Group", "Model_ID", "Heterogeneity_Variant")
  required_columns(source_scores, gate_keys, "MA12D source model scores")
  if (!"included_in_stack" %in% names(source_scores)) {
    if (!all(c("reliability_flag", "N_Folds_Completed") %in% names(source_scores))) {
      stop("[BLOCKER] MA12D cannot inherit MA12C inclusion gate from source model scores.")
    }
    K <- suppressWarnings(as.integer(manifest$K[1]))
    partial_run <- if ("Partial_Run" %in% names(manifest)) isTRUE(as_bool(manifest$Partial_Run[1])) else FALSE
    source_scores$included_in_stack <- source_scores$reliability_flag %in% c("OK", "CAUTION") &
      ifelse(partial_run, suppressWarnings(as.integer(source_scores$N_Folds_Completed)) > 0,
             suppressWarnings(as.integer(source_scores$N_Folds_Completed)) == K)
  }
  required_columns(
    source_scores,
    c(gate_keys, "reliability_flag", "included_in_stack", "N_Folds_Completed",
      "N_Test_Obs_Total", "elpd_kfold", "mean_lpd_obs", "RMSE", "MAE"),
    "MA12D source model scores"
  )
  if (anyDuplicated(source_scores[, gate_keys, drop = FALSE])) {
    stop("[BLOCKER] MA12D source model scores contain duplicate source-gate keys.")
  }

  gate <- source_scores[, c(gate_keys, "reliability_flag", "included_in_stack",
                            "N_Folds_Completed", "N_Test_Obs_Total", "elpd_kfold",
                            "mean_lpd_obs", "RMSE", "MAE"), drop = FALSE]
  names(gate)[match(
    c("reliability_flag", "included_in_stack", "N_Folds_Completed", "N_Test_Obs_Total",
      "elpd_kfold", "mean_lpd_obs", "RMSE", "MAE"),
    names(gate)
  )] <- c("Source_Reliability_Flag", "Source_Included_In_Stack",
          "Source_N_Folds_Completed", "Source_N_Test_Obs_Total",
          "Source_ELPD_KFold", "Source_Mean_LPD_Obs", "Source_RMSE", "Source_MAE")

  out <- left_join(manifest, gate, by = gate_keys)
  if (any(is.na(out$Source_Reliability_Flag)) || any(is.na(out$Source_Included_In_Stack))) {
    unmatched <- out$Task_Key[is.na(out$Source_Reliability_Flag) | is.na(out$Source_Included_In_Stack)]
    stop("[BLOCKER] MA12D task(s) cannot be matched to source MA12C model scores: ",
         paste(utils::head(unmatched, 20), collapse = "; "))
  }
  out$Source_Included_In_Stack <- as_bool(out$Source_Included_In_Stack)
  out$MA12D_Primary_Stack_Eligible <- if (isTRUE(ma12d_env$allow_restack_excluded)) TRUE else out$Source_Included_In_Stack
  out$Source_Model_Scores_Path <- source_contract$model_scores_path
  out
}

source_contract <- require_source_run_contract(resolve_source_kfold_run_root())
output_run_root <- make_output_run_root()
tables_dir <- file.path(output_run_root, "tables")
logs_dir <- file.path(output_run_root, "logs")
task_output_dir <- file.path(output_run_root, "task_results")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(task_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "grouped_new_firm_marginal"), recursive = TRUE, showWarnings = FALSE)
writeLines(output_run_root, file.path(output_root, "grouped_new_firm_marginal", "LATEST_RUN.txt"))

manifest <- read.csv(source_contract$manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
status <- read.csv(source_contract$status_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns(manifest, c("Task_Key", "Target_Sample", "Fold_Assignment_Path", "Fold_ID", "fit_path",
                             "Model_ID", "Model_Name", "Heterogeneity_Variant", "Target_Space",
                             "Sample_Group"), "MA12D source task manifest")
required_columns(status, c("Task_Key", "status"), "MA12D source task status")
if (!"Required" %in% names(status) && "Required" %in% names(manifest)) {
  status <- merge(status, manifest[, c("Task_Key", "Required"), drop = FALSE], by = "Task_Key", all.x = TRUE, sort = FALSE)
}
if (!"Required" %in% names(status)) status$Required <- TRUE
accrual_task_status_blocker(status, required_col = "Required", context = "ma12d source grouped K-fold tasks")
required_source <- as_bool(status$Required)
not_success <- required_source & !status$status %in% "SUCCESS"
if (any(not_success, na.rm = TRUE)) {
  stop("[BLOCKER] MA12D requires all required MA12 grouped K-fold tasks to be completed successfully. Non-success tasks: ",
       paste(status$Task_Key[not_success], collapse = "; "))
}

manifest <- normalize_task_paths(manifest)
manifest$Requires_Marginal_New_Firm <- firm_re_indicator(manifest$Heterogeneity_Variant)
manifest$Source_KFold_Run_Root <- source_contract$root
manifest$Source_KFold_Run_Root_Resolution <- source_contract$resolution
manifest$Source_KFold_Manifest_Path <- source_contract$manifest_path
manifest$Source_KFold_Status_Path <- source_contract$status_path
manifest$Source_KFold_Model_Scores_Path <- source_contract$model_scores_path
manifest$Source_KFold_Observation_Scores_Path <- source_contract$observation_scores_path
manifest$Source_KFold_Fold_Assignment_Path <- manifest$Fold_Assignment_Path
manifest$Output_Run_Root <- output_run_root
manifest$MA12D_Result_Path <- file.path(task_output_dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", manifest$Task_Key), "_ma12d_result.rds"))
manifest$N_New_Firm_Draws <- ma12d_env$new_firm_draws
manifest$Max_Posterior_Draws <- ma12d_env$max_posterior_draws
manifest$MA12D_Seed <- ma12d_env$seed
manifest$Prediction_Rule <- ifelse(manifest$Requires_Marginal_New_Firm,
                                   "grouped_firm_marginal_new_firm_integrated",
                                   "grouped_firm_log_lik_re_formula_NA_population_level_copied")
manifest$Source_Model_Scores_MD5 <- file_md5(source_contract$model_scores_path)
manifest$Source_Observation_Scores_MD5 <- file_md5(source_contract$observation_scores_path)
manifest$Restack_Excluded_Allowed <- ma12d_env$allow_restack_excluded
manifest <- inherit_source_model_gate(source_contract, manifest)

missing_fits <- manifest$fit_path[!file.exists(manifest$fit_path)]
missing_folds <- manifest$Fold_Assignment_Path[!file.exists(manifest$Fold_Assignment_Path)]
if (length(missing_fits)) {
  message("[NOTICE] MA12D prepare found missing fitted objects; collector will emit BLOCKED_MISSING_FITS if unresolved.")
}
if (length(missing_folds)) {
  stop("[BLOCKER] MA12D found missing grouped fold assignment(s): ",
       paste(utils::head(unique(missing_folds), 10), collapse = "; "))
}

write_dual_csv(manifest, "table_ma12d_grouped_new_firm_marginal_task_manifest.csv", tables_dir)
initial_status <- data.frame(
  Task_Key = manifest$Task_Key,
  status = "PENDING",
  reason = NA_character_,
  Required = if ("Required" %in% names(manifest)) manifest$Required else TRUE,
  MA12D_Primary_Stack_Eligible = manifest$MA12D_Primary_Stack_Eligible,
  Source_Reliability_Flag = manifest$Source_Reliability_Flag,
  Source_Included_In_Stack = manifest$Source_Included_In_Stack,
  fit_path = manifest$fit_path,
  result_path = manifest$MA12D_Result_Path,
  runtime_seconds = NA_real_,
  stringsAsFactors = FALSE
)
write_dual_csv(initial_status, "table_ma12d_grouped_new_firm_marginal_task_status.csv", tables_dir)

run_manifest <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = as.character(script_start_time),
  End_Time = as.character(Sys.time()),
  Runtime_Seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  Source_KFold_Run_Root = source_contract$root,
  Source_KFold_Run_Root_Resolution = source_contract$resolution,
  Source_KFold_Manifest_Path = source_contract$manifest_path,
  Source_KFold_Status_Path = source_contract$status_path,
  Source_KFold_Model_Scores_Path = source_contract$model_scores_path,
  Source_KFold_Observation_Scores_Path = source_contract$observation_scores_path,
  Source_KFold_Fold_Assignment_Path = paste(unique(manifest$Fold_Assignment_Path), collapse = ";"),
  Output_Run_Root = output_run_root,
  Source_Row_KFold_Run_Root_Reserved = ma12d_env$source_row_kfold_run_root,
  N_New_Firm_Draws = ma12d_env$new_firm_draws,
  Max_Posterior_Draws = ma12d_env$max_posterior_draws,
  MA12D_Seed = ma12d_env$seed,
  Force_Recompute = ma12d_env$force_recompute,
  Restack_Excluded_Allowed = ma12d_env$allow_restack_excluded,
  Weight_Change_Material_Threshold = ma12d_env$material_weight_change,
  Source_Model_Scores_MD5 = file_md5(source_contract$model_scores_path),
  Source_Observation_Scores_MD5 = file_md5(source_contract$observation_scores_path),
  Refits_Performed = FALSE,
  stringsAsFactors = FALSE
)
write_csv_safely(run_manifest, file.path(logs_dir, "ma12d_prepare_run_config_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\n[SUCCESS] MA12D prepare completed.\n")
cat("Output run root:", output_run_root, "\n")
cat("Task manifest:", file.path(tables_dir, "table_ma12d_grouped_new_firm_marginal_task_manifest.csv"), "\n")
phase_end("ma12d", "Prepare grouped-firm marginal new-firm rescoring tasks")
