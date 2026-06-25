# Script: ma13c_collect_row_level_exact_kfold_scores.R
# Purpose: Collect row-level exact K-fold scores and ma14-compatible run outputs.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma13c", "Collect row-level exact K-fold scores")

script_name <- "scripts/ma13c_collect_row_level_exact_kfold_scores.R"
script_version <- "split-worker-v1"
script_start_time <- Sys.time()

compat_tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(compat_tables_dir, "table_ma13_row_kfold_task_manifest.csv")
status_path <- file.path(compat_tables_dir, "table_ma13_row_kfold_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) {
  stop("[BLOCKER] ma13c requires ma13a manifest and ma13b task status.")
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "ma13c row K-fold collect")
if (!"result_path" %in% names(manifest)) manifest$result_path <- manifest$prediction_path
if (!"Row_KFold_Root" %in% names(manifest)) stop("[BLOCKER] ma13c manifest lacks Row_KFold_Root.")

run_root <- manifest$Row_KFold_Root[1]
base_root <- dirname(run_root)
tables_dir <- file.path(run_root, "tables")
logs_dir <- file.path(run_root, "logs")
dir.create(base_root, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

results <- lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] ma13c missing row K-fold task result: ", path)
  readRDS(path)
})
fold_diagnostics <- bind_rows(lapply(results, `[[`, "fold_diag"))
obs_scores <- bind_rows(lapply(results, `[[`, "obs_scores"))

write_dual_csv <- function(x, file_name) {
  run_path <- file.path(tables_dir, file_name)
  compat_path <- file.path(compat_tables_dir, file_name)
  write_csv_safely(x, run_path, row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safely(x, compat_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(run_path)
}

write_dual_csv(fold_diagnostics, "table_winsor_row_exact_kfold_refit_diagnostics.csv")
write_dual_csv(obs_scores, "table_winsor_row_exact_kfold_observation_scores.csv")

K <- as.integer(manifest$K[1])
run_mode <- as.character(manifest$run_mode[1])
partial_run <- isTRUE(as.logical(manifest$Partial_Run[1]))
included <- obs_scores[obs_scores$primary_row_target_inclusion %in% c(TRUE, "TRUE", 1L), , drop = FALSE]

model_scores <- included %>%
  group_by(target_space, sample_group, model_id, model_name, heterogeneity_variant) %>%
  summarise(
    n_obs_scored = n(),
    elpd_exact_row_kfold = sum(log_predictive_density, na.rm = TRUE),
    mean_lpd = mean(log_predictive_density, na.rm = TRUE),
    sd_lpd = stats::sd(log_predictive_density, na.rm = TRUE),
    n_new_company_excluded_from_primary = sum(new_company_in_row_fold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    fold_diagnostics %>%
      group_by(Target_Space, Sample_Group, Model_ID, Model_Name, Heterogeneity_Variant) %>%
      summarise(
        n_folds_attempted = n(),
        n_folds_completed = sum(Completed, na.rm = TRUE),
        max_rhat_max = suppressWarnings(max(Max_Rhat, na.rm = TRUE)),
        min_ess_bulk_min = suppressWarnings(min(Min_ESS_Bulk, na.rm = TRUE)),
        min_ess_tail_min = suppressWarnings(min(Min_ESS_Tail, na.rm = TRUE)),
        ess_warning_any = any(ESS_Warning %in% c(TRUE, "TRUE", 1L), na.rm = TRUE),
        divergences_total = sum(Divergences, na.rm = TRUE),
        treedepth_warnings_total = sum(Treedepth_Warnings, na.rm = TRUE),
        n_test_obs_no_same_firm_history = sum(N_Test_Obs_No_Same_Firm_History, na.rm = TRUE),
        any_new_company_in_row_fold = any(Any_New_Company_In_Row_Fold, na.rm = TRUE),
        failure_reason = paste(na.omit(unique(Failure_Reason)), collapse = " | "),
        .groups = "drop"
      ),
    by = c("target_space" = "Target_Space", "sample_group" = "Sample_Group",
           "model_id" = "Model_ID", "model_name" = "Model_Name",
           "heterogeneity_variant" = "Heterogeneity_Variant")
  ) %>%
  mutate(
    max_rhat_max = ifelse(is.infinite(max_rhat_max), NA_real_, max_rhat_max),
    min_ess_bulk_min = ifelse(is.infinite(min_ess_bulk_min), NA_real_, min_ess_bulk_min),
    min_ess_tail_min = ifelse(is.infinite(min_ess_tail_min), NA_real_, min_ess_tail_min),
    reliability_flag = case_when(
      n_folds_completed == 0 ~ "FAILED",
      !partial_run & n_folds_completed < K ~ "LOW_RELIABILITY",
      divergences_total > 0 | treedepth_warnings_total > 0 ~ "LOW_RELIABILITY",
      is.na(max_rhat_max) | is.na(min_ess_bulk_min) | is.na(min_ess_tail_min) ~ "LOW_RELIABILITY",
      max_rhat_max <= 1.01 & min_ess_bulk_min >= 400 & min_ess_tail_min >= 400 ~ "OK",
      max_rhat_max <= 1.05 & min_ess_bulk_min >= 100 & min_ess_tail_min >= 100 ~ "CAUTION",
      TRUE ~ "LOW_RELIABILITY"
    ),
    included_in_stack = reliability_flag %in% c("OK", "CAUTION") &
      ifelse(partial_run, n_folds_completed > 0, n_folds_completed == K),
    refit_type = "exact_refit",
    validation_unit = "row_level",
    primary_row_target_excludes_new_company_rows = TRUE
  )
write_dual_csv(model_scores, "table_winsor_row_exact_kfold_model_scores.csv")

build_row_weights <- function(target_space) {
  included_models <- model_scores %>%
    filter(target_space == !!target_space, sample_group == "main_common", included_in_stack == TRUE) %>%
    arrange(model_id, heterogeneity_variant)
  if (nrow(included_models) == 0 || nrow(included) == 0) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included_models))) {
    row <- included_models[i, ]
    key <- model_key_sampled(row$model_id, row$target_space, row$sample_group, row$heterogeneity_variant, "_row_exact_kfold")
    one <- included %>%
      filter(target_space == !!target_space, model_id == row$model_id,
             heterogeneity_variant == row$heterogeneity_variant) %>%
      arrange(observation_id)
    if (nrow(one) != row$n_obs_scored) next
    score_list[[key]] <- one$log_predictive_density
    meta_keys <- c(meta_keys, key)
  }
  if (!length(score_list)) return(data.frame())
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) {
    stop("[BLOCKER] Row-level exact K-fold score vectors have unequal lengths for ", target_space)
  }
  lpd_matrix <- do.call(cbind, score_list)
  colnames(lpd_matrix) <- names(score_list)
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  meta_idx <- match(names(weights), meta_keys)
  included_models[meta_idx, ] %>%
    mutate(
      model_key_row_exact_kfold = names(weights),
      weight_row_exact_kfold = as.numeric(weights),
      singleton_elpd = as.numeric(colSums(lpd_matrix)[names(weights)])
    ) %>%
    arrange(desc(weight_row_exact_kfold)) %>%
    mutate(rank_row_exact_kfold = row_number()) %>%
    select(target_space, sample_group, model_id, model_name, heterogeneity_variant,
           model_key_row_exact_kfold, weight_row_exact_kfold, rank_row_exact_kfold,
           elpd_exact_row_kfold, singleton_elpd, mean_lpd, sd_lpd, n_obs_scored,
           reliability_flag, refit_type, validation_unit, primary_row_target_excludes_new_company_rows)
}

weights_ep <- build_row_weights("ex_post")
weights_rt <- build_row_weights("real_time")
write_dual_csv(weights_ep, "table_winsor_row_exact_kfold_weights_ex_post.csv")
write_dual_csv(weights_rt, "table_winsor_row_exact_kfold_weights_no_lookahead.csv")

all_required_success <- all(status$status[status$Required %in% c(TRUE, "TRUE", 1L)] == "SUCCESS")
weights_available <- nrow(weights_rt) > 0 && (!any(manifest$Target_Space == "ex_post") || nrow(weights_ep) > 0)
primary_allowed <- all_required_success && weights_available &&
  identical(run_mode, "FULL_MODE") && !partial_run && identical(K, 5L)
manifest_row <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = as.character(script_start_time),
  End_Time = as.character(Sys.time()),
  K = K,
  Run_Mode = run_mode,
  Run_ID = manifest$Run_ID[1],
  Config_Tag = manifest$Config_Tag[1],
  Row_KFold_Root = run_root,
  Chains = manifest$chains[1],
  Cores = manifest$cores[1],
  Iter = manifest$iter[1],
  Warmup = manifest$warmup[1],
  Adapt_Delta = manifest$adapt_delta[1],
  Max_Treedepth = manifest$max_treedepth[1],
  Refresh = manifest$refresh[1],
  Backend = manifest$backend[1],
  Sampler_Profile = manifest$sampler_profile[1],
  Config_Source = manifest$config_source[1],
  RNG_Context = "row_kfold_run_manifest",
  Canonical_Seed = accrual_base_seed(),
  Effective_Seed = accrual_seed_for("row_kfold_run_manifest"),
  Preflight_Only = FALSE,
  Partial_Run = partial_run,
  Primary_Inference_Allowed = primary_allowed,
  Completed_Run_Pin_Eligible = primary_allowed,
  Completed_Run_Pin_Updated = primary_allowed,
  Status = if (all_required_success) "COMPLETED" else "FAILED",
  stringsAsFactors = FALSE
)
write_csv_safely(manifest_row, file.path(logs_dir, "row_exact_kfold_run_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writeLines(run_root, file.path(base_root, "LATEST_RUN.txt"))
if (primary_allowed) writeLines(run_root, file.path(base_root, "LATEST_COMPLETED_RUN.txt"))

message("ma13c collected row K-fold task results and wrote ma14-compatible run outputs.")
phase_end("ma13c", "Collect row-level exact K-fold scores")
