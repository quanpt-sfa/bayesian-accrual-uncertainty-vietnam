# Script: ma12c_collect_grouped_kfold_firm_scores.R
# Purpose: Collect grouped-firm exact K-fold scores and ma14-compatible run outputs.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma12c", "Collect grouped-firm exact K-fold scores")

script_name <- "scripts/ma12c_collect_grouped_kfold_firm_scores.R"
script_version <- "split-worker-v1"
script_start_time <- Sys.time()

compat_tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(compat_tables_dir, "table_ma12_grouped_kfold_task_manifest.csv")
status_path <- file.path(compat_tables_dir, "table_ma12_grouped_kfold_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) {
  stop("[BLOCKER] ma12c requires ma12a manifest and ma12b task status.")
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "ma12c grouped K-fold collect")
if (!"result_path" %in% names(manifest)) manifest$result_path <- manifest$prediction_path
if (!"Kfold_Run_Root" %in% names(manifest)) stop("[BLOCKER] ma12c manifest lacks Kfold_Run_Root.")

run_root <- manifest$Kfold_Run_Root[1]
base_root <- dirname(run_root)
tables_dir <- file.path(run_root, "tables")
logs_dir <- file.path(run_root, "logs")
dir.create(base_root, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

results <- lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] ma12c missing grouped K-fold task result: ", path)
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

write_dual_csv(fold_diagnostics, "table_winsor_kfold_refit_diagnostics.csv")
write_dual_csv(obs_scores, "table_winsor_kfold_observation_scores.csv")

K <- as.integer(manifest$K[1])
run_mode <- as.character(manifest$run_mode[1])
partial_run <- isTRUE(as.logical(manifest$Partial_Run[1]))

fold_scores <- obs_scores %>%
  group_by(Target_Space, Sample_Group, Fold_ID, Model_ID, Model_Name, Heterogeneity_Variant) %>%
  summarise(
    N_Test_Obs = n(),
    N_Test_Firms = n_distinct(company),
    elpd_fold = sum(lpd_obs, na.rm = TRUE),
    mean_lpd_obs = mean(lpd_obs, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    MAE = mean(abs_error, na.rm = TRUE),
    .groups = "drop"
  )
write_dual_csv(fold_scores, "table_winsor_kfold_fold_scores.csv")

model_scores <- fold_diagnostics %>%
  group_by(Target_Space, Sample_Group, Model_ID, Model_Name, Heterogeneity_Variant) %>%
  summarise(
    N_Folds_Attempted = n(),
    N_Folds_Completed = sum(Completed, na.rm = TRUE),
    max_rhat_max = suppressWarnings(max(Max_Rhat, na.rm = TRUE)),
    min_ess_bulk = suppressWarnings(min(Min_ESS_Bulk, na.rm = TRUE)),
    min_ess_tail = suppressWarnings(min(Min_ESS_Tail, na.rm = TRUE)),
    ess_warning_any = any(ESS_Warning %in% c(TRUE, "TRUE", 1L), na.rm = TRUE),
    divergences_total = sum(Divergences, na.rm = TRUE),
    treedepth_warnings_total = sum(Treedepth_Warnings, na.rm = TRUE),
    Runtime_Seconds = sum(Runtime_Seconds, na.rm = TRUE),
    exclusion_reason = paste(na.omit(unique(Failure_Reason)), collapse = " | "),
    .groups = "drop"
  ) %>%
  left_join(
    fold_scores %>%
      group_by(Target_Space, Sample_Group, Model_ID, Model_Name, Heterogeneity_Variant) %>%
      summarise(
        N_Test_Obs_Total = sum(N_Test_Obs),
        N_Test_Firms_Total = sum(N_Test_Firms),
        elpd_kfold = sum(elpd_fold),
        mean_lpd_obs = weighted.mean(mean_lpd_obs, N_Test_Obs),
        se_elpd_fold = stats::sd(elpd_fold),
        RMSE = sqrt(weighted.mean(RMSE^2, N_Test_Obs)),
        MAE = weighted.mean(MAE, N_Test_Obs),
        .groups = "drop"
      ),
    by = c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant")
  ) %>%
  mutate(
    max_rhat_max = ifelse(is.infinite(max_rhat_max), NA_real_, max_rhat_max),
    min_ess_bulk = ifelse(is.infinite(min_ess_bulk), NA_real_, min_ess_bulk),
    min_ess_tail = ifelse(is.infinite(min_ess_tail), NA_real_, min_ess_tail),
    reliability_flag = case_when(
      N_Folds_Completed == 0 ~ "FAILED",
      !partial_run & N_Folds_Completed < K ~ "LOW_RELIABILITY",
      divergences_total > 0 | treedepth_warnings_total > 0 ~ "LOW_RELIABILITY",
      !is.na(max_rhat_max) & !is.na(min_ess_bulk) & !is.na(min_ess_tail) &
        max_rhat_max <= 1.01 & min_ess_bulk >= 400 & min_ess_tail >= 400 ~ "OK",
      !is.na(max_rhat_max) & !is.na(min_ess_bulk) & !is.na(min_ess_tail) &
        max_rhat_max <= 1.05 & min_ess_bulk >= 100 & min_ess_tail >= 100 ~ "CAUTION",
      TRUE ~ "LOW_RELIABILITY"
    ),
    included_in_stack = reliability_flag %in% c("OK", "CAUTION") &
      ifelse(partial_run, N_Folds_Completed > 0, N_Folds_Completed == K),
    exclusion_reason = ifelse(included_in_stack, NA_character_, exclusion_reason)
  )
write_dual_csv(model_scores, "table_winsor_kfold_model_scores.csv")

build_kfold_weights <- function(target_space) {
  included <- model_scores %>%
    filter(Target_Space == target_space, Sample_Group == "main_common", included_in_stack == TRUE) %>%
    arrange(Model_ID, Heterogeneity_Variant)
  if (nrow(included) == 0 || nrow(obs_scores) == 0) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, ]
    key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_kfold")
    one <- obs_scores %>%
      filter(Target_Space == target_space, Sample_Group == row$Sample_Group,
             Model_ID == row$Model_ID, Heterogeneity_Variant == row$Heterogeneity_Variant) %>%
      arrange(company, year)
    if (nrow(one) != row$N_Test_Obs_Total) next
    score_list[[key]] <- one$lpd_obs
    meta_keys <- c(meta_keys, key)
  }
  if (!length(score_list)) return(data.frame())
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) {
    stop("[BLOCKER] Grouped exact K-fold score vectors have unequal lengths for ", target_space)
  }
  lpd_matrix <- do.call(cbind, score_list)
  colnames(lpd_matrix) <- names(score_list)
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  singleton_elpd <- colSums(lpd_matrix)
  meta_idx <- match(names(weights), meta_keys)
  best_elpd_key <- names(singleton_elpd)[which.max(singleton_elpd)]
  top_weight_key <- names(weights)[which.max(weights)]
  included[meta_idx, ] %>%
    mutate(
      Model_Key_KFold = names(weights),
      Weight_KFold = as.numeric(weights),
      Singleton_ELPD = as.numeric(singleton_elpd[names(weights)]),
      Best_Singleton_ELPD_Key = best_elpd_key,
      Top_Weight_Key = top_weight_key,
      Top_Weight_Not_Best_Singleton = max(weights) > 0.999 && !identical(top_weight_key, best_elpd_key)
    ) %>%
    arrange(desc(Weight_KFold)) %>%
    mutate(Rank_KFold = row_number(), M10_Included = FALSE) %>%
    select(Target_Space, Sample_Group, M10_Included, Model_ID, Model_Name, Heterogeneity_Variant,
           Model_Key_KFold, Weight_KFold, Rank_KFold, elpd_kfold, Singleton_ELPD,
           mean_lpd_obs, RMSE, MAE, reliability_flag,
           Best_Singleton_ELPD_Key, Top_Weight_Key, Top_Weight_Not_Best_Singleton)
}

weights_ep <- build_kfold_weights("ex_post")
weights_rt <- build_kfold_weights("real_time")
write_dual_csv(weights_ep, "table_winsor_kfold_weights_ex_post.csv")
write_dual_csv(weights_rt, "table_winsor_kfold_weights_no_lookahead.csv")

all_required_success <- all(status$status[status$Required %in% c(TRUE, "TRUE", 1L)] == "SUCCESS")
weights_available <- nrow(weights_ep) > 0 && nrow(weights_rt) > 0
pin_eligible <- all_required_success && weights_available &&
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
  Kfold_Run_Root = run_root,
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
  RNG_Context = "grouped_kfold_run_manifest",
  Canonical_Seed = accrual_base_seed(),
  Effective_Seed = accrual_seed_for("grouped_kfold_run_manifest"),
  Preflight_Only = FALSE,
  Partial_Run = partial_run,
  Completed_Run_Pin_Eligible = pin_eligible,
  Completed_Run_Pin_Updated = pin_eligible,
  Status = if (all_required_success) "COMPLETED" else "FAILED",
  stringsAsFactors = FALSE
)
write_csv_safely(manifest_row, file.path(logs_dir, "run_config_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writeLines(run_root, file.path(base_root, "LATEST_RUN.txt"))
if (pin_eligible) writeLines(run_root, file.path(base_root, "LATEST_COMPLETED_RUN.txt"))

message("ma12c collected grouped K-fold task results and wrote ma14-compatible run outputs.")
phase_end("ma12c", "Collect grouped-firm exact K-fold scores")
