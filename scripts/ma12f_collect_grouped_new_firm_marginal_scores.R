# Script: ma12f_collect_grouped_new_firm_marginal_scores.R
# Purpose: Collect MA12D v1.1 grouped-firm marginal new-firm rescoring outputs.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma12f", "Collect grouped-firm marginal new-firm rescoring scores")

script_name <- "scripts/ma12f_collect_grouped_new_firm_marginal_scores.R"
script_version <- "marginal-new-firm-rescore-v1.1-collect"
script_start_time <- Sys.time()

ma12d_env <- list(
  material_weight_change = env_num("ACCRUAL_MA12D_WEIGHT_CHANGE_MATERIAL", 0.05, min = 0),
  allow_restack_excluded = env_flag("ACCRUAL_MA12D_ALLOW_RESTACK_EXCLUDED", "FALSE"),
  source_row_kfold_run_root = trimws(env_value("ACCRUAL_MA12D_SOURCE_ROW_KFOLD_RUN_ROOT", ""))
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

resolve_output_run_root <- function() {
  explicit <- trimws(env_value("ACCRUAL_MA12D_OUTPUT_RUN_ROOT", ""))
  if (nonempty(explicit)) return(normalizePath(explicit, winslash = "/", mustWork = FALSE))
  pinned <- single_line(file.path(output_root, "grouped_new_firm_marginal", "LATEST_RUN.txt"))
  if (nonempty(pinned)) return(normalizePath(pinned, winslash = "/", mustWork = FALSE))
  stop("[BLOCKER] MA12F cannot resolve MA12D output run root. Run ma12d_prepare first or set ACCRUAL_MA12D_OUTPUT_RUN_ROOT.")
}

required_columns <- function(df, cols, context) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop("[BLOCKER] ", context, " missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

as_bool <- function(x) {
  if (is.logical(x)) return(x)
  as.character(x) %in% c("TRUE", "true", "True", "1", "yes", "YES")
}

file_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

firm_re_indicator <- function(variant) {
  grepl("Firm RE|Random Intercept|firm_RE|firmre", as.character(variant), ignore.case = TRUE)
}

write_dual_csv <- function(x, file_name, tables_dir, compat_tables_dir = file.path(output_root, "tables")) {
  write_csv_safely(x, file.path(tables_dir, file_name), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safely(x, file.path(compat_tables_dir, file_name), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(file.path(tables_dir, file_name))
}

source_contract_from_manifest <- function(manifest) {
  list(
    root = manifest$Source_KFold_Run_Root[1],
    resolution = manifest$Source_KFold_Run_Root_Resolution[1],
    manifest_path = manifest$Source_KFold_Manifest_Path[1],
    status_path = manifest$Source_KFold_Status_Path[1],
    model_scores_path = manifest$Source_KFold_Model_Scores_Path[1],
    observation_scores_path = manifest$Source_KFold_Observation_Scores_Path[1],
    fold_assignment_path = paste(unique(manifest$Fold_Assignment_Path), collapse = ";"),
    weights_ex_post_path = file.path(manifest$Source_KFold_Run_Root[1], "tables", "table_winsor_kfold_weights_ex_post.csv"),
    weights_no_lookahead_path = file.path(manifest$Source_KFold_Run_Root[1], "tables", "table_winsor_kfold_weights_no_lookahead.csv")
  )
}

write_blocker_decision <- function(decision_code, interpretation, source_contract, manifest = NULL, tables_dir, output_run_root) {
  target_spaces <- if (!is.null(manifest) && "Target_Space" %in% names(manifest)) unique(as.character(manifest$Target_Space)) else NA_character_
  if (!length(target_spaces)) target_spaces <- NA_character_
  source_fold_path <- if (!is.null(manifest) && "Fold_Assignment_Path" %in% names(manifest)) {
    paste(unique(as.character(manifest$Fold_Assignment_Path)), collapse = ";")
  } else {
    NA_character_
  }
  decision <- data.frame(
    Target_Space = target_spaces,
    Top_Cell_Population_Level = NA_character_,
    Top_Cell_Marginal_New_Firm = NA_character_,
    Aggregate_FirmRE_Weight_Population_Level = NA_real_,
    Aggregate_FirmRE_Weight_Marginal_New_Firm = NA_real_,
    Absolute_FirmRE_Weight_Change = NA_real_,
    Top_Cell_Changed = NA,
    Conclusion_Changed = NA,
    Decision = decision_code,
    Interpretation = interpretation,
    Restack_Excluded_Allowed = ma12d_env$allow_restack_excluded,
    N_Source_Included_Candidates = if (!is.null(manifest) && "Source_Included_In_Stack" %in% names(manifest)) sum(as_bool(manifest$Source_Included_In_Stack), na.rm = TRUE) else NA_integer_,
    N_MA12D_Primary_Stack_Candidates = if (!is.null(manifest) && "MA12D_Primary_Stack_Eligible" %in% names(manifest)) sum(as_bool(manifest$MA12D_Primary_Stack_Eligible), na.rm = TRUE) else NA_integer_,
    N_Successfully_Rescored_Candidates = NA_integer_,
    N_Excluded_By_Source_Gate = if (!is.null(manifest) && "Source_Included_In_Stack" %in% names(manifest)) sum(!as_bool(manifest$Source_Included_In_Stack), na.rm = TRUE) else NA_integer_,
    Source_Model_Scores_Path = source_contract$model_scores_path,
    Source_KFold_Run_Root = source_contract$root,
    Source_KFold_Run_Root_Resolution = source_contract$resolution,
    Source_KFold_Manifest_Path = source_contract$manifest_path,
    Source_KFold_Status_Path = source_contract$status_path,
    Source_KFold_Model_Scores_Path = source_contract$model_scores_path,
    Source_KFold_Observation_Scores_Path = source_contract$observation_scores_path,
    Source_KFold_Fold_Assignment_Path = source_fold_path,
    Output_Run_Root = output_run_root,
    stringsAsFactors = FALSE
  )
  write_dual_csv(decision, "table_grouped_marginal_new_firm_decision.csv", tables_dir)
  invisible(decision)
}

read_weight_file <- function(path, target_space) {
  if (!file.exists(path)) return(data.frame())
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(x)) return(data.frame())
  x$Target_Space <- target_space
  x
}

standard_weights <- function(x, weight_col) {
  if (!nrow(x)) return(data.frame())
  data.frame(
    Target_Space = as.character(x$Target_Space),
    Model_ID = as.character(x$Model_ID),
    Model_Name = as.character(x$Model_Name),
    Heterogeneity_Variant = as.character(x$Heterogeneity_Variant),
    Weight = suppressWarnings(as.numeric(x[[weight_col]])),
    Rank = if ("Rank_KFold" %in% names(x)) suppressWarnings(as.integer(x$Rank_KFold)) else NA_integer_,
    stringsAsFactors = FALSE
  )
}

output_run_root <- resolve_output_run_root()
tables_dir <- file.path(output_run_root, "tables")
logs_dir <- file.path(output_run_root, "logs")
manifest_path <- file.path(tables_dir, "table_ma12d_grouped_new_firm_marginal_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma12d_grouped_new_firm_marginal_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) {
  stop("[BLOCKER] MA12F requires MA12D manifest and MA12E task status.")
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
task_status <- read.csv(status_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns(manifest, c("Task_Key", "MA12D_Result_Path", "Output_Run_Root",
                             "Source_Reliability_Flag", "Source_Included_In_Stack",
                             "MA12D_Primary_Stack_Eligible",
                             "Source_KFold_Run_Root", "Source_KFold_Run_Root_Resolution",
                             "Source_KFold_Manifest_Path", "Source_KFold_Status_Path",
                             "Source_KFold_Model_Scores_Path", "Source_KFold_Observation_Scores_Path"),
                 "MA12F task manifest")
required_columns(task_status, c("Task_Key", "status", "Required", "result_path"), "MA12F task status")
source_contract <- source_contract_from_manifest(manifest)

failed_required <- as_bool(task_status$Required) & task_status$status %in% c("FAILED", "BLOCKED_MISSING_FIT")
if (any(failed_required, na.rm = TRUE)) {
  reasons <- task_status$reason[failed_required]
  if (any(grepl("Missing required MA12 fit object|missing required fitted", reasons, ignore.case = TRUE), na.rm = TRUE)) {
    write_blocker_decision(
      "BLOCKED_MISSING_FITS",
      paste("Required MA12 fitted brms object(s) were unavailable; MA12D did not refit. Details:",
            paste(reasons, collapse = " | ")),
      source_contract, manifest, tables_dir, output_run_root
    )
  } else if (any(grepl("random-intercept SD|sd_\\.\\*__Intercept|sigma_u|Firm-RE task has no group-level intercept",
                       reasons, ignore.case = TRUE), na.rm = TRUE)) {
    write_blocker_decision(
      "BLOCKED_UNVERIFIED_FIRMRE_SD",
      paste("A Firm-RE model could not identify the firm random-intercept standard deviation:",
            paste(reasons, collapse = " | ")),
      source_contract, manifest, tables_dir, output_run_root
    )
  }
  accrual_task_status_blocker(task_status, required_col = "Required", context = "ma12f grouped new-firm rescoring collect")
}

success_status <- task_status[task_status$status == "SUCCESS", , drop = FALSE]
if (!nrow(success_status)) stop("[BLOCKER] MA12D produced no successful task results.")
missing_results <- success_status$result_path[!file.exists(success_status$result_path)]
if (length(missing_results)) {
  stop("[BLOCKER] MA12F missing successful task result RDS: ",
       paste(utils::head(missing_results, 10), collapse = "; "))
}
results <- lapply(success_status$result_path, readRDS)
obs_scores <- bind_rows(lapply(results, `[[`, "obs_scores"))
fold_diagnostics <- bind_rows(lapply(results, `[[`, "fold_diag"))
if (!nrow(obs_scores)) stop("[BLOCKER] MA12D produced no observation scores.")
if (!all(is.finite(obs_scores$lpd_obs))) stop("[BLOCKER] MA12D exported non-finite lpd_obs values.")

write_dual_csv(obs_scores, "table_winsor_kfold_observation_scores_marginal_new_firm.csv", tables_dir)

fold_scores <- obs_scores %>%
  group_by(.data$Target_Space, .data$Sample_Group, .data$Fold_ID, .data$Model_ID,
           .data$Model_Name, .data$Heterogeneity_Variant) %>%
  summarise(
    N_Test_Obs = n(),
    N_Test_Firms = n_distinct(.data$company),
    elpd_fold = sum(.data$lpd_obs),
    mean_lpd_obs = mean(.data$lpd_obs),
    RMSE = sqrt(mean((.data$y_actual - dplyr::coalesce(.data$pred_mean_marginal_new_firm,
                                                       .data$pred_mean_population_level_existing))^2, na.rm = TRUE)),
    MAE = mean(abs(.data$y_actual - dplyr::coalesce(.data$pred_mean_marginal_new_firm,
                                                    .data$pred_mean_population_level_existing)), na.rm = TRUE),
    marginal_score_used_any = any(.data$marginal_score_used),
    Source_Reliability_Flag = dplyr::first(.data$Source_Reliability_Flag),
    Source_Included_In_Stack = dplyr::first(.data$Source_Included_In_Stack),
    MA12D_Primary_Stack_Eligible = dplyr::first(.data$MA12D_Primary_Stack_Eligible),
    Restack_Excluded_Allowed = dplyr::first(.data$Restack_Excluded_Allowed),
    N_New_Firm_Draws = suppressWarnings(max(.data$N_New_Firm_Draws, na.rm = TRUE)),
    N_Posterior_Draws_Used = suppressWarnings(max(.data$N_Posterior_Draws_Used, na.rm = TRUE)),
    Scoring_Rule = "marginal_new_firm_integrated",
    Source_KFold_Run_Root = dplyr::first(.data$Source_KFold_Run_Root),
    Source_KFold_Run_Root_Resolution = dplyr::first(.data$Source_KFold_Run_Root_Resolution),
    Source_KFold_Manifest_Path = dplyr::first(.data$Source_KFold_Manifest_Path),
    Source_KFold_Status_Path = dplyr::first(.data$Source_KFold_Status_Path),
    Source_KFold_Model_Scores_Path = dplyr::first(.data$Source_KFold_Model_Scores_Path),
    Source_KFold_Observation_Scores_Path = dplyr::first(.data$Source_KFold_Observation_Scores_Path),
    Source_KFold_Fold_Assignment_Path = dplyr::first(.data$Source_KFold_Fold_Assignment_Path),
    Output_Run_Root = dplyr::first(.data$Output_Run_Root),
    .groups = "drop"
  ) %>%
  mutate(N_Posterior_Draws_Used = ifelse(is.infinite(.data$N_Posterior_Draws_Used), NA_real_, .data$N_Posterior_Draws_Used))
write_dual_csv(fold_scores, "table_winsor_kfold_fold_scores_marginal_new_firm.csv", tables_dir)

source_gate <- manifest %>%
  distinct(.data$Target_Space, .data$Sample_Group, .data$Model_ID, .data$Model_Name,
           .data$Heterogeneity_Variant, .data$Source_Reliability_Flag,
           .data$Source_Included_In_Stack, .data$MA12D_Primary_Stack_Eligible,
           .data$Source_N_Folds_Completed, .data$Source_N_Test_Obs_Total,
           .data$Source_ELPD_KFold, .data$Restack_Excluded_Allowed,
           .data$Source_Model_Scores_Path)

model_scores <- fold_diagnostics %>%
  group_by(.data$Target_Space, .data$Sample_Group, .data$Model_ID,
           .data$Model_Name, .data$Heterogeneity_Variant) %>%
  summarise(
    N_Folds_Attempted = n(),
    N_Folds_Completed = sum(.data$Completed, na.rm = TRUE),
    N_New_Firm_Draws = suppressWarnings(max(.data$N_New_Firm_Draws, na.rm = TRUE)),
    N_Posterior_Draws_Used = suppressWarnings(max(.data$N_Posterior_Draws_Used, na.rm = TRUE)),
    Runtime_Seconds = sum(.data$Runtime_Seconds, na.rm = TRUE),
    exclusion_reason = paste(na.omit(unique(.data$Failure_Reason)), collapse = " | "),
    .groups = "drop"
  ) %>%
  left_join(
    fold_scores %>%
      group_by(.data$Target_Space, .data$Sample_Group, .data$Model_ID,
               .data$Model_Name, .data$Heterogeneity_Variant) %>%
      summarise(
        N_Test_Obs_Total = sum(.data$N_Test_Obs),
        N_Test_Firms_Total = sum(.data$N_Test_Firms),
        elpd_kfold = sum(.data$elpd_fold),
        mean_lpd_obs = weighted.mean(.data$mean_lpd_obs, .data$N_Test_Obs),
        se_elpd_fold = stats::sd(.data$elpd_fold),
        RMSE = sqrt(weighted.mean(.data$RMSE^2, .data$N_Test_Obs)),
        MAE = weighted.mean(.data$MAE, .data$N_Test_Obs),
        Scoring_Rule = dplyr::first(.data$Scoring_Rule),
        .groups = "drop"
      ),
    by = c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant")
  ) %>%
  left_join(source_gate, by = c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant")) %>%
  mutate(
    N_Posterior_Draws_Used = ifelse(is.infinite(.data$N_Posterior_Draws_Used), NA_real_, .data$N_Posterior_Draws_Used),
    reliability_flag = .data$Source_Reliability_Flag,
    included_in_stack = .data$MA12D_Primary_Stack_Eligible,
    Scoring_Rule = "marginal_new_firm_integrated",
    exclusion_reason = ifelse(.data$included_in_stack, NA_character_, .data$exclusion_reason)
  )
write_dual_csv(model_scores, "table_winsor_kfold_model_scores_marginal_new_firm.csv", tables_dir)

build_kfold_weights <- function(target_space) {
  included <- model_scores %>%
    filter(.data$Target_Space == target_space,
           .data$Sample_Group == "main_common",
           .data$MA12D_Primary_Stack_Eligible == TRUE) %>%
    arrange(.data$Model_ID, .data$Heterogeneity_Variant)
  if (!nrow(included)) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, ]
    key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_kfold_marginal_new_firm")
    one <- obs_scores %>%
      filter(.data$Target_Space == target_space,
             .data$Sample_Group == row$Sample_Group,
             .data$Model_ID == row$Model_ID,
             .data$Heterogeneity_Variant == row$Heterogeneity_Variant) %>%
      arrange(.data$company, .data$year)
    if (nrow(one) != row$N_Test_Obs_Total) next
    score_list[[key]] <- one$lpd_obs
    meta_keys <- c(meta_keys, key)
  }
  if (!length(score_list)) return(data.frame())
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) {
    stop("[BLOCKER] MA12D exact grouped K-fold score vectors have unequal lengths for ", target_space)
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
      Rank_KFold = rank(-as.numeric(weights), ties.method = "first"),
      Singleton_ELPD = as.numeric(singleton_elpd[names(weights)]),
      Best_Singleton_ELPD_Key = best_elpd_key,
      Top_Weight_Key = top_weight_key,
      Top_Weight_Not_Best_Singleton = max(weights) > 0.999 && !identical(top_weight_key, best_elpd_key),
      M10_Included = FALSE,
      Restack_Excluded_Allowed = ma12d_env$allow_restack_excluded,
      Scoring_Rule = "marginal_new_firm_integrated"
    ) %>%
    arrange(desc(.data$Weight_KFold)) %>%
    select(Target_Space, Sample_Group, M10_Included, Model_ID,
           Model_Name, Heterogeneity_Variant, Model_Key_KFold,
           Weight_KFold, Rank_KFold, elpd_kfold, Singleton_ELPD,
           mean_lpd_obs, RMSE, MAE, reliability_flag, included_in_stack,
           Source_Reliability_Flag, Source_Included_In_Stack,
           MA12D_Primary_Stack_Eligible, Restack_Excluded_Allowed,
           Best_Singleton_ELPD_Key, Top_Weight_Key, Top_Weight_Not_Best_Singleton,
           Scoring_Rule, N_New_Firm_Draws, N_Posterior_Draws_Used)
}

weights_ep <- build_kfold_weights("ex_post")
weights_rt <- build_kfold_weights("real_time")
write_dual_csv(weights_ep, "table_winsor_kfold_weights_ex_post_marginal_new_firm.csv", tables_dir)
write_dual_csv(weights_rt, "table_winsor_kfold_weights_no_lookahead_marginal_new_firm.csv", tables_dir)

population_weights <- bind_rows(
  read_weight_file(source_contract$weights_ex_post_path, "ex_post"),
  read_weight_file(source_contract$weights_no_lookahead_path, "real_time")
)
marginal_weights <- bind_rows(weights_ep, weights_rt)
pop_std <- standard_weights(population_weights, "Weight_KFold")
mar_std <- standard_weights(marginal_weights, "Weight_KFold")
if (!nrow(pop_std) && !nrow(mar_std)) {
  stop("[BLOCKER] MA12D could not build population-vs-marginal comparison because both weight tables are empty.")
}
comparison <- full_join(
  pop_std %>% rename(Weight_Population_Level = Weight, Rank_Population_Level = Rank),
  mar_std %>% rename(Weight_Marginal_New_Firm = Weight, Rank_Marginal_New_Firm = Rank),
  by = c("Target_Space", "Model_ID", "Model_Name", "Heterogeneity_Variant")
) %>%
  mutate(
    Weight_Population_Level = ifelse(is.na(.data$Weight_Population_Level), 0, .data$Weight_Population_Level),
    Weight_Marginal_New_Firm = ifelse(is.na(.data$Weight_Marginal_New_Firm), 0, .data$Weight_Marginal_New_Firm),
    Weight_Difference = .data$Weight_Marginal_New_Firm - .data$Weight_Population_Level,
    is_firm_re = firm_re_indicator(.data$Heterogeneity_Variant)
  )

top_pop <- comparison %>%
  group_by(.data$Target_Space) %>%
  slice_max(.data$Weight_Population_Level, n = 1, with_ties = FALSE) %>%
  transmute(Target_Space,
            Top_Cell_Population_Level = paste(.data$Model_ID, .data$Heterogeneity_Variant, sep = "::"))
top_mar <- comparison %>%
  group_by(.data$Target_Space) %>%
  slice_max(.data$Weight_Marginal_New_Firm, n = 1, with_ties = FALSE) %>%
  transmute(Target_Space,
            Top_Cell_Marginal_New_Firm = paste(.data$Model_ID, .data$Heterogeneity_Variant, sep = "::"))
firmre_summary <- comparison %>%
  group_by(.data$Target_Space) %>%
  summarise(
    FirmRE_Weight_Population_Level = sum(.data$Weight_Population_Level[.data$is_firm_re], na.rm = TRUE),
    FirmRE_Weight_Marginal_New_Firm = sum(.data$Weight_Marginal_New_Firm[.data$is_firm_re], na.rm = TRUE),
    FirmRE_Weight_Difference = FirmRE_Weight_Marginal_New_Firm - FirmRE_Weight_Population_Level,
    .groups = "drop"
  )
comparison <- comparison %>%
  left_join(top_pop, by = "Target_Space") %>%
  left_join(top_mar, by = "Target_Space") %>%
  left_join(firmre_summary, by = "Target_Space") %>%
  select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant,
         Weight_Population_Level, Weight_Marginal_New_Firm, Weight_Difference,
         Rank_Population_Level, Rank_Marginal_New_Firm,
         Top_Cell_Population_Level, Top_Cell_Marginal_New_Firm,
         FirmRE_Weight_Population_Level, FirmRE_Weight_Marginal_New_Firm,
         FirmRE_Weight_Difference)
write_dual_csv(comparison, "table_grouped_population_vs_marginal_new_firm_weight_comparison.csv", tables_dir)

success_counts <- model_scores %>%
  distinct(.data$Target_Space, .data$Sample_Group, .data$Model_ID, .data$Heterogeneity_Variant) %>%
  count(.data$Target_Space, name = "N_Successfully_Rescored_Candidates")
source_counts <- manifest %>%
  distinct(.data$Target_Space, .data$Sample_Group, .data$Model_ID, .data$Heterogeneity_Variant,
           .data$Source_Included_In_Stack, .data$MA12D_Primary_Stack_Eligible) %>%
  mutate(
    Source_Included_In_Stack = as_bool(.data$Source_Included_In_Stack),
    MA12D_Primary_Stack_Eligible = as_bool(.data$MA12D_Primary_Stack_Eligible)
  ) %>%
  group_by(.data$Target_Space) %>%
  summarise(
    N_Source_Included_Candidates = sum(.data$Source_Included_In_Stack, na.rm = TRUE),
    N_MA12D_Primary_Stack_Candidates = sum(.data$MA12D_Primary_Stack_Eligible, na.rm = TRUE),
    N_Excluded_By_Source_Gate = sum(!.data$Source_Included_In_Stack, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(success_counts, by = "Target_Space") %>%
  mutate(N_Successfully_Rescored_Candidates = ifelse(is.na(.data$N_Successfully_Rescored_Candidates), 0L, .data$N_Successfully_Rescored_Candidates))

decision <- firmre_summary %>%
  left_join(top_pop, by = "Target_Space") %>%
  left_join(top_mar, by = "Target_Space") %>%
  left_join(source_counts, by = "Target_Space") %>%
  mutate(
    Aggregate_FirmRE_Weight_Population_Level = .data$FirmRE_Weight_Population_Level,
    Aggregate_FirmRE_Weight_Marginal_New_Firm = .data$FirmRE_Weight_Marginal_New_Firm,
    Absolute_FirmRE_Weight_Change = abs(.data$FirmRE_Weight_Difference),
    Top_Cell_Changed = .data$Top_Cell_Population_Level != .data$Top_Cell_Marginal_New_Firm,
    Conclusion_Changed = (.data$Aggregate_FirmRE_Weight_Population_Level >= 0.5) !=
      (.data$Aggregate_FirmRE_Weight_Marginal_New_Firm >= 0.5),
    Decision = case_when(
      ma12d_env$allow_restack_excluded ~ "DIAGNOSTIC_ONLY_RESTACK_EXCLUDED",
      .data$Top_Cell_Changed | .data$Conclusion_Changed ~ "REVISE_PRIMARY_RESULT",
      .data$Absolute_FirmRE_Weight_Change >= ma12d_env$material_weight_change ~ "QUALIFIES_PRIMARY_RESULT",
      TRUE ~ "PASS_PRIMARY_ALIGNMENT"
    ),
    Interpretation = case_when(
      .data$Decision == "DIAGNOSTIC_ONLY_RESTACK_EXCLUDED" ~ "Restacking source-excluded candidates was explicitly enabled; MA12D output is diagnostic only.",
      .data$Decision == "PASS_PRIMARY_ALIGNMENT" ~ "Marginal new-firm Firm-RE scoring preserves the original grouped population-level conclusion under the inherited MA12C source gate.",
      .data$Decision == "QUALIFIES_PRIMARY_RESULT" ~ "The qualitative conclusion remains aligned, but the aggregate Firm-RE weight changes materially under the inherited MA12C source gate.",
      TRUE ~ "The top grouped benchmark cell or aggregate Firm-RE conclusion changes under marginal new-firm scoring."
    ),
    Restack_Excluded_Allowed = ma12d_env$allow_restack_excluded,
    Source_Model_Scores_Path = source_contract$model_scores_path,
    Source_KFold_Run_Root = source_contract$root,
    Source_KFold_Run_Root_Resolution = source_contract$resolution,
    Source_KFold_Manifest_Path = source_contract$manifest_path,
    Source_KFold_Status_Path = source_contract$status_path,
    Source_KFold_Model_Scores_Path = source_contract$model_scores_path,
    Source_KFold_Observation_Scores_Path = source_contract$observation_scores_path,
    Source_KFold_Fold_Assignment_Path = source_contract$fold_assignment_path,
    Output_Run_Root = output_run_root
  ) %>%
  select(Target_Space, Top_Cell_Population_Level, Top_Cell_Marginal_New_Firm,
         Aggregate_FirmRE_Weight_Population_Level, Aggregate_FirmRE_Weight_Marginal_New_Firm,
         Absolute_FirmRE_Weight_Change, Top_Cell_Changed, Conclusion_Changed,
         Decision, Interpretation, Restack_Excluded_Allowed,
         N_Source_Included_Candidates, N_MA12D_Primary_Stack_Candidates,
         N_Successfully_Rescored_Candidates, N_Excluded_By_Source_Gate,
         Source_Model_Scores_Path, Source_KFold_Run_Root,
         Source_KFold_Run_Root_Resolution, Source_KFold_Manifest_Path,
         Source_KFold_Status_Path, Source_KFold_Model_Scores_Path,
         Source_KFold_Observation_Scores_Path, Source_KFold_Fold_Assignment_Path,
         Output_Run_Root)
write_dual_csv(decision, "table_grouped_marginal_new_firm_decision.csv", tables_dir)

all_required_success <- all(task_status$status[as_bool(task_status$Required)] == "SUCCESS")
weights_available <- nrow(weights_ep) > 0 && nrow(weights_rt) > 0
pin_eligible <- all_required_success && weights_available
run_manifest <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = as.character(script_start_time),
  End_Time = as.character(Sys.time()),
  Runtime_Seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  Output_Run_Root = output_run_root,
  Source_KFold_Run_Root = source_contract$root,
  Source_KFold_Run_Root_Resolution = source_contract$resolution,
  Source_KFold_Manifest_Path = source_contract$manifest_path,
  Source_KFold_Status_Path = source_contract$status_path,
  Source_KFold_Model_Scores_Path = source_contract$model_scores_path,
  Source_KFold_Observation_Scores_Path = source_contract$observation_scores_path,
  Source_KFold_Fold_Assignment_Path = source_contract$fold_assignment_path,
  Source_Row_KFold_Run_Root_Reserved = ma12d_env$source_row_kfold_run_root,
  Restack_Excluded_Allowed = ma12d_env$allow_restack_excluded,
  Weight_Change_Material_Threshold = ma12d_env$material_weight_change,
  Prior_Set_ID = prior_set_id,
  Likelihood_Family = likelihood_family,
  Model_Structure = model_structure,
  Prediction_Rule = "grouped_firm_marginal_new_firm_integrated",
  Refits_Performed = FALSE,
  Source_Manifest_MD5 = file_md5(source_contract$manifest_path),
  Source_Status_MD5 = file_md5(source_contract$status_path),
  Source_Model_Scores_MD5 = file_md5(source_contract$model_scores_path),
  Source_Observation_Scores_MD5 = file_md5(source_contract$observation_scores_path),
  Completed_Run_Pin_Eligible = pin_eligible,
  Completed_Run_Pin_Updated = pin_eligible,
  Status = if (all_required_success) "COMPLETED" else "FAILED",
  stringsAsFactors = FALSE
)
write_csv_safely(run_manifest, file.path(logs_dir, "run_config_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))
if (pin_eligible) {
  writeLines(output_run_root, file.path(output_root, "grouped_new_firm_marginal", "LATEST_COMPLETED_RUN.txt"))
}

cat("\n[SUCCESS] MA12F grouped-firm marginal new-firm collection completed.\n")
cat("Output run root:", output_run_root, "\n")
cat("Completed-run pin updated:", pin_eligible, "\n")
phase_end("ma12f", "Collect grouped-firm marginal new-firm rescoring scores")
