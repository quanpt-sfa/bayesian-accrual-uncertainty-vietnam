# Script: se08c_collect_fold_local_preprocessing_sensitivity.R
# Purpose: Collect fold-local preprocessing sensitivity outputs and decisions.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("se08c", "Collect fold-local preprocessing sensitivity")

se08_root <- file.path(output_root, "sensitivity", "fold_local_preprocessing")
tables_dir <- file.path(se08_root, "tables")
logs_dir <- file.path(se08_root, "logs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

manifest_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) {
  stop("[BLOCKER] se08c requires se08a manifest and se08b task status.")
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "se08 fold-local preprocessing collect")
if (!all(status$status[status$Required %in% c(TRUE, "TRUE", 1L)] == "SUCCESS")) {
  stop("[BLOCKER] se08c requires all required fold-local preprocessing tasks to complete successfully.")
}

results <- lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] se08c missing task result: ", path)
  readRDS(path)
})
fold_diagnostics <- bind_rows(lapply(results, `[[`, "fold_diag"))
preprocessing_audit <- bind_rows(lapply(results, `[[`, "preprocessing_audit"))
obs_all <- lapply(results, `[[`, "obs_scores")
grouped_obs <- bind_rows(obs_all[manifest$Validation_Scheme == "grouped_firm_kfold"])
row_obs <- bind_rows(obs_all[manifest$Validation_Scheme == "row_exact_kfold"])

write_csv_safely(preprocessing_audit, file.path(tables_dir, "table_se08_fold_local_preprocessing_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(
  preprocessing_audit %>%
    filter(.data$preprocessing_step == "winsorization") %>%
    group_by(.data$validation_scheme, .data$target_space, .data$variable) %>%
    summarise(
      n_fold_model_tasks = n(),
      mean_train_cutoff_p01 = mean(.data$train_cutoff_p01, na.rm = TRUE),
      mean_train_cutoff_p99 = mean(.data$train_cutoff_p99, na.rm = TRUE),
      max_abs_delta_train_vs_global_p01 = max(abs(.data$delta_train_vs_global_p01), na.rm = TRUE),
      max_abs_delta_train_vs_global_p99 = max(abs(.data$delta_train_vs_global_p99), na.rm = TRUE),
      max_share_test_capped_low = max(.data$share_test_capped_low, na.rm = TRUE),
      max_share_test_capped_high = max(.data$share_test_capped_high, na.rm = TRUE),
      .groups = "drop"
    ),
  file.path(tables_dir, "table_se08_fold_local_cutoff_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write_csv_safely(
  preprocessing_audit %>%
    filter(.data$preprocessing_step == "standardization") %>%
    group_by(.data$validation_scheme, .data$target_space, .data$variable) %>%
    summarise(
      n_fold_model_tasks = n(),
      mean_train_mean = mean(.data$train_mean, na.rm = TRUE),
      sd_train_mean = stats::sd(.data$train_mean, na.rm = TRUE),
      mean_train_sd = mean(.data$train_sd, na.rm = TRUE),
      min_train_sd = min(.data$train_sd, na.rm = TRUE),
      .groups = "drop"
    ),
  file.path(tables_dir, "table_se08_fold_local_standardization_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write_csv_safely(grouped_obs, file.path(tables_dir, "table_se08_grouped_fold_local_observation_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(row_obs, file.path(tables_dir, "table_se08_row_fold_local_observation_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")

K_by_scheme <- manifest %>% group_by(.data$Validation_Scheme) %>% summarise(K = max(as.integer(.data$K)), .groups = "drop")
partial_run <- FALSE

reliability_label <- function(n_completed, K, divergences_total, treedepth_warnings_total, max_rhat, min_ess_bulk, min_ess_tail) {
  dplyr::case_when(
    n_completed == 0 ~ "FAILED",
    !partial_run & n_completed < K ~ "LOW_RELIABILITY",
    divergences_total > 0 | treedepth_warnings_total > 0 ~ "LOW_RELIABILITY",
    !is.na(max_rhat) & !is.na(min_ess_bulk) & !is.na(min_ess_tail) & max_rhat <= 1.01 & min_ess_bulk >= 400 & min_ess_tail >= 400 ~ "OK",
    !is.na(max_rhat) & !is.na(min_ess_bulk) & !is.na(min_ess_tail) & max_rhat <= 1.05 & min_ess_bulk >= 100 & min_ess_tail >= 100 ~ "CAUTION",
    TRUE ~ "LOW_RELIABILITY"
  )
}

grouped_fold_scores <- grouped_obs %>%
  group_by(.data$Target_Space, .data$Sample_Group, .data$Fold_ID, .data$Model_ID, .data$Model_Name, .data$Heterogeneity_Variant) %>%
  summarise(
    N_Test_Obs = n(),
    N_Test_Firms = n_distinct(.data$company),
    elpd_fold = sum(.data$lpd_obs, na.rm = TRUE),
    mean_lpd_obs = mean(.data$lpd_obs, na.rm = TRUE),
    .groups = "drop"
  )

grouped_model_scores <- fold_diagnostics %>%
  filter(.data$Validation_Scheme == "grouped_firm_kfold") %>%
  group_by(.data$Validation_Scheme, .data$Target_Space, .data$Sample_Group, .data$Model_ID, .data$Model_Name, .data$Heterogeneity_Variant) %>%
  summarise(
    N_Folds_Attempted = n(),
    N_Folds_Completed = sum(.data$Completed, na.rm = TRUE),
    max_rhat_max = max(.data$Max_Rhat, na.rm = TRUE),
    min_ess_bulk = min(.data$Min_ESS_Bulk, na.rm = TRUE),
    min_ess_tail = min(.data$Min_ESS_Tail, na.rm = TRUE),
    ess_warning_any = any(.data$ESS_Warning %in% c(TRUE, "TRUE", 1L), na.rm = TRUE),
    divergences_total = sum(.data$Divergences, na.rm = TRUE),
    treedepth_warnings_total = sum(.data$Treedepth_Warnings, na.rm = TRUE),
    Runtime_Seconds = sum(.data$Runtime_Seconds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    grouped_fold_scores %>%
      group_by(.data$Target_Space, .data$Sample_Group, .data$Model_ID, .data$Model_Name, .data$Heterogeneity_Variant) %>%
      summarise(
        N_Test_Obs_Total = sum(.data$N_Test_Obs),
        N_Test_Firms_Total = sum(.data$N_Test_Firms),
        elpd_kfold = sum(.data$elpd_fold),
        mean_lpd_obs = weighted.mean(.data$mean_lpd_obs, .data$N_Test_Obs),
        .groups = "drop"
      ),
    by = c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant")
  ) %>%
  mutate(
    max_rhat_max = ifelse(is.infinite(.data$max_rhat_max), NA_real_, .data$max_rhat_max),
    min_ess_bulk = ifelse(is.infinite(.data$min_ess_bulk), NA_real_, .data$min_ess_bulk),
    min_ess_tail = ifelse(is.infinite(.data$min_ess_tail), NA_real_, .data$min_ess_tail)
  ) %>%
  left_join(K_by_scheme, by = c("Validation_Scheme" = "Validation_Scheme")) %>%
  mutate(
    reliability_flag = reliability_label(.data$N_Folds_Completed, .data$K, .data$divergences_total,
                                         .data$treedepth_warnings_total, .data$max_rhat_max,
                                         .data$min_ess_bulk, .data$min_ess_tail),
    included_in_stack = .data$reliability_flag %in% c("OK", "CAUTION") & .data$N_Folds_Completed == .data$K
  )
write_csv_safely(grouped_model_scores, file.path(tables_dir, "table_se08_grouped_fold_local_model_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")

row_included <- row_obs[row_obs$primary_row_target_inclusion %in% c(TRUE, "TRUE", 1L), , drop = FALSE]
row_model_scores <- row_included %>%
  group_by(.data$target_space, .data$sample_group, .data$model_id, .data$model_name, .data$heterogeneity_variant) %>%
  summarise(
    n_obs_scored = n(),
    elpd_exact_row_kfold = sum(.data$log_predictive_density, na.rm = TRUE),
    mean_lpd = mean(.data$log_predictive_density, na.rm = TRUE),
    sd_lpd = stats::sd(.data$log_predictive_density, na.rm = TRUE),
    n_new_company_excluded_from_primary = sum(.data$new_company_in_row_fold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    fold_diagnostics %>%
      filter(.data$Validation_Scheme == "row_exact_kfold") %>%
      group_by(.data$Target_Space, .data$Sample_Group, .data$Model_ID, .data$Model_Name, .data$Heterogeneity_Variant) %>%
      summarise(
        n_folds_attempted = n(),
        n_folds_completed = sum(.data$Completed, na.rm = TRUE),
        max_rhat_max = max(.data$Max_Rhat, na.rm = TRUE),
        min_ess_bulk_min = min(.data$Min_ESS_Bulk, na.rm = TRUE),
        min_ess_tail_min = min(.data$Min_ESS_Tail, na.rm = TRUE),
        divergences_total = sum(.data$Divergences, na.rm = TRUE),
        treedepth_warnings_total = sum(.data$Treedepth_Warnings, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("target_space" = "Target_Space", "sample_group" = "Sample_Group",
           "model_id" = "Model_ID", "model_name" = "Model_Name",
           "heterogeneity_variant" = "Heterogeneity_Variant")
  ) %>%
  mutate(
    max_rhat_max = ifelse(is.infinite(.data$max_rhat_max), NA_real_, .data$max_rhat_max),
    min_ess_bulk_min = ifelse(is.infinite(.data$min_ess_bulk_min), NA_real_, .data$min_ess_bulk_min),
    min_ess_tail_min = ifelse(is.infinite(.data$min_ess_tail_min), NA_real_, .data$min_ess_tail_min),
    reliability_flag = reliability_label(.data$n_folds_completed, max(K_by_scheme$K[K_by_scheme$Validation_Scheme == "row_exact_kfold"]),
                                         .data$divergences_total, .data$treedepth_warnings_total,
                                         .data$max_rhat_max, .data$min_ess_bulk_min, .data$min_ess_tail_min),
    included_in_stack = .data$reliability_flag %in% c("OK", "CAUTION") &
      .data$n_folds_completed == max(K_by_scheme$K[K_by_scheme$Validation_Scheme == "row_exact_kfold"]),
    refit_type = "fold_local_exact_refit",
    validation_unit = "row_level",
    primary_row_target_excludes_new_company_rows = TRUE
  )
write_csv_safely(row_model_scores, file.path(tables_dir, "table_se08_row_fold_local_model_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")

build_grouped_weights <- function(target_space) {
  included <- grouped_model_scores %>%
    filter(.data$Target_Space == target_space, .data$Sample_Group == "main_common", .data$included_in_stack == TRUE) %>%
    arrange(.data$Model_ID, .data$Heterogeneity_Variant)
  if (!nrow(included)) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, ]
    key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_se08_grouped_fold_local")
    one <- grouped_obs %>%
      filter(.data$Target_Space == target_space, .data$Sample_Group == row$Sample_Group,
             .data$Model_ID == row$Model_ID, .data$Heterogeneity_Variant == row$Heterogeneity_Variant) %>%
      arrange(.data$company, .data$year)
    score_list[[key]] <- one$lpd_obs
    meta_keys <- c(meta_keys, key)
  }
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) stop("[BLOCKER] se08 grouped score vectors have unequal lengths for ", target_space)
  lpd_matrix <- do.call(cbind, score_list)
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  meta_idx <- match(names(weights), meta_keys)
  included[meta_idx, ] %>%
    mutate(
      Model_Key_Fold_Local = names(weights),
      Weight_Fold_Local = as.numeric(weights),
      Singleton_ELPD = as.numeric(colSums(lpd_matrix)[names(weights)]),
      Rank_Fold_Local = rank(-as.numeric(weights), ties.method = "first")
    ) %>%
    arrange(.data$Rank_Fold_Local)
}

build_row_weights <- function(target_space) {
  included <- row_model_scores %>%
    filter(.data$target_space == target_space, .data$sample_group == "main_common", .data$included_in_stack == TRUE) %>%
    arrange(.data$model_id, .data$heterogeneity_variant)
  if (!nrow(included)) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, ]
    key <- model_key_sampled(row$model_id, row$target_space, row$sample_group, row$heterogeneity_variant, "_se08_row_fold_local")
    one <- row_included %>%
      filter(.data$target_space == target_space, .data$model_id == row$model_id,
             .data$heterogeneity_variant == row$heterogeneity_variant) %>%
      arrange(.data$observation_id)
    score_list[[key]] <- one$log_predictive_density
    meta_keys <- c(meta_keys, key)
  }
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) stop("[BLOCKER] se08 row score vectors have unequal lengths for ", target_space)
  lpd_matrix <- do.call(cbind, score_list)
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  meta_idx <- match(names(weights), meta_keys)
  included[meta_idx, ] %>%
    mutate(
      model_key_fold_local = names(weights),
      weight_fold_local = as.numeric(weights),
      singleton_elpd = as.numeric(colSums(lpd_matrix)[names(weights)]),
      rank_fold_local = rank(-as.numeric(weights), ties.method = "first")
    ) %>%
    arrange(.data$rank_fold_local)
}

grouped_ep <- build_grouped_weights("ex_post")
grouped_rt <- build_grouped_weights("real_time")
row_ep <- build_row_weights("ex_post")
row_rt <- build_row_weights("real_time")
write_csv_safely(grouped_ep, file.path(tables_dir, "table_se08_grouped_fold_local_weights_ex_post.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(grouped_rt, file.path(tables_dir, "table_se08_grouped_fold_local_weights_no_lookahead.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(row_ep, file.path(tables_dir, "table_se08_row_fold_local_weights_ex_post.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(row_rt, file.path(tables_dir, "table_se08_row_fold_local_weights_no_lookahead.csv"), row.names = FALSE, fileEncoding = "UTF-8")

read_pin_root <- function(kind) {
  pin <- file.path(output_root, if (identical(kind, "grouped_firm")) "kfold_firm" else "row_exact_kfold", "LATEST_COMPLETED_RUN.txt")
  if (!file.exists(pin)) return(NA_character_)
  trimws(readLines(pin, warn = FALSE)[1])
}

read_primary_weights <- function(kind, target_space) {
  root <- read_pin_root(kind)
  file_name <- if (identical(kind, "grouped_firm")) {
    if (identical(target_space, "ex_post")) "table_winsor_kfold_weights_ex_post.csv" else "table_winsor_kfold_weights_no_lookahead.csv"
  } else {
    if (identical(target_space, "ex_post")) "table_winsor_row_exact_kfold_weights_ex_post.csv" else "table_winsor_row_exact_kfold_weights_no_lookahead.csv"
  }
  candidates <- c(file.path(root, "tables", file_name), file.path(output_root, "tables", file_name))
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path) || !nzchar(path)) return(data.frame())
  x <- read.csv(path, stringsAsFactors = FALSE)
  x$.source_path <- path
  x
}

standardize_weight_table <- function(x, validation_scheme, target_space, source_type) {
  if (!nrow(x)) return(data.frame())
  model_col <- intersect(c("Model_ID", "model_id"), names(x))[1]
  name_col <- intersect(c("Model_Name", "model_name"), names(x))[1]
  variant_col <- intersect(c("Heterogeneity_Variant", "heterogeneity_variant"), names(x))[1]
  weight_col <- intersect(c("Weight_KFold", "weight_row_exact_kfold", "Weight_Fold_Local", "weight_fold_local"), names(x))[1]
  rank_col <- intersect(c("Rank_KFold", "rank_row_exact_kfold", "Rank_Fold_Local", "rank_fold_local"), names(x))[1]
  data.frame(
    validation_scheme = validation_scheme,
    target_space = target_space,
    source_type = source_type,
    model_id = x[[model_col]],
    model_name = if (!is.na(name_col)) x[[name_col]] else x[[model_col]],
    heterogeneity_variant = x[[variant_col]],
    weight = suppressWarnings(as.numeric(x[[weight_col]])),
    rank = if (!is.na(rank_col)) suppressWarnings(as.integer(x[[rank_col]])) else rank(-suppressWarnings(as.numeric(x[[weight_col]])), ties.method = "first"),
    source_path = if (".source_path" %in% names(x)) x$.source_path else se08_root,
    stringsAsFactors = FALSE
  )
}

weight_long <- bind_rows(
  standardize_weight_table(read_primary_weights("grouped_firm", "ex_post"), "grouped_firm_kfold", "ex_post", "global"),
  standardize_weight_table(read_primary_weights("grouped_firm", "real_time"), "grouped_firm_kfold", "real_time", "global"),
  standardize_weight_table(read_primary_weights("row", "ex_post"), "row_exact_kfold", "ex_post", "global"),
  standardize_weight_table(read_primary_weights("row", "real_time"), "row_exact_kfold", "real_time", "global"),
  standardize_weight_table(grouped_ep, "grouped_firm_kfold", "ex_post", "fold_local"),
  standardize_weight_table(grouped_rt, "grouped_firm_kfold", "real_time", "fold_local"),
  standardize_weight_table(row_ep, "row_exact_kfold", "ex_post", "fold_local"),
  standardize_weight_table(row_rt, "row_exact_kfold", "real_time", "fold_local")
)

weight_comparison <- weight_long %>%
  select("validation_scheme", "target_space", "model_id", "model_name", "heterogeneity_variant", "source_type", "weight") %>%
  tidyr::pivot_wider(names_from = "source_type", values_from = "weight") %>%
  mutate(
    absolute_difference = .data$fold_local - .data$global,
    relative_difference = .data$absolute_difference / pmax(abs(.data$global), .Machine$double.eps)
  )
write_csv_safely(weight_comparison, file.path(tables_dir, "table_se08_fold_local_vs_global_weight_comparison.csv"), row.names = FALSE, fileEncoding = "UTF-8")

firmre_summary <- weight_long %>%
  mutate(is_firm_re = grepl("Firm RE|Random Intercept", .data$heterogeneity_variant, ignore.case = TRUE)) %>%
  group_by(.data$target_space, .data$validation_scheme, .data$source_type) %>%
  summarise(firmre_weight = sum(.data$weight[.data$is_firm_re], na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = "validation_scheme", values_from = "firmre_weight") %>%
  mutate(
    row_minus_grouped_firmre_shift = .data$row_exact_kfold - .data$grouped_firm_kfold,
    row_over_grouped_firmre_ratio = .data$row_exact_kfold / pmax(.data$grouped_firm_kfold, .Machine$double.eps)
  ) %>%
  select("target_space", "source_type", "grouped_firm_kfold", "row_exact_kfold",
         "row_minus_grouped_firmre_shift", "row_over_grouped_firmre_ratio") %>%
  tidyr::pivot_wider(
    names_from = "source_type",
    values_from = c("grouped_firm_kfold", "row_exact_kfold", "row_minus_grouped_firmre_shift", "row_over_grouped_firmre_ratio")
  ) %>%
  mutate(
    shift_absolute_difference = .data$row_minus_grouped_firmre_shift_fold_local - .data$row_minus_grouped_firmre_shift_global,
    shift_relative_to_global = .data$row_minus_grouped_firmre_shift_fold_local / pmax(abs(.data$row_minus_grouped_firmre_shift_global), .Machine$double.eps)
  )
write_csv_safely(firmre_summary, file.path(tables_dir, "table_se08_fold_local_vs_global_firmre_shift_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

top_model_comparison <- weight_long %>%
  group_by(.data$target_space, .data$validation_scheme, .data$source_type) %>%
  arrange(.data$rank, desc(.data$weight), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(top_model_key = paste(.data$model_id, .data$heterogeneity_variant, sep = " | "),
         top_heterogeneity_axis = ifelse(grepl("Firm RE|Random Intercept", .data$heterogeneity_variant, ignore.case = TRUE), "firm_re", "pooled_or_cross_sectional")) %>%
  select("target_space", "validation_scheme", "source_type", "top_model_key", "top_heterogeneity_axis", "weight") %>%
  tidyr::pivot_wider(names_from = "source_type", values_from = c("top_model_key", "top_heterogeneity_axis", "weight")) %>%
  mutate(
    top_model_same = .data$top_model_key_global == .data$top_model_key_fold_local,
    heterogeneity_axis_same = .data$top_heterogeneity_axis_global == .data$top_heterogeneity_axis_fold_local
  )
write_csv_safely(top_model_comparison, file.path(tables_dir, "table_se08_fold_local_vs_global_top_model_comparison.csv"), row.names = FALSE, fileEncoding = "UTF-8")

decision_rows <- lapply(seq_len(nrow(firmre_summary)), function(i) {
  row <- firmre_summary[i, ]
  target_space <- row$target_space
  global_shift <- row$row_minus_grouped_firmre_shift_global
  fold_shift <- row$row_minus_grouped_firmre_shift_fold_local
  rel <- fold_shift / pmax(abs(global_shift), .Machine$double.eps)
  top_target <- top_model_comparison[top_model_comparison$target_space == target_space, , drop = FALSE]
  grouped_axis <- top_target[top_target$validation_scheme == "grouped_firm_kfold", , drop = FALSE]
  row_axis <- top_target[top_target$validation_scheme == "row_exact_kfold", , drop = FALSE]
  shift_decision <- if (is.na(fold_shift) || is.na(global_shift)) {
    "FAIL"
  } else if (fold_shift <= 0 || abs(fold_shift) < 1e-8) {
    "FAIL"
  } else if (rel < 0.70) {
    "WARN"
  } else {
    "PASS"
  }
  axis_decision <- if (nrow(grouped_axis) && nrow(row_axis) &&
                       grouped_axis$top_heterogeneity_axis_fold_local %in% c("pooled_or_cross_sectional") &&
                       row_axis$top_heterogeneity_axis_fold_local %in% c("firm_re")) {
    "PASS"
  } else if (nrow(grouped_axis) && nrow(row_axis) &&
             grouped_axis$top_heterogeneity_axis_fold_local == grouped_axis$top_heterogeneity_axis_global &&
             row_axis$top_heterogeneity_axis_fold_local == row_axis$top_heterogeneity_axis_global) {
    "PASS"
  } else {
    "WARN"
  }
  data.frame(
    decision_id = c(paste0(target_space, "_firmre_shift"), paste0(target_space, "_top_model_axis")),
    target_space = target_space,
    metric = c("row_minus_grouped_Firm_RE_shift", "top_model_heterogeneity_axis"),
    global_value = c(global_shift, paste(top_target$top_heterogeneity_axis_global, collapse = ";")),
    fold_local_value = c(fold_shift, paste(top_target$top_heterogeneity_axis_fold_local, collapse = ";")),
    absolute_difference = c(fold_shift - global_shift, NA_real_),
    relative_difference = c(rel, NA_real_),
    decision = c(shift_decision, axis_decision),
    interpretation = c(
      "PASS if row-minus-grouped Firm-RE shift remains positive and at least 70% of the global-preprocessing shift.",
      "PASS if the grouped-vs-row pooling/firm-specificity conclusion remains substantively unchanged."
    ),
    stringsAsFactors = FALSE
  )
})
decision <- bind_rows(decision_rows)
write_csv_safely(decision, file.path(tables_dir, "table_se08_fold_local_sensitivity_decision.csv"), row.names = FALSE, fileEncoding = "UTF-8")

manifest_row <- data.frame(
  Script_Name = "scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R",
  Sensitivity_Root = se08_root,
  N_Tasks = nrow(manifest),
  N_Completed = sum(status$status == "SUCCESS"),
  Decision_Overall = if (any(decision$decision == "FAIL")) "FAIL" else if (any(decision$decision == "WARN")) "WARN" else "PASS",
  Output_Tables_Dir = tables_dir,
  stringsAsFactors = FALSE
)
write_csv_safely(manifest_row, file.path(logs_dir, "se08_fold_local_preprocessing_collect_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")

message("se08c collected fold-local preprocessing sensitivity outputs.")
phase_end("se08c", "Collect fold-local preprocessing sensitivity")
