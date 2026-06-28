# Script: se08d_construct_fold_local_DA_reclassification.R
# Purpose: Construct fold-local DA objects and RQ2 reclassification sensitivity.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("se08d", "Construct fold-local DA and RQ2 reclassification sensitivity")

script_name <- "scripts/sensitivity/se08d_construct_fold_local_DA_reclassification.R"
script_version <- "fold-local-da-rq2-v1"

se08_root <- file.path(output_root, "sensitivity", "fold_local_preprocessing")
tables_dir <- file.path(se08_root, "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  grouped_scores = file.path(tables_dir, "table_se08_grouped_fold_local_observation_scores.csv"),
  row_scores = file.path(tables_dir, "table_se08_row_fold_local_observation_scores.csv"),
  grouped_weights_ex_post = file.path(tables_dir, "table_se08_grouped_fold_local_weights_ex_post.csv"),
  grouped_weights_rt = file.path(tables_dir, "table_se08_grouped_fold_local_weights_no_lookahead.csv"),
  row_weights_ex_post = file.path(tables_dir, "table_se08_row_fold_local_weights_ex_post.csv"),
  row_weights_rt = file.path(tables_dir, "table_se08_row_fold_local_weights_no_lookahead.csv"),
  global_jaccard = file.path(output_root, "diagnostics", "table_exact_kfold_reclassification_jaccard.csv")
)

must_read_csv <- function(path) {
  if (!file.exists(path)) stop("[BLOCKER] se08d missing required input: ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

optional_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
md5_or_na <- function(path) if (file.exists(path)) tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_) else NA_character_

standardize_grouped_scores <- function(x) {
  required <- c("Target_Space", "Sample_Group", "Fold_ID", "Obs_ID", "company", "year", "Model_ID",
                "Model_Name", "Heterogeneity_Variant", "y_actual", "lpd_obs", "pred_mean", "pred_sd")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("[BLOCKER] se08d grouped scores lack required columns: ", paste(missing, collapse = ", "))
  data.frame(
    validation_scheme = "grouped_firm_kfold",
    target_space = x$Target_Space,
    sample_group = x$Sample_Group,
    fold_id = as.integer(x$Fold_ID),
    observation_id = x$Obs_ID,
    company = as.character(x$company),
    year = x$year,
    model_id = x$Model_ID,
    model_name = x$Model_Name,
    heterogeneity_variant = x$Heterogeneity_Variant,
    observed_TA_scaled = suppressWarnings(as.numeric(x$y_actual)),
    log_predictive_density = suppressWarnings(as.numeric(x$lpd_obs)),
    pred_mean = suppressWarnings(as.numeric(x$pred_mean)),
    pred_sd = suppressWarnings(as.numeric(x$pred_sd)),
    prediction_rule = if ("Prediction_Rule" %in% names(x)) x$Prediction_Rule else NA_character_,
    same_firm_history_available = NA,
    new_company_in_row_fold = NA,
    primary_row_target_inclusion = TRUE,
    stringsAsFactors = FALSE
  )
}

standardize_row_scores <- function(x) {
  required <- c("target_space", "sample_group", "fold", "observation_id", "company", "year", "model_id",
                "model_name", "heterogeneity_variant", "observed_TA_scaled", "log_predictive_density", "pred_mean", "pred_sd")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("[BLOCKER] se08d row scores lack required columns: ", paste(missing, collapse = ", "))
  data.frame(
    validation_scheme = "row_exact_kfold",
    target_space = x$target_space,
    sample_group = x$sample_group,
    fold_id = as.integer(x$fold),
    observation_id = x$observation_id,
    company = as.character(x$company),
    year = x$year,
    model_id = x$model_id,
    model_name = x$model_name,
    heterogeneity_variant = x$heterogeneity_variant,
    observed_TA_scaled = suppressWarnings(as.numeric(x$observed_TA_scaled)),
    log_predictive_density = suppressWarnings(as.numeric(x$log_predictive_density)),
    pred_mean = suppressWarnings(as.numeric(x$pred_mean)),
    pred_sd = suppressWarnings(as.numeric(x$pred_sd)),
    prediction_rule = if ("prediction_rule" %in% names(x)) x$prediction_rule else NA_character_,
    same_firm_history_available = if ("same_firm_history_available" %in% names(x)) x$same_firm_history_available else NA,
    new_company_in_row_fold = if ("new_company_in_row_fold" %in% names(x)) x$new_company_in_row_fold else NA,
    primary_row_target_inclusion = if ("primary_row_target_inclusion" %in% names(x)) x$primary_row_target_inclusion else TRUE,
    stringsAsFactors = FALSE
  )
}

standardize_weights <- function(x, validation_scheme, target_space) {
  if (!nrow(x)) return(data.frame())
  model_col <- intersect(c("Model_ID", "model_id"), names(x))[1]
  name_col <- intersect(c("Model_Name", "model_name"), names(x))[1]
  sample_col <- intersect(c("Sample_Group", "sample_group"), names(x))[1]
  variant_col <- intersect(c("Heterogeneity_Variant", "heterogeneity_variant"), names(x))[1]
  weight_col <- intersect(c("Weight_Fold_Local", "weight_fold_local"), names(x))[1]
  if (any(is.na(c(model_col, variant_col, weight_col)))) stop("[BLOCKER] se08d cannot standardize fold-local weights for ", validation_scheme, " / ", target_space)
  out <- data.frame(
    validation_scheme = validation_scheme,
    target_space = target_space,
    sample_group = if (!is.na(sample_col)) x[[sample_col]] else "main_common",
    model_id = x[[model_col]],
    model_name = if (!is.na(name_col)) x[[name_col]] else x[[model_col]],
    heterogeneity_variant = x[[variant_col]],
    weight = suppressWarnings(as.numeric(x[[weight_col]])),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$weight) & out$weight > 1e-8, , drop = FALSE]
  if (nrow(out)) out$weight <- out$weight / sum(out$weight)
  out
}

collapse_text <- function(x) {
  x <- unique(as.character(x[!is.na(x)]))
  if (!length(x)) return(NA_character_)
  paste(x, collapse = ";")
}

construct_da <- function(scores, weights, validation_scheme) {
  active <- weights[weights$validation_scheme == validation_scheme, , drop = FALSE]
  if (!nrow(active)) stop("[BLOCKER] se08d has no active fold-local weights for ", validation_scheme)
  x <- scores %>%
    inner_join(active, by = c("validation_scheme", "target_space", "sample_group", "model_id", "model_name", "heterogeneity_variant"))
  if (!nrow(x)) stop("[BLOCKER] se08d cannot join scores to weights for ", validation_scheme)
  x %>%
    group_by(.data$validation_scheme, .data$target_space, .data$sample_group,
             .data$company, .data$year, .data$observation_id, .data$fold_id) %>%
    summarise(
      observed_TA_scaled = first(.data$observed_TA_scaled),
      stacked_pred_mean = ifelse(any(!is.finite(.data$pred_mean)), NA_real_, sum(.data$weight * .data$pred_mean)),
      n_models = n(),
      n_missing_pred_mean = sum(!is.finite(.data$pred_mean)),
      n_missing_pred_sd = sum(!is.finite(.data$pred_sd)),
      n_nonpositive_pred_sd = sum(is.finite(.data$pred_sd) & .data$pred_sd <= 0),
      stacked_pred_second_moment = ifelse(any(!is.finite(.data$pred_sd) | !is.finite(.data$pred_mean)),
                                          NA_real_,
                                          sum(.data$weight * (.data$pred_sd^2 + .data$pred_mean^2))),
      prediction_rule = collapse_text(.data$prediction_rule),
      same_firm_history_available = collapse_text(.data$same_firm_history_available),
      new_company_in_row_fold = collapse_text(.data$new_company_in_row_fold),
      primary_row_target_inclusion = collapse_text(.data$primary_row_target_inclusion),
      .groups = "drop"
    ) %>%
    mutate(
      stacked_pred_var = .data$stacked_pred_second_moment - .data$stacked_pred_mean^2,
      stacked_pred_sd = sqrt(pmax(.data$stacked_pred_var, 0)),
      stacked_pred_sd = ifelse(.data$n_missing_pred_sd > 0 | !is.finite(.data$stacked_pred_sd) | .data$stacked_pred_sd <= 0, NA_real_, .data$stacked_pred_sd),
      DA_raw = .data$observed_TA_scaled - .data$stacked_pred_mean,
      DA_z_estimation = .data$DA_raw / .data$stacked_pred_sd,
      Script_Name = script_name,
      Script_Version = script_version
    )
}

finite_gate <- function(df) {
  df %>%
    group_by(.data$validation_scheme, .data$target_space) %>%
    summarise(
      n_observations = n(),
      n_finite_DA_raw = sum(is.finite(.data$DA_raw)),
      n_finite_DA_z_estimation = sum(is.finite(.data$DA_z_estimation)),
      n_missing_pred_mean = sum(.data$n_missing_pred_mean > 0 | !is.finite(.data$stacked_pred_mean)),
      n_missing_pred_sd = sum(.data$n_missing_pred_sd > 0 | !is.finite(.data$stacked_pred_sd)),
      n_nonpositive_pred_sd = sum(.data$n_nonpositive_pred_sd > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      share_finite_DA_raw = .data$n_finite_DA_raw / .data$n_observations,
      share_finite_DA_z_estimation = .data$n_finite_DA_z_estimation / .data$n_observations,
      decision = dplyr::case_when(
        .data$n_finite_DA_raw == 0 ~ "FAIL",
        .data$share_finite_DA_raw < 0.999 ~ "FAIL",
        .data$share_finite_DA_z_estimation < 0.90 ~ "WARN",
        .data$n_missing_pred_sd > 0 | .data$n_nonpositive_pred_sd > 0 ~ "WARN",
        TRUE ~ "PASS"
      )
    )
}

rank_top <- function(keys, score, top_n) {
  ord <- order(-score, keys$company, keys$year)
  rank <- integer(length(score))
  rank[ord] <- seq_along(score)
  rank <= top_n
}

matched_jaccard <- function(joined, target_space, metric) {
  gcol <- paste0(metric, "_grouped")
  rcol <- paste0(metric, "_row")
  target <- joined[joined$target_space == target_space, , drop = FALSE]
  row_score <- abs(suppressWarnings(as.numeric(target[[rcol]])))
  grouped_score <- abs(suppressWarnings(as.numeric(target[[gcol]])))
  keep <- is.finite(row_score) & is.finite(grouped_score)
  target <- target[keep, , drop = FALSE]
  row_score <- row_score[keep]
  grouped_score <- grouped_score[keep]
  N <- nrow(target)
  if (!N) return(list(summary = data.frame(), sets = data.frame()))
  top_n <- max(1L, as.integer(ceiling(0.05 * N)))
  keys <- data.frame(company = as.character(target$company), year = target$year, stringsAsFactors = FALSE)
  row_flag <- rank_top(keys, row_score, top_n)
  grouped_flag <- rank_top(keys, grouped_score, top_n)
  intersection <- sum(row_flag & grouped_flag)
  union <- sum(row_flag | grouped_flag)
  only_row <- sum(row_flag & !grouped_flag)
  only_grouped <- sum(!row_flag & grouped_flag)
  spearman <- if (N >= 2) suppressWarnings(stats::cor(row_score, grouped_score, method = "spearman", use = "complete.obs")) else NA_real_
  metric_class <- if (identical(metric, "DA_raw")) "primary_magnitude_raw" else "primary_magnitude_estimation_scaled"
  summary <- data.frame(
    target_space = target_space,
    score_variable = paste0("abs(", metric, ")"),
    source_score_variable = metric,
    score_transform = "absolute_value",
    reported_score_variable = paste0("abs(", metric, ")"),
    metric_class = metric_class,
    Primary_Inference_Allowed = TRUE,
    N_joined = N,
    top_n = top_n,
    effective_top_share = top_n / N,
    row_top_n = sum(row_flag),
    grouped_top_n = sum(grouped_flag),
    intersection = intersection,
    union = union,
    intersection_n = intersection,
    union_n = union,
    only_row = only_row,
    only_grouped = only_grouped,
    only_row_n = only_row,
    only_grouped_n = only_grouped,
    neither_flagged_n = sum(!row_flag & !grouped_flag),
    jaccard = if (union > 0) intersection / union else NA_real_,
    switch_rate = (only_row + only_grouped) / N,
    Spearman = spearman,
    spearman_rank_correlation = spearman,
    decision_label = dplyr::case_when(
      is.na(if (union > 0) intersection / union else NA_real_) ~ "FAIL_NO_COMPARABLE_SET",
      (intersection / union) < 0.60 ~ "HIGH_MATERIAL_TOP_TAIL_TURNOVER",
      (intersection / union) < 0.80 ~ "MATERIAL_TOP_TAIL_TURNOVER",
      TRUE ~ "LOW_TOP_TAIL_TURNOVER"
    ),
    interpretation = "Matched-size top-5% row-vs-grouped fold-local DA object comparison.",
    stringsAsFactors = FALSE
  )
  sets <- data.frame(
    target_space = target_space,
    score_variable = paste0("abs(", metric, ")"),
    source_score_variable = metric,
    company = keys$company,
    year = keys$year,
    row_score = row_score,
    grouped_score = grouped_score,
    row_top5_flag = row_flag,
    grouped_top5_flag = grouped_flag,
    membership_class = dplyr::case_when(
      row_flag & grouped_flag ~ "both",
      row_flag & !grouped_flag ~ "row_only",
      !row_flag & grouped_flag ~ "grouped_only",
      TRUE ~ "neither"
    ),
    stringsAsFactors = FALSE
  )
  list(summary = summary, sets = sets)
}

grouped_scores <- standardize_grouped_scores(must_read_csv(paths$grouped_scores))
row_scores <- standardize_row_scores(must_read_csv(paths$row_scores))
weights <- bind_rows(
  standardize_weights(must_read_csv(paths$grouped_weights_ex_post), "grouped_firm_kfold", "ex_post"),
  standardize_weights(must_read_csv(paths$grouped_weights_rt), "grouped_firm_kfold", "real_time"),
  standardize_weights(must_read_csv(paths$row_weights_ex_post), "row_exact_kfold", "ex_post"),
  standardize_weights(must_read_csv(paths$row_weights_rt), "row_exact_kfold", "real_time")
)
if (!nrow(weights)) stop("[BLOCKER] se08d found no active fold-local weights.")

grouped_da <- construct_da(grouped_scores, weights, "grouped_firm_kfold")
row_da <- construct_da(row_scores, weights, "row_exact_kfold")
grouped_out_path <- file.path(tables_dir, "final_se08_fold_local_uncertainty_adjusted_accruals_grouped.csv")
row_out_path <- file.path(tables_dir, "final_se08_fold_local_uncertainty_adjusted_accruals_row.csv")
write_csv_safely(grouped_da, grouped_out_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(row_da, row_out_path, row.names = FALSE, fileEncoding = "UTF-8")

source_manifest <- data.frame(
  artifact = c("grouped_scores", "row_scores", "grouped_weights_ex_post", "grouped_weights_no_lookahead",
               "row_weights_ex_post", "row_weights_no_lookahead", "grouped_da", "row_da"),
  path = c(paths$grouped_scores, paths$row_scores, paths$grouped_weights_ex_post, paths$grouped_weights_rt,
           paths$row_weights_ex_post, paths$row_weights_rt, grouped_out_path, row_out_path),
  exists = file.exists(c(paths$grouped_scores, paths$row_scores, paths$grouped_weights_ex_post, paths$grouped_weights_rt,
                         paths$row_weights_ex_post, paths$row_weights_rt, grouped_out_path, row_out_path)),
  file_size = vapply(c(paths$grouped_scores, paths$row_scores, paths$grouped_weights_ex_post, paths$grouped_weights_rt,
                       paths$row_weights_ex_post, paths$row_weights_rt, grouped_out_path, row_out_path), file_size_or_na, numeric(1)),
  mtime = vapply(c(paths$grouped_scores, paths$row_scores, paths$grouped_weights_ex_post, paths$grouped_weights_rt,
                   paths$row_weights_ex_post, paths$row_weights_rt, grouped_out_path, row_out_path), mtime_or_na, character(1)),
  md5 = vapply(c(paths$grouped_scores, paths$row_scores, paths$grouped_weights_ex_post, paths$grouped_weights_rt,
                 paths$row_weights_ex_post, paths$row_weights_rt, grouped_out_path, row_out_path), md5_or_na, character(1)),
  script_name = script_name,
  script_version = script_version,
  stringsAsFactors = FALSE
)
write_csv_safely(source_manifest, file.path(tables_dir, "table_se08_fold_local_DA_source_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")

finite <- bind_rows(finite_gate(grouped_da), finite_gate(row_da))
write_csv_safely(finite, file.path(tables_dir, "table_se08_fold_local_DA_finite_gate.csv"), row.names = FALSE, fileEncoding = "UTF-8")

join_cols <- c("target_space", "company", "year")
joined <- grouped_da %>%
  select("target_space", "company", "year", "DA_raw", "DA_z_estimation") %>%
  rename(DA_raw_grouped = "DA_raw", DA_z_estimation_grouped = "DA_z_estimation") %>%
  inner_join(
    row_da %>%
      select("target_space", "company", "year", "DA_raw", "DA_z_estimation") %>%
      rename(DA_raw_row = "DA_raw", DA_z_estimation_row = "DA_z_estimation"),
    by = join_cols
  )

metric_results <- list()
for (space in sort(unique(joined$target_space))) {
  for (metric in c("DA_raw", "DA_z_estimation")) {
    metric_results[[length(metric_results) + 1L]] <- matched_jaccard(joined, space, metric)
  }
}
jaccard <- bind_rows(lapply(metric_results, `[[`, "summary"))
sets <- bind_rows(lapply(metric_results, `[[`, "sets"))
write_csv_safely(jaccard, file.path(tables_dir, "table_se08_fold_local_reclassification_jaccard.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(sets, file.path(tables_dir, "table_se08_fold_local_top_tail_membership_sets.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(
  jaccard[, intersect(c("target_space", "score_variable", "source_score_variable", "Spearman", "spearman_rank_correlation", "jaccard"), names(jaccard)), drop = FALSE],
  file.path(tables_dir, "table_se08_fold_local_spearman_rank_correlation.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

global_j <- must_read_csv(paths$global_jaccard)
comparison <- build_se08d_rq2_global_fold_local_comparison(global_j, jaccard)
write_csv_safely(comparison, file.path(tables_dir, "table_se08_fold_local_vs_global_reclassification_comparison.csv"), row.names = FALSE, fileEncoding = "UTF-8")

decision <- decide_se08d_rq2_global_fold_local(comparison)
write_csv_safely(decision, file.path(tables_dir, "table_se08_fold_local_RQ2_decision.csv"), row.names = FALSE, fileEncoding = "UTF-8")

message("se08d constructed fold-local DA and RQ2 reclassification sensitivity outputs.")
phase_end("se08d", "Construct fold-local DA and RQ2 reclassification sensitivity")
