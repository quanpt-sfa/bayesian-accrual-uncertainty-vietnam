# -----------------------------------------------------------------------------
# Script: di03_exact_kfold_reclassification_audit.R
# Purpose: Audit RQ2 validation-target reclassification by comparing exact
#          row-level K-fold DA against exact grouped-firm K-fold DA.
#
# Intended use:
#   Rscript scripts/diagnostics/di03_exact_kfold_reclassification_audit.R
#
# This is an artifact-only diagnostic script. It does not fit or refit models.
# Primary RQ2 evidence is restricted to matched-size top-5% Jaccard overlap for
# absolute abnormal accrual magnitude, not posterior tail flags.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di03", "Exact K-fold reclassification audit")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

script_start_time <- Sys.time()
script_name <- "scripts/diagnostics/di03_exact_kfold_reclassification_audit.R"
script_version <- "2026-06-22-v1-matched-top5-magnitude"

diagnostics_dir <- file.path(output_root, "diagnostics")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

grouped_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv")
row_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
di02_decision_path <- file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv")

jaccard_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_jaccard.csv")
sets_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_sets.csv")
decision_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_decision.csv")
note_path <- file.path(diagnostics_dir, "exact_kfold_reclassification_reviewer_note.md")

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}

as_score <- function(x) suppressWarnings(as.numeric(x))

empty_jaccard <- function() {
  data.frame(
    target_space = character(0),
    score_variable = character(0),
    metric_class = character(0),
    Primary_Inference_Allowed = logical(0),
    N_joined = integer(0),
    top_n = integer(0),
    effective_top_share = numeric(0),
    row_top_n = integer(0),
    grouped_top_n = integer(0),
    intersection_n = integer(0),
    union_n = integer(0),
    only_row_n = integer(0),
    only_grouped_n = integer(0),
    neither_flagged_n = integer(0),
    jaccard = numeric(0),
    switch_rate = numeric(0),
    spearman_rank_correlation = numeric(0),
    row_score_column = character(0),
    grouped_score_column = character(0),
    cutoff_tie_flag_row = logical(0),
    cutoff_tie_flag_grouped = logical(0),
    suppression_reason = character(0),
    interpretation = character(0),
    stringsAsFactors = FALSE
  )
}

empty_sets <- function() {
  data.frame(
    target_space = character(0),
    score_variable = character(0),
    company = character(0),
    year = integer(0),
    row_score = numeric(0),
    grouped_score = numeric(0),
    row_rank = integer(0),
    grouped_rank = integer(0),
    row_top5_flag = logical(0),
    grouped_top5_flag = logical(0),
    membership_class = character(0),
    stringsAsFactors = FALSE
  )
}

metric_specs <- data.frame(
  score_variable = c(
    "DA_raw_stacked",
    "DA_z_estimation_stacked",
    "DA_z_predictive_stacked",
    "DA_tail_flag_95",
    "DA_tail_flag_98",
    "DA_ppd_tail_prob_two_sided"
  ),
  metric_class = c(
    "primary_magnitude_raw",
    "primary_magnitude_estimation_scaled",
    "secondary_predictive_scaled_magnitude",
    "supplementary_tail_based_or_posterior_predictive",
    "supplementary_tail_based_or_posterior_predictive",
    "supplementary_tail_based_or_posterior_predictive"
  ),
  transform = c("abs", "abs", "abs", "identity", "identity", "inverse_tail_probability"),
  stringsAsFactors = FALSE
)

read_di02_decision <- function(path) {
  x <- safe_read_csv(path)
  if (is.null(x) || !"audit_decision" %in% names(x) || nrow(x) == 0) return(NA_character_)
  as.character(x$audit_decision[[1]])
}

di02_decision <- read_di02_decision(di02_decision_path)
di02_tail_suppression_required <- identical(
  di02_decision,
  "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES"
)
tail_suppression_reason <- "Suppressed from primary reporting by di02 new-firm predictive integration audit."

score_from_column <- function(x, transform) {
  x <- as_score(x)
  if (identical(transform, "abs")) return(abs(x))
  if (identical(transform, "inverse_tail_probability")) return(1 - x)
  x
}

rank_one_side <- function(keys, score, top_n) {
  ranked <- data.frame(
    row_id = seq_len(nrow(keys)),
    company_chr = as.character(keys$company),
    year_num = suppressWarnings(as.numeric(keys$year)),
    year_chr = as.character(keys$year),
    score = score,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(score), company_chr, year_num, year_chr, row_id) %>%
    mutate(rank = row_number(), top_flag = rank <= top_n)

  out <- data.frame(
    row_id = seq_len(nrow(keys)),
    rank = NA_integer_,
    top_flag = FALSE,
    stringsAsFactors = FALSE
  )
  out$rank[ranked$row_id] <- ranked$rank
  out$top_flag[ranked$row_id] <- ranked$top_flag

  tie_flag <- FALSE
  if (top_n > 0 && nrow(ranked) > top_n) {
    tie_flag <- isTRUE(ranked$score[[top_n]] == ranked$score[[top_n + 1]])
  }

  list(rank = out$rank, top_flag = out$top_flag, cutoff_tie_flag = tie_flag)
}

interpret_metric <- function(metric_class, jaccard, top_n) {
  paste0(
    "Matched top-5% sets are forced to size ", top_n,
    "; Jaccard 1 means identical flagged firm-years, Jaccard 0 means no overlap, ",
    "and lower values indicate stronger validation-target sensitivity in composition. ",
    "This row is classified as ", metric_class, "."
  )
}

compute_metric <- function(joined, target_space_value, spec) {
  score_variable <- spec$score_variable[[1]]
  grouped_col <- paste0(score_variable, "_grouped")
  row_col <- paste0(score_variable, "_row")

  if (!all(c(grouped_col, row_col) %in% names(joined))) {
    return(list(jaccard = empty_jaccard(), sets = empty_sets()))
  }

  target_df <- joined %>% filter(target_space == target_space_value)
  if (nrow(target_df) == 0) return(list(jaccard = empty_jaccard(), sets = empty_sets()))

  row_score <- score_from_column(target_df[[row_col]], spec$transform[[1]])
  grouped_score <- score_from_column(target_df[[grouped_col]], spec$transform[[1]])
  keep <- is.finite(row_score) & is.finite(grouped_score)
  target_df <- target_df[keep, , drop = FALSE]
  row_score <- row_score[keep]
  grouped_score <- grouped_score[keep]
  N <- nrow(target_df)
  if (N == 0) return(list(jaccard = empty_jaccard(), sets = empty_sets()))

  top_n <- ceiling(0.05 * N)
  top_n <- max(1L, as.integer(top_n))
  keys <- target_df %>% select(company, year)

  row_rank <- rank_one_side(keys, row_score, top_n)
  grouped_rank <- rank_one_side(keys, grouped_score, top_n)
  row_flag <- row_rank$top_flag
  grouped_flag <- grouped_rank$top_flag

  intersection_n <- sum(row_flag & grouped_flag)
  union_n <- sum(row_flag | grouped_flag)
  only_row_n <- sum(row_flag & !grouped_flag)
  only_grouped_n <- sum(!row_flag & grouped_flag)
  neither_flagged_n <- sum(!row_flag & !grouped_flag)
  jaccard <- if (union_n > 0) intersection_n / union_n else NA_real_
  switch_rate <- (only_row_n + only_grouped_n) / N
  spearman <- if (N >= 2) {
    suppressWarnings(cor(row_score, grouped_score, method = "spearman", use = "complete.obs"))
  } else {
    NA_real_
  }

  metric_class <- spec$metric_class[[1]]
  tail_metric <- identical(metric_class, "supplementary_tail_based_or_posterior_predictive")
  primary_allowed <- metric_class %in% c("primary_magnitude_raw", "primary_magnitude_estimation_scaled")
  suppression_reason <- if (tail_metric && di02_tail_suppression_required) {
    tail_suppression_reason
  } else if (identical(metric_class, "secondary_predictive_scaled_magnitude")) {
    "Secondary magnitude diagnostic; predictive scaling is not treated as primary matched-5% evidence."
  } else if (tail_metric) {
    "Supplementary tail-based/posterior-predictive diagnostic; not primary matched-5% evidence."
  } else {
    NA_character_
  }

  sets <- data.frame(
    target_space = target_space_value,
    score_variable = score_variable,
    company = as.character(target_df$company),
    year = target_df$year,
    row_score = row_score,
    grouped_score = grouped_score,
    row_rank = row_rank$rank,
    grouped_rank = grouped_rank$rank,
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

  jaccard_row <- data.frame(
    target_space = target_space_value,
    score_variable = score_variable,
    metric_class = metric_class,
    Primary_Inference_Allowed = primary_allowed,
    N_joined = N,
    top_n = top_n,
    effective_top_share = top_n / N,
    row_top_n = sum(row_flag),
    grouped_top_n = sum(grouped_flag),
    intersection_n = intersection_n,
    union_n = union_n,
    only_row_n = only_row_n,
    only_grouped_n = only_grouped_n,
    neither_flagged_n = neither_flagged_n,
    jaccard = jaccard,
    switch_rate = switch_rate,
    spearman_rank_correlation = spearman,
    row_score_column = row_col,
    grouped_score_column = grouped_col,
    cutoff_tie_flag_row = row_rank$cutoff_tie_flag,
    cutoff_tie_flag_grouped = grouped_rank$cutoff_tie_flag,
    suppression_reason = suppression_reason,
    interpretation = interpret_metric(metric_class, jaccard, top_n),
    stringsAsFactors = FALSE
  )

  list(jaccard = jaccard_row, sets = sets)
}

grouped_df <- safe_read_csv(grouped_path)
row_df <- safe_read_csv(row_path)
required_join_cols <- c("company", "year", "target_space")

if (is.null(grouped_df) || is.null(row_df) ||
    !all(required_join_cols %in% names(grouped_df)) ||
    !all(required_join_cols %in% names(row_df))) {
  jaccard_table <- empty_jaccard()
  sets_table <- empty_sets()
} else {
  keep_cols <- unique(c(required_join_cols, metric_specs$score_variable))
  grouped_keep <- grouped_df[, intersect(keep_cols, names(grouped_df)), drop = FALSE]
  row_keep <- row_df[, intersect(keep_cols, names(row_df)), drop = FALSE]

  joined <- inner_join(
    grouped_keep,
    row_keep,
    by = required_join_cols,
    suffix = c("_grouped", "_row")
  )

  metric_results <- list()
  target_spaces <- sort(unique(as.character(joined$target_space)))
  for (target_space_value in target_spaces) {
    for (i in seq_len(nrow(metric_specs))) {
      metric_results[[length(metric_results) + 1]] <- compute_metric(joined, target_space_value, metric_specs[i, , drop = FALSE])
    }
  }
  jaccard_table <- bind_rows(lapply(metric_results, `[[`, "jaccard"))
  sets_table <- bind_rows(lapply(metric_results, `[[`, "sets"))
}

if (nrow(jaccard_table) == 0) jaccard_table <- empty_jaccard()
if (nrow(sets_table) == 0) sets_table <- empty_sets()

primary_rows <- jaccard_table %>%
  filter(metric_class %in% c("primary_magnitude_raw", "primary_magnitude_estimation_scaled"))
tail_rows <- jaccard_table %>%
  filter(metric_class == "supplementary_tail_based_or_posterior_predictive")

has_primary <- nrow(primary_rows) > 0
any_primary_low <- has_primary && any(primary_rows$jaccard < 0.60, na.rm = TRUE)
any_primary_moderate <- has_primary && any(primary_rows$jaccard < 0.80 & primary_rows$jaccard >= 0.60, na.rm = TRUE)
tail_suppressed <- nrow(tail_rows) > 0 && di02_tail_suppression_required

audit_decision <- if (!has_primary) {
  "FAIL_NO_PRIMARY_MAGNITUDE_RECLASSIFICATION"
} else if (any_primary_low) {
  "WARN_PRIMARY_MAGNITUDE_JACCARD_LOW"
} else if (any_primary_moderate) {
  "WARN_PRIMARY_MAGNITUDE_JACCARD_MODERATE"
} else if (tail_suppressed) {
  "WARN_TAIL_SUPPRESSED"
} else {
  "PASS_PRIMARY_MAGNITUDE_RECLASSIFICATION_AVAILABLE"
}

decision <- data.frame(
  audit_decision = audit_decision,
  primary_rule = "Primary RQ2 matched-5% Jaccard evidence uses only abs(DA_raw_stacked) and abs(DA_z_estimation_stacked).",
  n_primary_magnitude_rows = nrow(primary_rows),
  n_target_spaces_with_primary = length(unique(primary_rows$target_space)),
  min_primary_magnitude_jaccard = if (has_primary) min(primary_rows$jaccard, na.rm = TRUE) else NA_real_,
  any_primary_jaccard_below_0_60 = any_primary_low,
  any_primary_jaccard_below_0_80 = any_primary_moderate || any_primary_low,
  tail_metrics_available = nrow(tail_rows) > 0,
  di02_decision = ifelse(is.na(di02_decision), NA_character_, di02_decision),
  tail_metrics_suppressed_by_di02 = tail_suppressed,
  grouped_input = grouped_path,
  row_input = row_path,
  di02_decision_input = di02_decision_path,
  grouped_input_exists = file.exists(grouped_path),
  row_input_exists = file.exists(row_path),
  di02_decision_input_exists = file.exists(di02_decision_path),
  script_name = script_name,
  script_version = script_version,
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  stringsAsFactors = FALSE
)

note <- c(
  "# Exact K-fold reclassification audit",
  "",
  paste0("Script: `", script_name, "`"),
  paste0("Version: `", script_version, "`"),
  "",
  "## Primary estimand",
  "",
  "Primary RQ2 evidence is matched-size top-5% Jaccard overlap by absolute abnormal accrual magnitude.",
  "The primary score variables are `abs(DA_raw_stacked)` and `abs(DA_z_estimation_stacked)`.",
  "Tail flags, posterior predictive tail probabilities, `DA_ppd_percentile`, and posterior-tail surprise scores are not used to define primary matched-5% sets.",
  "",
  "## Interpretation",
  "",
  "Jaccard = 1 means row-level and grouped-firm validation flag exactly the same firm-years at the matched top-5% threshold.",
  "Jaccard = 0 means no overlap between the two matched top-5% sets.",
  "Lower Jaccard indicates stronger validation-target sensitivity at the flagged firm-year level.",
  "Because both A and B are forced to have the same size, the Jaccard reflects composition change rather than a difference in flagging volume.",
  "",
  "## Decision",
  "",
  paste0("- audit_decision: `", decision$audit_decision[[1]], "`"),
  paste0("- n_primary_magnitude_rows: ", decision$n_primary_magnitude_rows[[1]]),
  paste0("- n_target_spaces_with_primary: ", decision$n_target_spaces_with_primary[[1]]),
  paste0("- min_primary_magnitude_jaccard: ", decision$min_primary_magnitude_jaccard[[1]]),
  paste0("- di02_decision: `", ifelse(is.na(di02_decision), "NA", di02_decision), "`"),
  paste0("- tail_metrics_suppressed_by_di02: ", decision$tail_metrics_suppressed_by_di02[[1]])
)

write.csv(jaccard_table, jaccard_path, row.names = FALSE)
write.csv(sets_table, sets_path, row.names = FALSE)
write.csv(decision, decision_path, row.names = FALSE)
writeLines(note, note_path)

cat("\n[SUCCESS] Exact K-fold reclassification audit completed.\n")
cat("Decision:", decision$audit_decision[[1]], "\n")
cat("Jaccard table:", jaccard_path, "\n")
cat("Sets table:", sets_path, "\n")
cat("Decision table:", decision_path, "\n")
cat("Reviewer note:", note_path, "\n")

phase_end("di03", "Exact K-fold reclassification audit")
