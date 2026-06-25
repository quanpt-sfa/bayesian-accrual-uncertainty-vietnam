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
script_version <- "2026-06-22-v2-hardened-reclassification"

diagnostics_dir <- file.path(output_root, "diagnostics")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

grouped_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv")
row_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
di02_decision_path <- file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv")

jaccard_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_jaccard.csv")
sets_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_sets.csv")
tail_overlap_path <- file.path(diagnostics_dir, "table_exact_kfold_tail_flag_natural_overlap.csv")
join_coverage_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_join_coverage.csv")
decision_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_decision.csv")
io_manifest_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_io_manifest.csv")
note_path <- file.path(diagnostics_dir, "exact_kfold_reclassification_reviewer_note.md")

required_join_cols <- c("company", "year", "target_space")
di02_suppression_decision <- "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES"
tail_suppression_reason <- "Suppressed from primary reporting by di02 new-firm predictive integration audit."

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}

file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}
git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}

as_score <- function(x) suppressWarnings(as.numeric(x))

reported_score_name <- function(source_score_variable, score_transform) {
  if (identical(score_transform, "absolute_value")) return(paste0("abs(", source_score_variable, ")"))
  if (identical(score_transform, "inverse_tail_probability")) return(paste0("1 - ", source_score_variable))
  source_score_variable
}

empty_jaccard <- function() {
  data.frame(
    target_space = character(0),
    score_variable = character(0),
    source_score_variable = character(0),
    score_transform = character(0),
    reported_score_variable = character(0),
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
    cutoff_tie_count_row = integer(0),
    cutoff_tie_count_grouped = integer(0),
    suppression_reason = character(0),
    interpretation = character(0),
    stringsAsFactors = FALSE
  )
}

empty_sets <- function() {
  data.frame(
    target_space = character(0),
    score_variable = character(0),
    source_score_variable = character(0),
    score_transform = character(0),
    reported_score_variable = character(0),
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

empty_tail_overlap <- function() {
  data.frame(
    target_space = character(0),
    source_score_variable = character(0),
    score_transform = character(0),
    reported_score_variable = character(0),
    metric_class = character(0),
    Primary_Inference_Allowed = logical(0),
    N_joined = integer(0),
    row_flag_n = integer(0),
    grouped_flag_n = integer(0),
    intersection_n = integer(0),
    union_n = integer(0),
    jaccard_natural_flag_overlap = numeric(0),
    only_row_n = integer(0),
    only_grouped_n = integer(0),
    neither_flagged_n = integer(0),
    row_flag_share = numeric(0),
    grouped_flag_share = numeric(0),
    switch_rate = numeric(0),
    suppression_reason = character(0),
    stringsAsFactors = FALSE
  )
}

matched_metric_specs <- data.frame(
  source_score_variable = c(
    "DA_raw_stacked",
    "DA_z_estimation_stacked",
    "DA_z_predictive_stacked",
    "DA_ppd_tail_prob_two_sided"
  ),
  score_transform = c(
    "absolute_value",
    "absolute_value",
    "absolute_value",
    "inverse_tail_probability"
  ),
  metric_class = c(
    "primary_magnitude_raw",
    "primary_magnitude_estimation_scaled",
    "secondary_predictive_scaled_magnitude",
    "supplementary_tail_based_or_posterior_predictive"
  ),
  stringsAsFactors = FALSE
)
matched_metric_specs$reported_score_variable <- mapply(
  reported_score_name,
  matched_metric_specs$source_score_variable,
  matched_metric_specs$score_transform,
  USE.NAMES = FALSE
)

tail_flag_specs <- data.frame(
  source_score_variable = c("DA_tail_flag_95", "DA_tail_flag_98"),
  score_transform = "natural_binary_flag",
  reported_score_variable = c("DA_tail_flag_95", "DA_tail_flag_98"),
  metric_class = "supplementary_tail_based_or_posterior_predictive",
  stringsAsFactors = FALSE
)

read_di02_decision <- function(path) {
  x <- safe_read_csv(path)
  if (is.null(x) || !"audit_decision" %in% names(x) || nrow(x) == 0) return(NA_character_)
  as.character(x$audit_decision[[1]])
}

di02_decision <- read_di02_decision(di02_decision_path)
di02_tail_suppression_required <- identical(di02_decision, di02_suppression_decision)

score_from_column <- function(x, score_transform) {
  x <- as_score(x)
  if (identical(score_transform, "absolute_value")) return(abs(x))
  if (identical(score_transform, "inverse_tail_probability")) return(1 - x)
  x
}

assert_file_exists <- function(path, label) {
  if (!file.exists(path)) stop("[BLOCKER] Missing ", label, ": ", path)
}

assert_required_cols <- function(df, cols, label, path) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop("[BLOCKER] ", label, " lacks required column(s): ", paste(missing, collapse = ", "), " in ", path)
  }
}

assert_unique_keys <- function(df, label, path) {
  dup <- df %>%
    count(company, year, target_space, name = "n") %>%
    filter(n > 1)
  if (nrow(dup) > 0) {
    example <- paste(dup$company[[1]], dup$year[[1]], dup$target_space[[1]], sep = "/")
    stop("[BLOCKER] Duplicate company-year-target_space keys in ", label, ": ", path,
         ". Example key: ", example, "; duplicate groups=", nrow(dup))
  }
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

  out <- data.frame(row_id = seq_len(nrow(keys)), rank = NA_integer_, top_flag = FALSE, stringsAsFactors = FALSE)
  out$rank[ranked$row_id] <- ranked$rank
  out$top_flag[ranked$row_id] <- ranked$top_flag

  cutoff_score <- if (top_n > 0 && nrow(ranked) >= top_n) ranked$score[[top_n]] else NA_real_
  cutoff_tie_count <- if (is.finite(cutoff_score)) sum(ranked$score == cutoff_score, na.rm = TRUE) else NA_integer_
  cutoff_tie_flag <- isTRUE(cutoff_tie_count > 1)

  list(
    rank = out$rank,
    top_flag = out$top_flag,
    cutoff_tie_flag = cutoff_tie_flag,
    cutoff_tie_count = cutoff_tie_count
  )
}

interpret_metric <- function(metric_class, jaccard, top_n) {
  paste0(
    "Matched top-5% sets are forced to size ", top_n,
    "; Jaccard 1 means identical flagged firm-years, Jaccard 0 means no overlap, ",
    "and lower values indicate stronger validation-target sensitivity in composition. ",
    "This row is classified as ", metric_class, "."
  )
}

assert_matched_invariants <- function(row) {
  problems <- character()
  if (!identical(as.integer(row$row_top_n), as.integer(row$top_n))) problems <- c(problems, "row_top_n != top_n")
  if (!identical(as.integer(row$grouped_top_n), as.integer(row$top_n))) problems <- c(problems, "grouped_top_n != top_n")
  if (!identical(as.integer(row$union_n), as.integer(row$top_n * 2L - row$intersection_n))) {
    problems <- c(problems, "union_n != top_n * 2 - intersection_n")
  }
  if (!identical(as.integer(row$neither_flagged_n), as.integer(row$N_joined - row$union_n))) {
    problems <- c(problems, "neither_flagged_n != N_joined - union_n")
  }
  if (length(problems)) {
    stop("[BLOCKER] Matched-size invariant failed for ",
         row$target_space, " / ", row$reported_score_variable, ": ",
         paste(problems, collapse = "; "))
  }
}

compute_matched_metric <- function(joined, target_space_value, spec) {
  source_score_variable <- spec$source_score_variable[[1]]
  score_transform <- spec$score_transform[[1]]
  reported_score_variable <- spec$reported_score_variable[[1]]
  grouped_col <- paste0(source_score_variable, "_grouped")
  row_col <- paste0(source_score_variable, "_row")

  if (!all(c(grouped_col, row_col) %in% names(joined))) {
    return(list(jaccard = empty_jaccard(), sets = empty_sets()))
  }

  target_df <- joined %>% filter(target_space == target_space_value)
  if (nrow(target_df) == 0) return(list(jaccard = empty_jaccard(), sets = empty_sets()))

  row_score <- score_from_column(target_df[[row_col]], score_transform)
  grouped_score <- score_from_column(target_df[[grouped_col]], score_transform)
  keep <- is.finite(row_score) & is.finite(grouped_score)
  target_df <- target_df[keep, , drop = FALSE]
  row_score <- row_score[keep]
  grouped_score <- grouped_score[keep]
  N <- nrow(target_df)
  if (N == 0) return(list(jaccard = empty_jaccard(), sets = empty_sets()))

  top_n <- max(1L, as.integer(ceiling(0.05 * N)))
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
  primary_allowed <- metric_class %in% c("primary_magnitude_raw", "primary_magnitude_estimation_scaled")
  suppression_reason <- if (identical(metric_class, "supplementary_tail_based_or_posterior_predictive") &&
                            di02_tail_suppression_required) {
    tail_suppression_reason
  } else if (identical(metric_class, "secondary_predictive_scaled_magnitude")) {
    "Secondary magnitude diagnostic; predictive scaling is not treated as primary matched-5% evidence."
  } else if (identical(metric_class, "supplementary_tail_based_or_posterior_predictive")) {
    "Supplementary posterior-predictive diagnostic; not primary matched-5% evidence."
  } else {
    NA_character_
  }

  sets <- data.frame(
    target_space = target_space_value,
    score_variable = reported_score_variable,
    source_score_variable = source_score_variable,
    score_transform = score_transform,
    reported_score_variable = reported_score_variable,
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
    score_variable = reported_score_variable,
    source_score_variable = source_score_variable,
    score_transform = score_transform,
    reported_score_variable = reported_score_variable,
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
    cutoff_tie_count_row = row_rank$cutoff_tie_count,
    cutoff_tie_count_grouped = grouped_rank$cutoff_tie_count,
    suppression_reason = suppression_reason,
    interpretation = interpret_metric(metric_class, jaccard, top_n),
    stringsAsFactors = FALSE
  )
  assert_matched_invariants(jaccard_row[1, , drop = FALSE])
  list(jaccard = jaccard_row, sets = sets)
}

compute_tail_overlap <- function(joined, target_space_value, spec) {
  source_score_variable <- spec$source_score_variable[[1]]
  grouped_col <- paste0(source_score_variable, "_grouped")
  row_col <- paste0(source_score_variable, "_row")
  if (!all(c(grouped_col, row_col) %in% names(joined))) return(empty_tail_overlap())

  target_df <- joined %>% filter(target_space == target_space_value)
  if (nrow(target_df) == 0) return(empty_tail_overlap())

  row_flag <- as_score(target_df[[row_col]]) > 0
  grouped_flag <- as_score(target_df[[grouped_col]]) > 0
  keep <- !is.na(row_flag) & !is.na(grouped_flag)
  row_flag <- row_flag[keep]
  grouped_flag <- grouped_flag[keep]
  N <- length(row_flag)
  if (N == 0) return(empty_tail_overlap())

  intersection_n <- sum(row_flag & grouped_flag)
  union_n <- sum(row_flag | grouped_flag)
  only_row_n <- sum(row_flag & !grouped_flag)
  only_grouped_n <- sum(!row_flag & grouped_flag)
  neither_flagged_n <- sum(!row_flag & !grouped_flag)
  suppression_reason <- if (di02_tail_suppression_required) {
    tail_suppression_reason
  } else {
    "Supplementary natural tail-flag overlap; not primary matched-5% evidence."
  }

  data.frame(
    target_space = target_space_value,
    source_score_variable = source_score_variable,
    score_transform = spec$score_transform[[1]],
    reported_score_variable = spec$reported_score_variable[[1]],
    metric_class = spec$metric_class[[1]],
    Primary_Inference_Allowed = FALSE,
    N_joined = N,
    row_flag_n = sum(row_flag),
    grouped_flag_n = sum(grouped_flag),
    intersection_n = intersection_n,
    union_n = union_n,
    jaccard_natural_flag_overlap = if (union_n > 0) intersection_n / union_n else NA_real_,
    only_row_n = only_row_n,
    only_grouped_n = only_grouped_n,
    neither_flagged_n = neither_flagged_n,
    row_flag_share = sum(row_flag) / N,
    grouped_flag_share = sum(grouped_flag) / N,
    switch_rate = (only_row_n + only_grouped_n) / N,
    suppression_reason = suppression_reason,
    stringsAsFactors = FALSE
  )
}

make_join_coverage <- function(grouped_df, row_df, joined) {
  grouped_counts <- grouped_df %>% count(target_space, name = "grouped_n_by_target_space")
  row_counts <- row_df %>% count(target_space, name = "row_n_by_target_space")
  joined_counts <- joined %>% count(target_space, name = "joined_n_by_target_space")
  full_join(grouped_counts, row_counts, by = "target_space") %>%
    full_join(joined_counts, by = "target_space") %>%
    mutate(
      grouped_n_by_target_space = ifelse(is.na(grouped_n_by_target_space), 0L, grouped_n_by_target_space),
      row_n_by_target_space = ifelse(is.na(row_n_by_target_space), 0L, row_n_by_target_space),
      joined_n_by_target_space = ifelse(is.na(joined_n_by_target_space), 0L, joined_n_by_target_space),
      grouped_unmatched_n = grouped_n_by_target_space - joined_n_by_target_space,
      row_unmatched_n = row_n_by_target_space - joined_n_by_target_space
    ) %>%
    arrange(target_space)
}

assert_file_exists(grouped_path, "grouped exact-KFold DA output")
assert_file_exists(row_path, "row exact-KFold DA output")

grouped_df <- safe_read_csv(grouped_path)
row_df <- safe_read_csv(row_path)
if (is.null(grouped_df)) stop("[BLOCKER] Could not read grouped exact-KFold DA output: ", grouped_path)
if (is.null(row_df)) stop("[BLOCKER] Could not read row exact-KFold DA output: ", row_path)

assert_required_cols(grouped_df, required_join_cols, "grouped exact-KFold DA output", grouped_path)
assert_required_cols(row_df, required_join_cols, "row exact-KFold DA output", row_path)
assert_unique_keys(grouped_df, "grouped exact-KFold DA output", grouped_path)
assert_unique_keys(row_df, "row exact-KFold DA output", row_path)

keep_cols <- unique(c(required_join_cols, matched_metric_specs$source_score_variable, tail_flag_specs$source_score_variable))
grouped_keep <- grouped_df[, intersect(keep_cols, names(grouped_df)), drop = FALSE]
row_keep <- row_df[, intersect(keep_cols, names(row_df)), drop = FALSE]

joined <- inner_join(grouped_keep, row_keep, by = required_join_cols, suffix = c("_grouped", "_row"))
join_coverage <- make_join_coverage(grouped_df, row_df, joined)
if (nrow(joined) == 0) stop("[BLOCKER] Exact-KFold grouped and row DA outputs have zero company-year-target_space overlap.")

metric_results <- list()
tail_results <- list()
target_spaces <- sort(unique(as.character(joined$target_space)))
for (target_space_value in target_spaces) {
  for (i in seq_len(nrow(matched_metric_specs))) {
    metric_results[[length(metric_results) + 1]] <- compute_matched_metric(joined, target_space_value, matched_metric_specs[i, , drop = FALSE])
  }
  for (i in seq_len(nrow(tail_flag_specs))) {
    tail_results[[length(tail_results) + 1]] <- compute_tail_overlap(joined, target_space_value, tail_flag_specs[i, , drop = FALSE])
  }
}

jaccard_table <- bind_rows(lapply(metric_results, `[[`, "jaccard"))
sets_table <- bind_rows(lapply(metric_results, `[[`, "sets"))
tail_overlap <- bind_rows(tail_results)
if (nrow(jaccard_table) == 0) jaccard_table <- empty_jaccard()
if (nrow(sets_table) == 0) sets_table <- empty_sets()
if (nrow(tail_overlap) == 0) tail_overlap <- empty_tail_overlap()

primary_rows <- jaccard_table %>%
  filter(metric_class %in% c("primary_magnitude_raw", "primary_magnitude_estimation_scaled"))
secondary_rows <- jaccard_table %>%
  filter(metric_class == "secondary_predictive_scaled_magnitude")
posterior_predictive_rows <- jaccard_table %>%
  filter(metric_class == "supplementary_tail_based_or_posterior_predictive")
tail_metrics_exist <- nrow(tail_overlap) > 0 || nrow(posterior_predictive_rows) > 0

has_primary <- nrow(primary_rows) > 0
any_primary_low <- has_primary && any(primary_rows$jaccard < 0.60, na.rm = TRUE)
any_primary_moderate <- has_primary && any(primary_rows$jaccard < 0.80 & primary_rows$jaccard >= 0.60, na.rm = TRUE)

primary_magnitude_decision <- if (!has_primary) {
  "FAIL_NO_PRIMARY_MAGNITUDE_RECLASSIFICATION"
} else if (any_primary_low) {
  "WARN_PRIMARY_MAGNITUDE_JACCARD_LOW"
} else if (any_primary_moderate) {
  "WARN_PRIMARY_MAGNITUDE_JACCARD_MODERATE"
} else {
  "PASS_PRIMARY_MAGNITUDE_RECLASSIFICATION_AVAILABLE"
}

secondary_predictive_scaled_decision <- if (nrow(secondary_rows) > 0) {
  "PASS_SECONDARY_PREDICTIVE_SCALED_DIAGNOSTIC_AVAILABLE"
} else {
  "WARN_SECONDARY_PREDICTIVE_SCALED_DIAGNOSTIC_UNAVAILABLE"
}

tail_reporting_decision <- if (di02_tail_suppression_required && tail_metrics_exist) {
  "WARN_TAIL_SUPPRESSED_BY_DI02"
} else {
  "PASS_NO_PRIMARY_TAIL_REPORTING"
}

warnings <- character()
if (identical(tail_reporting_decision, "WARN_TAIL_SUPPRESSED_BY_DI02")) {
  warnings <- c(warnings, "tail/posterior-predictive metrics suppressed from primary reporting by di02")
}
if (any(join_coverage$grouped_unmatched_n > 0 | join_coverage$row_unmatched_n > 0)) {
  warnings <- c(warnings, "grouped/row exact-KFold DA join is not complete for all target spaces")
}

overall_reporting_decision <- primary_magnitude_decision

decision <- data.frame(
  audit_decision = overall_reporting_decision,
  primary_magnitude_decision = primary_magnitude_decision,
  secondary_predictive_scaled_decision = secondary_predictive_scaled_decision,
  tail_reporting_decision = tail_reporting_decision,
  overall_reporting_decision = overall_reporting_decision,
  warnings = paste(warnings, collapse = "; "),
  primary_rule = "Primary RQ2 matched-5% Jaccard evidence uses only abs(DA_raw_stacked) and abs(DA_z_estimation_stacked).",
  n_primary_magnitude_rows = nrow(primary_rows),
  n_target_spaces_with_primary = length(unique(primary_rows$target_space)),
  min_primary_magnitude_jaccard = if (has_primary) min(primary_rows$jaccard, na.rm = TRUE) else NA_real_,
  any_primary_jaccard_below_0_60 = any_primary_low,
  any_primary_jaccard_below_0_80 = any_primary_moderate || any_primary_low,
  secondary_predictive_scaled_available = nrow(secondary_rows) > 0,
  tail_metrics_available = tail_metrics_exist,
  di02_decision = ifelse(is.na(di02_decision), NA_character_, di02_decision),
  tail_metrics_suppressed_by_di02 = identical(tail_reporting_decision, "WARN_TAIL_SUPPRESSED_BY_DI02"),
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

primary_summary <- primary_rows %>%
  select(target_space, reported_score_variable, metric_class, N_joined, top_n, jaccard, switch_rate, spearman_rank_correlation)
raw_summary <- primary_rows %>% filter(reported_score_variable == "abs(DA_raw_stacked)")
scaled_summary <- primary_rows %>% filter(reported_score_variable == "abs(DA_z_estimation_stacked)")
fmt_jaccard <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || all(is.na(x))) return("NA")
  paste(format(round(x, 3), trim = TRUE), collapse = ", ")
}

note <- c(
  "# Exact K-fold reclassification audit",
  "",
  paste0("Script: `", script_name, "`"),
  paste0("Version: `", script_version, "`"),
  "",
  "## Matched-size top-5% Jaccard",
  "",
  "For each target space and magnitude score, let A be firm-years in the top 5% by the row-level exact-KFold score and B be firm-years in the top 5% by the grouped-firm exact-KFold score.",
  "The statistic is J(A,B) = |A intersection B| / |A union B|.",
  "Matched-size means both A and B are forced to have top_n = ceiling(0.05 * N), so Jaccard reflects composition change rather than flag-volume differences.",
  "",
  "## Primary estimand",
  "",
  "Primary RQ2 evidence is restricted to absolute abnormal accrual magnitude: `abs(DA_raw_stacked)` and `abs(DA_z_estimation_stacked)`.",
  "`abs(DA_z_predictive_stacked)` is secondary because it uses predictive scaling.",
  "`DA_tail_flag_95`, `DA_tail_flag_98`, and posterior-predictive tail quantities are supplementary/non-primary under di02.",
  "",
  "## Primary summaries",
  "",
  paste0("- Raw |DA| Jaccard by target space: ", fmt_jaccard(raw_summary$jaccard)),
  paste0("- Estimation-scaled |DA| Jaccard by target space: ", fmt_jaccard(scaled_summary$jaccard)),
  "Raw |DA| top-tail composition is stable if its Jaccard is high.",
  "Estimation-scaled |DA| top-tail composition is validation-target sensitive if its Jaccard is low.",
  "",
  "## Decision",
  "",
  paste0("- primary_magnitude_decision: `", decision$primary_magnitude_decision[[1]], "`"),
  paste0("- secondary_predictive_scaled_decision: `", decision$secondary_predictive_scaled_decision[[1]], "`"),
  paste0("- tail_reporting_decision: `", decision$tail_reporting_decision[[1]], "`"),
  paste0("- overall_reporting_decision: `", decision$overall_reporting_decision[[1]], "`"),
  paste0("- warnings: ", ifelse(nzchar(decision$warnings[[1]]), decision$warnings[[1]], "None")),
  paste0("- di02_decision: `", ifelse(is.na(di02_decision), "NA", di02_decision), "`"),
  "",
  "## Primary rows",
  "",
  if (nrow(primary_summary)) {
    apply(primary_summary, 1, function(x) paste0("- ", x[["target_space"]], " / ", x[["reported_score_variable"]], ": Jaccard=", x[["jaccard"]], ", switch_rate=", x[["switch_rate"]]))
  } else {
    "- No primary magnitude rows were computed."
  }
)

write_csv_safely(jaccard_table, jaccard_path, row.names = FALSE)
write_csv_safely(sets_table, sets_path, row.names = FALSE)
write_csv_safely(tail_overlap, tail_overlap_path, row.names = FALSE)
write_csv_safely(join_coverage, join_coverage_path, row.names = FALSE)
write_csv_safely(decision, decision_path, row.names = FALSE)
writeLines(note, note_path)

script_end_time <- Sys.time()
manifest_paths <- c(
  grouped_path,
  row_path,
  di02_decision_path,
  jaccard_path,
  sets_path,
  tail_overlap_path,
  join_coverage_path,
  decision_path,
  note_path,
  io_manifest_path
)
io_manifest <- data.frame(
  script_name = script_name,
  script_version = script_version,
  git_commit = git_commit_or_na(),
  start_time = as.character(script_start_time),
  end_time = as.character(script_end_time),
  runtime_seconds = as.numeric(difftime(script_end_time, script_start_time, units = "secs")),
  classification = c(rep("input", 3), rep("output", length(manifest_paths) - 3)),
  path = manifest_paths,
  exists = file.exists(manifest_paths),
  file_size = vapply(manifest_paths, file_size_or_na, numeric(1)),
  mtime = vapply(manifest_paths, mtime_or_na, character(1)),
  md5_hash = vapply(manifest_paths, file_hash_or_na, character(1)),
  stringsAsFactors = FALSE
)
write_csv_safely(io_manifest, io_manifest_path, row.names = FALSE)
io_manifest$exists <- file.exists(manifest_paths)
io_manifest$file_size <- vapply(manifest_paths, file_size_or_na, numeric(1))
io_manifest$mtime <- vapply(manifest_paths, mtime_or_na, character(1))
io_manifest$md5_hash <- vapply(manifest_paths, file_hash_or_na, character(1))
write_csv_safely(io_manifest, io_manifest_path, row.names = FALSE)

cat("\n[SUCCESS] Exact K-fold reclassification audit completed.\n")
cat("Primary decision:", decision$primary_magnitude_decision[[1]], "\n")
cat("Tail decision:", decision$tail_reporting_decision[[1]], "\n")
cat("Jaccard table:", jaccard_path, "\n")
cat("Natural tail table:", tail_overlap_path, "\n")
cat("Join coverage:", join_coverage_path, "\n")
cat("Decision table:", decision_path, "\n")
cat("IO manifest:", io_manifest_path, "\n")
cat("Reviewer note:", note_path, "\n")

phase_end("di03", "Exact K-fold reclassification audit")
