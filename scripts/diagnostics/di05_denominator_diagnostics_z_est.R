# -----------------------------------------------------------------------------
# Script: di05_denominator_diagnostics_z_est.R
# Purpose: Decompose estimation-scaled DA top-tail reclassification into raw DA
#          numerator versus SD(mu) denominator mechanisms.
#
# Intended use:
#   Rscript scripts/diagnostics/di05_denominator_diagnostics_z_est.R
#
# This is an artifact-only diagnostic. It does not fit or refit models.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di05", "Denominator diagnostics for estimation-scaled DA")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

diagnostics_dir <- file.path(output_root, "diagnostics")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

grouped_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv")
row_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
sets_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_sets.csv")

distribution_path <- file.path(diagnostics_dir, "table_denominator_distribution_by_target.csv")
correlation_path <- file.path(diagnostics_dir, "table_denominator_rank_correlation.csv")
decomp_path <- file.path(diagnostics_dir, "table_z_est_decomposition_jaccard.csv")
profile_path <- file.path(diagnostics_dir, "table_top5_membership_denominator_profile.csv")
capping_path <- file.path(diagnostics_dir, "table_z_est_denominator_capping_sensitivity.csv")
scatter_path <- file.path(diagnostics_dir, "figure_denominator_row_vs_grouped_scatter.png")
waterfall_path <- file.path(diagnostics_dir, "figure_z_est_decomposition_waterfall.png")
note_path <- file.path(diagnostics_dir, "denominator_diagnostics_note.md")

required_da_cols <- c("company", "year", "target_space", "DA_raw_stacked", "DA_z_estimation_stacked", "NDA_sd_epred_stacked")

read_required <- function(path, label) {
  if (!file.exists(path)) stop("[BLOCKER] Missing ", label, ": ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

assert_cols <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("[BLOCKER] ", label, " lacks column(s): ", paste(missing, collapse = ", "))
}

num <- function(x) suppressWarnings(as.numeric(x))

safe_cor <- function(x, y, method) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}

rank_percentile <- function(x) {
  x <- num(x)
  ok <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (sum(ok) > 1) out[ok] <- (rank(x[ok], ties.method = "average") - 0.5) / sum(ok)
  out
}

quant <- function(x, p) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, p, names = FALSE, na.rm = TRUE, type = 7))
}

top_flags <- function(score, company, year, top_n) {
  ord <- order(-score, company, year, na.last = TRUE)
  out <- rep(FALSE, length(score))
  if (top_n > 0 && length(ord) >= top_n) out[ord[seq_len(top_n)]] <- TRUE
  out
}

jaccard_row <- function(df, target_space, label, score_a, score_b) {
  n <- nrow(df)
  if (!n) return(NULL)
  top_n <- ceiling(0.05 * n)
  a <- top_flags(score_a, df$company, df$year, top_n)
  b <- top_flags(score_b, df$company, df$year, top_n)
  inter <- sum(a & b)
  union <- sum(a | b)
  data.frame(
    target_space = target_space,
    comparison = label,
    N_joined = n,
    top_n = top_n,
    intersection_n = inter,
    union_n = union,
    jaccard = ifelse(union > 0, inter / union, NA_real_),
    only_A_n = sum(a & !b),
    only_B_n = sum(!a & b),
    switch_rate = sum(xor(a, b)) / n,
    stringsAsFactors = FALSE
  )
}

grouped <- read_required(grouped_path, "grouped exact-KFold DA")
row <- read_required(row_path, "row exact-KFold DA")
sets <- read_required(sets_path, "di03 reclassification sets")
assert_cols(grouped, required_da_cols, "grouped exact-KFold DA")
assert_cols(row, required_da_cols, "row exact-KFold DA")
assert_cols(sets, c("company", "year", "target_space", "membership_class"), "di03 sets")

joined <- row %>%
  select(any_of(c(required_da_cols, "NDA_sd_predict_stacked", "firm_history_length", "history_length", "volatility", "ROA_volatility"))) %>%
  rename_with(~ paste0(.x, "_row"), -all_of(c("company", "year", "target_space"))) %>%
  inner_join(
    grouped %>%
      select(any_of(c(required_da_cols, "NDA_sd_predict_stacked", "firm_history_length", "history_length", "volatility", "ROA_volatility"))) %>%
      rename_with(~ paste0(.x, "_grouped"), -all_of(c("company", "year", "target_space"))),
    by = c("company", "year", "target_space")
  ) %>%
  mutate(
    abs_raw_row = abs(num(.data$DA_raw_stacked_row)),
    abs_raw_grouped = abs(num(.data$DA_raw_stacked_grouped)),
    denom_row = num(.data$NDA_sd_epred_stacked_row),
    denom_grouped = num(.data$NDA_sd_epred_stacked_grouped),
    abs_z_est_row = abs(num(.data$DA_z_estimation_stacked_row)),
    abs_z_est_grouped = abs(num(.data$DA_z_estimation_stacked_grouped)),
    denom_ratio_row_over_grouped = .data$denom_row / .data$denom_grouped,
    denom_rank_pct_row = rank_percentile(.data$denom_row),
    denom_rank_pct_grouped = rank_percentile(.data$denom_grouped)
  )

if (!nrow(joined)) stop("[BLOCKER] Row/grouped DA inner join produced zero rows.")

z_sets <- sets %>%
  mutate(reported = if ("reported_score_variable" %in% names(.)) .data$reported_score_variable else .data$score_variable) %>%
  filter(.data$reported %in% c("abs(DA_z_estimation_stacked)", "DA_z_estimation_stacked")) %>%
  select(company, year, target_space, membership_class, row_top5_flag, grouped_top5_flag)

joined_sets <- joined %>%
  left_join(z_sets, by = c("company", "year", "target_space")) %>%
  mutate(membership_class = ifelse(is.na(.data$membership_class), "not_in_di03_z_est_set", .data$membership_class))

dist_rows <- lapply(split(joined, joined$target_space), function(df) {
  bind_rows(
    data.frame(target_space = df$target_space[1], source = "row", denominator = df$denom_row),
    data.frame(target_space = df$target_space[1], source = "grouped", denominator = df$denom_grouped)
  ) %>%
    group_by(.data$target_space, .data$source) %>%
    summarise(
      N = n(),
      p1 = quant(.data$denominator, 0.01),
      p5 = quant(.data$denominator, 0.05),
      p25 = quant(.data$denominator, 0.25),
      median = quant(.data$denominator, 0.50),
      p75 = quant(.data$denominator, 0.75),
      p95 = quant(.data$denominator, 0.95),
      p99 = quant(.data$denominator, 0.99),
      min_denominator = min(.data$denominator, na.rm = TRUE),
      near_zero_denominator_n = sum(is.finite(.data$denominator) & .data$denominator <= sqrt(.Machine$double.eps)),
      .groups = "drop"
    )
})
distribution <- bind_rows(dist_rows)

correlation <- joined %>%
  group_by(.data$target_space) %>%
  summarise(
    N_joined = n(),
    spearman_abs_raw = safe_cor(.data$abs_raw_row, .data$abs_raw_grouped, "spearman"),
    pearson_abs_raw = safe_cor(.data$abs_raw_row, .data$abs_raw_grouped, "pearson"),
    spearman_denominator = safe_cor(.data$denom_row, .data$denom_grouped, "spearman"),
    pearson_denominator = safe_cor(.data$denom_row, .data$denom_grouped, "pearson"),
    spearman_denominator_percentile = safe_cor(.data$denom_rank_pct_row, .data$denom_rank_pct_grouped, "spearman"),
    spearman_abs_z_est = safe_cor(.data$abs_z_est_row, .data$abs_z_est_grouped, "spearman"),
    pearson_abs_z_est = safe_cor(.data$abs_z_est_row, .data$abs_z_est_grouped, "pearson"),
    mean_denominator_ratio_row_over_grouped = mean(.data$denom_ratio_row_over_grouped, na.rm = TRUE),
    median_denominator_ratio_row_over_grouped = stats::median(.data$denom_ratio_row_over_grouped, na.rm = TRUE),
    p5_denominator_ratio_row_over_grouped = quant(.data$denom_ratio_row_over_grouped, 0.05),
    p95_denominator_ratio_row_over_grouped = quant(.data$denom_ratio_row_over_grouped, 0.95),
    share_denominator_ratio_below_0_5 = mean(.data$denom_ratio_row_over_grouped < 0.5, na.rm = TRUE),
    share_denominator_ratio_above_2 = mean(.data$denom_ratio_row_over_grouped > 2, na.rm = TRUE),
    .groups = "drop"
  )

profile <- joined_sets %>%
  group_by(.data$target_space, .data$membership_class) %>%
  summarise(
    N = n(),
    median_abs_DA_raw_row = stats::median(.data$abs_raw_row, na.rm = TRUE),
    median_abs_DA_raw_grouped = stats::median(.data$abs_raw_grouped, na.rm = TRUE),
    median_denominator_row = stats::median(.data$denom_row, na.rm = TRUE),
    median_denominator_grouped = stats::median(.data$denom_grouped, na.rm = TRUE),
    median_abs_DA_z_est_row = stats::median(.data$abs_z_est_row, na.rm = TRUE),
    median_abs_DA_z_est_grouped = stats::median(.data$abs_z_est_grouped, na.rm = TRUE),
    median_denominator_ratio_row_over_grouped = stats::median(.data$denom_ratio_row_over_grouped, na.rm = TRUE),
    median_firm_history_length = stats::median(num(if ("firm_history_length_row" %in% names(cur_data())) .data$firm_history_length_row else if ("history_length_row" %in% names(cur_data())) .data$history_length_row else NA_real_), na.rm = TRUE),
    median_volatility = stats::median(num(if ("volatility_row" %in% names(cur_data())) .data$volatility_row else if ("ROA_volatility_row" %in% names(cur_data())) .data$ROA_volatility_row else NA_real_), na.rm = TRUE),
    .groups = "drop"
  )

decomp <- bind_rows(lapply(split(joined, joined$target_space), function(df) {
  score_a <- abs(df$DA_raw_stacked_row / pmax(df$denom_row, .Machine$double.eps))
  score_b <- abs(df$DA_raw_stacked_grouped / pmax(df$denom_grouped, .Machine$double.eps))
  score_c <- abs(df$DA_raw_stacked_row / pmax(df$denom_grouped, .Machine$double.eps))
  score_d <- abs(df$DA_raw_stacked_grouped / pmax(df$denom_row, .Machine$double.eps))
  bind_rows(
    jaccard_row(df, df$target_space[1], "A_row_num_row_denom_vs_B_grouped_num_grouped_denom", score_a, score_b),
    jaccard_row(df, df$target_space[1], "A_vs_C_row_num_grouped_denom", score_a, score_c),
    jaccard_row(df, df$target_space[1], "A_vs_D_grouped_num_row_denom", score_a, score_d)
  )
}))

cap_score <- function(num_x, denom_x, cap_type) {
  denom_x <- num(denom_x)
  lower <- switch(cap_type,
    p1 = quant(denom_x, 0.01),
    p5 = quant(denom_x, 0.05),
    epsilon = sqrt(.Machine$double.eps),
    winsor_p1 = quant(denom_x, 0.01),
    NA_real_
  )
  if (identical(cap_type, "winsor_p1")) denom_x <- pmin(pmax(denom_x, lower), quant(denom_x, 0.99))
  abs(num_x / pmax(denom_x, lower, .Machine$double.eps))
}

capping <- bind_rows(lapply(split(joined, joined$target_space), function(df) {
  bind_rows(lapply(c("p1", "p5", "epsilon", "winsor_p1"), function(cap_type) {
    jaccard_row(
      df,
      df$target_space[1],
      paste0("row_vs_grouped_z_est_with_denominator_cap_", cap_type),
      cap_score(df$DA_raw_stacked_row, df$denom_row, cap_type),
      cap_score(df$DA_raw_stacked_grouped, df$denom_grouped, cap_type)
    )
  }))
}))

write.csv(distribution, distribution_path, row.names = FALSE)
write.csv(correlation, correlation_path, row.names = FALSE)
write.csv(decomp, decomp_path, row.names = FALSE)
write.csv(profile, profile_path, row.names = FALSE)
write.csv(capping, capping_path, row.names = FALSE)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  fig <- ggplot2::ggplot(joined, ggplot2::aes(x = denom_grouped, y = denom_row)) +
    ggplot2::geom_point(alpha = 0.35, size = 0.8) +
    ggplot2::facet_wrap(~ target_space, scales = "free") +
    ggplot2::labs(x = "Grouped SD(mu)", y = "Row SD(mu)") +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(scatter_path, fig, width = 7, height = 4, dpi = 160)

  fig2 <- ggplot2::ggplot(decomp, ggplot2::aes(x = comparison, y = jaccard, fill = target_space)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Decomposition comparison", y = "Matched top-5% Jaccard") +
    ggplot2::theme_minimal(base_size = 10)
  ggplot2::ggsave(waterfall_path, fig2, width = 8, height = 4.5, dpi = 160)
}

note <- c(
  "# Denominator Diagnostics Note",
  "",
  "This diagnostic tests whether estimation-scaled DA turnover is driven by raw abnormal accrual numerator changes or by SD(mu) denominator instability.",
  "",
  "A = abs(row raw DA / row SD(mu)); B = abs(grouped raw DA / grouped SD(mu)); C = abs(row raw DA / grouped SD(mu)); D = abs(grouped raw DA / row SD(mu)).",
  "If A vs C remains high while A vs D is low, numerator changes dominate. If A vs C is low, denominator changes are sufficient to move observations across the matched top-5% boundary.",
  "",
  paste0("Joined firm-years: ", nrow(joined)),
  paste0("Target spaces: ", paste(sort(unique(joined$target_space)), collapse = ", "))
)
writeLines(note, note_path, useBytes = TRUE)

cat("[SUCCESS] di05 outputs written under ", diagnostics_dir, "\n", sep = "")
phase_end("di05", "Denominator diagnostics for estimation-scaled DA")
