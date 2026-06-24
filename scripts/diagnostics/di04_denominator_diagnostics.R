# -----------------------------------------------------------------------------
# Script: di04_denominator_diagnostics.R
# Purpose: Diagnose whether estimation-scaled exact-KFold DA reclassification is
#          driven by SD(mu) denominator behavior rather than raw DA magnitude.
#
# Intended use:
#   Rscript scripts/diagnostics/di04_denominator_diagnostics.R
#
# This is artifact-only. It reads existing exact-KFold DA outputs and diagnostics
# and does not fit or refit any Bayesian model.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di04", "Denominator diagnostics for exact-KFold DA")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

script_start_time <- Sys.time()
script_name <- "scripts/diagnostics/di04_denominator_diagnostics.R"
script_version <- "2026-06-25-v1-denominator-diagnostics"

diagnostics_dir <- file.path(output_root, "diagnostics")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

grouped_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv")
row_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
jaccard_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_jaccard.csv")
source_manifest_path <- file.path(output_root, "tables", "table_DA_exact_kfold_source_manifest.csv")
grouped_pin_path <- file.path(winsor_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt")
row_pin_path <- file.path(winsor_root, "row_exact_kfold", "LATEST_COMPLETED_RUN.txt")

distribution_path <- file.path(diagnostics_dir, "table_denominator_sd_mu_distribution.csv")
comparison_path <- file.path(diagnostics_dir, "table_denominator_sd_mu_row_grouped_comparison.csv")
capped_jaccard_path <- file.path(diagnostics_dir, "table_denominator_capped_jaccard.csv")
z_est_vs_z_pred_path <- file.path(diagnostics_dir, "table_da_z_est_vs_z_pred_comparison.csv")
decision_path <- file.path(diagnostics_dir, "table_denominator_diagnostics_decision.csv")
io_manifest_path <- file.path(diagnostics_dir, "table_denominator_diagnostics_io_manifest.csv")
note_path <- file.path(diagnostics_dir, "denominator_diagnostics_reviewer_note.md")

safe_read_csv <- function(path, required = FALSE, label = basename(path)) {
  if (!file.exists(path)) {
    if (required) stop("[BLOCKER] Missing ", label, ": ", path)
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

num <- function(x) suppressWarnings(as.numeric(x))

file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}
git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}

detect_col <- function(df, base_names, label) {
  nms <- names(df)
  for (nm in base_names) {
    if (nm %in% nms) return(nm)
  }
  for (nm in base_names) {
    hit <- grep(paste0("^", gsub("([\\W])", "\\\\\\1", nm), "($|_)"), nms, value = TRUE)
    if (length(hit)) return(hit[[1]])
  }
  stop("[BLOCKER] Could not detect required ", label, " column. Tried: ", paste(base_names, collapse = ", "))
}

assert_keys <- function(df, label) {
  keys <- c("company", "year", "target_space")
  missing <- setdiff(keys, names(df))
  if (length(missing)) stop("[BLOCKER] ", label, " lacks join key(s): ", paste(missing, collapse = ", "))
  dup <- duplicated(df[keys])
  if (any(dup)) {
    bad <- df[which(dup)[1], keys, drop = FALSE]
    stop("[BLOCKER] ", label, " has duplicate company-year-target_space key, example: ",
         paste(unlist(bad), collapse = " / "))
  }
}

quant <- function(x, p) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE, type = 7))
}

safe_cor <- function(x, y, method) {
  x <- num(x)
  y <- num(y)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}

safe_mean <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_median <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  stats::median(x)
}

top_flags <- function(score, company, year, top_n) {
  score <- num(score)
  rank_score <- ifelse(is.finite(score), score, -Inf)
  ord <- order(-rank_score, as.character(company), suppressWarnings(as.integer(year)), na.last = TRUE)
  out <- rep(FALSE, length(score))
  if (top_n > 0 && length(ord) >= top_n) out[ord[seq_len(top_n)]] <- TRUE
  out
}

top_rank <- function(score, company, year) {
  score <- num(score)
  rank_score <- ifelse(is.finite(score), score, -Inf)
  ord <- order(-rank_score, as.character(company), suppressWarnings(as.integer(year)), na.last = TRUE)
  out <- integer(length(score))
  out[ord] <- seq_along(ord)
  out
}

matched_jaccard_row <- function(df, target_space, score_variable, denominator_variant, row_score, grouped_score) {
  n <- nrow(df)
  top_n <- ceiling(0.05 * n)
  row_flag <- top_flags(row_score, df$company, df$year, top_n)
  grouped_flag <- top_flags(grouped_score, df$company, df$year, top_n)
  inter <- sum(row_flag & grouped_flag)
  union <- sum(row_flag | grouped_flag)
  if (sum(row_flag) != top_n || sum(grouped_flag) != top_n) {
    stop("[BLOCKER] Matched top-5 invariant failed in denominator diagnostic: ", target_space, " / ", denominator_variant)
  }
  data.frame(
    target_space = target_space,
    score_variable = score_variable,
    denominator_variant = denominator_variant,
    N_joined = n,
    top_n = top_n,
    row_top_n = sum(row_flag),
    grouped_top_n = sum(grouped_flag),
    intersection_n = inter,
    union_n = union,
    only_row_n = sum(row_flag & !grouped_flag),
    only_grouped_n = sum(grouped_flag & !row_flag),
    jaccard = ifelse(union > 0, inter / union, NA_real_),
    switch_rate = sum(xor(row_flag, grouped_flag)) / n,
    spearman_rank_correlation = safe_cor(row_score, grouped_score, "spearman"),
    interpretation = "Matched top-5% Jaccard after modifying only the SD(mu) denominator of DA_z_est.",
    stringsAsFactors = FALSE
  )
}

modify_denominator <- function(denom, variant) {
  denom <- num(denom)
  if (identical(variant, "original_denominator")) return(denom)
  if (identical(variant, "winsor_p01_p99")) return(pmin(pmax(denom, quant(denom, 0.01)), quant(denom, 0.99)))
  if (identical(variant, "winsor_p05_p95")) return(pmin(pmax(denom, quant(denom, 0.05)), quant(denom, 0.95)))
  if (identical(variant, "floor_p01")) return(pmax(denom, quant(denom, 0.01)))
  if (identical(variant, "floor_p05")) return(pmax(denom, quant(denom, 0.05)))
  if (identical(variant, "within_target_space_median_denominator")) return(rep(safe_median(denom), length(denom)))
  stop("[BLOCKER] Unknown denominator variant: ", variant)
}

distribution_rows <- function(df, source_label, denom_col, pred_denom_col = NA_character_) {
  cols <- c(denom_col, pred_denom_col)
  names(cols) <- c("NDA_sd_epred_stacked", "NDA_sd_predict_stacked")
  cols <- cols[!is.na(cols)]
  bind_rows(lapply(names(cols), function(denom_name) {
    value <- num(df[[cols[[denom_name]]]])
    data.frame(
      source = source_label,
      target_space = df$target_space,
      denominator_name = denom_name,
      denominator_value = value,
      stringsAsFactors = FALSE
    )
  })) %>%
    group_by(.data$source, .data$target_space, .data$denominator_name) %>%
    summarise(
      N = n(),
      mean = safe_mean(.data$denominator_value),
      sd = stats::sd(num(.data$denominator_value), na.rm = TRUE),
      min = quant(.data$denominator_value, 0),
      p01 = quant(.data$denominator_value, 0.01),
      p05 = quant(.data$denominator_value, 0.05),
      p10 = quant(.data$denominator_value, 0.10),
      p25 = quant(.data$denominator_value, 0.25),
      median = quant(.data$denominator_value, 0.50),
      p75 = quant(.data$denominator_value, 0.75),
      p90 = quant(.data$denominator_value, 0.90),
      p95 = quant(.data$denominator_value, 0.95),
      p99 = quant(.data$denominator_value, 0.99),
      max = quant(.data$denominator_value, 1),
      zero_or_nonpositive_n = sum(is.finite(num(.data$denominator_value)) & num(.data$denominator_value) <= 0),
      nonfinite_n = sum(!is.finite(num(.data$denominator_value))),
      .groups = "drop"
    )
}

grouped <- safe_read_csv(grouped_path, required = TRUE, label = "grouped exact-KFold DA")
row <- safe_read_csv(row_path, required = TRUE, label = "row exact-KFold DA")
assert_keys(grouped, "grouped exact-KFold DA")
assert_keys(row, "row exact-KFold DA")

row_cols <- list(
  raw = detect_col(row, c("DA_raw_stacked", "DA_raw_stacked_ep_winsor", "DA_raw_stacked_rt_winsor"), "row raw DA"),
  z_est = detect_col(row, c("DA_z_estimation_stacked", "DA_z_estimation_stacked_ep_winsor", "DA_z_estimation_stacked_rt_winsor"), "row DA_z_est"),
  z_pred = detect_col(row, c("DA_z_predictive_stacked", "DA_z_predictive_stacked_ep_winsor", "DA_z_predictive_stacked_rt_winsor"), "row DA_z_pred"),
  denom_est = detect_col(row, c("NDA_sd_epred_stacked", "NDA_sd_mu_stacked", "NDA_sd_estimation_stacked"), "row SD(mu) denominator")
)
grouped_cols <- list(
  raw = detect_col(grouped, c("DA_raw_stacked", "DA_raw_stacked_ep_winsor", "DA_raw_stacked_rt_winsor"), "grouped raw DA"),
  z_est = detect_col(grouped, c("DA_z_estimation_stacked", "DA_z_estimation_stacked_ep_winsor", "DA_z_estimation_stacked_rt_winsor"), "grouped DA_z_est"),
  z_pred = detect_col(grouped, c("DA_z_predictive_stacked", "DA_z_predictive_stacked_ep_winsor", "DA_z_predictive_stacked_rt_winsor"), "grouped DA_z_pred"),
  denom_est = detect_col(grouped, c("NDA_sd_epred_stacked", "NDA_sd_mu_stacked", "NDA_sd_estimation_stacked"), "grouped SD(mu) denominator")
)
row_cols$denom_pred <- tryCatch(detect_col(row, c("NDA_sd_predict_stacked", "NDA_sd_predictive_stacked"), "row predictive denominator"), error = function(e) NA_character_)
grouped_cols$denom_pred <- tryCatch(detect_col(grouped, c("NDA_sd_predict_stacked", "NDA_sd_predictive_stacked"), "grouped predictive denominator"), error = function(e) NA_character_)

select_for_join <- function(df, cols) {
  out <- df[, c("company", "year", "target_space", unlist(cols[!is.na(cols)])), drop = FALSE]
  names(out) <- make.unique(names(out))
  out
}

joined <- select_for_join(row, row_cols) %>%
  rename(
    DA_raw_row = all_of(row_cols$raw),
    DA_z_est_row = all_of(row_cols$z_est),
    DA_z_pred_row = all_of(row_cols$z_pred),
    denom_est_row = all_of(row_cols$denom_est)
  ) %>%
  { if (!is.na(row_cols$denom_pred)) rename(., denom_pred_row = all_of(row_cols$denom_pred)) else mutate(., denom_pred_row = NA_real_) } %>%
  inner_join(
    select_for_join(grouped, grouped_cols) %>%
      rename(
        DA_raw_grouped = all_of(grouped_cols$raw),
        DA_z_est_grouped = all_of(grouped_cols$z_est),
        DA_z_pred_grouped = all_of(grouped_cols$z_pred),
        denom_est_grouped = all_of(grouped_cols$denom_est)
      ) %>%
      { if (!is.na(grouped_cols$denom_pred)) rename(., denom_pred_grouped = all_of(grouped_cols$denom_pred)) else mutate(., denom_pred_grouped = NA_real_) },
    by = c("company", "year", "target_space")
  ) %>%
  mutate(
    DA_raw_row = num(.data$DA_raw_row),
    DA_raw_grouped = num(.data$DA_raw_grouped),
    DA_z_est_row = num(.data$DA_z_est_row),
    DA_z_est_grouped = num(.data$DA_z_est_grouped),
    DA_z_pred_row = num(.data$DA_z_pred_row),
    DA_z_pred_grouped = num(.data$DA_z_pred_grouped),
    denom_est_row = num(.data$denom_est_row),
    denom_est_grouped = num(.data$denom_est_grouped),
    denom_ratio_row_over_grouped = .data$denom_est_row / .data$denom_est_grouped
  )

if (!nrow(joined)) stop("[BLOCKER] Row/grouped exact-KFold DA inner join produced zero rows.")

distribution <- bind_rows(
  distribution_rows(row, "row_exact_kfold", row_cols$denom_est, row_cols$denom_pred),
  distribution_rows(grouped, "grouped_exact_kfold", grouped_cols$denom_est, grouped_cols$denom_pred)
)

comparison <- joined %>%
  group_by(.data$target_space) %>%
  summarise(
    N_joined = n(),
    pearson_sd_mu_row_grouped = safe_cor(.data$denom_est_row, .data$denom_est_grouped, "pearson"),
    spearman_sd_mu_row_grouped = safe_cor(.data$denom_est_row, .data$denom_est_grouped, "spearman"),
    mean_difference_row_minus_grouped = safe_mean(.data$denom_est_row - .data$denom_est_grouped),
    median_difference_row_minus_grouped = safe_median(.data$denom_est_row - .data$denom_est_grouped),
    p95_absolute_difference = quant(abs(.data$denom_est_row - .data$denom_est_grouped), 0.95),
    ratio_mean_row_over_grouped = safe_mean(.data$denom_ratio_row_over_grouped),
    ratio_median_row_over_grouped = safe_median(.data$denom_ratio_row_over_grouped),
    ratio_p05_row_over_grouped = quant(.data$denom_ratio_row_over_grouped, 0.05),
    ratio_p95_row_over_grouped = quant(.data$denom_ratio_row_over_grouped, 0.95),
    share_row_denominator_lt_grouped = mean(.data$denom_est_row < .data$denom_est_grouped, na.rm = TRUE),
    share_grouped_denominator_lt_row = mean(.data$denom_est_grouped < .data$denom_est_row, na.rm = TRUE),
    .groups = "drop"
  )

denominator_variants <- c(
  "original_denominator",
  "winsor_p01_p99",
  "winsor_p05_p95",
  "floor_p01",
  "floor_p05",
  "within_target_space_median_denominator"
)

capped_jaccard <- bind_rows(lapply(split(joined, joined$target_space), function(df) {
  bind_rows(lapply(denominator_variants, function(variant) {
    row_denom <- modify_denominator(df$denom_est_row, variant)
    grouped_denom <- modify_denominator(df$denom_est_grouped, variant)
    row_score <- abs(df$DA_raw_row / pmax(row_denom, .Machine$double.eps))
    grouped_score <- abs(df$DA_raw_grouped / pmax(grouped_denom, .Machine$double.eps))
    matched_jaccard_row(df, df$target_space[[1]], "abs(DA_raw_stacked / SD_mu_modified)", variant, row_score, grouped_score)
  }))
}))

z_est_vs_z_pred <- bind_rows(lapply(c("row", "grouped"), function(source_label) {
  bind_rows(lapply(split(joined, joined$target_space), function(df) {
    z_est <- if (source_label == "row") abs(df$DA_z_est_row) else abs(df$DA_z_est_grouped)
    z_pred <- if (source_label == "row") abs(df$DA_z_pred_row) else abs(df$DA_z_pred_grouped)
    n <- nrow(df)
    top_n <- ceiling(0.05 * n)
    est_flag <- top_flags(z_est, df$company, df$year, top_n)
    pred_flag <- top_flags(z_pred, df$company, df$year, top_n)
    inter <- sum(est_flag & pred_flag)
    union <- sum(est_flag | pred_flag)
    data.frame(
      source = paste0(source_label, "_exact_kfold"),
      target_space = df$target_space[[1]],
      N = n,
      pearson_z_est_vs_z_pred = safe_cor(z_est, z_pred, "pearson"),
      spearman_z_est_vs_z_pred = safe_cor(z_est, z_pred, "spearman"),
      top5_jaccard_z_est_vs_z_pred = ifelse(union > 0, inter / union, NA_real_),
      top_n = top_n,
      intersection_n = inter,
      union_n = union,
      only_z_est_n = sum(est_flag & !pred_flag),
      only_z_pred_n = sum(!est_flag & pred_flag),
      interpretation = "Within-source matched top-5% overlap comparing abs(DA_z_est) with abs(DA_z_pred).",
      stringsAsFactors = FALSE
    )
  }))
}))

original_j <- capped_jaccard %>% filter(.data$denominator_variant == "original_denominator")
perturbed_j <- capped_jaccard %>% filter(.data$denominator_variant != "original_denominator")
decision_summary <- perturbed_j %>%
  group_by(.data$target_space) %>%
  summarise(
    original_jaccard = original_j$jaccard[match(.data$target_space[1], original_j$target_space)],
    perturbed_jaccard_min = min(.data$jaccard, na.rm = TRUE),
    perturbed_jaccard_max = max(.data$jaccard, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    perturbed_jaccard_range = .data$perturbed_jaccard_max - .data$perturbed_jaccard_min,
    max_jaccard_increase_vs_original = .data$perturbed_jaccard_max - .data$original_jaccard,
    denominator_sensitive = is.finite(.data$max_jaccard_increase_vs_original) & .data$max_jaccard_increase_vs_original >= 0.20,
    denominator_driven = is.finite(.data$original_jaccard) & .data$original_jaccard < 0.60 &
      is.finite(.data$perturbed_jaccard_max) & .data$perturbed_jaccard_max >= 0.80
  )

overall_decision <- dplyr::case_when(
  !nrow(decision_summary) ~ "INSUFFICIENT_INPUTS",
  any(decision_summary$denominator_driven, na.rm = TRUE) ~ "FAIL_DENOMINATOR_DRIVEN",
  any(decision_summary$denominator_sensitive, na.rm = TRUE) ~ "WARN_DENOMINATOR_SENSITIVE",
  TRUE ~ "PASS_DENOMINATOR_DIAGNOSTIC_STABLE"
)

decision <- decision_summary %>%
  mutate(
    diagnostic_decision = overall_decision,
    claim_assessment = dplyr::case_when(
      .data$denominator_driven ~ "Denominator perturbations materially eliminate low original Jaccard; treat DA_z_est turnover as denominator-driven.",
      .data$denominator_sensitive ~ "Denominator perturbations materially change Jaccard; denominator sensitivity weakens the claim.",
      TRUE ~ "Denominator perturbations do not materially eliminate the observed matched top-5% turnover."
    )
  ) %>%
  select(.data$diagnostic_decision, everything())

write.csv(distribution, distribution_path, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(comparison, comparison_path, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(capped_jaccard, capped_jaccard_path, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(z_est_vs_z_pred, z_est_vs_z_pred_path, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(decision, decision_path, row.names = FALSE, fileEncoding = "UTF-8")

input_paths <- c(grouped_path, row_path, jaccard_path, source_manifest_path, grouped_pin_path, row_pin_path)
output_paths <- c(distribution_path, comparison_path, capped_jaccard_path, z_est_vs_z_pred_path, decision_path, io_manifest_path, note_path)
io_manifest <- data.frame(
  script_name = script_name,
  script_version = script_version,
  git_commit = git_commit_or_na(),
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  io_class = c(rep("input", length(input_paths)), rep("output", length(output_paths))),
  path = c(input_paths, output_paths),
  exists = file.exists(c(input_paths, output_paths)),
  file_size_bytes = vapply(c(input_paths, output_paths), file_size_or_na, numeric(1)),
  modified_time = vapply(c(input_paths, output_paths), mtime_or_na, character(1)),
  md5 = vapply(c(input_paths, output_paths), file_hash_or_na, character(1)),
  output_root = output_root,
  stringsAsFactors = FALSE
)
write.csv(io_manifest, io_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")

note <- c(
  "# Denominator Diagnostics Reviewer Note",
  "",
  "These diagnostics test whether target-sensitive top-tail turnover in estimation-scaled abnormal accruals is driven by the SD(mu) denominator.",
  "",
  "`DA_z_est = DA_raw / SD(mu)`, where `SD(mu)` is the posterior uncertainty in the expected nondiscretionary accrual mean.",
  "",
  "The capped and floored variants recompute matched top-5% Jaccard after changing only the denominator: p01/p99 winsorization, p05/p95 winsorization, p01 floor, p05 floor, and within-target-space median replacement as a negative-control diagnostic.",
  "",
  paste0("Overall denominator diagnostic decision: `", overall_decision, "`."),
  "",
  "The `DA_z_est` versus `DA_z_pred` table compares estimation scaling with predictive scaling within each validation target.",
  "",
  "These diagnostics address measurement robustness. They do not prove earnings management or identify intent."
)
writeLines(note, note_path, useBytes = TRUE)

cat("[SUCCESS] di04 denominator diagnostics written under ", diagnostics_dir, "\n", sep = "")
phase_end("di04", "Denominator diagnostics for exact-KFold DA")
