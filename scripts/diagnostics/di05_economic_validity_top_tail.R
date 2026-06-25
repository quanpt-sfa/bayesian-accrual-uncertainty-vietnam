# -----------------------------------------------------------------------------
# Script: di05_economic_validity_top_tail.R
# Purpose: Supplementary economic-validity diagnostics for exact-KFold top-tail
#          membership classes.
#
# Intended use:
#   Rscript scripts/diagnostics/di05_economic_validity_top_tail.R
#
# This script does not fit or refit Bayesian models.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

source("scripts/ma00_setup.R")
phase_begin("di05", "Economic-validity diagnostics for exact-KFold top-tail groups")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

script_start_time <- Sys.time()
script_name <- "scripts/diagnostics/di05_economic_validity_top_tail.R"
script_version <- "2026-06-25-v1-economic-validity-top-tail"

diagnostics_dir <- file.path(output_root, "diagnostics")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

sets_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_sets.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
raw_path <- data_path

membership_path <- file.path(diagnostics_dir, "table_top_tail_set_membership_exact_kfold.csv")
counts_path <- file.path(diagnostics_dir, "table_top_tail_set_counts_exact_kfold.csv")
means_path <- file.path(diagnostics_dir, "table_top_tail_group_outcome_means.csv")
validity_path <- file.path(diagnostics_dir, "table_top_tail_group_economic_validity.csv")
decision_path <- file.path(diagnostics_dir, "table_top_tail_group_economic_validity_decision.csv")
io_manifest_path <- file.path(diagnostics_dir, "table_top_tail_group_economic_validity_io_manifest.csv")
note_path <- file.path(diagnostics_dir, "economic_validity_top_tail_reviewer_note.md")

read_required_csv <- function(path, label) {
  if (!file.exists(path)) stop("[BLOCKER] Missing ", label, ": ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

required_cols <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("[BLOCKER] ", label, " lacks column(s): ", paste(missing, collapse = ", "))
}

num <- function(x) suppressWarnings(as.numeric(x))

git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}
file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

cluster_for_fit <- function(fit, data) {
  mf <- stats::model.frame(fit)
  idx <- suppressWarnings(as.integer(rownames(mf)))
  if (length(idx) == nrow(mf) && all(!is.na(idx)) && all(idx >= 1) && all(idx <= nrow(data))) {
    return(data$company[idx])
  }
  rep(NA_character_, nrow(mf))
}

coef_table <- function(fit, data) {
  out <- NULL
  if (requireNamespace("sandwich", quietly = TRUE) && requireNamespace("lmtest", quietly = TRUE)) {
    cl <- cluster_for_fit(fit, data)
    if (length(cl) == stats::nobs(fit) && dplyr::n_distinct(cl, na.rm = TRUE) >= 2) {
      out <- tryCatch(lmtest::coeftest(fit, vcov. = sandwich::vcovCL(fit, cluster = cl)), error = function(e) NULL)
    }
  }
  if (is.null(out)) out <- summary(fit)$coefficients
  as.data.frame(out)
}

sets <- read_required_csv(sets_path, "di03 reclassification sets")
sample_rt <- read_required_csv(rt_sample_path, "winsor no-lookahead sample")
if (!file.exists(raw_path)) stop("[BLOCKER] Raw data workbook missing: ", raw_path)
raw <- readxl::read_excel(raw_path, sheet = "Sheet1")

required_cols(sets, c("company", "year", "target_space", "membership_class"), "di03 sets")
required_cols(sample_rt, c("company", "year", "industry", "Size", "ROA_curr", "revenue_growth", "A_lag", "TA_scaled"), "winsor no-lookahead sample")
required_cols(raw, c("company", "year", "A", "NI", "ROA", "CFO"), "raw workbook")

score_values <- c(
  "abs(DA_raw_stacked)", "abs(DA_z_estimation_stacked)", "abs(DA_z_predictive_stacked)",
  "DA_raw_stacked", "DA_z_estimation_stacked", "DA_z_predictive_stacked"
)

sets_primary <- sets %>%
  mutate(score_label = if ("reported_score_variable" %in% names(.)) .data$reported_score_variable else .data$score_variable) %>%
  filter(.data$target_space == "real_time", .data$score_label %in% score_values) %>%
  mutate(
    reported_score_variable = case_when(
      .data$score_label == "DA_raw_stacked" ~ "abs(DA_raw_stacked)",
      .data$score_label == "DA_z_estimation_stacked" ~ "abs(DA_z_estimation_stacked)",
      .data$score_label == "DA_z_predictive_stacked" ~ "abs(DA_z_predictive_stacked)",
      TRUE ~ .data$score_label
    ),
    RowOnlyTop5 = .data$membership_class == "row_only",
    GroupedOnlyTop5 = .data$membership_class == "grouped_only",
    CommonTop5 = .data$membership_class == "both",
    NeitherTop5 = .data$membership_class == "neither"
  )

if (!nrow(sets_primary)) stop("[BLOCKER] No real_time magnitude membership rows in: ", sets_path)

raw_leads <- raw %>%
  mutate(company = as.character(.data$company), year = as.integer(.data$year)) %>%
  arrange(.data$company, .data$year) %>%
  group_by(.data$company) %>%
  mutate(
    NI_lead = get_lead_contiguous(.data$NI, .data$year),
    ROA_lead = get_lead_contiguous(.data$ROA, .data$year),
    A_lead = get_lead_contiguous(.data$A, .data$year),
    CFO_lead = get_lead_contiguous(.data$CFO, .data$year),
    future_Earnings_persistence = .data$NI_lead / .data$A
  ) %>%
  ungroup() %>%
  transmute(
    company = as.character(.data$company),
    year = as.integer(.data$year),
    A = num(.data$A),
    future_CFO = num(.data$CFO_lead) / num(.data$A),
    future_Earnings = num(.data$NI_lead) / num(.data$A),
    future_ROA = ifelse(num(.data$A_lead) > 0, num(.data$NI_lead) / num(.data$A_lead), NA_real_),
    future_Earnings_persistence = num(.data$future_Earnings_persistence)
  )

sample_rt_leads <- sample_rt %>%
  mutate(company = as.character(.data$company), year = as.integer(.data$year)) %>%
  arrange(.data$company, .data$year) %>%
  group_by(.data$company) %>%
  mutate(TA_scaled_lead = get_lead_contiguous(.data$TA_scaled, .data$year)) %>%
  ungroup() %>%
  transmute(
    company = as.character(.data$company),
    year = as.integer(.data$year),
    accrual_reversal = -num(.data$TA_scaled_lead)
  )

membership_cols <- c(
  "target_space", "reported_score_variable", "company", "year", "row_score", "grouped_score",
  "row_rank", "grouped_rank", "row_top5_flag", "grouped_top5_flag", "membership_class",
  "RowOnlyTop5", "GroupedOnlyTop5", "CommonTop5", "NeitherTop5"
)
membership <- sets_primary %>%
  select(any_of(membership_cols)) %>%
  arrange(.data$target_space, .data$reported_score_variable, .data$company, .data$year)

analysis <- sample_rt %>%
  mutate(company = as.character(.data$company), year = as.integer(.data$year)) %>%
  select(all_of(c("company", "year", "industry", "Size", "ROA_curr", "revenue_growth", "A_lag"))) %>%
  inner_join(raw_leads, by = c("company", "year")) %>%
  left_join(sample_rt_leads, by = c("company", "year")) %>%
  inner_join(membership, by = c("company", "year"))

if (!nrow(analysis)) stop("[BLOCKER] Economic-validity membership join produced zero rows.")

outcomes <- c("future_CFO", "future_ROA", "future_Earnings", "future_Earnings_persistence", "accrual_reversal")
outcomes <- outcomes[outcomes %in% names(analysis)]
terms_interest <- c("RowOnlyTop5TRUE", "GroupedOnlyTop5TRUE", "CommonTop5TRUE")

fit_one <- function(df, score_label, outcome) {
  use <- df %>% filter(.data$reported_score_variable == !!score_label)
  use <- use[is.finite(num(use[[outcome]])), , drop = FALSE]
  if (nrow(use) < 30 || length(unique(use$industry)) < 2 || length(unique(use$year)) < 2) {
    return(data.frame(
      reported_score_variable = score_label, outcome = outcome, term = sub("TRUE$", "", terms_interest),
      coefficient = NA_real_, std_error = NA_real_, t_value = NA_real_, p_value = NA_real_,
      N_obs = nrow(use), N_firms = dplyr::n_distinct(use$company), r_squared = NA_real_, adj_r_squared = NA_real_,
      model_status = "insufficient_variation", stringsAsFactors = FALSE
    ))
  }
  form <- stats::as.formula(paste0(outcome, " ~ RowOnlyTop5 + GroupedOnlyTop5 + CommonTop5 + Size + ROA_curr + revenue_growth + factor(industry) + factor(year)"))
  fit <- tryCatch(stats::lm(form, data = use), error = function(e) NULL)
  if (is.null(fit)) {
    return(data.frame(
      reported_score_variable = score_label, outcome = outcome, term = sub("TRUE$", "", terms_interest),
      coefficient = NA_real_, std_error = NA_real_, t_value = NA_real_, p_value = NA_real_,
      N_obs = nrow(use), N_firms = dplyr::n_distinct(use$company), r_squared = NA_real_, adj_r_squared = NA_real_,
      model_status = "fit_failed", stringsAsFactors = FALSE
    ))
  }
  ct <- coef_table(fit, use)
  nm <- rownames(ct)
  bind_rows(lapply(terms_interest, function(term) {
    idx <- which(nm == term)
    data.frame(
      reported_score_variable = score_label,
      outcome = outcome,
      term = sub("TRUE$", "", term),
      coefficient = if (length(idx)) ct[idx[1], 1] else NA_real_,
      std_error = if (length(idx) && ncol(ct) >= 2) ct[idx[1], 2] else NA_real_,
      t_value = if (length(idx) && ncol(ct) >= 3) ct[idx[1], 3] else NA_real_,
      p_value = if (length(idx) && ncol(ct) >= 4) ct[idx[1], 4] else NA_real_,
      N_obs = stats::nobs(fit),
      N_firms = dplyr::n_distinct(use$company),
      r_squared = summary(fit)$r.squared,
      adj_r_squared = summary(fit)$adj.r.squared,
      model_status = "fit_ok",
      stringsAsFactors = FALSE
    )
  }))
}

validity <- bind_rows(lapply(unique(analysis$reported_score_variable), function(score_label) {
  bind_rows(lapply(outcomes, function(outcome) fit_one(analysis, score_label, outcome)))
}))

counts <- analysis %>%
  count(.data$reported_score_variable, .data$membership_class, name = "N") %>%
  group_by(.data$reported_score_variable) %>%
  mutate(share = .data$N / sum(.data$N)) %>%
  ungroup()

means <- analysis %>%
  group_by(.data$reported_score_variable, .data$membership_class) %>%
  summarise(across(all_of(outcomes), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
            N = n(), .groups = "drop")

strong <- validity %>%
  filter(.data$model_status == "fit_ok", is.finite(.data$coefficient), !is.na(.data$p_value), .data$p_value <= 0.10) %>%
  count(.data$reported_score_variable, .data$term, name = "significant_outcome_n")

decision_detail <- validity %>%
  filter(.data$model_status == "fit_ok") %>%
  group_by(.data$reported_score_variable) %>%
  summarise(
    fitted_tests = n(),
    significant_tests_p10 = sum(!is.na(.data$p_value) & .data$p_value <= 0.10),
    common_top5_significant_tests_p10 = sum(.data$term == "CommonTop5" & !is.na(.data$p_value) & .data$p_value <= 0.10),
    row_only_significant_tests_p10 = sum(.data$term == "RowOnlyTop5" & !is.na(.data$p_value) & .data$p_value <= 0.10),
    grouped_only_significant_tests_p10 = sum(.data$term == "GroupedOnlyTop5" & !is.na(.data$p_value) & .data$p_value <= 0.10),
    .groups = "drop"
  )

overall_decision <- dplyr::case_when(
  nrow(validity) == 0 || !any(validity$model_status == "fit_ok") ~ "FAIL_ECONOMIC_VALIDITY_UNAVAILABLE",
  any(decision_detail$common_top5_significant_tests_p10 > 0, na.rm = TRUE) ~ "PASS_COMMON_TOP_TAIL_ECONOMIC_SIGNAL_AVAILABLE",
  any(decision_detail$row_only_significant_tests_p10 > 0 | decision_detail$grouped_only_significant_tests_p10 > 0, na.rm = TRUE) ~ "WARN_TARGET_SPECIFIC_TOP_TAIL_ECONOMIC_SIGNAL",
  TRUE ~ "WARN_NO_STRONG_TOP_TAIL_ECONOMIC_SIGNAL"
)

decision <- decision_detail %>%
  mutate(
    economic_validity_decision = overall_decision,
    interpretation = dplyr::case_when(
      .data$common_top5_significant_tests_p10 > 0 ~ "Common top-tail membership has downstream economic signal; core extremes are economically interpretable.",
      .data$row_only_significant_tests_p10 > 0 | .data$grouped_only_significant_tests_p10 > 0 ~ "Economic signal is target-specific; interpret validation-target sensitivity as substantively relevant but supplementary.",
      TRUE ~ "Top-tail membership has limited downstream economic signal in these supplementary tests."
    )
  )

write_csv_safely(membership, membership_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(counts, counts_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(means, means_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(validity, validity_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(decision, decision_path, row.names = FALSE, fileEncoding = "UTF-8")

note <- c(
  "# Economic Validity Top-Tail Reviewer Note",
  "",
  "This diagnostic links exact-KFold top-tail membership classes to future operating outcomes and accrual reversal measures.",
  "",
  "The omitted membership class is `neither`. Regressions estimate RowOnlyTop5, GroupedOnlyTop5, and CommonTop5 with industry and year fixed effects and standard accounting controls.",
  "",
  "Standard errors are firm-clustered when sandwich/lmtest are available and there are at least two firms in the fitted sample.",
  "",
  paste0("Decision: `", overall_decision, "`."),
  "",
  "Primary exact-KFold magnitude evidence remains the main RQ2 evidence; this table is a supplementary economic-validity check.",
  "",
  "This is supplementary economic-validity evidence. It does not replace primary exact-KFold magnitude evidence and does not prove managerial intent."
)
writeLines(note, note_path, useBytes = TRUE)

input_paths <- c(sets_path, rt_sample_path, raw_path)
output_paths <- c(membership_path, counts_path, means_path, validity_path, decision_path, note_path)
io_paths <- c(input_paths, output_paths)
io_manifest <- rbind(
data.frame(
  script_name = script_name,
  script_version = script_version,
  git_commit = git_commit_or_na(),
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  io_class = c(rep("input", length(input_paths)), rep("output", length(output_paths))),
  path = io_paths,
  exists = file.exists(io_paths),
  file_size_bytes = vapply(io_paths, file_size_or_na, numeric(1)),
  modified_time = vapply(io_paths, mtime_or_na, character(1)),
  md5 = vapply(io_paths, file_hash_or_na, character(1)),
  output_root = output_root,
  stringsAsFactors = FALSE
),
data.frame(
  script_name = script_name,
  script_version = script_version,
  git_commit = git_commit_or_na(),
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  io_class = "output",
  path = io_manifest_path,
  exists = TRUE,
  file_size_bytes = NA_real_,
  modified_time = NA_character_,
  md5 = "self_referential_manifest",
  output_root = output_root,
  stringsAsFactors = FALSE
)
)
write_csv_safely(io_manifest, io_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")

cat("[SUCCESS] di05 economic-validity outputs written under ", diagnostics_dir, "\n", sep = "")
phase_end("di05", "Economic-validity diagnostics for exact-KFold top-tail groups")
