# -----------------------------------------------------------------------------
# Script: di06_outcome_validation_top5_membership.R
# Purpose: Supplementary economic-validity validation using exact-KFold top-5%
#          membership classes: row-only, grouped-only, and common top-tail.
#
# Intended use:
#   Rscript scripts/diagnostics/di06_outcome_validation_top5_membership.R
#
# This script does not fit or refit Bayesian models.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

source("scripts/ma00_setup.R")
phase_begin("di06", "Outcome validation for top-5 membership")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

validation_dir <- file.path(output_root, "validation", "top5_membership")
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

sets_path <- file.path(output_root, "diagnostics", "table_exact_kfold_reclassification_sets.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
raw_path <- data_path

results_path <- file.path(validation_dir, "table_outcome_validation_top5_membership.csv")
framework_path <- file.path(validation_dir, "table_outcome_validation_preinterpretation_matrix.csv")
n_path <- file.path(validation_dir, "table_outcome_validation_n_by_membership.csv")
means_path <- file.path(validation_dir, "table_outcome_validation_marginal_means.csv")
note_path <- file.path(validation_dir, "outcome_validation_top5_note.md")

read_required <- function(path, label) {
  if (!file.exists(path)) stop("[BLOCKER] Missing ", label, ": ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

required_cols <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("[BLOCKER] ", label, " lacks column(s): ", paste(missing, collapse = ", "))
}

num <- function(x) suppressWarnings(as.numeric(x))

get_lead <- function(x, yr, n = 1) {
  lead_val <- dplyr::lead(x, n)
  lead_yr <- dplyr::lead(yr, n)
  ifelse(!is.na(lead_yr) & lead_yr == (yr + n), lead_val, NA)
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

sets <- read_required(sets_path, "di03 reclassification sets")
sample_rt <- read_required(rt_sample_path, "winsor no-lookahead sample")
if (!file.exists(raw_path)) stop("[BLOCKER] Raw data workbook missing: ", raw_path)
raw <- readxl::read_excel(raw_path, sheet = "Sheet1")

required_cols(sets, c("company", "year", "target_space", "membership_class"), "di03 sets")
required_cols(sample_rt, c("company", "year", "industry", "Size", "ROA_curr", "revenue_growth", "A_lag", "TA_scaled"), "winsor no-lookahead sample")
required_cols(raw, c("company", "year", "A", "NI", "ROA", "CFO"), "raw workbook")

score_values <- c("abs(DA_raw_stacked)", "abs(DA_z_estimation_stacked)", "abs(DA_z_predictive_stacked)",
                  "DA_raw_stacked", "DA_z_estimation_stacked", "DA_z_predictive_stacked")

sets_primary <- sets %>%
  mutate(score_label = if ("reported_score_variable" %in% names(.)) .data$reported_score_variable else .data$score_variable) %>%
  filter(.data$target_space == "real_time", .data$score_label %in% score_values) %>%
  mutate(
    score_label = case_when(
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

if (!nrow(sets_primary)) stop("[BLOCKER] No real_time primary/secondary magnitude membership rows in: ", sets_path)

raw_leads <- raw %>%
  mutate(company = as.character(.data$company), year = as.integer(.data$year)) %>%
  arrange(.data$company, .data$year) %>%
  group_by(.data$company) %>%
  mutate(
    NI_lead = get_lead(.data$NI, .data$year),
    ROA_lead = get_lead(.data$ROA, .data$year),
    A_lead = get_lead(.data$A, .data$year),
    CFO_lead = get_lead(.data$CFO, .data$year),
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
  mutate(TA_scaled_lead = get_lead(.data$TA_scaled, .data$year)) %>%
  ungroup() %>%
  transmute(
    company = as.character(.data$company),
    year = as.integer(.data$year),
    accrual_reversal = -num(.data$TA_scaled_lead)
  )

analysis <- sample_rt %>%
  mutate(company = as.character(.data$company), year = as.integer(.data$year)) %>%
  select(company, year, industry, Size, ROA_curr, revenue_growth, A_lag) %>%
  inner_join(raw_leads, by = c("company", "year")) %>%
  left_join(sample_rt_leads, by = c("company", "year")) %>%
  inner_join(sets_primary, by = c("company", "year"))

if (!nrow(analysis)) stop("[BLOCKER] Outcome membership join produced zero rows.")

outcomes <- c("future_CFO", "future_ROA", "future_Earnings", "future_Earnings_persistence", "accrual_reversal")
outcomes <- outcomes[outcomes %in% names(analysis)]
terms_interest <- c("RowOnlyTop5TRUE", "GroupedOnlyTop5TRUE", "CommonTop5TRUE")

fit_one <- function(df, score_label, outcome) {
  use <- df %>% filter(.data$score_label == !!score_label)
  use <- use[is.finite(num(use[[outcome]])), , drop = FALSE]
  if (nrow(use) < 30 || length(unique(use$industry)) < 2 || length(unique(use$year)) < 2) {
    return(data.frame(
      score_variable = score_label, outcome = outcome, term = terms_interest,
      coefficient = NA_real_, std_error = NA_real_, t_value = NA_real_, p_value = NA_real_,
      N_obs = nrow(use), N_firms = dplyr::n_distinct(use$company), model_status = "insufficient_variation",
      stringsAsFactors = FALSE
    ))
  }
  form <- stats::as.formula(paste0(outcome, " ~ RowOnlyTop5 + GroupedOnlyTop5 + CommonTop5 + Size + ROA_curr + revenue_growth + factor(industry) + factor(year)"))
  fit <- tryCatch(stats::lm(form, data = use), error = function(e) NULL)
  if (is.null(fit)) {
    return(data.frame(score_variable = score_label, outcome = outcome, term = terms_interest,
                      coefficient = NA_real_, std_error = NA_real_, t_value = NA_real_, p_value = NA_real_,
                      N_obs = nrow(use), N_firms = dplyr::n_distinct(use$company), model_status = "fit_failed",
                      stringsAsFactors = FALSE))
  }
  ct <- coef_table(fit, use)
  nm <- rownames(ct)
  bind_rows(lapply(terms_interest, function(term) {
    idx <- which(nm == term)
    data.frame(
      score_variable = score_label,
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

results <- bind_rows(lapply(unique(analysis$score_label), function(score_label) {
  bind_rows(lapply(outcomes, function(outcome) fit_one(analysis, score_label, outcome)))
}))

n_by <- analysis %>%
  count(.data$score_label, .data$membership_class, name = "N") %>%
  group_by(.data$score_label) %>%
  mutate(share = .data$N / sum(.data$N)) %>%
  ungroup()

means <- analysis %>%
  group_by(.data$score_label, .data$membership_class) %>%
  summarise(across(all_of(outcomes), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
            N = n(), .groups = "drop")

framework <- data.frame(
  empirical_pattern = c(
    "CommonTop5 strong; RowOnlyTop5 and GroupedOnlyTop5 weak",
    "GroupedOnlyTop5 strong",
    "RowOnlyTop5 strong",
    "No membership group strong"
  ),
  interpretation = c(
    "Core economic extremes are stable; validation-target sensitivity is mainly boundary or uncertainty turbulence.",
    "Grouped validation has stronger out-of-firm economic content.",
    "Within-firm history contains economically relevant information, but this is not necessarily out-of-time validity.",
    "Measurement sensitivity exists statistically but lacks downstream economic content for tested outcomes."
  ),
  primary_status = "supplementary_outcome_validation_not_a_replacement_for_primary_exact_kfold_magnitude_evidence",
  stringsAsFactors = FALSE
)

write.csv(results, results_path, row.names = FALSE)
write.csv(framework, framework_path, row.names = FALSE)
write.csv(n_by, n_path, row.names = FALSE)
write.csv(means, means_path, row.names = FALSE)

note <- c(
  "# Outcome Validation Top-5 Membership Note",
  "",
  "Models use neither as the omitted membership class and estimate RowOnlyTop5, GroupedOnlyTop5, and CommonTop5 with industry and year fixed effects.",
  "Standard errors are firm-clustered when sandwich/lmtest are available and there are at least two firms in the fitted sample.",
  "",
  "Pre-interpretation framework:",
  "- If CommonTop5 is strong while RowOnly/GroupedOnly are weak, core economic extremes are stable and target sensitivity is mainly boundary/uncertainty turbulence.",
  "- If GroupedOnlyTop5 is strong, grouped validation has stronger out-of-firm economic content.",
  "- If RowOnlyTop5 is strong, within-firm history contains economically relevant information, but not necessarily out-of-time validity.",
  "- If no group is strong, measurement sensitivity exists statistically but lacks downstream economic content for tested outcomes.",
  "",
  paste0("Analysis rows: ", nrow(analysis)),
  paste0("Score variables: ", paste(unique(analysis$score_label), collapse = ", "))
)
writeLines(note, note_path, useBytes = TRUE)

cat("[SUCCESS] di06 outputs written under ", validation_dir, "\n", sep = "")
phase_end("di06", "Outcome validation for top-5 membership")
