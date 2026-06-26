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
script_version <- "2026-06-26-v3-bh-sign-effectsize-influence"

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
influence_path <- file.path(diagnostics_dir, "table_top_tail_group_influence_by_firm.csv")
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

sign_label <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(!is.finite(x), NA_character_,
         ifelse(x > 0, "positive", ifelse(x < 0, "negative", "zero")))
}

expected_sign_for <- function(outcome) {
  dplyr::case_when(
    outcome %in% c("future_CFO", "future_ROA", "future_Earnings") ~ "negative",
    outcome == "accrual_reversal" ~ "positive",
    TRUE ~ NA_character_
  )
}

expected_sign_rationale_for <- function(outcome) {
  dplyr::case_when(
    outcome %in% c("future_CFO", "future_ROA", "future_Earnings") ~
      "Top-tail abnormal-accrual screens are expected to be followed by weaker future performance; this expected sign is imposed equally on RowOnlyTop5, GroupedOnlyTop5, and CommonTop5.",
    outcome == "accrual_reversal" ~
      "accrual_reversal is coded as -TA_scaled_{t+1}; under current-period income-increasing accrual reversal, stronger reversal implies a positive coefficient.",
    TRUE ~ NA_character_
  )
}

add_bh_by_family <- function(df, family_cols, p_col = "p_value", q_col = "q_value_BH_score_family") {
  df %>%
    group_by(across(all_of(family_cols))) %>%
    group_modify(function(.x, .g) {
      .x[[q_col]] <- NA_real_
      idx <- which(is.finite(num(.x[[p_col]])))
      if (length(idx)) .x[[q_col]][idx] <- stats::p.adjust(num(.x[[p_col]])[idx], method = "BH")
      .x
    }) %>%
    ungroup()
}

add_bh_global <- function(df, p_col = "p_value", q_col = "q_value_BH_global") {
  df[[q_col]] <- NA_real_
  idx <- which(is.finite(num(df[[p_col]])))
  if (length(idx)) df[[q_col]][idx] <- stats::p.adjust(num(df[[p_col]])[idx], method = "BH")
  df
}

add_relative_magnitude <- function(df) {
  df %>%
    group_by(.data$reported_score_variable, .data$outcome) %>%
    group_modify(function(.x, .g) {
      abs_coef <- abs(num(.x$coefficient))
      max_abs <- suppressWarnings(max(abs_coef[is.finite(abs_coef)], na.rm = TRUE))
      if (!is.finite(max_abs)) max_abs <- NA_real_
      common_abs <- abs_coef[.x$term == "CommonTop5"]
      row_abs <- abs_coef[.x$term == "RowOnlyTop5"]
      grouped_abs <- abs_coef[.x$term == "GroupedOnlyTop5"]
      common_abs <- if (length(common_abs)) common_abs[[1]] else NA_real_
      row_abs <- if (length(row_abs)) row_abs[[1]] else NA_real_
      grouped_abs <- if (length(grouped_abs)) grouped_abs[[1]] else NA_real_
      .x$abs_coefficient <- abs_coef
      .x$max_abs_coefficient_same_outcome_score <- max_abs
      .x$abs_coef_share_of_max_same_outcome_score <- ifelse(is.finite(max_abs) & max_abs > 0, abs_coef / max_abs, NA_real_)
      .x$abs_coef_ratio_to_common_same_outcome_score <- ifelse(is.finite(common_abs) & common_abs > 0, abs_coef / common_abs, NA_real_)
      .x$abs_coef_ratio_to_rowonly_same_outcome_score <- ifelse(is.finite(row_abs) & row_abs > 0, abs_coef / row_abs, NA_real_)
      .x$abs_coef_ratio_to_groupedonly_same_outcome_score <- ifelse(is.finite(grouped_abs) & grouped_abs > 0, abs_coef / grouped_abs, NA_real_)
      .x
    }) %>%
    ungroup()
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
    CFO_lead = get_lead_contiguous(.data$CFO, .data$year)
    # NOTE: future_Earnings_persistence is NOT computed here.
    # It was previously defined as NI_lead / A, which is identical to future_Earnings
    # (NI_lead / A). That duplicate outcome inflated economic-validity test counts
    # from 4 to 5 outcomes and from 12 to 15 coefficient tests. Removed 2026-06-26.
  ) %>%
  ungroup() %>%
  transmute(
    company = as.character(.data$company),
    year = as.integer(.data$year),
    A = num(.data$A),
    future_CFO = num(.data$CFO_lead) / num(.data$A),
    future_Earnings = num(.data$NI_lead) / num(.data$A),
    future_ROA = ifelse(num(.data$A_lead) > 0, num(.data$NI_lead) / num(.data$A_lead), NA_real_)
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

# Corrected outcome set: 4 outcome definitions across 3 economic constructs
# (cash realization = future_CFO; future earnings under two asset scalings =
# future_Earnings [NI_lead/A] and future_ROA [NI_lead/A_lead]; accrual reversal),
# times 3 membership terms -> 12 coefficient tests per score variable.
# NOTE: future_ROA and future_Earnings share the NI_{t+1} numerator and differ only
# by asset scaling, so they are NOT fully independent constructs. di05b's
# EV_DENOMINATOR_HARMONIZED specification diagnoses whether membership sign patterns
# survive the current-asset vs lead-asset scaling choice. Do not describe these as
# "4 independent outcomes" in the manuscript; describe them as 4 outcome definitions.
# future_Earnings_persistence was removed because it duplicated future_Earnings (both = NI_lead / A).
# See script_version for correction history.
outcomes <- c("future_CFO", "future_ROA", "future_Earnings", "accrual_reversal")
outcomes <- outcomes[outcomes %in% names(analysis)]

# ── Guardrail: prevent future_Earnings_persistence from silently re-entering ──
if ("future_Earnings_persistence" %in% outcomes) {
  stop("[GUARDRAIL] future_Earnings_persistence must not appear in the di05 outcomes vector. ",
       "It duplicates future_Earnings (both = NI_lead / A). Remove it or implement a ",
       "theoretically distinct persistence model before including it.")
}
# ── Guardrail: detect duplicate outcome columns in analysis data ──
outcome_cols_in_data <- outcomes[outcomes %in% names(analysis)]
if (length(outcome_cols_in_data) >= 2) {
  outcome_matrix <- sapply(outcome_cols_in_data, function(col) analysis[[col]])
  for (i in seq_len(ncol(outcome_matrix) - 1)) {
    for (j in seq(i + 1, ncol(outcome_matrix))) {
      vals_i <- outcome_matrix[, i]; vals_j <- outcome_matrix[, j]
      finite_both <- is.finite(vals_i) & is.finite(vals_j)
      if (sum(finite_both) > 10 && isTRUE(all.equal(vals_i[finite_both], vals_j[finite_both], tolerance = 1e-12))) {
        stop("[GUARDRAIL] Outcomes '", outcome_cols_in_data[i], "' and '", outcome_cols_in_data[j],
             "' are numerically identical in the analysis data. Remove the duplicate before proceeding.")
      }
    }
  }
}
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

outcome_stats <- bind_rows(lapply(unique(analysis$reported_score_variable), function(score_label) {
  use_score <- analysis %>% filter(.data$reported_score_variable == !!score_label)
  bind_rows(lapply(outcomes, function(outcome) {
    vals <- num(use_score[[outcome]])
    vals <- vals[is.finite(vals)]
    data.frame(
      reported_score_variable = score_label,
      outcome = outcome,
      outcome_N_nonmissing = length(vals),
      outcome_mean = if (length(vals)) mean(vals) else NA_real_,
      outcome_sd = if (length(vals) > 1) stats::sd(vals) else NA_real_,
      outcome_abs_mean = if (length(vals)) mean(abs(vals)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}))

validity <- validity %>%
  left_join(outcome_stats, by = c("reported_score_variable", "outcome")) %>%
  mutate(
    n_outcome_definitions = length(outcomes),
    # future_ROA and future_Earnings share the NI_{t+1} numerator (differ only by asset
    # scaling), so the 4 definitions span 3 distinct economic constructs, not 4.
    n_distinct_constructs = 3L,
    # Backward-compatible alias retained for downstream readers (ma17/tests). It now
    # carries the count of outcome DEFINITIONS, not independent constructs; see
    # n_distinct_constructs and the reviewer note for the correct interpretation.
    n_independent_outcomes = length(outcomes),
    n_terms_tested = length(terms_interest),
    expected_tests_per_score = length(outcomes) * length(terms_interest),
    expected_sign = expected_sign_for(.data$outcome),
    expected_sign_rationale = expected_sign_rationale_for(.data$outcome),
    observed_sign = sign_label(.data$coefficient),
    sign_consistent = dplyr::case_when(
      !is.finite(num(.data$coefficient)) | is.na(.data$expected_sign) ~ NA,
      .data$expected_sign == "negative" ~ num(.data$coefficient) < 0,
      .data$expected_sign == "positive" ~ num(.data$coefficient) > 0,
      TRUE ~ NA
    ),
    sign_pattern = dplyr::case_when(
      is.na(.data$sign_consistent) ~ "not_evaluable",
      .data$sign_consistent ~ "sign_consistent",
      TRUE ~ "sign_reversal_or_wrong_direction"
    ),
    effect_size_sd = ifelse(is.finite(.data$outcome_sd) & .data$outcome_sd > 0,
                            num(.data$coefficient) / .data$outcome_sd, NA_real_),
    effect_size_abs_mean = ifelse(is.finite(.data$outcome_abs_mean) & .data$outcome_abs_mean > 0,
                                  num(.data$coefficient) / .data$outcome_abs_mean, NA_real_)
  ) %>%
  add_bh_by_family(family_cols = c("reported_score_variable"),
                   p_col = "p_value",
                   q_col = "q_value_BH_score_family") %>%
  add_bh_global(p_col = "p_value", q_col = "q_value_BH_global") %>%
  add_relative_magnitude()

counts <- analysis %>%
  count(.data$reported_score_variable, .data$membership_class, name = "N") %>%
  group_by(.data$reported_score_variable) %>%
  mutate(share = .data$N / sum(.data$N)) %>%
  ungroup()

means <- analysis %>%
  group_by(.data$reported_score_variable, .data$membership_class) %>%
  summarise(across(all_of(outcomes), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
            N = n(), .groups = "drop")

influence <- bind_rows(lapply(outcomes, function(outcome) {
  analysis %>%
    filter(is.finite(num(.data[[outcome]]))) %>%
    count(.data$reported_score_variable, .data$membership_class, .data$company, name = "firm_obs") %>%
    group_by(.data$reported_score_variable, .data$membership_class) %>%
    summarise(
      outcome = outcome,
      N_obs = sum(.data$firm_obs),
      N_firms = dplyr::n_distinct(.data$company),
      max_obs_per_firm = max(.data$firm_obs),
      max_firm_share = max(.data$firm_obs) / sum(.data$firm_obs),
      n_firms_ge_5pct_share = sum((.data$firm_obs / sum(.data$firm_obs)) >= 0.05),
      n_firms_ge_10pct_share = sum((.data$firm_obs / sum(.data$firm_obs)) >= 0.10),
      .groups = "drop"
    )
}))

strong <- validity %>%
  filter(.data$model_status == "fit_ok", is.finite(.data$coefficient), !is.na(.data$p_value), .data$p_value <= 0.10) %>%
  count(.data$reported_score_variable, .data$term, name = "significant_outcome_n")

decision_detail <- validity %>%
  filter(.data$model_status == "fit_ok") %>%
  group_by(.data$reported_score_variable) %>%
  summarise(
    n_outcome_definitions = dplyr::n_distinct(.data$outcome),
    n_distinct_constructs = 3L,
    n_independent_outcomes = dplyr::n_distinct(.data$outcome),
    n_terms_tested = dplyr::n_distinct(.data$term),
    expected_tests_per_score = dplyr::n_distinct(.data$outcome) * dplyr::n_distinct(.data$term),
    fitted_tests = n(),
    significant_tests_p10 = sum(!is.na(.data$p_value) & .data$p_value <= 0.10),
    significant_tests_q10_BH_score_family = sum(!is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10),
    significant_tests_q10_BH_global = sum(!is.na(.data$q_value_BH_global) & .data$q_value_BH_global <= 0.10),
    common_top5_significant_tests_p10 = sum(.data$term == "CommonTop5" & !is.na(.data$p_value) & .data$p_value <= 0.10),
    row_only_significant_tests_p10 = sum(.data$term == "RowOnlyTop5" & !is.na(.data$p_value) & .data$p_value <= 0.10),
    grouped_only_significant_tests_p10 = sum(.data$term == "GroupedOnlyTop5" & !is.na(.data$p_value) & .data$p_value <= 0.10),
    common_top5_significant_tests_q10_BH_score_family = sum(.data$term == "CommonTop5" & !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10),
    row_only_significant_tests_q10_BH_score_family = sum(.data$term == "RowOnlyTop5" & !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10),
    grouped_only_significant_tests_q10_BH_score_family = sum(.data$term == "GroupedOnlyTop5" & !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10),
    sign_inconsistent_significant_tests_q10_BH_score_family = sum(.data$sign_consistent == FALSE & !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10, na.rm = TRUE),
    grouped_only_wrong_direction_tests_q10_BH_score_family = sum(.data$term == "GroupedOnlyTop5" & .data$sign_consistent == FALSE & !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10, na.rm = TRUE),
    .groups = "drop"
  )

overall_decision <- dplyr::case_when(
  nrow(validity) == 0 || !any(validity$model_status == "fit_ok") ~ "FAIL_ECONOMIC_VALIDITY_UNAVAILABLE",
  any(decision_detail$common_top5_significant_tests_q10_BH_score_family > 0, na.rm = TRUE) ~ "PASS_COMMON_TOP_TAIL_ECONOMIC_SIGNAL_AVAILABLE",
  any(decision_detail$row_only_significant_tests_q10_BH_score_family > 0 | decision_detail$grouped_only_significant_tests_q10_BH_score_family > 0, na.rm = TRUE) ~ "WARN_TARGET_SPECIFIC_TOP_TAIL_ECONOMIC_SIGNAL",
  TRUE ~ "WARN_NO_STRONG_TOP_TAIL_ECONOMIC_SIGNAL"
)

decision <- decision_detail %>%
  mutate(
    economic_validity_decision = overall_decision,
    interpretation = dplyr::case_when(
      .data$common_top5_significant_tests_q10_BH_score_family > 0 ~ "Common top-tail membership has BH-adjusted downstream economic signal; core extremes are economically interpretable as supplementary screening evidence.",
      .data$row_only_significant_tests_q10_BH_score_family > 0 | .data$grouped_only_significant_tests_q10_BH_score_family > 0 ~ "Economic signal is target-specific after BH adjustment; interpret validation-target sensitivity as substantively relevant but supplementary.",
      TRUE ~ "Top-tail membership has limited BH-adjusted downstream economic signal in these supplementary tests."
    ),
    multiplicity_note = "Primary BH family is within reported_score_variable: 4 outcome definitions x 3 membership terms = 12 tests. The 4 definitions span 3 distinct constructs (cash realization; future earnings under two asset scalings; accrual reversal), since future_ROA and future_Earnings share the NI_{t+1} numerator. q_value_BH_global is also reported across all economic-validity tests as a stricter transparency check.",
    sign_note = "Expected signs are imposed equally on RowOnlyTop5, GroupedOnlyTop5, and CommonTop5. For future performance outcomes the expected sign is negative; for accrual_reversal, coded as -TA_scaled_{t+1}, the expected sign is positive."
  )

write_csv_safely(membership, membership_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(counts, counts_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(means, means_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(influence, influence_path, row.names = FALSE, fileEncoding = "UTF-8")
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
  "This is supplementary economic-validity evidence. It does not replace primary exact-KFold magnitude evidence and does not prove managerial intent.",
  "",
  "## Expected-sign convention",
  "",
  "Expected signs are imposed equally on `RowOnlyTop5`, `GroupedOnlyTop5`, and `CommonTop5`. A top-tail abnormal-accrual screen should not receive a separate expected-sign waiver merely because it is grouped-only.",
  "For `future_CFO`, `future_ROA`, and `future_Earnings`, the expected sign is negative: top-tail abnormal-accrual observations are expected to be followed by weaker future operating performance if the screen captures economically adverse accrual intensity.",
  "For `accrual_reversal`, the script hardcodes the expected sign as positive because the outcome is defined as `-TA_scaled_{t+1}`. Under current-period income-increasing accrual reversal, next-period total accruals should be lower/negative, making `-TA_scaled_{t+1}` larger.",
  "A positive grouped-only coefficient on future performance is therefore classified as sign-inconsistent rather than treated as having no ex-ante expected sign.",
  "",
  "## Multiplicity convention",
  "",
  "The corrected diagnostic reports four outcome definitions: future CFO, future ROA, future earnings, and accrual reversal.",
  "These four definitions span three distinct economic constructs: cash realization (future CFO), future earnings under two asset scalings (future_Earnings = NI_{t+1}/A and future_ROA = NI_{t+1}/A_{t+1}), and accrual reversal. future_ROA and future_Earnings are therefore not fully independent: they share the NI_{t+1} numerator and differ only by the asset-scaling denominator.",
  "The companion robustness script (di05b) includes an `EV_DENOMINATOR_HARMONIZED` specification that re-estimates NI and CFO outcomes under both current-asset and lead-asset scaling to verify that membership sign patterns are not artifacts of the denominator choice.",
  "The primary BH correction family is within each `reported_score_variable`: 4 outcome definitions times 3 membership terms = 12 coefficient tests.",
  "The table also reports `q_value_BH_global`, a stricter BH adjustment across all economic-validity tests, for transparency.",
  "",
  "## Effect-size and influence diagnostics",
  "",
  "The validity table reports `effect_size_sd` and `effect_size_abs_mean` so coefficient magnitudes can be compared across outcomes.",
  "It also reports relative absolute coefficient magnitudes within each score/outcome family, allowing row-only, grouped-only, and common coefficients to be compared directly.",
  "The companion influence table reports how many firms contribute to each top-tail membership class and whether observations are concentrated in a small number of firms.",
  "",
  "## Correction Note (2026-06-26)",
  "",
  "The previous economic-validity draft double-counted future earnings because `future_Earnings_persistence` duplicated `future_Earnings`.",
  "Both were computed as `NI_lead / A`, making them numerically identical.",
  "The correction reduces the count denominator from 15 coefficient tests to 12 coefficient tests, and from five named outcomes to four outcome definitions (spanning three distinct constructs; future_ROA and future_Earnings share the NI_{t+1} numerator).",
  "The qualitative pattern should be interpreted after this correction: the correction affects the count, not necessarily the existence of the economic-validity signal."
)
writeLines(note, note_path, useBytes = TRUE)

input_paths <- c(sets_path, rt_sample_path, raw_path)
output_paths <- c(membership_path, counts_path, means_path, influence_path, validity_path, decision_path, note_path)
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
