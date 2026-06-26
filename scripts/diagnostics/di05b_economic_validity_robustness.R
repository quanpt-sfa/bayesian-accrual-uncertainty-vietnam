# -----------------------------------------------------------------------------
# Script: di05b_economic_validity_robustness.R
# Purpose: Supplementary robustness diagnostics for di05 economic-validity tests.
#
# Intended use:
#   Rscript scripts/diagnostics/di05b_economic_validity_robustness.R
#
# This script does NOT fit or refit Bayesian models. It only runs downstream
# OLS-style economic-validity robustness diagnostics using already-generated
# exact-KFold top-tail membership outputs.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

source("scripts/ma00_setup.R")
phase_begin("di05b", "Economic-validity robustness diagnostics for exact-KFold top-tail groups")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

script_start_time <- Sys.time()
script_name <- "scripts/diagnostics/di05b_economic_validity_robustness.R"
script_version <- "2026-06-26-v2-safe-cluster-vcov-status"

diagnostics_dir <- file.path(output_root, "diagnostics")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

sets_path <- file.path(diagnostics_dir, "table_exact_kfold_reclassification_sets.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
raw_path <- data_path

robustness_path <- file.path(diagnostics_dir, "table_top_tail_group_economic_validity_robustness.csv")
decision_path <- file.path(diagnostics_dir, "table_top_tail_group_economic_validity_robustness_decision.csv")
denominator_path <- file.path(diagnostics_dir, "table_top_tail_group_denominator_sensitivity.csv")
influence_path <- file.path(diagnostics_dir, "table_top_tail_group_influence_by_firm.csv")
drop_one_path <- file.path(diagnostics_dir, "table_top_tail_group_drop_one_firm_influence.csv")
io_manifest_path <- file.path(diagnostics_dir, "table_top_tail_group_economic_validity_robustness_io_manifest.csv")
note_path <- file.path(diagnostics_dir, "economic_validity_robustness_reviewer_note.md")

# Optional because drop-one-firm refits many OLS models. This is still not a
# Bayesian refit; it is a lightweight OLS influence diagnostic.
RUN_DROP_ONE_FIRM <- toupper(Sys.getenv("ACCRUAL_DI05B_RUN_DROP_ONE_FIRM", "FALSE")) %in% c("TRUE", "1", "YES", "Y")

read_required_csv <- function(path, label) {
  if (!file.exists(path)) stop("[BLOCKER] Missing ", label, ": ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

required_cols <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("[BLOCKER] ", label, " lacks column(s): ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

num <- function(x) suppressWarnings(as.numeric(x))

safe_div <- function(a, b) {
  a <- num(a); b <- num(b)
  ifelse(is.finite(a) & is.finite(b) & b != 0, a / b, NA_real_)
}

winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  x <- num(x)
  finite <- is.finite(x)
  if (sum(finite) < 5) return(x)
  qs <- stats::quantile(x[finite], probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  x[finite] <- pmin(pmax(x[finite], qs[1]), qs[2])
  x
}

git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}
file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

cluster_for_fit <- function(fit, data, cluster_col) {
  mf <- stats::model.frame(fit)
  idx <- suppressWarnings(as.integer(rownames(mf)))
  if (length(idx) == nrow(mf) && all(!is.na(idx)) && all(idx >= 1) && all(idx <= nrow(data))) {
    return(data[[cluster_col]][idx])
  }
  rep(NA_character_, nrow(mf))
}

cluster_frame_for_fit <- function(fit, data, cluster_cols) {
  mf <- stats::model.frame(fit)
  idx <- suppressWarnings(as.integer(rownames(mf)))
  if (length(idx) == nrow(mf) && all(!is.na(idx)) && all(idx >= 1) && all(idx <= nrow(data))) {
    out <- data[idx, cluster_cols, drop = FALSE]
    return(out)
  }
  out <- data.frame(matrix(NA, nrow = nrow(mf), ncol = length(cluster_cols)))
  names(out) <- cluster_cols
  out
}

vcovCL_safe <- function(fit, cluster, fix_psd = TRUE) {
  if (!requireNamespace("sandwich", quietly = TRUE)) return(NULL)
  args <- list(x = fit, cluster = cluster)
  # sandwich::vcovCL has a `fix` argument in recent versions. It applies an
  # eigendecomposition correction when the clustered covariance matrix is not
  # positive semidefinite. This is especially useful for two-way clustering with
  # short panels and many fixed effects.
  if ("fix" %in% names(formals(sandwich::vcovCL))) args$fix <- isTRUE(fix_psd)
  tryCatch(do.call(sandwich::vcovCL, args), error = function(e) NULL)
}

vcov_has_valid_diagonal <- function(vc) {
  if (is.null(vc)) return(FALSE)
  d <- suppressWarnings(diag(vc))
  length(d) > 0 && all(is.finite(d)) && all(d >= -1e-12)
}

coeftest_from_vcov <- function(fit, vc) {
  if (is.null(vc) || !requireNamespace("lmtest", quietly = TRUE)) return(NULL)
  out <- tryCatch(
    suppressWarnings(lmtest::coeftest(fit, vcov. = vc)),
    error = function(e) NULL
  )
  if (is.null(out)) return(NULL)
  # Coerce via as.matrix() (NOT as.data.frame()) so the 4 columns are preserved.
  # as.data.frame() on a coeftest object collapses to a single column, which would
  # make the ncol(...) >= 2 guard below silently never fire.
  out_mat <- as.matrix(out)
  # Standard errors are column 2. If sqrt(diag(vcov)) failed, this column contains
  # NaN/NA and must not be used silently -> reject so caller falls back to OLS.
  if (ncol(out_mat) >= 2 && any(!is.finite(out_mat[, 2]))) return(NULL)
  out
}

coef_table <- function(fit, data, se_type = "firm") {
  out <- NULL
  se_method_used <- "ols_default"
  se_status <- "ols_default"

  if (requireNamespace("sandwich", quietly = TRUE) && requireNamespace("lmtest", quietly = TRUE)) {
    if (identical(se_type, "firm")) {
      cl <- cluster_for_fit(fit, data, "company")
      if (length(cl) == stats::nobs(fit) && dplyr::n_distinct(cl, na.rm = TRUE) >= 2) {
        vc <- vcovCL_safe(fit, cluster = cl, fix_psd = TRUE)
        if (vcov_has_valid_diagonal(vc)) {
          out <- coeftest_from_vcov(fit, vc)
          if (!is.null(out)) {
            se_method_used <- "firm_cluster_vcovCL_fix_if_supported"
            se_status <- "cluster_se_ok"
          }
        } else {
          se_status <- "invalid_firm_cluster_vcov"
        }
      } else {
        se_status <- "insufficient_firm_clusters_for_cluster_se"
      }
    } else if (identical(se_type, "two_way")) {
      cl <- cluster_frame_for_fit(fit, data, c("company", "year"))
      if (nrow(cl) == stats::nobs(fit) &&
          dplyr::n_distinct(cl$company, na.rm = TRUE) >= 2 &&
          dplyr::n_distinct(cl$year, na.rm = TRUE) >= 2) {
        vc <- vcovCL_safe(fit, cluster = cl, fix_psd = TRUE)
        if (vcov_has_valid_diagonal(vc)) {
          out <- coeftest_from_vcov(fit, vc)
          if (!is.null(out)) {
            se_method_used <- "two_way_cluster_vcovCL_fix_if_supported"
            se_status <- "cluster_se_ok"
          } else {
            se_status <- "two_way_cluster_coeftest_invalid_after_vcov"
          }
        } else {
          se_status <- "invalid_two_way_cluster_vcov"
        }
      } else {
        se_status <- "insufficient_clusters_for_two_way_cluster_se"
      }
    }
  } else {
    se_status <- "sandwich_or_lmtest_unavailable"
  }

  if (is.null(out)) {
    out <- summary(fit)$coefficients
    se_method_used <- "ols_summary_fallback"
    se_status <- paste0("fallback_ols_after_", se_status)
  }

  # CRITICAL: `out` may be a `coeftest` object (matrix-like with a special class).
  # Calling as.data.frame() directly on a coeftest object collapses it into a
  # single useless list-column named "x" and DROPS the coefficient rownames,
  # which silently turns every extracted coefficient into NA downstream.
  # Coerce via as.matrix() first to preserve both the 4 columns
  # (Estimate / Std. Error / t / p) and the coefficient rownames.
  out_mat <- as.matrix(out)
  out_df <- as.data.frame(out_mat, stringsAsFactors = FALSE)
  rownames(out_df) <- rownames(out_mat)
  attr(out_df, "se_method_used") <- se_method_used
  attr(out_df, "se_status") <- se_status
  out_df
}

p_adjust_with_na <- function(p) {
  out <- rep(NA_real_, length(p))
  idx <- which(is.finite(p))
  if (length(idx)) out[idx] <- stats::p.adjust(p[idx], method = "BH")
  out
}

add_bh_q_values <- function(df) {
  if (!nrow(df)) {
    df$q_value_BH_score_family <- numeric(0)
    df$q_value_BH_global <- numeric(0)
    df$n_tests_BH_score_family <- integer(0)
    df$n_tests_BH_global <- integer(0)
    return(df)
  }

  df <- df %>%
    group_by(.data$specification_id, .data$reported_score_variable) %>%
    group_modify(function(.x, .g) {
      .x$q_value_BH_score_family <- p_adjust_with_na(.x$p_value)
      .x$n_tests_BH_score_family <- sum(is.finite(.x$p_value))
      .x
    }) %>%
    ungroup()

  df$q_value_BH_global <- p_adjust_with_na(df$p_value)
  df$n_tests_BH_global <- sum(is.finite(df$p_value))
  df
}

observed_sign <- function(x) {
  x <- num(x)
  ifelse(!is.finite(x), NA_character_,
         ifelse(x > 0, "positive", ifelse(x < 0, "negative", "zero")))
}

expected_sign_for <- function(outcome) {
  # Convention fixed ex ante:
  #   - Top-tail abnormal-accrual screens are expected to predict weaker future
  #     performance/realization outcomes, hence negative signs for CFO/earnings/ROA.
  #   - accrual_reversal is coded as -TA_scaled_{t+1}; stronger next-period reversal
  #     implies lower/negative next-period accruals and therefore a positive value.
  if (identical(outcome, "accrual_reversal")) return("positive")
  "negative"
}

sign_is_consistent <- function(coef, expected) {
  s <- observed_sign(coef)
  ifelse(is.na(s) | is.na(expected), NA,
         ifelse(expected == "negative", s == "negative",
                ifelse(expected == "positive", s == "positive", NA)))
}

add_relative_magnitude <- function(df) {
  if (!nrow(df)) return(df)

  key <- c("specification_id", "reported_score_variable", "outcome")
  common <- df %>%
    filter(.data$term == "CommonTop5") %>%
    transmute(
      specification_id, reported_score_variable, outcome,
      common_coef = .data$coefficient
    )
  rowonly <- df %>%
    filter(.data$term == "RowOnlyTop5") %>%
    transmute(
      specification_id, reported_score_variable, outcome,
      rowonly_coef = .data$coefficient
    )
  groupedonly <- df %>%
    filter(.data$term == "GroupedOnlyTop5") %>%
    transmute(
      specification_id, reported_score_variable, outcome,
      groupedonly_coef = .data$coefficient
    )

  df %>%
    left_join(common, by = key) %>%
    left_join(rowonly, by = key) %>%
    left_join(groupedonly, by = key) %>%
    group_by(.data$specification_id, .data$reported_score_variable, .data$outcome) %>%
    mutate(
      abs_coefficient = abs(.data$coefficient),
      max_abs_coefficient_same_outcome_score = {
        vals <- abs(.data$coefficient[is.finite(.data$coefficient)])
        if (length(vals)) max(vals) else NA_real_
      },
      abs_coef_share_of_max_same_outcome_score = ifelse(
        is.finite(.data$max_abs_coefficient_same_outcome_score) &
          .data$max_abs_coefficient_same_outcome_score > 0,
        .data$abs_coefficient / .data$max_abs_coefficient_same_outcome_score,
        NA_real_
      ),
      abs_coef_ratio_to_common_same_outcome_score = ifelse(
        is.finite(.data$common_coef) & abs(.data$common_coef) > 0,
        abs(.data$coefficient) / abs(.data$common_coef),
        NA_real_
      ),
      abs_coef_ratio_to_rowonly_same_outcome_score = ifelse(
        is.finite(.data$rowonly_coef) & abs(.data$rowonly_coef) > 0,
        abs(.data$coefficient) / abs(.data$rowonly_coef),
        NA_real_
      ),
      abs_coef_ratio_to_groupedonly_same_outcome_score = ifelse(
        is.finite(.data$groupedonly_coef) & abs(.data$groupedonly_coef) > 0,
        abs(.data$coefficient) / abs(.data$groupedonly_coef),
        NA_real_
      )
    ) %>%
    ungroup()
}

sets <- read_required_csv(sets_path, "di03 reclassification sets")
sample_rt <- read_required_csv(rt_sample_path, "winsor no-lookahead sample")
if (!file.exists(raw_path)) stop("[BLOCKER] Raw data workbook missing: ", raw_path)
raw <- readxl::read_excel(raw_path, sheet = "Sheet1")

required_cols(sets, c("company", "year", "target_space", "membership_class"), "di03 sets")
required_cols(
  sample_rt,
  c("company", "year", "industry", "Size", "ROA_curr", "revenue_growth", "A_lag", "TA_scaled"),
  "winsor no-lookahead sample"
)
required_cols(raw, c("company", "year", "A", "NI", "CFO"), "raw workbook")

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
    A_lead = get_lead_contiguous(.data$A, .data$year),
    CFO_lead = get_lead_contiguous(.data$CFO, .data$year)
  ) %>%
  ungroup() %>%
  transmute(
    company = as.character(.data$company),
    year = as.integer(.data$year),
    A = num(.data$A),
    A_lead = num(.data$A_lead),
    NI_lead = num(.data$NI_lead),
    CFO_lead = num(.data$CFO_lead),
    future_CFO = safe_div(.data$CFO_lead, .data$A),
    future_CFO_lead_assets = safe_div(.data$CFO_lead, .data$A_lead),
    future_Earnings = safe_div(.data$NI_lead, .data$A),
    future_ROA = safe_div(.data$NI_lead, .data$A_lead),
    future_NI_scaled_current_assets = safe_div(.data$NI_lead, .data$A),
    future_NI_scaled_lead_assets = safe_div(.data$NI_lead, .data$A_lead)
  )

sample_rt_panel <- sample_rt %>%
  mutate(company = as.character(.data$company), year = as.integer(.data$year)) %>%
  group_by(.data$company) %>%
  summarise(
    panel_length = dplyr::n_distinct(.data$year[!is.na(.data$year)]),
    first_year_in_sample = min(.data$year, na.rm = TRUE),
    last_year_in_sample = max(.data$year, na.rm = TRUE),
    .groups = "drop"
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
  inner_join(sample_rt_panel, by = "company") %>%
  mutate(firm_age_in_panel = .data$year - .data$first_year_in_sample + 1L) %>%
  inner_join(raw_leads, by = c("company", "year")) %>%
  left_join(sample_rt_leads, by = c("company", "year")) %>%
  inner_join(membership, by = c("company", "year"))

if (!nrow(analysis)) stop("[BLOCKER] Economic-validity robustness membership join produced zero rows.")

# Outcome definitions:
# Core reporting outcomes retain the di05 labels but should be interpreted as
# four outcome definitions, not four fully independent constructs. future_ROA
# and future_Earnings both use NI_{t+1} in the numerator and differ by asset
# scaling. Denominator-harmonized specifications below diagnose this explicitly.
core_outcomes <- c("future_CFO", "future_ROA", "future_Earnings", "accrual_reversal")
core_outcomes <- core_outcomes[core_outcomes %in% names(analysis)]

denominator_outcomes <- c(
  "future_NI_scaled_current_assets",
  "future_NI_scaled_lead_assets",
  "future_CFO",
  "future_CFO_lead_assets"
)
denominator_outcomes <- denominator_outcomes[denominator_outcomes %in% names(analysis)]

if ("future_Earnings_persistence" %in% names(analysis)) {
  stop("[GUARDRAIL] future_Earnings_persistence must not appear in di05b analysis data.")
}
if ("future_Earnings_persistence" %in% c(core_outcomes, denominator_outcomes)) {
  stop("[GUARDRAIL] future_Earnings_persistence must not appear in di05b outcome sets.")
}

specs <- list(
  list(
    specification_id = "EV_BASELINE_CORE",
    specification_family = "baseline",
    outcome_set = core_outcomes,
    controls = c("Size", "ROA_curr", "revenue_growth"),
    fixed_effects = c("industry", "year"),
    se_type = "firm",
    winsorize_outcome = FALSE,
    role = "main robustness baseline matching di05"
  ),
  list(
    specification_id = "EV_NO_CONTROLS_CORE",
    specification_family = "control_sensitivity",
    outcome_set = core_outcomes,
    controls = character(),
    fixed_effects = c("industry", "year"),
    se_type = "firm",
    winsorize_outcome = FALSE,
    role = "removes accounting controls while retaining industry/year FE"
  ),
  list(
    specification_id = "EV_PANEL_LENGTH_CORE",
    specification_family = "composition_sensitivity",
    outcome_set = core_outcomes,
    controls = c("Size", "ROA_curr", "revenue_growth", "panel_length", "firm_age_in_panel"),
    fixed_effects = c("industry", "year"),
    se_type = "firm",
    winsorize_outcome = FALSE,
    role = "preferred composition diagnostic; retains between-firm variation"
  ),
  list(
    specification_id = "EV_FIRM_FE_CORE",
    specification_family = "within_firm_stress_test",
    outcome_set = core_outcomes,
    controls = c("Size", "ROA_curr", "revenue_growth"),
    fixed_effects = c("company", "year"),
    se_type = "firm",
    winsorize_outcome = FALSE,
    role = "within-firm stress test; do not interpret loss of significance as composition proof"
  ),
  list(
    specification_id = "EV_WINSORIZED_OUTCOME_CORE",
    specification_family = "outcome_outlier_sensitivity",
    outcome_set = core_outcomes,
    controls = c("Size", "ROA_curr", "revenue_growth"),
    fixed_effects = c("industry", "year"),
    se_type = "firm",
    winsorize_outcome = TRUE,
    role = "winsorizes each outcome at 1/99 percentiles before fitting"
  ),
  list(
    specification_id = "EV_TWO_WAY_CLUSTER_CORE",
    specification_family = "se_sensitivity",
    outcome_set = core_outcomes,
    controls = c("Size", "ROA_curr", "revenue_growth"),
    fixed_effects = c("industry", "year"),
    se_type = "two_way",
    winsorize_outcome = FALSE,
    role = "two-way firm/year clustered SE when sandwich/lmtest support it"
  ),
  list(
    specification_id = "EV_DENOMINATOR_HARMONIZED",
    specification_family = "denominator_sensitivity",
    outcome_set = denominator_outcomes,
    controls = c("Size", "ROA_curr", "revenue_growth"),
    fixed_effects = c("industry", "year"),
    se_type = "firm",
    winsorize_outcome = FALSE,
    role = "checks whether NI/CFO signs persist under current-asset vs lead-asset scaling"
  )
)

terms_interest <- c("RowOnlyTop5TRUE", "GroupedOnlyTop5TRUE", "CommonTop5TRUE")

make_formula <- function(outcome, controls, fixed_effects) {
  rhs <- c("RowOnlyTop5", "GroupedOnlyTop5", "CommonTop5", controls)
  fe_terms <- paste0("factor(", fixed_effects, ")")
  rhs <- c(rhs, fe_terms)
  stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
}

fit_one <- function(df, spec, score_label, outcome) {
  use <- df %>% filter(.data$reported_score_variable == !!score_label)
  if (isTRUE(spec$winsorize_outcome)) {
    use[[outcome]] <- winsorize_vec(use[[outcome]])
  }
  use <- use[is.finite(num(use[[outcome]])), , drop = FALSE]
  rownames(use) <- seq_len(nrow(use))

  if (nrow(use) < 30 ||
      length(unique(use$year[!is.na(use$year)])) < 2 ||
      length(unique(use$company[!is.na(use$company)])) < 2) {
    return(data.frame(
      specification_id = spec$specification_id,
      specification_family = spec$specification_family,
      specification_role = spec$role,
      reported_score_variable = score_label,
      outcome = outcome,
      term = sub("TRUE$", "", terms_interest),
      coefficient = NA_real_, std_error = NA_real_, t_value = NA_real_, p_value = NA_real_,
      N_obs = nrow(use), N_firms = dplyr::n_distinct(use$company),
      r_squared = NA_real_, adj_r_squared = NA_real_,
      outcome_sd = if (nrow(use)) stats::sd(num(use[[outcome]]), na.rm = TRUE) else NA_real_,
      outcome_abs_mean = if (nrow(use)) mean(abs(num(use[[outcome]])), na.rm = TRUE) else NA_real_,
      model_status = "insufficient_variation",
      se_type = spec$se_type,
      fixed_effects = paste(spec$fixed_effects, collapse = "+"),
      controls = paste(spec$controls, collapse = "+"),
      winsorize_outcome = isTRUE(spec$winsorize_outcome),
      stringsAsFactors = FALSE
    ))
  }

  form <- make_formula(outcome, spec$controls, spec$fixed_effects)
  fit <- tryCatch(stats::lm(form, data = use), error = function(e) NULL)
  if (is.null(fit)) {
    return(data.frame(
      specification_id = spec$specification_id,
      specification_family = spec$specification_family,
      specification_role = spec$role,
      reported_score_variable = score_label,
      outcome = outcome,
      term = sub("TRUE$", "", terms_interest),
      coefficient = NA_real_, std_error = NA_real_, t_value = NA_real_, p_value = NA_real_,
      N_obs = nrow(use), N_firms = dplyr::n_distinct(use$company),
      r_squared = NA_real_, adj_r_squared = NA_real_,
      outcome_sd = stats::sd(num(use[[outcome]]), na.rm = TRUE),
      outcome_abs_mean = mean(abs(num(use[[outcome]])), na.rm = TRUE),
      model_status = "fit_failed",
      se_type = spec$se_type,
      fixed_effects = paste(spec$fixed_effects, collapse = "+"),
      controls = paste(spec$controls, collapse = "+"),
      winsorize_outcome = isTRUE(spec$winsorize_outcome),
      stringsAsFactors = FALSE
    ))
  }

  ct <- coef_table(fit, use, se_type = spec$se_type)
  se_method_used <- attr(ct, "se_method_used", exact = TRUE)
  se_status <- attr(ct, "se_status", exact = TRUE)
  if (is.null(se_method_used)) se_method_used <- NA_character_
  if (is.null(se_status)) se_status <- NA_character_
  nm <- rownames(ct)
  out_sd <- stats::sd(num(use[[outcome]]), na.rm = TRUE)
  out_abs_mean <- mean(abs(num(use[[outcome]])), na.rm = TRUE)

  bind_rows(lapply(terms_interest, function(term) {
    idx <- which(nm == term)
    coef <- if (length(idx)) ct[idx[1], 1] else NA_real_
    exp_sign <- expected_sign_for(outcome)
    data.frame(
      specification_id = spec$specification_id,
      specification_family = spec$specification_family,
      specification_role = spec$role,
      reported_score_variable = score_label,
      outcome = outcome,
      term = sub("TRUE$", "", term),
      coefficient = coef,
      std_error = if (length(idx) && ncol(ct) >= 2) ct[idx[1], 2] else NA_real_,
      t_value = if (length(idx) && ncol(ct) >= 3) ct[idx[1], 3] else NA_real_,
      p_value = if (length(idx) && ncol(ct) >= 4) ct[idx[1], 4] else NA_real_,
      expected_sign = exp_sign,
      observed_sign = observed_sign(coef),
      sign_consistent = sign_is_consistent(coef, exp_sign),
      N_obs = stats::nobs(fit),
      N_firms = dplyr::n_distinct(use$company),
      r_squared = summary(fit)$r.squared,
      adj_r_squared = summary(fit)$adj.r.squared,
      outcome_sd = out_sd,
      outcome_abs_mean = out_abs_mean,
      effect_size_sd = ifelse(is.finite(out_sd) && out_sd > 0, coef / out_sd, NA_real_),
      effect_size_abs_mean = ifelse(is.finite(out_abs_mean) && out_abs_mean > 0, coef / out_abs_mean, NA_real_),
      model_status = "fit_ok",
      se_type = spec$se_type,
      se_method_used = se_method_used,
      se_status = se_status,
      fixed_effects = paste(spec$fixed_effects, collapse = "+"),
      controls = paste(spec$controls, collapse = "+"),
      winsorize_outcome = isTRUE(spec$winsorize_outcome),
      stringsAsFactors = FALSE
    )
  }))
}

score_labels <- unique(analysis$reported_score_variable)

robustness <- bind_rows(lapply(specs, function(spec) {
  bind_rows(lapply(score_labels, function(score_label) {
    bind_rows(lapply(spec$outcome_set, function(outcome) {
      fit_one(analysis, spec, score_label, outcome)
    }))
  }))
}))

# ── GUARDRAIL: a successful run must actually extract coefficients ───────────
# A fit can succeed (model_status == "fit_ok", valid R^2) while coefficient
# extraction silently fails (e.g. a coeftest->as.data.frame coercion that drops
# rownames turns every which(rownames == term) into an empty match -> NA). Such a
# run would still exit 0 and print [SUCCESS] with an all-NA coefficient column.
# Refuse to proceed if too many fit_ok rows carry NA coefficients.
local({
  fit_ok <- robustness$model_status == "fit_ok"
  n_fit_ok <- sum(fit_ok, na.rm = TRUE)
  n_coef_ok <- sum(fit_ok & is.finite(suppressWarnings(as.numeric(robustness$coefficient))), na.rm = TRUE)
  if (n_fit_ok > 0L && n_coef_ok < 0.5 * n_fit_ok) {
    stop("[BLOCKER] ", n_fit_ok - n_coef_ok, " of ", n_fit_ok,
         " fit_ok rows have NA coefficients. Coefficient extraction is broken ",
         "(not a statistical result). Refusing to write robustness output. ",
         "Check coef_table()/coeftest() coercion and term-name matching.")
  }
  cat(sprintf("[di05b] coefficient extraction check: %d/%d fit_ok rows have finite coefficients.\n",
              n_coef_ok, n_fit_ok))
})

robustness <- robustness %>%
  mutate(
    sign_pattern = case_when(
      is.na(.data$sign_consistent) ~ "unknown",
      .data$sign_consistent ~ "consistent_with_expected_sign",
      TRUE ~ "opposite_to_expected_sign"
    )
  )

robustness <- add_bh_q_values(robustness)
robustness <- add_relative_magnitude(robustness)

decision <- robustness %>%
  filter(.data$model_status == "fit_ok") %>%
  group_by(.data$specification_id, .data$specification_family, .data$reported_score_variable) %>%
  summarise(
    n_reported_outcomes = dplyr::n_distinct(.data$outcome),
    n_terms = dplyr::n_distinct(.data$term),
    n_tests = n(),
    n_tests_BH_score_family = max(.data$n_tests_BH_score_family, na.rm = TRUE),
    significant_tests_p10 = sum(!is.na(.data$p_value) & .data$p_value <= 0.10),
    significant_tests_q10_score_family = sum(!is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10),
    significant_tests_q10_global = sum(!is.na(.data$q_value_BH_global) & .data$q_value_BH_global <= 0.10),
    sign_consistent_q10_score_family = sum(!is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10 & .data$sign_consistent %in% TRUE),
    sign_reversal_q10_score_family = sum(!is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10 & .data$sign_consistent %in% FALSE),
    grouped_only_sign_reversal_q10_score_family = sum(
      .data$term == "GroupedOnlyTop5" &
        !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10 &
        .data$sign_consistent %in% FALSE
    ),
    row_only_sign_consistent_q10_score_family = sum(
      .data$term == "RowOnlyTop5" &
        !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10 &
        .data$sign_consistent %in% TRUE
    ),
    common_sign_consistent_q10_score_family = sum(
      .data$term == "CommonTop5" &
        !is.na(.data$q_value_BH_score_family) & .data$q_value_BH_score_family <= 0.10 &
        .data$sign_consistent %in% TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    robustness_decision = case_when(
      .data$grouped_only_sign_reversal_q10_score_family > 0 ~ "WARN_GROUPED_ONLY_SIGN_REVERSAL_AVAILABLE",
      .data$row_only_sign_consistent_q10_score_family > 0 | .data$common_sign_consistent_q10_score_family > 0 ~ "PASS_SIGN_CONSISTENT_SIGNAL_AVAILABLE",
      TRUE ~ "WARN_NO_BH_SUPPORTED_ECONOMIC_SIGNAL"
    )
  )

denominator_sensitivity <- robustness %>%
  filter(.data$specification_family == "denominator_sensitivity") %>%
  arrange(.data$reported_score_variable, .data$outcome, .data$term)

influence_by_firm <- bind_rows(lapply(score_labels, function(score_label) {
  bind_rows(lapply(core_outcomes, function(outcome) {
    bind_rows(lapply(c("RowOnlyTop5", "GroupedOnlyTop5", "CommonTop5"), function(term_label) {
      flag_col <- term_label
      use <- analysis %>%
        filter(.data$reported_score_variable == !!score_label, .data[[flag_col]] %in% TRUE, is.finite(num(.data[[outcome]])))
      if (!nrow(use)) {
        return(data.frame(
          reported_score_variable = score_label,
          outcome = outcome,
          term = term_label,
          N_obs = 0L,
          N_firms = 0L,
          max_obs_per_firm = NA_integer_,
          max_firm_share = NA_real_,
          n_firms_ge_5pct_share = NA_integer_,
          n_firms_ge_10pct_share = NA_integer_,
          top_firm = NA_character_,
          stringsAsFactors = FALSE
        ))
      }
      firm_counts <- use %>% count(.data$company, name = "firm_obs_n") %>% arrange(desc(.data$firm_obs_n), .data$company)
      data.frame(
        reported_score_variable = score_label,
        outcome = outcome,
        term = term_label,
        N_obs = nrow(use),
        N_firms = dplyr::n_distinct(use$company),
        max_obs_per_firm = max(firm_counts$firm_obs_n),
        max_firm_share = max(firm_counts$firm_obs_n) / nrow(use),
        n_firms_ge_5pct_share = sum(firm_counts$firm_obs_n / nrow(use) >= 0.05),
        n_firms_ge_10pct_share = sum(firm_counts$firm_obs_n / nrow(use) >= 0.10),
        top_firm = as.character(firm_counts$company[[1]]),
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

drop_one_summary <- data.frame(
  specification_id = character(),
  reported_score_variable = character(),
  outcome = character(),
  term = character(),
  full_coefficient = numeric(),
  drop_one_min = numeric(),
  drop_one_max = numeric(),
  drop_one_median = numeric(),
  max_abs_change = numeric(),
  sign_flip_n = integer(),
  firms_dropped_n = integer(),
  stringsAsFactors = FALSE
)

if (RUN_DROP_ONE_FIRM) {
  message("[di05b] Running optional drop-one-firm OLS influence diagnostic.")
  base_spec <- specs[[which(vapply(specs, function(s) identical(s$specification_id, "EV_BASELINE_CORE"), logical(1)))[1]]]

  fit_coef_only <- function(df, spec, score_label, outcome) {
    use <- df %>% filter(.data$reported_score_variable == !!score_label)
    use <- use[is.finite(num(use[[outcome]])), , drop = FALSE]
    rownames(use) <- seq_len(nrow(use))
    if (nrow(use) < 30 || length(unique(use$year)) < 2 || length(unique(use$company)) < 2) return(NULL)
    fit <- tryCatch(stats::lm(make_formula(outcome, spec$controls, spec$fixed_effects), data = use), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    cf <- stats::coef(fit)
    cf
  }

  drop_one_summary <- bind_rows(lapply(score_labels, function(score_label) {
    bind_rows(lapply(core_outcomes, function(outcome) {
      full_cf <- fit_coef_only(analysis, base_spec, score_label, outcome)
      if (is.null(full_cf)) return(NULL)
      bind_rows(lapply(c("RowOnlyTop5", "GroupedOnlyTop5", "CommonTop5"), function(term_label) {
        term_name <- paste0(term_label, "TRUE")
        if (!term_name %in% names(full_cf)) return(NULL)
        firms <- sort(unique(analysis$company[analysis$reported_score_variable == score_label]))
        vals <- vapply(firms, function(fm) {
          cf <- fit_coef_only(analysis[analysis$company != fm, , drop = FALSE], base_spec, score_label, outcome)
          if (is.null(cf) || !term_name %in% names(cf)) return(NA_real_)
          unname(cf[[term_name]])
        }, numeric(1))
        vals <- vals[is.finite(vals)]
        full_val <- unname(full_cf[[term_name]])
        data.frame(
          specification_id = "EV_BASELINE_CORE",
          reported_score_variable = score_label,
          outcome = outcome,
          term = term_label,
          full_coefficient = full_val,
          drop_one_min = if (length(vals)) min(vals) else NA_real_,
          drop_one_max = if (length(vals)) max(vals) else NA_real_,
          drop_one_median = if (length(vals)) stats::median(vals) else NA_real_,
          max_abs_change = if (length(vals)) max(abs(vals - full_val)) else NA_real_,
          sign_flip_n = if (length(vals)) sum(observed_sign(vals) != observed_sign(full_val), na.rm = TRUE) else NA_integer_,
          firms_dropped_n = length(vals),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }))
} else {
  message("[di05b] Optional drop-one-firm diagnostic skipped. Set ACCRUAL_DI05B_RUN_DROP_ONE_FIRM=TRUE to run it.")
}

write_csv_safely(robustness, robustness_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(decision, decision_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(denominator_sensitivity, denominator_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(influence_by_firm, influence_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(drop_one_summary, drop_one_path, row.names = FALSE, fileEncoding = "UTF-8")

note <- c(
  "# Economic Validity Robustness Reviewer Note",
  "",
  "This diagnostic is a post-processing robustness layer for `di05`. It does not fit or refit Bayesian models.",
  "",
  "## Multiplicity convention",
  "",
  "The primary BH family is defined within each `(specification_id, reported_score_variable)` family.",
  "For the core outcome set, this corresponds to 4 outcome definitions x 3 top-tail membership terms = 12 tests.",
  "A global BH q-value across all robustness rows is also reported for transparency.",
  "",
  "## Cluster-robust covariance handling",
  "",
  "For cluster-robust standard errors, the script attempts `sandwich::vcovCL(..., fix=TRUE)` when the installed `sandwich` version supports the `fix` argument.",
  "This avoids NaN standard errors from non-positive-semidefinite clustered covariance matrices in short panels or high-dimensional FE specifications.",
  "The output table reports `se_method_used` and `se_status`; any OLS fallback should be treated as diagnostic only, not as successful two-way clustered inference.",
  "",
  "## Expected-sign convention",
  "",
  "For future-performance and realization outcomes (`future_CFO`, `future_ROA`, `future_Earnings`, and denominator-harmonized NI/CFO variants),",
  "all top-tail terms have the same expected sign: negative. This includes `GroupedOnlyTop5`.",
  "Therefore, a positive grouped-only coefficient is explicitly classified as opposite to the expected sign rather than exempted from sign evaluation.",
  "",
  "For `accrual_reversal`, the script uses `accrual_reversal = -TA_scaled_{t+1}`. Under this coding, stronger next-period reversal implies lower/negative next-period accruals and therefore a positive value; the expected sign is positive.",
  "",
  "## Firm-FE interpretation",
  "",
  "Firm fixed effects are included only as a within-firm stress test. Because top-tail membership classes are partly between-firm constructs, loss of statistical significance under firm FE is not interpreted as proof of composition-driven results.",
  "Composition sensitivity should be evaluated primarily using the panel-length/firm-age specification, which retains between-firm variation while controlling for observable panel support.",
  "",
  "## Denominator sensitivity",
  "",
  "The core table reports both `future_Earnings = NI_{t+1}/A_t` and `future_ROA = NI_{t+1}/A_{t+1}`. These are not treated as fully independent constructs because both use future net income in the numerator.",
  "The denominator-harmonized specification reports NI and CFO scaled by current assets and lead assets to diagnose whether sign patterns are driven by denominator choice.",
  "",
  "## Influence diagnostics",
  "",
  "The influence-by-firm table reports how concentrated each top-tail class is by firm.",
  "The optional drop-one-firm diagnostic is skipped by default; set `ACCRUAL_DI05B_RUN_DROP_ONE_FIRM=TRUE` to run it.",
  "",
  "Primary exact-KFold magnitude evidence remains the main RQ2 evidence. These economic-validity robustness checks are supplementary construct-validity diagnostics and do not prove managerial intent."
)
writeLines(note, note_path, useBytes = TRUE)

input_paths <- c(sets_path, rt_sample_path, raw_path)
output_paths <- c(robustness_path, decision_path, denominator_path, influence_path, drop_one_path, note_path)
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

cat("[SUCCESS] di05b economic-validity robustness outputs written under ", diagnostics_dir, "\n", sep = "")
phase_end("di05b", "Economic-validity robustness diagnostics for exact-KFold top-tail groups")
