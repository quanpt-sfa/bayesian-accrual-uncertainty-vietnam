# -----------------------------------------------------------------------------
# Script: 19_sensitivity_validation.R
# Purpose: Outcome validation rerun for each sensitivity-scenario DA output.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/00_helpers.R")
ensure_analysis_dirs()
ensure_sensitivity_dirs()
validate_final_analysis_config("sensitivity validation", final_mode = TRUE)

dry_run <- env_flag("ACCRUAL_DRY_RUN", "TRUE")
scenarios <- selected_sensitivity_scenarios()
ep_sample_path <- file.path(input_winsor_root, "tables", "final_common_ex_post_sample_winsor.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
data_path <- data_path

if (!dry_run) {
  for (pkg in c("readxl", "sandwich", "lmtest")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("[BLOCKER] Package required for validation is missing: ", pkg)
  }
  if (!file.exists(ep_sample_path) || !file.exists(rt_sample_path)) stop("[BLOCKER] Missing winsor sample files.")
  if (!file.exists(data_path)) stop("[BLOCKER] Raw data workbook missing: ", data_path)
}

plan_rows <- list()
result_rows <- list()

get_lead <- function(x, yr, n = 1) {
  lead_val <- dplyr::lead(x, n)
  lead_yr <- dplyr::lead(yr, n)
  ifelse(!is.na(lead_yr) & lead_yr == (yr + n), lead_val, NA)
}

run_one_validation <- function(scenario, da_df, sample_df, raw_leads, space_name) {
  df <- sample_df %>%
    select(company, year, industry, Size, ROA_curr, revenue_growth, A_lag) %>%
    inner_join(da_df %>% filter(target_space == !!space_name), by = c("company", "year", "industry")) %>%
    inner_join(raw_leads, by = c("company", "year")) %>%
    mutate(
      current_Earnings = NI / A_lag,
      future_CFO = CFO_lead_raw / A,
      future_Earnings = NI_lead / A,
      future_ROA = ifelse(A_lead > 0, NI_lead / A_lead, NA_real_),
      Abs_DA_z_predictive = abs(DA_z_predictive),
      Abs_DA_raw = abs(DA_raw),
      Surprise_High = DA_surprise_score
    )

  predictors <- c("Abs_DA_z_predictive", "Abs_DA_raw", "DA_tail_flag_95", "DA_tail_flag_98", "DA_ppd_tail_prob_two_sided", "DA_surprise_score")
  predictors <- predictors[predictors %in% names(df)]
  outcomes <- c("future_CFO", "future_Earnings", "future_ROA", "future_Earnings_persistence")
  out <- list()

  for (outcome in outcomes) {
    for (pred in predictors) {
      circularity <- if (space_name == "ex_post" && outcome %in% c("future_CFO", "future_Earnings", "future_Earnings_persistence")) {
        "HIGH_LOOKAHEAD_OR_CIRCULARITY_RISK"
      } else if (space_name == "real_time" && outcome %in% c("future_CFO", "future_Earnings", "future_Earnings_persistence")) {
        "LOWER_RISK_REAL_TIME_DA"
      } else {
        "LOW"
      }

      form_str <- if (outcome == "future_Earnings_persistence") {
        sprintf("future_Earnings ~ current_Earnings * %s + Size + revenue_growth + factor(industry) + factor(year)", pred)
      } else {
        sprintf("%s ~ %s + Size + ROA_curr + revenue_growth + factor(industry) + factor(year)", outcome, pred)
      }

      for (weighted in c(FALSE, TRUE)) {
        fit_data <- df
        fit <- NULL
        weight_var <- "None"
        if (weighted) {
          weight_var <- "NDA_sd_predict_stacked"
          if (!weight_var %in% names(fit_data)) next
          fit_data$reg_weight <- 1 / pmax(fit_data[[weight_var]]^2, .Machine$double.eps)
          fit <- tryCatch(lm(as.formula(form_str), data = fit_data, weights = reg_weight), error = function(e) NULL)
        } else {
          fit <- tryCatch(lm(as.formula(form_str), data = fit_data), error = function(e) NULL)
        }
        if (is.null(fit)) next
        coef_m <- tryCatch(
          lmtest::coeftest(fit, vcov. = sandwich::vcovCL(fit, cluster = ~company)),
          error = function(e) summary(fit)$coefficients
        )
        term_name <- pred
        if (outcome == "future_Earnings_persistence") {
          idx <- grep(paste0("current_Earnings.*", pred, "|", pred, ".*current_Earnings"), rownames(coef_m))
          if (length(idx) > 0) term_name <- rownames(coef_m)[idx[1]]
        }
        if (!term_name %in% rownames(coef_m)) next
        pval_matches <- grep("Pr\\(", colnames(coef_m), value = TRUE)
        pval_col <- if (length(pval_matches) > 0) pval_matches[1] else NA_character_
        stat_matches <- grep("t value|z value", colnames(coef_m), value = TRUE)
        stat_col <- if (length(stat_matches) > 0) stat_matches[1] else NA_character_
        out[[length(out) + 1]] <- data.frame(
          scenario = scenario,
          target_space = space_name,
          outcome = outcome,
          DA_measure_used = pred,
          weighted = weighted,
          weight_var = weight_var,
          coefficient = coef_m[term_name, "Estimate"],
          standard_error = coef_m[term_name, "Std. Error"],
          statistic = if (!is.na(stat_col)) coef_m[term_name, stat_col] else NA_real_,
          p_value = if (!is.na(pval_col)) coef_m[term_name, pval_col] else NA_real_,
          sign = sign(coef_m[term_name, "Estimate"]),
          magnitude = abs(coef_m[term_name, "Estimate"]),
          status = if (!is.na(pval_col) && coef_m[term_name, pval_col] < 0.05) "STATISTICALLY_FLAGGED" else "NOT_FLAGGED",
          circularity_risk = circularity,
          N_Obs = nobs(fit),
          R2 = summary(fit)$r.squared,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  bind_rows(out)
}

if (!dry_run) {
  df_ep <- read.csv(ep_sample_path, stringsAsFactors = FALSE)
  df_rt <- read.csv(rt_sample_path, stringsAsFactors = FALSE)
  df_raw <- readxl::read_excel(data_path, sheet = "Sheet1") %>%
    arrange(company, year) %>%
    group_by(company) %>%
    mutate(
      NI_lead = get_lead(NI, year),
      ROA_lead = get_lead(ROA, year),
      A_lead = get_lead(A, year),
      CFO_lead_raw = get_lead(CFO, year)
    ) %>%
    ungroup() %>%
    select(company, year, A, NI, ROA, CFO, NI_lead, ROA_lead, A_lead, CFO_lead_raw)
}

for (scenario in scenarios$Scenario) {
  scenario_root <- sensitivity_root(scenario)
  da_path <- sensitivity_accruals_path(scenario)
  plan_rows[[length(plan_rows) + 1]] <- data.frame(
    scenario = scenario,
    dry_run = dry_run,
    da_path = da_path,
    action = if (dry_run) "PLAN_ONLY" else if (file.exists(da_path)) "RUN_VALIDATION" else "BLOCKED_MISSING_DA",
    stringsAsFactors = FALSE
  )
  if (dry_run) next
  if (!file.exists(da_path)) stop("[BLOCKER] Missing scenario DA file: ", da_path)
  da_df <- read.csv(da_path, stringsAsFactors = FALSE)
  res <- bind_rows(
    run_one_validation(scenario, da_df, df_ep, df_raw, "ex_post"),
    run_one_validation(scenario, da_df, df_rt, df_raw, "real_time")
  )
  write.csv(res, file.path(scenario_root, "validation", paste0("table_sensitivity_validation_", scenario, ".csv")), row.names = FALSE)
  result_rows[[length(result_rows) + 1]] <- res
}

plan_df <- bind_rows(plan_rows)
results_df <- bind_rows(result_rows)
tables_root <- file.path(sensitivity_root(), "tables")
write.csv(plan_df, file.path(tables_root, "sensitivity_validation_plan.csv"), row.names = FALSE)
write.csv(results_df, file.path(tables_root, "sensitivity_validation_summary.csv"), row.names = FALSE)

writeLines(c(
  "Sensitivity validation notes",
  sprintf("Dry run: %s", dry_run),
  "Validation is rerun separately for each scenario and target space.",
  "Ex-post DA validation against future CFO/earnings is flagged for look-ahead/circularity risk when applicable."
), file.path(sensitivity_root(), "logs", "sensitivity_validation_notes.txt"))

cat("\n[SUCCESS] Sensitivity validation completed.\n")
