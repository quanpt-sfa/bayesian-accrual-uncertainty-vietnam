# -----------------------------------------------------------------------------
# Script: 21_validation_on_scaleaware_student_DA.R
# Purpose: Rerun outcome validation on the current ACCRUAL_OUTPUT_ROOT DA file.
# -----------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(sandwich)
library(lmtest)

source("scripts/00_helpers.R")
ensure_analysis_dirs()
validate_final_analysis_config("Phase 6b baseline validation", final_mode = TRUE)

validation_root <- file.path(output_root, "validation")
dir.create(validation_root, recursive = TRUE, showWarnings = FALSE)

master_path <- baseline_accruals_path()
ep_sample_path <- file.path(input_winsor_root, "tables", "final_common_ex_post_sample_winsor.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
data_path <- data_path

if (!file.exists(master_path)) stop("[BLOCKER] Missing current-root DA file: ", master_path)
if (!file.exists(ep_sample_path)) stop("[BLOCKER] Missing winsor ex-post sample: ", ep_sample_path)
if (!file.exists(rt_sample_path)) stop("[BLOCKER] Missing winsor no-lookahead sample: ", rt_sample_path)
if (!file.exists(data_path)) stop("[BLOCKER] Raw data workbook missing: ", data_path)

master_df <- read.csv(master_path, stringsAsFactors = FALSE)
df_ep_sample <- read.csv(ep_sample_path, stringsAsFactors = FALSE)
df_rt_sample <- read.csv(rt_sample_path, stringsAsFactors = FALSE)
df_raw <- readxl::read_excel(data_path, sheet = "Sheet1")

get_lead <- function(x, yr, n = 1) {
  lead_val <- dplyr::lead(x, n)
  lead_yr <- dplyr::lead(yr, n)
  ifelse(!is.na(lead_yr) & lead_yr == (yr + n), lead_val, NA)
}

df_raw_leads <- df_raw %>%
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

add_metadata <- function(df) {
  df %>%
    mutate(
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      DA_Source = "scale-aware Student-t baseline",
      Output_Root = output_root
    )
}

empty_validation_results <- function() {
  data.frame(
    Space = character(),
    Outcome = character(),
    Predictor = character(),
    Weighted = logical(),
    Weight_Var = character(),
    Circularity_Risk = character(),
    Coefficient = numeric(),
    Std_Error = numeric(),
    t_value = numeric(),
    p_value = numeric(),
    R2 = numeric(),
    Adj_R2 = numeric(),
    N_Obs = integer(),
    stringsAsFactors = FALSE
  )
}

cluster_for_fit <- function(fit, data) {
  mf <- model.frame(fit)
  idx <- suppressWarnings(as.integer(rownames(mf)))
  if (length(idx) == nrow(mf) && all(!is.na(idx)) && all(idx >= 1) && all(idx <= nrow(data))) {
    return(data$company[idx])
  }
  rep(NA_character_, nrow(mf))
}

safe_coeftest <- function(fit, data) {
  cl <- cluster_for_fit(fit, data)
  if (length(cl) == nobs(fit) && any(!is.na(cl)) && dplyr::n_distinct(cl, na.rm = TRUE) >= 2) {
    out <- tryCatch(
      lmtest::coeftest(fit, vcov. = sandwich::vcovCL(fit, cluster = cl)),
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)
  }
  summary(fit)$coefficients
}

safe_weight <- function(x) {
  w <- 1 / pmax(x^2, .Machine$double.eps)
  w[!is.finite(w) | w <= 0] <- NA_real_
  w
}

validation_debug_rows <- list()
add_validation_debug <- function(space, suffix, df_merged, candidate_predictors, predictors, note) {
  outcome_vars <- c("future_CFO", "future_Earnings", "future_ROA", "current_Earnings")
  controls <- c("Size", "ROA_curr", "revenue_growth", "industry", "year", "company")
  validation_debug_rows[[length(validation_debug_rows) + 1]] <<- data.frame(
    Space = space,
    Suffix = suffix,
    N_Merged = nrow(df_merged),
    N_Companies = if ("company" %in% names(df_merged)) dplyr::n_distinct(df_merged$company) else NA_integer_,
    Candidate_Predictors = paste(candidate_predictors, collapse = ";"),
    Present_Predictors = paste(predictors, collapse = ";"),
    Missing_Predictors = paste(setdiff(candidate_predictors, names(df_merged)), collapse = ";"),
    Missing_Controls = paste(setdiff(controls, names(df_merged)), collapse = ";"),
    Outcome_Nonmissing = paste(
      vapply(intersect(outcome_vars, names(df_merged)), function(v) sum(!is.na(df_merged[[v]])), integer(1)),
      names(vapply(intersect(outcome_vars, names(df_merged)), function(v) sum(!is.na(df_merged[[v]])), integer(1))),
      sep = ":",
      collapse = ";"
    ),
    Note = note,
    stringsAsFactors = FALSE
  )
}


run_validation <- function(space_name, sample_df, suffix) {
  df_merged <- sample_df %>%
    select(company, year, industry, Size, ROA_curr, revenue_growth, A_lag) %>%
    # Drop columns already taken from the sample before joining master_df.
    # Otherwise dplyr creates Size.x/Size.y, ROA_curr.x/ROA_curr.y, etc.,
    # while the validation formulas still refer to Size, ROA_curr, revenue_growth.
    inner_join(
      master_df %>% select(-any_of(c("industry", "Size", "ROA_curr", "revenue_growth", "A_lag"))),
      by = c("company", "year")
    ) %>%
    inner_join(df_raw_leads, by = c("company", "year")) %>%
    mutate(
      current_Earnings = NI / A_lag,
      future_CFO = CFO_lead_raw / A,
      future_Earnings = NI_lead / A,
      future_ROA = ifelse(A_lead > 0, NI_lead / A_lead, NA_real_),
      abs_DA_Jones_OLS_winsor = abs(DA_Jones_OLS_winsor),
      abs_DA_ModJones_OLS_winsor = abs(DA_ModJones_OLS_winsor),
      abs_DA_PerfModJones_OLS_winsor = abs(DA_PerfModJones_OLS_winsor)
    )

  candidate_predictors <- c(
    paste0("Abs_DA_z_estimation_stacked_", suffix, "_winsor"),
    paste0("Abs_DA_z_predictive_stacked_", suffix, "_winsor"),
    paste0("DA_tail_flag_95_", suffix, "_winsor"),
    paste0("Abs_DA_raw_stacked_", suffix, "_winsor"),
    "abs_DA_Jones_OLS_winsor",
    "abs_DA_ModJones_OLS_winsor",
    "abs_DA_PerfModJones_OLS_winsor"
  )
  predictors <- candidate_predictors[candidate_predictors %in% names(df_merged)]
  if (length(predictors) == 0) {
    add_validation_debug(space_name, suffix, df_merged, candidate_predictors, predictors, "No candidate predictors found after joins.")
  }
  outcomes <- c("future_CFO", "future_Earnings", "future_ROA", "future_Earnings_persistence")

  results <- list()
  for (outcome in outcomes) {
    for (pred in predictors) {
      circ_risk <- if (outcome == "future_CFO" && grepl("stacked", pred)) {
        if (suffix == "ep") "High" else "Moderate"
      } else {
        "Low"
      }

      form_str <- if (outcome == "future_Earnings_persistence") {
        sprintf("future_Earnings ~ current_Earnings * %s + Size + revenue_growth + factor(industry) + factor(year)", pred)
      } else {
        sprintf("%s ~ %s + Size + ROA_curr + revenue_growth + factor(industry) + factor(year)", outcome, pred)
      }

      fit_unweighted <- tryCatch(lm(as.formula(form_str), data = df_merged), error = function(e) NULL)
      if (!is.null(fit_unweighted)) {
        coef_m <- safe_coeftest(fit_unweighted, df_merged)
        term_name <- pred
        if (outcome == "future_Earnings_persistence") {
          idx <- grep(paste0("current_Earnings.*", pred, "|", pred, ".*current_Earnings"), rownames(coef_m))
          if (length(idx) > 0) term_name <- rownames(coef_m)[idx[1]]
        }
        if (term_name %in% rownames(coef_m)) {
          results[[length(results) + 1]] <- data.frame(
            Space = space_name,
            Outcome = outcome,
            Predictor = pred,
            Weighted = FALSE,
            Weight_Var = "None",
            Circularity_Risk = circ_risk,
            Coefficient = coef_m[term_name, "Estimate"],
            Std_Error = coef_m[term_name, "Std. Error"],
            t_value = coef_m[term_name, "t value"],
            p_value = coef_m[term_name, "Pr(>|t|)"],
            R2 = summary(fit_unweighted)$r.squared,
            Adj_R2 = summary(fit_unweighted)$adj.r.squared,
            N_Obs = nobs(fit_unweighted),
            stringsAsFactors = FALSE
          )
        }
      }

      if (grepl("stacked", pred)) {
        weight_col <- if (grepl("estimation", pred)) {
          paste0("NDA_sd_epred_stacked_", suffix, "_winsor")
        } else {
          paste0("NDA_sd_predict_stacked_", suffix, "_winsor")
        }
        if (weight_col %in% names(df_merged)) {
          df_merged$reg_weight <- safe_weight(df_merged[[weight_col]])
          fit_weighted <- tryCatch(lm(as.formula(form_str), data = df_merged, weights = reg_weight), error = function(e) NULL)
          if (!is.null(fit_weighted)) {
            coef_m <- safe_coeftest(fit_weighted, df_merged)
            term_name <- pred
            if (outcome == "future_Earnings_persistence") {
              idx <- grep(paste0("current_Earnings.*", pred, "|", pred, ".*current_Earnings"), rownames(coef_m))
              if (length(idx) > 0) term_name <- rownames(coef_m)[idx[1]]
            }
            if (term_name %in% rownames(coef_m)) {
              results[[length(results) + 1]] <- data.frame(
                Space = space_name,
                Outcome = outcome,
                Predictor = pred,
                Weighted = TRUE,
                Weight_Var = weight_col,
                Circularity_Risk = circ_risk,
                Coefficient = coef_m[term_name, "Estimate"],
                Std_Error = coef_m[term_name, "Std. Error"],
                t_value = coef_m[term_name, "t value"],
                p_value = coef_m[term_name, "Pr(>|t|)"],
                R2 = summary(fit_weighted)$r.squared,
                Adj_R2 = summary(fit_weighted)$adj.r.squared,
                N_Obs = nobs(fit_weighted),
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
    }
  }

  if (length(results) == 0) {
    add_validation_debug(space_name, suffix, df_merged, candidate_predictors, predictors, "No regression rows were produced. Check missing controls, all-NA outcomes, singular fits, or term-name mismatch.")
    return(empty_validation_results())
  }
  do.call(rbind, results)
}

validation_results <- bind_rows(
  run_validation("ex_post", df_ep_sample, "ep"),
  run_validation("real_time", df_rt_sample, "rt")
) %>%
  add_metadata()

if (length(validation_debug_rows) > 0) {
  write.csv(bind_rows(validation_debug_rows),
            file.path(validation_root, "table_validation_debug_scaleaware_student.csv"),
            row.names = FALSE)
}

if (nrow(validation_results) == 0) {
  stop("[BLOCKER] Phase 6b validation produced zero regression rows. ",
       "This usually means predictor/control names changed after joins, outcomes are all missing, ",
       "or all validation fits failed. See: ",
       file.path(validation_root, "table_validation_debug_scaleaware_student.csv"))
}

unweighted_df <- validation_results %>% filter(Weighted == FALSE)
weighted_df <- validation_results %>% filter(Weighted == TRUE)

write.csv(unweighted_df, file.path(validation_root, "table_unweighted_validation_scaleaware_student.csv"), row.names = FALSE)
write.csv(weighted_df, file.path(validation_root, "table_precision_weighted_validation_scaleaware_student.csv"), row.names = FALSE)
write.csv(validation_results, file.path(validation_root, "table_validation_comparison_summary_scaleaware_student.csv"), row.names = FALSE)

writeLines(c(
  "Phase 6b validation on scale-aware Student-t DA",
  sprintf("DA source: %s", master_path),
  sprintf("Prior set: %s", prior_set_id),
  sprintf("Likelihood family: %s", likelihood_family),
  sprintf("Model structure: %s", model_structure),
  "Validation tables are rerun from the current-root DA file and do not reuse old wide-prior Gaussian validation outputs."
), file.path(validation_root, "phase6b_validation_scaleaware_student_notes.txt"))

cat("\n[SUCCESS] Phase 6b validation on current-root DA completed.\n")
