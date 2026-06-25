# -----------------------------------------------------------------------------
# Script: ma17_export_tables_figures.R
# Purpose: Generate manuscript-ready Chapter 3 methods/audit tables from existing
#          pipeline outputs without refitting Bayesian models.
#
# Prior predictive manuscript-acceptance rule:
# A prior is acceptable under Chapter 3 if mass outside |TA| > 1 is no more
# than 5%, mass outside |TA| > 2 is no more than 1%, and the prior predictive
# 1st-to-99th percentile range is no more than three times the empirical range
# unless explicitly justified.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("ma17", "Export tables and figures")
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

chapter3_prior_thresholds <- chapter3_prior_predictive_thresholds()
PRIOR_MAX_MASS_ABS_GT_1 <- env_num("PRIOR_MAX_MASS_ABS_GT_1", chapter3_prior_thresholds$abs_gt_1_pass, min = 0)
PRIOR_MAX_MASS_ABS_GT_2 <- env_num("PRIOR_MAX_MASS_ABS_GT_2", chapter3_prior_thresholds$abs_gt_2_pass, min = 0)
PRIOR_MAX_RANGE_RATIO <- env_num("PRIOR_MAX_RANGE_RATIO", chapter3_prior_thresholds$range_ratio_pass, min = 0)

RQ2_SPEARMAN_MODERATE <- 0.95
RQ2_SPEARMAN_HIGH <- 0.90
RQ2_TOP5_JACCARD_MODERATE <- 0.80
RQ2_TOP5_JACCARD_HIGH <- 0.60
RQ2_FLAG_SWITCH_MODERATE <- 0.05
RQ2_FLAG_SWITCH_HIGH <- 0.10
RQ2_FLAG_COUNT_REL_CHANGE_MATERIAL <- 0.25
MIN_FIRMS_PER_FOLD_STABLE <- env_int("ACCRUAL_METHODS_MIN_FIRMS_PER_FOLD_STABLE", 30L, min = 1L)

report_dir <- file.path(reports_root, "chapter3_methods_tables")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
warnings_for_author <- character()
generated_files <- character()

add_warning <- function(x) {
  warnings_for_author <<- unique(c(warnings_for_author, x))
  warning(x, call. = FALSE)
}

path_table <- function(file) file.path(output_root, "tables", file)
path_baseline_table <- function(file) file.path(baseline_root, "tables", file)

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

read_required_gate <- function(path, column, label) {
  x <- safe_read_csv(path)
  if (is.null(x) || !nrow(x) || !column %in% names(x)) {
    stop("[GATE BLOCKER] Missing or invalid ", label, " decision table: ", path)
  }
  as.character(x[[column]][1])
}

finite_gate_path <- path_table("table_DA_finite_gate_decision.csv")
new_firm_gate_path <- path_table(file.path("..", "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv"))
if (!file.exists(new_firm_gate_path)) {
  new_firm_gate_path <- file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_decision.csv")
}
exact_kfold_reclassification_decision_path <- file.path(output_root, "diagnostics", "table_exact_kfold_reclassification_decision.csv")
exact_kfold_reclassification_jaccard_path <- file.path(output_root, "diagnostics", "table_exact_kfold_reclassification_jaccard.csv")
denominator_diagnostics_decision_path <- file.path(output_root, "diagnostics", "table_denominator_diagnostics_decision.csv")
denominator_capped_jaccard_path <- file.path(output_root, "diagnostics", "table_denominator_capped_jaccard.csv")
da_z_est_vs_z_pred_comparison_path <- file.path(output_root, "diagnostics", "table_da_z_est_vs_z_pred_comparison.csv")
economic_validity_path <- file.path(output_root, "diagnostics", "table_top_tail_group_economic_validity.csv")
economic_validity_means_path <- file.path(output_root, "diagnostics", "table_top_tail_group_outcome_means.csv")
economic_validity_decision_path <- file.path(output_root, "diagnostics", "table_top_tail_group_economic_validity_decision.csv")
temporal_dependence_premium_path <- file.path(output_root, "simulation", "temporal_dependence", "tables", "table_temporal_dependence_firmre_premium.csv")
temporal_dependence_decision_path <- file.path(output_root, "simulation", "temporal_dependence", "tables", "table_temporal_dependence_decision.csv")
DA_Finite_Gate_Decision <- read_required_gate(finite_gate_path, "gate_decision", "DA finite")
New_Firm_Predictive_Gate_Decision <- read_required_gate(new_firm_gate_path, "audit_decision", "new-firm predictive")
Exact_KFold_Reclassification_Decision_Table <- safe_read_csv(exact_kfold_reclassification_decision_path)
Exact_KFold_Reclassification_Decision <- if (!is.null(Exact_KFold_Reclassification_Decision_Table) &&
                                             "overall_reporting_decision" %in% names(Exact_KFold_Reclassification_Decision_Table) &&
                                             nrow(Exact_KFold_Reclassification_Decision_Table) > 0) {
  as.character(Exact_KFold_Reclassification_Decision_Table$overall_reporting_decision[[1]])
} else if (!is.null(Exact_KFold_Reclassification_Decision_Table) &&
           "audit_decision" %in% names(Exact_KFold_Reclassification_Decision_Table) &&
           nrow(Exact_KFold_Reclassification_Decision_Table) > 0) {
  as.character(Exact_KFold_Reclassification_Decision_Table$audit_decision[[1]])
} else {
  NA_character_
}
Primary_Magnitude_Reclassification_Decision <- if (!is.null(Exact_KFold_Reclassification_Decision_Table) &&
                                                   "primary_magnitude_decision" %in% names(Exact_KFold_Reclassification_Decision_Table) &&
                                                   nrow(Exact_KFold_Reclassification_Decision_Table) > 0) {
  as.character(Exact_KFold_Reclassification_Decision_Table$primary_magnitude_decision[[1]])
} else if (!is.na(Exact_KFold_Reclassification_Decision)) {
  Exact_KFold_Reclassification_Decision
} else {
  NA_character_
}
Tail_Reclassification_Reporting_Decision <- if (identical(New_Firm_Predictive_Gate_Decision, "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES")) {
  "SUPPRESSED_OR_NON_PRIMARY_BY_DI02"
} else if (!is.null(Exact_KFold_Reclassification_Decision_Table) &&
           "tail_reporting_decision" %in% names(Exact_KFold_Reclassification_Decision_Table) &&
           nrow(Exact_KFold_Reclassification_Decision_Table) > 0) {
  as.character(Exact_KFold_Reclassification_Decision_Table$tail_reporting_decision[[1]])
} else {
  NA_character_
}
allowed_finite_decisions <- c("PASS", "PASS_WITH_STRUCTURAL_NA_ONLY", "WARN_SECONDARY_NONFINITE_ONLY")
if (!DA_Finite_Gate_Decision %in% allowed_finite_decisions) {
  stop("[GATE BLOCKER] DA finite gate is not passable for primary RQ2/export: ", DA_Finite_Gate_Decision)
}
allow_suppressed_tail_flags <- env_flag("ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS", "FALSE")
if (identical(New_Firm_Predictive_Gate_Decision, "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES") &&
    !allow_suppressed_tail_flags) {
  stop("[GATE BLOCKER] New-firm predictive audit requires tail-flag suppression/non-primary treatment. ",
       "Set ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS=TRUE only when downstream reporting preserves that suppression.")
}
RQ2_Primary_Output_Allowed <- DA_Finite_Gate_Decision %in% allowed_finite_decisions &&
  New_Firm_Predictive_Gate_Decision %in% c(
    "PASS_FOR_AVAILABLE_FIRMRE_OUT_OF_FIRM_QUANTITIES",
    "NO_FIRMRE_OUT_OF_FIRM_PRIMARY_QUANTITIES_DETECTED"
  )
Tail_Flag_Primary_Output_Allowed <- !identical(New_Firm_Predictive_Gate_Decision, "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES") &&
  RQ2_Primary_Output_Allowed
ExactKFold_Magnitude_RQ2_Primary_Output_Allowed <- FALSE
Exact_KFold_Reclassification_Audit_Status <- if (!is.null(Exact_KFold_Reclassification_Decision_Table) &&
                                                 nrow(Exact_KFold_Reclassification_Decision_Table) > 0) {
  "available"
} else {
  "missing_or_unavailable"
}
Primary_Magnitude_Reclassification_Min_Jaccard <- if (!is.null(Exact_KFold_Reclassification_Decision_Table) &&
                                                      "min_primary_magnitude_jaccard" %in% names(Exact_KFold_Reclassification_Decision_Table) &&
                                                      nrow(Exact_KFold_Reclassification_Decision_Table) > 0) {
  Exact_KFold_Reclassification_Decision_Table$min_primary_magnitude_jaccard[[1]]
} else {
  NA_real_
}
Tail_Reclassification_Primary_Status <- if (isTRUE(Tail_Flag_Primary_Output_Allowed)) {
  "primary_allowed"
} else {
  "suppressed_or_non_primary"
}

safe_n <- function(x) if (is.null(x)) NA_integer_ else nrow(x)

safe_min <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else min(x)
}

safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else max(x)
}

fmt <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

write_md_table <- function(df, path, title = NULL, digits = 4) {
  out <- df
  for (nm in names(out)) {
    if (is.numeric(out[[nm]])) out[[nm]] <- ifelse(is.na(out[[nm]]), "", format(round(out[[nm]], digits), trim = TRUE, scientific = FALSE))
    out[[nm]] <- gsub("\\|", "/", fmt(out[[nm]]), fixed = FALSE)
  }
  lines <- character()
  if (!is.null(title)) lines <- c(lines, paste0("# ", title), "")
  lines <- c(
    lines,
    paste0("| ", paste(names(out), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(out)), collapse = " | "), " |")
  )
  if (nrow(out)) {
    for (i in seq_len(nrow(out))) {
      lines <- c(lines, paste0("| ", paste(as.character(out[i, ]), collapse = " | "), " |"))
    }
  }
  writeLines(lines, path, useBytes = TRUE)
}

write_outputs <- function(df, stem, title) {
  csv_path <- file.path(report_dir, paste0(stem, ".csv"))
  md_path <- file.path(report_dir, paste0(stem, ".md"))
  write_csv_safely(df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  write_md_table(df, md_path, title = title)
  generated_files <<- unique(c(generated_files, csv_path, md_path))
  invisible(df)
}

sample_specs <- data.frame(
  sample = c("ex-post", "no-lookahead", "secondary volatility ex-post", "secondary volatility no-lookahead",
             "secondary operating-cycle ex-post", "secondary operating-cycle no-lookahead"),
  file = c("final_common_ex_post_sample_winsor.csv", "final_common_realtime_sample_winsor.csv",
           "final_M08_ex_post_subsample_winsor.csv", "final_M08_realtime_subsample_winsor.csv",
           "final_secondary_operating_cycle_ex_post_sample_winsor.csv",
           "final_secondary_operating_cycle_realtime_sample_winsor.csv"),
  sample_short = c("ex_post", "real_time", "volatility_ex_post", "volatility_real_time",
                   "operating_cycle_ex_post", "operating_cycle_real_time"),
  stringsAsFactors = FALSE
)

sample_data <- setNames(lapply(sample_specs$file, function(f) safe_read_csv(path_table(f))), sample_specs$sample_short)
missing_samples <- sample_specs$file[vapply(sample_data, is.null, logical(1))]
if (length(missing_samples)) add_warning(paste("Missing sample files:", paste(missing_samples, collapse = ", ")))

sample_stats <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(list(n_obs = 0L, n_firms = 0L, min_year = NA, max_year = NA, n_years = 0L))
  }
  list(
    n_obs = nrow(df),
    n_firms = length(unique(df$company)),
    min_year = safe_min(df$year),
    max_year = safe_max(df$year),
    n_years = length(unique(df$year[!is.na(df$year)]))
  )
}

raw <- NULL
metadata_sheets <- character()
if (file.exists(data_path)) {
  metadata_sheets <- tryCatch(readxl::excel_sheets(data_path), error = function(e) character())
  raw <- readxl::read_excel(data_path, sheet = "Sheet1")
} else {
  add_warning(paste("Raw data workbook missing:", data_path))
}

raw_valid <- raw_after_A <- raw_after_year <- df_vars_light <- NULL
if (!is.null(raw)) {
  raw_valid <- raw %>%
    mutate(company = normalize_join_key_values(.data$company),
           year = suppressWarnings(as.integer(.data$year))) %>%
    filter(!is.na(.data$company), nzchar(.data$company), !is.na(.data$year))
  raw_after_A <- raw_valid %>% filter(!is.na(.data$A), .data$A != 0)
  raw_after_year <- raw_after_A %>%
    mutate(PPE = ifelse(.data$PPE == 0, NA, .data$PPE),
           REC = ifelse(.data$REC == 0, NA, .data$REC),
           COGS = ifelse(.data$COGS == 0, NA, .data$COGS)) %>%
    filter(.data$year > 2015) %>%
    arrange(.data$company, .data$year)
  get_lag_local <- function(x, yr, n = 1) {
    lag_val <- dplyr::lag(x, n)
    lag_yr <- dplyr::lag(yr, n)
    ifelse(!is.na(lag_yr) & lag_yr == (yr - n), lag_val, NA)
  }
  df_vars_light <- raw_after_year %>%
    group_by(.data$company) %>%
    mutate(
      A_lag = get_lag_local(.data$A, .data$year),
      REV_lag = get_lag_local(.data$REV, .data$year),
      REC_lag = get_lag_local(.data$REC, .data$year),
      CFO_lag = get_lag_local(.data$CFO, .data$year),
      ROA_lag = get_lag_local(.data$ROA, .data$year),
      has_valid_denominator = !is.na(.data$A_lag) & .data$A_lag != 0
    ) %>%
    ungroup()
}

flow_row <- function(stage, df, notes = "") {
  st <- sample_stats(df)
  data.frame(
    stage = stage,
    n_observations = st$n_obs,
    n_firms = st$n_firms,
    n_years = st$n_years,
    min_year = st$min_year,
    max_year = st$max_year,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

sample_flow <- bind_rows(
  flow_row("Raw firm-year observations loaded from data/raw/data.xlsx", raw, "Sheet1 raw rows."),
  flow_row("Observations after valid firm identifier and year parsing", raw_valid, "company normalized; year parsed as integer."),
  flow_row("Observations after dropping zero or invalid lagged total assets", raw_after_A, "Current total assets A == 0 dropped before lag construction."),
  flow_row("Observations after lag-continuity requirements", if (is.null(df_vars_light)) NULL else df_vars_light %>% filter(.data$has_valid_denominator), "Requires continuous lagged A with nonzero denominator."),
  flow_row("Observations available for final common ex-post sample", sample_data$ex_post, sample_specs$file[sample_specs$sample_short == "ex_post"]),
  flow_row("Observations available for final common no-lookahead sample", sample_data$real_time, sample_specs$file[sample_specs$sample_short == "real_time"]),
  flow_row("Observations available for secondary volatility sample", sample_data$volatility_ex_post, sample_specs$file[sample_specs$sample_short == "volatility_ex_post"]),
  flow_row("Observations available for secondary operating-cycle sample", sample_data$operating_cycle_ex_post, sample_specs$file[sample_specs$sample_short == "operating_cycle_ex_post"])
)
write_outputs(sample_flow, "table_3_1_sample_flow", "Table 3.1 Sample Flow")

panel_row <- function(df, label) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(sample = label, n_observations = 0L, n_firms = 0L, min_year = NA, max_year = NA,
                      mean_observations_per_firm = NA, median_observations_per_firm = NA,
                      min_observations_per_firm = NA, max_observations_per_firm = NA,
                      share_firms_1_year = NA, share_firms_2_years = NA, share_firms_3_years = NA,
                      share_firms_4_years = NA, share_firms_5plus_years = NA,
                      singleton_firms = NA, firms_eligible_for_grouped_validation = NA,
                      firms_excluded_from_grouped_validation = NA, stringsAsFactors = FALSE))
  }
  per_firm <- df %>% distinct(.data$company, .data$year) %>% count(.data$company, name = "n_years")
  n_firms <- nrow(per_firm)
  data.frame(
    sample = label,
    n_observations = nrow(df),
    n_firms = n_firms,
    min_year = safe_min(df$year),
    max_year = safe_max(df$year),
    mean_observations_per_firm = mean(per_firm$n_years),
    median_observations_per_firm = stats::median(per_firm$n_years),
    min_observations_per_firm = min(per_firm$n_years),
    max_observations_per_firm = max(per_firm$n_years),
    share_firms_1_year = mean(per_firm$n_years == 1),
    share_firms_2_years = mean(per_firm$n_years == 2),
    share_firms_3_years = mean(per_firm$n_years == 3),
    share_firms_4_years = mean(per_firm$n_years == 4),
    share_firms_5plus_years = mean(per_firm$n_years >= 5),
    singleton_firms = sum(per_firm$n_years == 1),
    firms_eligible_for_grouped_validation = sum(per_firm$n_years >= 2),
    firms_excluded_from_grouped_validation = sum(per_firm$n_years < 2),
    stringsAsFactors = FALSE
  )
}

panel_coverage <- bind_rows(
  panel_row(sample_data$ex_post, "ex-post"),
  panel_row(sample_data$real_time, "no-lookahead"),
  panel_row(sample_data$volatility_ex_post, "secondary volatility"),
  panel_row(sample_data$operating_cycle_ex_post, "secondary operating-cycle")
)
write_outputs(panel_coverage, "table_3_2_panel_coverage", "Table 3.2 Panel Coverage")

industry_cell_row <- function(df, label) {
  if (is.null(df) || !nrow(df) || !"industry" %in% names(df)) {
    return(data.frame(sample = label, n_industry_year_cells = 0L, mean_observations_per_cell = NA,
                      median_observations_per_cell = NA, min_observations_per_cell = NA,
                      max_observations_per_cell = NA, n_cells_1_observation = NA,
                      share_cells_1_observation = NA, n_cells_lt_5 = NA, share_cells_lt_5 = NA,
                      n_cells_lt_10 = NA, share_cells_lt_10 = NA, stringsAsFactors = FALSE))
  }
  cells <- df %>% count(.data$industry, .data$year, name = "n")
  data.frame(
    sample = label,
    n_industry_year_cells = nrow(cells),
    mean_observations_per_cell = mean(cells$n),
    median_observations_per_cell = stats::median(cells$n),
    min_observations_per_cell = min(cells$n),
    max_observations_per_cell = max(cells$n),
    n_cells_1_observation = sum(cells$n == 1),
    share_cells_1_observation = mean(cells$n == 1),
    n_cells_lt_5 = sum(cells$n < 5),
    share_cells_lt_5 = mean(cells$n < 5),
    n_cells_lt_10 = sum(cells$n < 10),
    share_cells_lt_10 = mean(cells$n < 10),
    stringsAsFactors = FALSE
  )
}

industry_year <- bind_rows(
  industry_cell_row(sample_data$ex_post, "ex-post"),
  industry_cell_row(sample_data$real_time, "no-lookahead"),
  industry_cell_row(sample_data$volatility_ex_post, "secondary volatility"),
  industry_cell_row(sample_data$operating_cycle_ex_post, "secondary operating-cycle")
)
write_outputs(industry_year, "table_3_3_industry_year_cells", "Table 3.3 Industry-Year Cells")

zero_vars <- data.frame(
  variable = c("PPE", "REC", "COGS", "INV", "REV", "A", "A_lag"),
  raw_variable = c("PPE", "REC", "COGS", "INV", "REV", "A", "A"),
  treatment = c("zero_to_NA", "zero_to_NA", "zero_to_NA", "not_subject_to_zero_to_na",
                "not_subject_to_zero_to_na", "drop_A_zero_row", "derived_valid_denominator"),
  affected_samples = c("ex-post, no-lookahead, volatility, operating-cycle",
                       "ex-post, no-lookahead, volatility, operating-cycle",
                       "operating-cycle",
                       "operating-cycle",
                       "operating-cycle denominator and growth",
                       "all samples", "all samples"),
  stringsAsFactors = FALSE
)

zero_audit <- lapply(seq_len(nrow(zero_vars)), function(i) {
  z <- zero_vars[i, ]
  raw_col <- if (!is.null(raw) && z$raw_variable %in% names(raw)) raw[[z$raw_variable]] else rep(NA_real_, 0)
  raw_nonmissing <- sum(!is.na(raw_col))
  zero_count <- sum(!is.na(raw_col) & raw_col == 0)
  converted <- if (z$treatment == "zero_to_NA") zero_count else 0L
  retained <- if (z$treatment == "not_subject_to_zero_to_na") zero_count else 0L
  lost <- NA_integer_
  if (!is.null(df_vars_light)) {
    if (z$variable %in% names(df_vars_light)) lost <- sum(is.na(df_vars_light[[z$variable]]))
    if (z$variable == "A_lag") lost <- sum(!df_vars_light$has_valid_denominator, na.rm = TRUE)
  }
  data.frame(
    variable = z$variable,
    raw_nonmissing_n = raw_nonmissing,
    zero_count_before_treatment = zero_count,
    zero_share_before_treatment = ifelse(raw_nonmissing > 0, zero_count / raw_nonmissing, NA_real_),
    number_converted_zero_to_NA = converted,
    number_retained_as_economic_zero = retained,
    observations_lost_downstream_because_of_variable = lost,
    affected_samples = z$affected_samples,
    audit_warning = if (z$treatment == "zero_to_NA") {
      "all_zero_values_treated_as_missing"
    } else if (z$treatment == "not_subject_to_zero_to_na") {
      "not_subject_to_zero_to_na"
    } else {
      "all_zero_values_treated_as_missing"
    },
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()
write_outputs(zero_audit, "table_3_4_zero_value_audit", "Table 3.4 Zero Value Audit")

registry <- safe_read_csv(path_table("table_model_registry_winsor.csv"))
if (is.null(registry)) registry <- safe_read_csv(path_baseline_table("table_model_registry.csv"))
formulas <- safe_read_csv(path_table("table_named_model_formulas_winsor.csv"))

citation_placeholder <- function(fam) {
  dplyr::case_when(
    grepl("Jones", fam, ignore.case = TRUE) ~ "Jones-family citation placeholder",
    grepl("Dechow|Modified", fam, ignore.case = TRUE) ~ "Modified-Jones citation placeholder",
    grepl("McNichols|Cash", fam, ignore.case = TRUE) ~ "Cash-flow/McNichols citation placeholder",
    grepl("Ball|Shivakumar|asym", fam, ignore.case = TRUE) ~ "Conditional conservatism citation placeholder",
    TRUE ~ "literature citation placeholder"
  )
}

if (is.null(registry)) {
  add_warning("Model registry missing; model-space matrix will contain no rows.")
  model_space <- data.frame()
  appendix_models <- data.frame()
} else {
  main_reg <- registry %>% filter(.data$Model_ID %in% sprintf("M%02d", 1:10))
  model_space <- main_reg %>%
    transmute(
      model_id = .data$Model_ID,
      model_name = .data$Model_Name,
      literature_family = .data$Literature_Family,
      representative_citation_placeholder = citation_placeholder(.data$Literature_Family),
      construct_rationale = .data$Rationale,
      dependent_variable = .data$Dependent_Variable,
      required_variables = .data$Required_Variables,
      ex_post_eligible = .data$Intended_Space %in% c("both", "ex_post") | .data$Model_ID %in% c("M08", "M10"),
      no_lookahead_eligible = .data$Intended_Space %in% c("both", "real_time", "no_lookahead") | .data$Model_ID == "M10",
      core_or_secondary = ifelse(.data$Secondary_Robustness %in% c(TRUE, "TRUE", "True"), "secondary", "core"),
      sample_used = .data$Sample_Group,
      reason_for_inclusion = .data$Main_Stack_Reason,
      lookahead_risk = .data$Lookahead_Status,
      notes = .data$Notes
    ) %>%
    arrange(.data$model_id)
  appendix_models <- registry %>%
    filter(!.data$Model_ID %in% sprintf("M%02d", 1:10)) %>%
    transmute(
      model_id = .data$Model_ID,
      model_name = .data$Model_Name,
      manuscript_label = "screened_external_data_extension_not_in_main_model_space",
      literature_family = .data$Literature_Family,
      required_variables = .data$Required_Variables,
      notes = .data$Notes
    )
}
write_outputs(model_space, "table_3_5_model_space_matrix", "Table 3.5 Model-Space Matrix")
write_outputs(appendix_models, "appendix_screened_external_data_extensions", "Appendix Screened External Data Extensions")

latest_completed_run_file <- file.path(output_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt")
if (!file.exists(latest_completed_run_file)) {
  stop("[BLOCKER] Missing grouped K-fold completed-run pin for manuscript export: ", latest_completed_run_file)
}
kfold_root <- trimws(readLines(latest_completed_run_file, warn = FALSE)[1])
if (!nzchar(kfold_root) || !dir.exists(kfold_root)) {
  stop("[BLOCKER] Invalid grouped K-fold completed-run root in manuscript export: ", latest_completed_run_file)
}
kfold_tables <- file.path(kfold_root, "tables")
if (!dir.exists(kfold_tables)) stop("[BLOCKER] Grouped K-fold completed-run tables directory is missing: ", kfold_tables)
kfold_manifest <- safe_read_csv(file.path(kfold_root, "logs", "run_config_manifest.csv"))
if (is.null(kfold_manifest)) stop("[BLOCKER] Grouped K-fold completed-run manifest is missing under: ", kfold_root)
fold_assignment <- safe_read_csv(file.path(kfold_tables, "table_winsor_firm_fold_assignment.csv"))
kfold_balance_in <- safe_read_csv(file.path(kfold_tables, "table_winsor_kfold_balance.csv"))

fold_diag_rows <- list()
if (!is.null(kfold_balance_in) && nrow(kfold_balance_in)) {
  for (i in seq_len(nrow(kfold_balance_in))) {
    b <- kfold_balance_in[i, ]
    sample_key <- if (identical(b$Target_Space, "ex_post")) "ex_post" else "real_time"
    df <- sample_data[[sample_key]]
    fold_id <- b$Fold_ID
    fold_companies <- if (!is.null(fold_assignment)) fold_assignment$company[fold_assignment$Fold_ID == fold_id] else character()
    fold_df <- if (!is.null(df) && length(fold_companies)) df[df$company %in% fold_companies, , drop = FALSE] else NULL
    ind_counts <- if (!is.null(fold_df) && nrow(fold_df) && "industry" %in% names(fold_df)) table(fold_df$industry) else integer()
    train_df <- if (!is.null(df) && length(fold_companies)) df[!df$company %in% fold_companies, , drop = FALSE] else NULL
    absent_training <- if (!is.null(df) && !is.null(train_df) && "industry" %in% names(df)) {
      any(!unique(df$industry) %in% unique(train_df$industry))
    } else {
      NA
    }
    fold_diag_rows[[length(fold_diag_rows) + 1]] <- data.frame(
      sample = b$Target_Space,
      K = if (!is.null(kfold_manifest) && "K" %in% names(kfold_manifest)) kfold_manifest$K[1] else NA,
      seed = if (!is.null(kfold_manifest) && "Seed" %in% names(kfold_manifest)) kfold_manifest$Seed[1] else NA,
      fold_id = fold_id,
      number_of_firms_in_fold = b$N_Firms,
      number_of_firm_year_observations_in_fold = b$N_Obs,
      min_year = b$Min_Year,
      max_year = b$Max_Year,
      number_of_industries_represented = if (!is.null(fold_df) && nrow(fold_df) && "industry" %in% names(fold_df)) length(unique(fold_df$industry)) else NA,
      number_of_industry_year_cells_represented = if (!is.null(fold_df) && nrow(fold_df) && "industry" %in% names(fold_df)) nrow(fold_df %>% distinct(.data$industry, .data$year)) else NA,
      largest_industry_share_within_fold = if (length(ind_counts)) max(ind_counts) / sum(ind_counts) else NA_real_,
      any_industry_absent_from_training_set = absent_training,
      fold_has_too_few_firms_for_stable_validation = b$N_Firms < MIN_FIRMS_PER_FOLD_STABLE,
      stringsAsFactors = FALSE
    )
  }
} else {
  add_warning("Grouped K-fold balance/fold assignment files are missing; fold balance tables will be warning placeholders.")
}
fold_balance <- bind_rows(fold_diag_rows)
if (!nrow(fold_balance)) {
  fold_balance <- data.frame(sample = NA, K = NA, seed = NA, fold_id = NA,
                             number_of_firms_in_fold = NA, number_of_firm_year_observations_in_fold = NA,
                             min_year = NA, max_year = NA, number_of_industries_represented = NA,
                             number_of_industry_year_cells_represented = NA,
                             largest_industry_share_within_fold = NA,
                             any_industry_absent_from_training_set = NA,
                             fold_has_too_few_firms_for_stable_validation = NA)
}
write_outputs(fold_balance, "table_3_6_grouped_kfold_fold_balance", "Table 3.6 Grouped K-Fold Fold Balance")

fold_summary <- fold_balance %>%
  filter(!is.na(.data$sample)) %>%
  group_by(.data$sample) %>%
  summarise(
    minimum_firms_per_fold = min(.data$number_of_firms_in_fold, na.rm = TRUE),
    maximum_firms_per_fold = max(.data$number_of_firms_in_fold, na.rm = TRUE),
    firm_count_imbalance_ratio = maximum_firms_per_fold / pmax(minimum_firms_per_fold, 1),
    minimum_observations_per_fold = min(.data$number_of_firm_year_observations_in_fold, na.rm = TRUE),
    maximum_observations_per_fold = max(.data$number_of_firm_year_observations_in_fold, na.rm = TRUE),
    observation_imbalance_ratio = maximum_observations_per_fold / pmax(minimum_observations_per_fold, 1),
    warnings = paste(unique(c(
      if (any(.data$fold_has_too_few_firms_for_stable_validation, na.rm = TRUE)) "fold_below_min_firm_threshold" else NA_character_,
      if (any(.data$any_industry_absent_from_training_set, na.rm = TRUE)) "industry_absent_from_training" else NA_character_
    )[!is.na(c(
      if (any(.data$fold_has_too_few_firms_for_stable_validation, na.rm = TRUE)) "fold_below_min_firm_threshold" else NA_character_,
      if (any(.data$any_industry_absent_from_training_set, na.rm = TRUE)) "industry_absent_from_training" else NA_character_
    ))]), collapse = ";"),
    .groups = "drop"
  )
if (!nrow(fold_summary)) fold_summary <- data.frame(sample = NA, minimum_firms_per_fold = NA, maximum_firms_per_fold = NA, firm_count_imbalance_ratio = NA, minimum_observations_per_fold = NA, maximum_observations_per_fold = NA, observation_imbalance_ratio = NA, warnings = "fold_outputs_missing")
write_outputs(fold_summary, "table_3_6b_grouped_kfold_balance_summary", "Table 3.6b Grouped K-Fold Balance Summary")

script_text <- function(path) if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
audit_prediction <- function(path, scheme) {
  txt <- script_text(path)
  uses_re_na <- grepl("re_formula\\s*=\\s*NA", txt)
  samples_new <- grepl("sample_new_levels|gaussian|uncertainty|marginal", txt, ignore.case = TRUE) && grepl("allow_new_levels\\s*=\\s*TRUE", txt)
  uses_loglik_full <- grepl("log_lik\\(", txt) && scheme == "LOFO"
  classification <- if (uses_loglik_full) {
    "conditional_prediction_leakage_risk"
  } else if (uses_re_na) {
    "population_level_new_firm_prediction"
  } else if (samples_new) {
    "marginal_new_firm_prediction"
  } else {
    "unclear_requires_manual_review"
  }
  data.frame(
    script = path,
    validation_scheme = scheme,
    model_variant = "Firm RE (Random Intercept + Year FE)",
    uses_fitted_random_effect_for_heldout_firm = classification == "conditional_prediction_leakage_risk",
    uses_population_level_prediction_re_formula_NA = uses_re_na && classification == "population_level_new_firm_prediction",
    marginalizes_or_samples_new_group_effect = samples_new,
    prediction_rule_classification = classification,
    reviewer_risk = ifelse(classification %in% c("conditional_prediction_leakage_risk", "unclear_requires_manual_review"), "author_review_required", "low"),
    notes = ifelse(scheme == "LOFO", "Grouped PSIS-LOFO sums observation log-likelihood by company from fitted models.", "Exact grouped K-fold held-out scoring inspected from script text."),
    stringsAsFactors = FALSE
  )
}
prediction_audit <- bind_rows(
  audit_prediction("scripts/robustness/ro01_lofo_stacking.R", "LOFO"),
  audit_prediction("scripts/ma12b_fit_grouped_kfold_firm_workers.R", "grouped_kfold")
)
if (any(prediction_audit$prediction_rule_classification == "unclear_requires_manual_review")) add_warning("Prediction-rule audit has unclear classifications.")
if (any(prediction_audit$prediction_rule_classification == "conditional_prediction_leakage_risk")) add_warning("LOFO prediction-rule audit detected conditional/full-fit grouped PSIS risk.")
write_outputs(prediction_audit, "table_3_7_hierarchical_prediction_rule_audit", "Table 3.7 Hierarchical Prediction-Rule Audit")

kfold_std_audit <- safe_read_csv(file.path(kfold_tables, "table_winsor_kfold_train_standardization_audit.csv"))
preprocessing_audit <- bind_rows(
  data.frame(
    preprocessing_step = "winsorization",
    script = "scripts/ma05_winsorize_common_samples.R",
    sample = "winsorized analysis samples",
    validation_scheme = "row_loo, LOFO, grouped_kfold",
    cutoff_or_parameter_source = "sample-specific full analysis sample 1/99 cutoffs",
    computed_using_full_sample = TRUE,
    computed_using_training_fold_only = FALSE,
    applied_to_heldout_fold = TRUE,
    leakage_risk_classification = "measurement_transformation_global_declared",
    required_action = "Report as declared measurement preprocessing; consider fold-specific sensitivity only if predictive validation is interpreted causally.",
    stringsAsFactors = FALSE
  ),
  data.frame(
    preprocessing_step = "standardization",
    script = "scripts/ma12c_collect_grouped_kfold_firm_scores.R",
    sample = "grouped K-fold train/test folds",
    validation_scheme = "grouped_kfold",
    cutoff_or_parameter_source = ifelse(!is.null(kfold_std_audit), "table_winsor_kfold_train_standardization_audit.csv Train_Mean/Train_SD", "missing audit output"),
    computed_using_full_sample = FALSE,
    computed_using_training_fold_only = !is.null(kfold_std_audit),
    applied_to_heldout_fold = !is.null(kfold_std_audit),
    leakage_risk_classification = ifelse(!is.null(kfold_std_audit), "no_leakage_training_fold_only", "unclear_requires_manual_review"),
    required_action = ifelse(!is.null(kfold_std_audit), "None for grouped K-fold standardization audit.", "Run split ma12a/ma12b/ma12c stages to produce train-standardization audit."),
    stringsAsFactors = FALSE
  )
)
if (any(preprocessing_audit$leakage_risk_classification %in% c("potential_predictive_leakage", "unclear_requires_manual_review"))) add_warning("Preprocessing audit has leakage/unclear risks.")
write_outputs(preprocessing_audit, "table_3_8_preprocessing_leakage_audit", "Table 3.8 Preprocessing Leakage Audit")

prior_summary <- safe_read_csv(path_table("table_prior_predictive_summary.csv"))
if (is.null(prior_summary)) {
  add_warning("Prior predictive diagnostics missing.")
  prior_diag <- data.frame()
} else {
  prior_diag <- prior_summary %>%
    mutate(
      empirical_width_1_99 = abs(.data$Observed_TA_P99 - .data$Observed_TA_P01),
      prior_width_1_99 = abs(.data$PriorPred_TA_P99 - .data$PriorPred_TA_P01),
      range_ratio = .data$prior_width_1_99 / pmax(.data$empirical_width_1_99, .Machine$double.eps),
      acceptance_status = ifelse(
        .data$range_ratio <= PRIOR_MAX_RANGE_RATIO &
          .data$PriorPred_Share_Abs_GT_1 <= PRIOR_MAX_MASS_ABS_GT_1 &
          .data$PriorPred_Share_Abs_GT_2 <= PRIOR_MAX_MASS_ABS_GT_2,
        "PASS", "WARN"
      ),
      reason = ifelse(.data$acceptance_status == "PASS", "Meets Chapter 3 prior predictive acceptance thresholds.", "One or more Chapter 3 prior predictive thresholds exceeded or unavailable.")
    ) %>%
    transmute(
      prior_set_id = .data$Prior_Set_ID,
      model_id = .data$Model_ID,
      model_variant = .data$Heterogeneity_Variant,
      likelihood = .data$Likelihood_Family,
      prior_predictive_n_draws = prior_pred_n_draws,
      prior_predictive_q01 = .data$PriorPred_TA_P01,
      q05 = NA_real_,
      q50 = .data$PriorPred_TA_Median,
      q95 = NA_real_,
      q99 = .data$PriorPred_TA_P99,
      min = NA_real_,
      max = NA_real_,
      empirical_TA_q01 = .data$Observed_TA_P01,
      empirical_TA_q99 = .data$Observed_TA_P99,
      prior_mass_outside_empirical_1_99_range = NA_real_,
      prior_mass_abs_TA_gt_1 = .data$PriorPred_Share_Abs_GT_1,
      prior_mass_abs_TA_gt_2 = .data$PriorPred_Share_Abs_GT_2,
      acceptance_status = .data$acceptance_status,
      reason = .data$reason
    )
}
write_outputs(prior_diag, "table_3_9_prior_predictive_diagnostics", "Table 3.9 Prior Predictive Diagnostics")

materiality <- data.frame(
  threshold_name = c("Spearman rank correlation below 0.95", "Spearman rank correlation below 0.90",
                     "top-5% Jaccard similarity below 0.80", "top-5% Jaccard similarity below 0.60",
                     "flag-switching rate above 5% of sample", "flag-switching rate above 10% of sample",
                     "absolute change in flagged count above 25% relative to baseline"),
  configurable_constant = c("RQ2_SPEARMAN_MODERATE", "RQ2_SPEARMAN_HIGH", "RQ2_TOP5_JACCARD_MODERATE",
                            "RQ2_TOP5_JACCARD_HIGH", "RQ2_FLAG_SWITCH_MODERATE", "RQ2_FLAG_SWITCH_HIGH",
                            "RQ2_FLAG_COUNT_REL_CHANGE_MATERIAL"),
  value = c(RQ2_SPEARMAN_MODERATE, RQ2_SPEARMAN_HIGH, RQ2_TOP5_JACCARD_MODERATE,
            RQ2_TOP5_JACCARD_HIGH, RQ2_FLAG_SWITCH_MODERATE, RQ2_FLAG_SWITCH_HIGH,
            RQ2_FLAG_COUNT_REL_CHANGE_MATERIAL),
  interpretation = c("moderate ranking instability", "high ranking instability", "moderate top-tail turnover",
                     "high top-tail turnover", "moderate screening instability", "high screening instability",
                     "material flag-volume change"),
  table_type = "ex_ante_design_threshold",
  stringsAsFactors = FALSE
)
write_outputs(materiality, "table_3_10_rq2_materiality_thresholds", "Table 3.10 RQ2 Materiality Thresholds")

exact_kfold_jaccard <- safe_read_csv(exact_kfold_reclassification_jaccard_path)
table_3_12_available <- FALSE
if (!is.null(exact_kfold_jaccard) && nrow(exact_kfold_jaccard) > 0) {
  if (!"source_score_variable" %in% names(exact_kfold_jaccard) && "score_variable" %in% names(exact_kfold_jaccard)) {
    exact_kfold_jaccard$source_score_variable <- exact_kfold_jaccard$score_variable
  }
  if (!"score_transform" %in% names(exact_kfold_jaccard)) {
    exact_kfold_jaccard$score_transform <- dplyr::case_when(
      exact_kfold_jaccard$metric_class %in% c("primary_magnitude_raw", "primary_magnitude_estimation_scaled", "secondary_predictive_scaled_magnitude") ~ "absolute_value",
      exact_kfold_jaccard$metric_class == "supplementary_tail_based_or_posterior_predictive" ~ "inverse_tail_probability",
      TRUE ~ "identity"
    )
  }
  if (!"reported_score_variable" %in% names(exact_kfold_jaccard) && "score_variable" %in% names(exact_kfold_jaccard)) {
    exact_kfold_jaccard$reported_score_variable <- exact_kfold_jaccard$score_variable
  }
  exact_kfold_jaccard$reported_score_variable <- dplyr::case_when(
    exact_kfold_jaccard$metric_class == "primary_magnitude_raw" ~ "abs(DA_raw_stacked)",
    exact_kfold_jaccard$metric_class == "primary_magnitude_estimation_scaled" ~ "abs(DA_z_estimation_stacked)",
    exact_kfold_jaccard$score_transform == "absolute_value" & !grepl("^abs\\(", exact_kfold_jaccard$reported_score_variable) ~ paste0("abs(", exact_kfold_jaccard$source_score_variable, ")"),
    TRUE ~ as.character(exact_kfold_jaccard$reported_score_variable)
  )
  if (!"suppression_reason" %in% names(exact_kfold_jaccard)) {
    exact_kfold_jaccard$suppression_reason <- NA_character_
  }
  table_3_12_cols <- c(
    "target_space",
    "reported_score_variable",
    "metric_class",
    "N_joined",
    "top_n",
    "intersection_n",
    "union_n",
    "only_row_n",
    "only_grouped_n",
    "jaccard",
    "switch_rate",
    "spearman_rank_correlation",
    "Primary_Inference_Allowed",
    "suppression_reason",
    "interpretation"
  )
  table_3_12 <- exact_kfold_jaccard %>%
    filter(metric_class %in% c("primary_magnitude_raw", "primary_magnitude_estimation_scaled")) %>%
    select(any_of(table_3_12_cols))
  if (nrow(table_3_12) > 0 && all(table_3_12_cols %in% names(table_3_12))) {
    write_outputs(table_3_12, "table_3_12_exact_kfold_reclassification_jaccard", "Table 3.12 Exact K-Fold Reclassification Jaccard")
    table_3_12_available <- TRUE
  }
}

denominator_decision <- safe_read_csv(denominator_diagnostics_decision_path)
denominator_capped_jaccard <- safe_read_csv(denominator_capped_jaccard_path)
da_z_est_vs_z_pred_comparison <- safe_read_csv(da_z_est_vs_z_pred_comparison_path)
table_3_13_available <- FALSE
if (!is.null(denominator_decision) && nrow(denominator_decision) > 0 &&
    !is.null(denominator_capped_jaccard) && nrow(denominator_capped_jaccard) > 0) {
  original_denominator <- denominator_capped_jaccard %>%
    filter(.data$denominator_variant == "original_denominator") %>%
    transmute(target_space = .data$target_space, original_DA_z_est_jaccard = .data$jaccard)
  perturbed_denominator <- denominator_capped_jaccard %>%
    filter(.data$denominator_variant != "original_denominator") %>%
    group_by(.data$target_space) %>%
    summarise(
      capped_floored_denominator_jaccard_min = min(.data$jaccard, na.rm = TRUE),
      capped_floored_denominator_jaccard_max = max(.data$jaccard, na.rm = TRUE),
      .groups = "drop"
    )
  z_pred_summary <- if (!is.null(da_z_est_vs_z_pred_comparison) && nrow(da_z_est_vs_z_pred_comparison) > 0) {
    da_z_est_vs_z_pred_comparison %>%
      group_by(.data$target_space) %>%
      summarise(
        DA_z_est_vs_DA_z_pred_top5_jaccard_min = min(.data$top5_jaccard_z_est_vs_z_pred, na.rm = TRUE),
        DA_z_est_vs_DA_z_pred_top5_jaccard_max = max(.data$top5_jaccard_z_est_vs_z_pred, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    data.frame(
      target_space = unique(denominator_decision$target_space),
      DA_z_est_vs_DA_z_pred_top5_jaccard_min = NA_real_,
      DA_z_est_vs_DA_z_pred_top5_jaccard_max = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  table_3_13 <- denominator_decision %>%
    select(any_of(c("diagnostic_decision", "target_space", "claim_assessment"))) %>%
    left_join(original_denominator, by = "target_space") %>%
    left_join(perturbed_denominator, by = "target_space") %>%
    left_join(z_pred_summary, by = "target_space") %>%
    mutate(
      paper_b_claim_support = dplyr::case_when(
        .data$diagnostic_decision == "PASS_DENOMINATOR_DIAGNOSTIC_STABLE" ~ "supports_measurement_robustness",
        .data$diagnostic_decision == "WARN_DENOMINATOR_SENSITIVE" ~ "weakens_claim_requires_qualification",
        .data$diagnostic_decision == "FAIL_DENOMINATOR_DRIVEN" ~ "does_not_support_denominator_robustness",
        TRUE ~ "insufficient_inputs"
      )
    )
  write_outputs(table_3_13, "table_3_13_denominator_diagnostics_summary", "Table 3.13 Denominator Diagnostics Summary")
  table_3_13_available <- TRUE
}

economic_validity <- safe_read_csv(economic_validity_path)
economic_validity_means <- safe_read_csv(economic_validity_means_path)
economic_validity_decision <- safe_read_csv(economic_validity_decision_path)
table_3_14_available <- FALSE
if (!is.null(economic_validity) && nrow(economic_validity) > 0 &&
    !is.null(economic_validity_decision) && nrow(economic_validity_decision) > 0) {
  economic_decision_value <- if ("economic_validity_decision" %in% names(economic_validity_decision)) {
    as.character(economic_validity_decision$economic_validity_decision[[1]])
  } else {
    NA_character_
  }
  decision_summary <- economic_validity_decision %>%
    select(any_of(c(
      "reported_score_variable", "fitted_tests", "significant_tests_p10",
      "common_top5_significant_tests_p10", "row_only_significant_tests_p10",
      "grouped_only_significant_tests_p10", "interpretation"
    )))
  validity_summary <- economic_validity %>%
    group_by(.data$reported_score_variable, .data$term) %>%
    summarise(
      tested_outcome_n = sum(.data$model_status == "fit_ok", na.rm = TRUE),
      significant_outcome_n_p10 = sum(!is.na(.data$p_value) & .data$p_value <= 0.10, na.rm = TRUE),
      min_p_value = suppressWarnings(min(.data$p_value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(min_p_value = ifelse(is.infinite(.data$min_p_value), NA_real_, .data$min_p_value))
  table_3_14 <- validity_summary %>%
    left_join(decision_summary, by = "reported_score_variable") %>%
    mutate(economic_validity_decision = economic_decision_value) %>%
    select(any_of(c(
      "reported_score_variable", "term", "tested_outcome_n", "significant_outcome_n_p10",
      "min_p_value", "fitted_tests", "significant_tests_p10",
      "common_top5_significant_tests_p10", "row_only_significant_tests_p10",
      "grouped_only_significant_tests_p10", "economic_validity_decision", "interpretation"
    )))
  if (!is.null(economic_validity_means) && nrow(economic_validity_means) > 0) {
    mean_cols <- grep("^mean_", names(economic_validity_means), value = TRUE)
    if (length(mean_cols)) {
      means_compact <- economic_validity_means %>%
        group_by(.data$reported_score_variable) %>%
        summarise(outcome_mean_fields_available = paste(mean_cols, collapse = ";"), .groups = "drop")
      table_3_14 <- table_3_14 %>% left_join(means_compact, by = "reported_score_variable")
    }
  }
  if (nrow(table_3_14) > 0) {
    write_outputs(table_3_14, "table_3_14_top_tail_economic_validity_summary", "Table 3.14 Top-Tail Economic Validity Summary")
    table_3_14_available <- TRUE
  }
}

temporal_premium <- safe_read_csv(temporal_dependence_premium_path)
temporal_decision <- safe_read_csv(temporal_dependence_decision_path)
table_3_15_available <- FALSE
if (!is.null(temporal_premium) && nrow(temporal_premium) > 0 &&
    !is.null(temporal_decision) && nrow(temporal_decision) > 0) {
  temporal_decision_value <- if ("temporal_decision" %in% names(temporal_decision)) {
    as.character(temporal_decision$temporal_decision[[1]])
  } else {
    NA_character_
  }
  table_3_15_cols <- c(
    "T", "rho", "sigma_firm", "mean_row_firmre_premium",
    "mean_grouped_firmre_premium", "mean_row_minus_grouped_firmre_premium",
    "share_row_minus_grouped_positive", "interpretation"
  )
  table_3_15 <- temporal_premium %>%
    select(any_of(table_3_15_cols)) %>%
    mutate(temporal_decision = temporal_decision_value)
  if (nrow(table_3_15) > 0) {
    write_outputs(table_3_15, "table_3_15_temporal_dependence_robustness_summary", "Table 3.15 Temporal Dependence Robustness Summary")
    table_3_15_available <- TRUE
  }
}

ExactKFold_Magnitude_RQ2_Primary_Output_Allowed <- table_3_12_available &&
  any(table_3_12$Primary_Inference_Allowed %in% TRUE &
        grepl("^primary_magnitude", table_3_12$metric_class))
RQ2_Magnitude_Primary_Output_Allowed <- ExactKFold_Magnitude_RQ2_Primary_Output_Allowed
RQ2_Tail_Primary_Output_Allowed <- Tail_Flag_Primary_Output_Allowed

git_hash <- tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(e) NA_character_)
script_hash <- tryCatch(as.character(tools::md5sum("scripts/ma17_export_tables_figures.R")), error = function(e) NA_character_)
diag <- safe_read_csv(path_table("table_brms_diagnostics_winsor.csv"))
model_inclusion_gate <- safe_read_csv(path_table("table_model_primary_inclusion_gate.csv"))
if (is.null(model_inclusion_gate) || !"Primary_Inclusion_Decision" %in% names(model_inclusion_gate)) {
  stop("[GATE BLOCKER] Missing or invalid model primary inclusion gate: ",
       path_table("table_model_primary_inclusion_gate.csv"))
}
manifest <- data.frame(
  item = c("raw_data_file_path", "raw_data_sheet_names", "common_sample_files_used", "model_registry_file",
           "model_formula_file", "winsorized_sample_files", "prior_set_id", "likelihood_family",
           "MCMC_chains", "iterations", "warmup", "seed", "adapt_delta", "max_treedepth",
           "validation_schemes_available", "DA_output_root", "timestamp", "git_commit_hash",
           "script_version_hash", "DA_Finite_Gate_Decision", "New_Firm_Predictive_Gate_Decision",
           "Exact_KFold_Reclassification_Audit_Status",
           "Exact_KFold_Reclassification_Decision", "Primary_Magnitude_Reclassification_Decision",
           "Primary_Magnitude_Reclassification_Min_Jaccard", "Tail_Reclassification_Reporting_Decision",
           "Tail_Reclassification_Primary_Status", "Di03_Output_Path",
           "Tail_Flag_Primary_Output_Allowed", "ExactKFold_Magnitude_RQ2_Primary_Output_Allowed",
           "RQ2_Magnitude_Primary_Output_Allowed", "RQ2_Tail_Primary_Output_Allowed",
           "Model_Primary_Inclusion_Gate",
           "MCMC_REVIEW_Inclusion_Rule", "Suppression_Override_Used",
           "Tail_Flag_Primary_Status"),
  value = c(
    data_path,
    paste(metadata_sheets, collapse = ", "),
    paste(sample_specs$file, collapse = ", "),
    path_table("table_model_registry_winsor.csv"),
    path_table("table_named_model_formulas_winsor.csv"),
    paste(sample_specs$file, collapse = ", "),
    prior_set_id,
    likelihood_family,
    if (!is.null(diag) && "Chains" %in% names(diag)) {
      paste(unique(diag$Chains), collapse = ",")
    } else if (!is.null(kfold_manifest) && "Chains" %in% names(kfold_manifest)) {
      kfold_manifest$Chains[1]
    } else {
      NA
    },
    if (!is.null(kfold_manifest) && "Iter" %in% names(kfold_manifest)) kfold_manifest$Iter[1] else NA,
    if (!is.null(kfold_manifest) && "Warmup" %in% names(kfold_manifest)) kfold_manifest$Warmup[1] else NA,
    if (!is.null(kfold_manifest) && "Seed" %in% names(kfold_manifest)) kfold_manifest$Seed[1] else 42,
    if (!is.null(kfold_manifest) && "Adapt_Delta" %in% names(kfold_manifest)) kfold_manifest$Adapt_Delta[1] else NA,
    if (!is.null(kfold_manifest) && "Max_Treedepth" %in% names(kfold_manifest)) kfold_manifest$Max_Treedepth[1] else NA,
    paste(c("row_loo", if (dir.exists(file.path(output_root, "lofo"))) "LOFO", if (dir.exists(file.path(output_root, "kfold_firm"))) "grouped_kfold"), collapse = ", "),
    output_root,
    as.character(Sys.time()),
    if (length(git_hash)) git_hash[1] else NA,
    script_hash,
    DA_Finite_Gate_Decision,
    New_Firm_Predictive_Gate_Decision,
    Exact_KFold_Reclassification_Audit_Status,
    Exact_KFold_Reclassification_Decision,
    Primary_Magnitude_Reclassification_Decision,
    Primary_Magnitude_Reclassification_Min_Jaccard,
    Tail_Reclassification_Reporting_Decision,
    Tail_Reclassification_Primary_Status,
    exact_kfold_reclassification_decision_path,
    Tail_Flag_Primary_Output_Allowed,
    ExactKFold_Magnitude_RQ2_Primary_Output_Allowed,
    RQ2_Magnitude_Primary_Output_Allowed,
    RQ2_Tail_Primary_Output_Allowed,
    path_table("table_model_primary_inclusion_gate.csv"),
    "PASS/OK included; FAIL/LOW_RELIABILITY excluded; REVIEW/CAUTION included only with MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS when exact-refit reliability is acceptable.",
    allow_suppressed_tail_flags,
    ifelse(Tail_Flag_Primary_Output_Allowed, "primary_allowed", "suppressed_or_non_primary")
  ),
  stringsAsFactors = FALSE
)
write_outputs(manifest, "table_3_11_code_manuscript_manifest", "Table 3.11 Code-Manuscript Manifest")

required_stems <- c(
  "table_3_1_sample_flow", "table_3_2_panel_coverage", "table_3_3_industry_year_cells",
  "table_3_4_zero_value_audit", "table_3_5_model_space_matrix",
  "appendix_screened_external_data_extensions", "table_3_6_grouped_kfold_fold_balance",
  "table_3_6b_grouped_kfold_balance_summary", "table_3_7_hierarchical_prediction_rule_audit",
  "table_3_8_preprocessing_leakage_audit", "table_3_9_prior_predictive_diagnostics",
  "table_3_10_rq2_materiality_thresholds", "table_3_11_code_manuscript_manifest"
)
if (table_3_12_available) {
  required_stems <- c(required_stems, "table_3_12_exact_kfold_reclassification_jaccard")
}
if (table_3_13_available) {
  required_stems <- c(required_stems, "table_3_13_denominator_diagnostics_summary")
}
if (table_3_14_available) {
  required_stems <- c(required_stems, "table_3_14_top_tail_economic_validity_summary")
}
if (table_3_15_available) {
  required_stems <- c(required_stems, "table_3_15_temporal_dependence_robustness_summary")
}

qc_rows <- list()
add_qc <- function(id, name, status, details) {
  qc_rows[[length(qc_rows) + 1]] <<- data.frame(check_id = id, check_name = name, status = status, details = details, stringsAsFactors = FALSE)
}

all_outputs_exist <- all(file.exists(file.path(report_dir, paste0(required_stems, ".csv"))), file.exists(file.path(report_dir, paste0(required_stems, ".md"))))
add_qc("QC01", "all required output files exist", ifelse(all_outputs_exist, "PASS", "FAIL"), paste("required stems:", length(required_stems)))
add_qc("QC02", "sample-flow table has nonzero final ex-post N", ifelse(any(sample_flow$stage == "Observations available for final common ex-post sample" & sample_flow$n_observations > 0), "PASS", "FAIL"), "")
add_qc("QC03", "sample-flow table has nonzero final no-lookahead N", ifelse(any(sample_flow$stage == "Observations available for final common no-lookahead sample" & sample_flow$n_observations > 0), "PASS", "FAIL"), "")
add_qc("QC04", "panel coverage table has nonzero firm counts", ifelse(any(panel_coverage$n_firms > 0, na.rm = TRUE), "PASS", "FAIL"), "")
add_qc("QC05", "industry-year table has nonzero cell counts", ifelse(any(industry_year$n_industry_year_cells > 0, na.rm = TRUE), "PASS", "FAIL"), "")
add_qc("QC06", "zero audit table includes all variables subject to zero treatment", ifelse(all(c("PPE", "REC", "COGS", "INV", "REV", "A", "A_lag") %in% zero_audit$variable), "PASS", "FAIL"), "")
add_qc("QC07", "model-space matrix has exactly M01-M10", ifelse(identical(sort(model_space$model_id), sprintf("M%02d", 1:10)), "PASS", "FAIL"), paste(model_space$model_id, collapse = ","))
add_qc("QC08", "appendix external-data table excluded from main model-space matrix", ifelse(!any(model_space$model_id %in% c("M11", "M12")), "PASS", "FAIL"), "")
add_qc("QC09", "fold-balance table exists if grouped K-fold outputs exist", ifelse(file.exists(file.path(report_dir, "table_3_6_grouped_kfold_fold_balance.csv")) && (is.null(kfold_balance_in) || nrow(fold_balance) > 0), "PASS", "WARN"), "")
add_qc("QC10", "prediction-rule audit classifies every hierarchical validation scheme", ifelse(!any(prediction_audit$prediction_rule_classification == "unclear_requires_manual_review"), "PASS", "WARN"), paste(prediction_audit$prediction_rule_classification, collapse = ";"))
add_qc("QC11", "preprocessing leakage audit classifies every preprocessing step", ifelse(!any(preprocessing_audit$leakage_risk_classification == "unclear_requires_manual_review"), "PASS", "WARN"), paste(preprocessing_audit$leakage_risk_classification, collapse = ";"))
add_qc("QC12", "prior predictive table exists if prior predictive outputs exist", ifelse(file.exists(path_table("table_prior_predictive_summary.csv")) && nrow(prior_diag) > 0, "PASS", "WARN"), "")
add_qc("QC13", "materiality threshold table exists", ifelse(file.exists(file.path(report_dir, "table_3_10_rq2_materiality_thresholds.csv")), "PASS", "FAIL"), "")
add_qc("QC14", "manifest table exists", ifelse(file.exists(file.path(report_dir, "table_3_11_code_manuscript_manifest.csv")), "PASS", "FAIL"), "")
add_qc("QC15", "exact-KFold reclassification manuscript table available when di03 decision exists",
       ifelse(file.exists(exact_kfold_reclassification_decision_path) && !table_3_12_available, "FAIL", "PASS"),
       ifelse(file.exists(exact_kfold_reclassification_decision_path), file.path(report_dir, "table_3_12_exact_kfold_reclassification_jaccard.csv"), "di03 decision not present"))
add_qc("QC16", "denominator diagnostics manuscript table available when di04 decision exists",
       ifelse(file.exists(denominator_diagnostics_decision_path) && !table_3_13_available, "FAIL",
              ifelse(!file.exists(denominator_diagnostics_decision_path), "WARN", "PASS")),
       ifelse(file.exists(denominator_diagnostics_decision_path), file.path(report_dir, "table_3_13_denominator_diagnostics_summary.csv"), "di04 decision not present"))
add_qc("QC17", "economic-validity manuscript table available when di05 decision exists",
       ifelse(file.exists(economic_validity_decision_path) && !table_3_14_available, "FAIL",
              ifelse(!file.exists(economic_validity_decision_path), "WARN", "PASS")),
       ifelse(file.exists(economic_validity_decision_path), file.path(report_dir, "table_3_14_top_tail_economic_validity_summary.csv"), "di05 decision not present"))
add_qc("QC18", "temporal-dependence robustness table available when temporal decision exists",
       ifelse(file.exists(temporal_dependence_decision_path) && !table_3_15_available, "FAIL",
              ifelse(!file.exists(temporal_dependence_decision_path), "WARN", "PASS")),
       ifelse(file.exists(temporal_dependence_decision_path), file.path(report_dir, "table_3_15_temporal_dependence_robustness_summary.csv"), "di09 temporal decision not present"))

report_path <- file.path(report_dir, "chapter3_methods_tables_report.md")
qc_path <- file.path(report_dir, "chapter3_methods_tables_qc.csv")
add_qc("QC19", "master report exists", "PASS", report_path)
qc <- bind_rows(qc_rows)
write_csv_safely(qc, qc_path, row.names = FALSE, fileEncoding = "UTF-8")
generated_files <- unique(c(generated_files, qc_path, report_path))

if (any(is.na(sample_flow$n_observations))) add_warning("One or more sample-flow counts are missing.")
if (!nrow(zero_audit)) add_warning("Zero treatment cannot be audited.")
if (is.null(fold_assignment) || is.null(kfold_balance_in)) add_warning("Fold assignment/balance files are missing.")
if (any(prediction_audit$prediction_rule_classification == "unclear_requires_manual_review")) add_warning("LOFO/grouped prediction rule is unclear.")
if (any(preprocessing_audit$leakage_risk_classification %in% c("potential_predictive_leakage", "unclear_requires_manual_review"))) add_warning("Preprocessing leakage risk is detected or unclear.")
if (is.null(prior_summary)) add_warning("Prior predictive diagnostics are missing.")
if (is.null(diag) || is.null(kfold_balance_in)) add_warning("MCMC/fold outputs are not fully available.")
if (!file.exists(denominator_diagnostics_decision_path)) add_warning("Denominator diagnostics are missing.")
if (!file.exists(economic_validity_decision_path)) add_warning("Economic-validity top-tail diagnostics are missing.")
if (!file.exists(temporal_dependence_decision_path)) add_warning("Temporal-dependence robustness outputs are missing.")

report_lines <- c(
  "# Chapter 3 Methods Tables Report",
  "",
  "These tables were generated by `source(\"scripts/ma17_export_tables_figures.R\")` from existing raw data, common-sample, winsorization, prior predictive, LOFO, grouped K-fold, and manifest outputs. The script does not refit Bayesian models or alter empirical design choices.",
  "",
  "## Generated Files",
  paste0("- `", sort(generated_files), "`"),
  "",
  "## Interpretation Notes",
  "- Sample, panel, and industry-year counts are computed from current pipeline outputs.",
  "- Model-space outputs restrict the main manuscript matrix to M01-M10; screened external-data extensions are separated into an appendix table.",
  "- Prior predictive acceptance status uses configurable thresholds declared at the top of the script.",
  "- RQ2 materiality thresholds are design thresholds, not empirical results.",
  "",
  "## Warnings Requiring Author Review",
  if (length(warnings_for_author)) paste0("- ", warnings_for_author) else "- None.",
  "",
  "## Final QC Checklist",
  paste0("- ", qc$check_id, " ", qc$check_name, ": ", qc$status, ifelse(nzchar(qc$details), paste0(" (", qc$details, ")"), "")),
  ""
)
writeLines(report_lines, report_path, useBytes = TRUE)

qc_counts <- table(factor(qc$status, levels = c("PASS", "WARN", "FAIL")))
overall <- if (any(qc$status == "FAIL")) "FAIL" else if (any(qc$status == "WARN")) "WARN" else "PASS"
cat("\nCHAPTER 3 METHODS TABLES QC: ", overall, "\n", sep = "")
cat("Output directory: ", report_dir, "\n", sep = "")
cat("Generated tables:\n")
for (f in sort(generated_files)) cat(" - ", f, "\n", sep = "")
cat("QC counts: PASS=", qc_counts[["PASS"]], " WARN=", qc_counts[["WARN"]], " FAIL=", qc_counts[["FAIL"]], "\n", sep = "")
if (length(warnings_for_author)) {
  cat("Author-review warnings:\n")
  for (w in warnings_for_author) cat(" - ", w, "\n", sep = "")
}
cat("Open master report with: start ", normalizePath(report_path, winslash = "\\", mustWork = FALSE), "\n", sep = "")
phase_end("ma17", "Export tables and figures")
