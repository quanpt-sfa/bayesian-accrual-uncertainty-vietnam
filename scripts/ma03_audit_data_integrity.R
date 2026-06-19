# -----------------------------------------------------------------------------
# Script: 03_audit_cogs_inv_operating_cycle.R
# Purpose: Audit corrected COGS/INV fields and operating_cycle after Phase 1.
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE)

source("scripts/ma00_setup.R")
phase_begin("ma03", "Data integrity audit")
ensure_baseline_dirs()

audit_table_path <- baseline_table_path("table_cogs_inv_operating_cycle_audit_corrected.csv")
audit_notes_path <- baseline_log_path("cogs_inv_correction_audit_notes.txt")
audit_status_path <- baseline_log_path("cogs_inv_correction_audit_status.txt")
top_extremes_path <- baseline_table_path("table_top_operating_cycle_extremes.csv")

required_files <- c(
  data_path,
  baseline_table_path("final_common_ex_post_sample.csv"),
  baseline_table_path("final_common_realtime_sample.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("[BLOCKER] Missing required corrected Phase 1 file(s): ", paste(missing_files, collapse = ", "))
}

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("[BLOCKER] Package 'readxl' is required to audit the raw data workbook.")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("[BLOCKER] Package 'dplyr' is required to audit operating-cycle extremes.")
}
library(dplyr)

q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7, names = FALSE))
}

num_summary <- function(prefix, x) {
  x <- suppressWarnings(as.numeric(x))
  out <- c(
    min = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
    p1 = q(x, 0.01),
    median = if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE),
    mean = if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
    p99 = q(x, 0.99),
    max = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  )
  stats::setNames(as.list(out), paste0(prefix, "_", names(out)))
}

lag_continuous <- function(x, yr, n = 1L) {
  lag_val <- c(rep(NA, n), head(x, -n))
  lag_yr <- c(rep(NA, n), head(yr, -n))
  ifelse(!is.na(lag_yr) & lag_yr == (yr - n), lag_val, NA)
}

lead_continuous <- function(x, yr, n = 1L) {
  lead_val <- c(tail(x, -n), rep(NA, n))
  lead_yr <- c(tail(yr, -n), rep(NA, n))
  ifelse(!is.na(lead_yr) & lead_yr == (yr + n), lead_val, NA)
}

df_raw <- as.data.frame(readxl::read_excel(data_path, sheet = "Sheet1"))
required_cols <- c("company", "year", "A", "NI", "REV", "CFO", "REC", "PPE", "ROA", "COGS", "INV")
missing_cols <- setdiff(required_cols, colnames(df_raw))
if (length(missing_cols) > 0) {
  stop("[BLOCKER] Raw data workbook is missing required columns: ", paste(missing_cols, collapse = ", "))
}

for (v in setdiff(required_cols, "company")) {
  df_raw[[v]] <- suppressWarnings(as.numeric(df_raw[[v]]))
}

df_step <- df_raw[df_raw$A != 0 & !is.na(df_raw$A), , drop = FALSE]
df_step$PPE[df_step$PPE == 0] <- NA_real_
df_step$REC[df_step$REC == 0] <- NA_real_
df_step$COGS[df_step$COGS == 0] <- NA_real_
df_clean <- df_step[df_step$year > 2015, , drop = FALSE]
df_clean <- df_clean[order(df_clean$company, df_clean$year), , drop = FALSE]

parts <- split(df_clean, df_clean$company)
parts <- lapply(parts, function(d) {
  d <- d[order(d$year), , drop = FALSE]
  d$A_lag <- lag_continuous(d$A, d$year)
  d$REV_lag <- lag_continuous(d$REV, d$year)
  d$REC_lag <- lag_continuous(d$REC, d$year)
  d$CFO_lag <- lag_continuous(d$CFO, d$year)
  d$ROA_lag <- lag_continuous(d$ROA, d$year)
  d$CFO_lead <- lead_continuous(d$CFO, d$year)
  d
})
df_vars <- do.call(rbind, parts)
row.names(df_vars) <- NULL

valid_den <- !is.na(df_vars$A_lag) & df_vars$A_lag != 0
df_vars$TA_scaled <- ifelse(valid_den, (df_vars$NI - df_vars$CFO) / df_vars$A_lag, NA_real_)
df_vars$inv_A_lag <- ifelse(valid_den, 1 / df_vars$A_lag, NA_real_)
df_vars$dREV_scaled <- ifelse(valid_den, (df_vars$REV - df_vars$REV_lag) / df_vars$A_lag, NA_real_)
df_vars$dREC_scaled <- ifelse(valid_den, (df_vars$REC - df_vars$REC_lag) / df_vars$A_lag, NA_real_)
df_vars$dREV_dREC_scaled <- ifelse(valid_den, ((df_vars$REV - df_vars$REV_lag) - (df_vars$REC - df_vars$REC_lag)) / df_vars$A_lag, NA_real_)
df_vars$PPE_scaled <- ifelse(valid_den, df_vars$PPE / df_vars$A_lag, NA_real_)
df_vars$CFO_lag_scaled <- ifelse(valid_den, df_vars$CFO_lag / df_vars$A_lag, NA_real_)
df_vars$CFO_curr_scaled <- ifelse(valid_den, df_vars$CFO / df_vars$A_lag, NA_real_)
df_vars$CFO_lead_scaled <- ifelse(valid_den, df_vars$CFO_lead / df_vars$A_lag, NA_real_)
df_vars$NEG_CFO <- ifelse(is.na(df_vars$CFO_curr_scaled), NA_real_, ifelse(df_vars$CFO_curr_scaled < 0, 1, 0))
df_vars$Size <- log(df_vars$A)
df_vars$operating_cycle <- ifelse(df_vars$REV > 0 & df_vars$COGS > 0, (df_vars$REC / df_vars$REV) + (df_vars$INV / df_vars$COGS), NA_real_)
df_vars$sales_growth <- ifelse(!is.na(df_vars$REV_lag) & df_vars$REV_lag > 0, (df_vars$REV / df_vars$REV_lag) - 1, NA_real_)
df_vars$INV_over_COGS <- ifelse(!is.na(df_vars$COGS) & df_vars$COGS != 0, df_vars$INV / df_vars$COGS, NA_real_)

core_ep_vars <- c(
  "TA_scaled", "inv_A_lag", "dREV_scaled", "dREC_scaled", "dREV_dREC_scaled",
  "PPE_scaled", "ROA_lag", "CFO_lag_scaled", "CFO_curr_scaled",
  "CFO_lead_scaled", "NEG_CFO", "Size", "operating_cycle", "sales_growth"
)
core_rt_vars <- setdiff(core_ep_vars, "CFO_lead_scaled")

dropped_due_operating_cycle <- function(vars) {
  other_vars <- setdiff(vars, "operating_cycle")
  other_ok <- stats::complete.cases(df_vars[, other_vars, drop = FALSE])
  sum(other_ok & is.na(df_vars$operating_cycle))
}

final_ep <- read.csv(baseline_table_path("final_common_ex_post_sample.csv"), stringsAsFactors = FALSE)
final_rt <- read.csv(baseline_table_path("final_common_realtime_sample.csv"), stringsAsFactors = FALSE)

ratio <- df_vars$INV_over_COGS
ratio_finite <- ratio[is.finite(ratio)]
ratio_extreme_share <- if (length(ratio_finite) == 0) NA_real_ else mean(ratio_finite > 5 | ratio_finite < 0)
ratio_very_extreme_share <- if (length(ratio_finite) == 0) NA_real_ else mean(ratio_finite > 10 | ratio_finite < 0)
oper_missing_share <- mean(is.na(df_vars$operating_cycle))

audit_status <- "PASS"
main_sample_status <- "PASS"
secondary_operating_cycle_status <- "PASS"
status_reasons <- character()

ratio_median <- if (length(ratio_finite) == 0) NA_real_ else stats::median(ratio_finite, na.rm = TRUE)
cogs_median <- stats::median(df_vars$COGS, na.rm = TRUE)
inv_median <- stats::median(df_vars$INV, na.rm = TRUE)
cogs_inv_suspect <- FALSE
tail_suspect <- FALSE

if (length(ratio_finite) < 100) {
  cogs_inv_suspect <- TRUE
  status_reasons <- c(status_reasons, "Fewer than 100 finite INV/COGS observations.")
}
if (!is.na(ratio_median) && ratio_median > 2) {
  cogs_inv_suspect <- TRUE
  status_reasons <- c(status_reasons, "Median INV/COGS is above 2, which is too high for continuing without review.")
}
if (!is.na(cogs_median) && !is.na(inv_median) && cogs_median < inv_median) {
  cogs_inv_suspect <- TRUE
  status_reasons <- c(status_reasons, "Median COGS is below median INV; COGS/INV may still be suspect.")
}
if (!is.na(ratio_extreme_share) && ratio_extreme_share > 0.05) {
  tail_suspect <- TRUE
  status_reasons <- c(status_reasons, "More than 5% of finite INV/COGS values are negative or above 5.")
}
if (!is.na(ratio_very_extreme_share) && ratio_very_extreme_share > 0.02) {
  tail_suspect <- TRUE
  status_reasons <- c(status_reasons, "More than 2% of finite INV/COGS values are negative or above 10.")
}
if (!is.na(q(ratio, 0.99)) && q(ratio, 0.99) > 10) {
  tail_suspect <- TRUE
  status_reasons <- c(status_reasons, "The 99th percentile of INV/COGS is above 10.")
}
if (!is.na(oper_missing_share) && oper_missing_share > 0.50) {
  tail_suspect <- TRUE
  status_reasons <- c(status_reasons, "More than half of cleaned rows have missing operating_cycle.")
}

if (cogs_inv_suspect) {
  audit_status <- "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT"
  main_sample_status <- "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT"
  secondary_operating_cycle_status <- "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT"
} else if (tail_suspect) {
  audit_status <- "PASS_FOR_MAIN_SAMPLE_SECONDARY_OC_REVIEW"
  main_sample_status <- "PASS"
  secondary_operating_cycle_status <- "REVIEW_REQUIRED_OPERATING_CYCLE_TAIL"
} else {
  audit_status <- "PASS"
  main_sample_status <- "PASS"
  secondary_operating_cycle_status <- "PASS"
}

audit_row <- c(
  list(
    Audit_Status = audit_status,
    Main_Sample_Status = main_sample_status,
    Secondary_OperatingCycle_Status = secondary_operating_cycle_status,
    Secondary_Operating_Cycle_Status = secondary_operating_cycle_status,
    Recommended_Action = ifelse(
      audit_status == "PASS_FOR_MAIN_SAMPLE_SECONDARY_OC_REVIEW",
      "Continue main pipeline; treat M10 as secondary robustness only.",
      ifelse(audit_status == "PASS", "Continue pipeline.", "Stop and review corrected COGS/INV fields.")
    ),
    N_Rows = nrow(df_vars),
    Final_Ex_Post_N = nrow(final_ep),
    Final_No_Lookahead_N = nrow(final_rt),
    COGS_Missing_Count = sum(is.na(df_vars$COGS)),
    INV_Missing_Count = sum(is.na(df_vars$INV)),
    COGS_Zero_Count = sum(df_raw$COGS == 0, na.rm = TRUE),
    INV_Zero_Count = sum(df_raw$INV == 0, na.rm = TRUE)
  ),
  num_summary("COGS", df_vars$COGS),
  num_summary("INV", df_vars$INV),
  num_summary("INV_over_COGS", df_vars$INV_over_COGS),
  list(Operating_Cycle_Missing_Count = sum(is.na(df_vars$operating_cycle))),
  num_summary("Operating_Cycle", df_vars$operating_cycle),
  list(
    Rows_Dropped_From_Core_Ex_Post_Because_Operating_Cycle = dropped_due_operating_cycle(core_ep_vars),
    Rows_Dropped_From_Core_No_Lookahead_Because_Operating_Cycle = dropped_due_operating_cycle(core_rt_vars),
    INV_over_COGS_Extreme_Share = ratio_extreme_share,
    Operating_Cycle_Missing_Share = oper_missing_share
  )
)
audit_df <- as.data.frame(audit_row, check.names = FALSE)
audit_df$Secondary_OperatingCycle_Status <- secondary_operating_cycle_status
audit_df$Secondary_Operating_Cycle_Status <- secondary_operating_cycle_status
write.csv(audit_df, audit_table_path, row.names = FALSE)

top_extremes <- df_vars %>%
  dplyr::mutate(
    REC_over_REV = ifelse(!is.na(REV) & REV != 0, REC / REV, NA_real_),
    Notes = "Top operating_cycle observation for audit only."
  ) %>%
  dplyr::arrange(dplyr::desc(operating_cycle)) %>%
  dplyr::select(company, year, industry, REV, REC, COGS, INV, REC_over_REV, INV_over_COGS, operating_cycle, Notes) %>%
  utils::head(50)
write.csv(top_extremes, top_extremes_path, row.names = FALSE)

old_compare_notes <- character()
quarantine_path <- Sys.getenv("ACCRUAL_COGS_INV_QUARANTINE_PATH", unset = "")
latest_quarantine_path <- file.path("out", "logs", "latest_invalid_cogs_inv_quarantine.txt")
if (quarantine_path == "" && file.exists(latest_quarantine_path)) {
  quarantine_path <- trimws(readLines(latest_quarantine_path, warn = FALSE)[1])
}
if (nzchar(quarantine_path)) {
  old_summary <- file.path(quarantine_path, normalizePath(baseline_table_path("table_common_sample_summary.csv"), winslash = "/", mustWork = FALSE))
  new_summary <- baseline_table_path("table_common_sample_summary.csv")
  if (file.exists(old_summary) && file.exists(new_summary)) {
    old_df <- read.csv(old_summary, stringsAsFactors = FALSE)
    new_df <- read.csv(new_summary, stringsAsFactors = FALSE)
    if ("Sample" %in% names(old_df) && "Sample" %in% names(new_df) &&
        "N_Obs" %in% names(old_df) && "N_Obs" %in% names(new_df)) {
      cmp <- merge(
        old_df[, c("Sample", "N_Obs")],
        new_df[, c("Sample", "N_Obs")],
        by = "Sample",
        all = TRUE,
        suffixes = c("_Old_Invalid", "_Corrected")
      )
      cmp$Difference <- cmp$N_Obs_Corrected - cmp$N_Obs_Old_Invalid
      cmp$Difference_Pct <- ifelse(is.na(cmp$N_Obs_Old_Invalid) | cmp$N_Obs_Old_Invalid == 0,
                                   NA_real_, 100 * cmp$Difference / cmp$N_Obs_Old_Invalid)
      old_compare_notes <- apply(cmp, 1, function(r) {
        sprintf("%s: old_invalid=%s corrected=%s diff=%s",
                r[["Sample"]], r[["N_Obs_Old_Invalid"]], r[["N_Obs_Corrected"]], r[["Difference"]])
      })
    }
  }
}

notes <- c(
  "COGS/INV correction audit after Phase 1",
  paste("Audit timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste("Audit status:", audit_status),
  paste("Main sample status:", main_sample_status),
  paste("Secondary operating-cycle status:", secondary_operating_cycle_status),
  paste("Recommended action:", ifelse(
    audit_status == "PASS_FOR_MAIN_SAMPLE_SECONDARY_OC_REVIEW",
    "continue main pipeline; treat M10 as secondary robustness only.",
    ifelse(audit_status == "PASS", "continue pipeline.", "stop and review corrected COGS/INV fields.")
  )),
  if (length(status_reasons) == 0) {
    "Plausibility statement: corrected COGS/INV and operating_cycle distributions look plausible enough to continue under conservative thresholds."
  } else if (audit_status == "PASS_FOR_MAIN_SAMPLE_SECONDARY_OC_REVIEW") {
    c("Plausibility statement: COGS/INV medians look plausible for the main sample, but operating-cycle upper tails require secondary robustness treatment.", paste("-", status_reasons))
  } else {
    c("Plausibility statement: REVIEW_REQUIRED before continuing.", paste("-", status_reasons))
  },
  paste("Corrected ex-post sample N:", nrow(final_ep)),
  paste("Corrected no-look-ahead sample N:", nrow(final_rt)),
  paste("INV/COGS p99:", format(q(ratio, 0.99), digits = 6)),
  paste("operating_cycle p99:", format(q(df_vars$operating_cycle, 0.99), digits = 6)),
  paste("Top operating-cycle extremes table:", top_extremes_path),
  if (length(old_compare_notes) > 0) c("", "Old invalid vs corrected sample comparison:", old_compare_notes) else "",
  paste("Audit table:", audit_table_path)
)
writeLines(notes, audit_notes_path)
writeLines(audit_status, audit_status_path)

cat("\n===== COGS/INV OPERATING CYCLE AUDIT =====\n")
cat("Audit status:", audit_status, "\n")
cat("Main sample status:", main_sample_status, "\n")
cat("Secondary operating-cycle status:", secondary_operating_cycle_status, "\n")
cat("Corrected ex-post N:", nrow(final_ep), "\n")
cat("Corrected no-look-ahead N:", nrow(final_rt), "\n")
cat("Audit table:", audit_table_path, "\n")
cat("Audit notes:", audit_notes_path, "\n")

if (audit_status == "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT") {
  quit(status = 2)
}
phase_end("ma03", "Data integrity audit")
