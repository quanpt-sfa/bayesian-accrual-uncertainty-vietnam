# -----------------------------------------------------------------------------
# Script: 02_v3_build_common_sample.R
# Purpose: Clean raw data, construct variables, and build core common samples.
# Author: Antigravity
# Date: 2026-06-04
# -----------------------------------------------------------------------------

source("scripts/v3/00_v3_winsor_helpers.R")

library(readxl)
library(dplyr)

# Paths
ensure_v3_baseline_dirs()
data_path <- v3_data_path
registry_path <- v3_baseline_table_path("table_v3_model_registry.csv")

if (!file.exists(data_path)) {
  stop("[BLOCKER] Data workbook not found at: ", data_path)
}
if (!file.exists(registry_path)) {
  stop("[BLOCKER] Model registry CSV not found. Please run scripts/v3/01_v3_setup_and_registry.R first.")
}

# Load Registry
registry <- read.csv(registry_path, stringsAsFactors = FALSE)
feasible_models <- registry %>% filter(Feasible_With_This_Data == "TRUE")

# Read Data
df_raw <- read_excel(data_path, sheet = "Sheet1")
df_metadata <- read_excel(data_path, sheet = "Sheet2")

# Count initial rows
n_raw_initial <- nrow(df_raw)
message("Initial raw rows: ", n_raw_initial)

# 1. APPLY ZERO-AS-MISSING TREATMENTS & YEAR RULE BEFORE CONSTRUCTING VARIABLES
# A: drop-row where A == 0
df_step1 <- df_raw %>% filter(A != 0)
n_drop_A_zero <- n_raw_initial - nrow(df_step1)
message("Rows dropped due to A == 0: ", n_drop_A_zero)

# PPE, REC, COGS: set to NA where they are 0
df_step2 <- df_step1 %>%
  mutate(
    PPE = ifelse(PPE == 0, NA, PPE),
    REC = ifelse(REC == 0, NA, REC),
    COGS = ifelse(COGS == 0, NA, COGS)
  )

# DROP the entire year 2015 from the dataset completely
df_clean <- df_step2 %>% filter(year > 2015)
n_drop_2015 <- nrow(df_step2) - nrow(df_clean)
message("Rows dropped because year is 2015: ", n_drop_2015)

# 2. CONSTRUCT DERIVED VARIABLES
# Arrange by company and year to ensure lags/leads are correct
df_clean <- df_clean %>%
  arrange(company, year)

# Helper function to get correct lag/lead (ensures year continuity)
get_lag <- function(x, yr, n = 1) {
  # returns lagged value only if the lag year is exactly yr - n
  lag_val <- dplyr::lag(x, n)
  lag_yr <- dplyr::lag(yr, n)
  ifelse(!is.na(lag_yr) & lag_yr == (yr - n), lag_val, NA)
}

get_lead <- function(x, yr, n = 1) {
  # returns lead value only if the lead year is exactly yr + n
  lead_val <- dplyr::lead(x, n)
  lead_yr <- dplyr::lead(yr, n)
  ifelse(!is.na(lead_yr) & lead_yr == (yr + n), lead_val, NA)
}

df_vars <- df_clean %>%
  group_by(company) %>%
  mutate(
    # Basic Lags
    A_lag   = get_lag(A, year),
    REV_lag = get_lag(REV, year),
    REC_lag = get_lag(REC, year),
    CFO_lag = get_lag(CFO, year),
    ROA_lag = get_lag(ROA, year),
    
    # Lead
    CFO_lead = get_lead(CFO, year)
  ) %>%
  ungroup()

# Apply denominator_invalid check for A_lag
df_vars <- df_vars %>%
  mutate(
    has_valid_denominator = !is.na(A_lag) & A_lag != 0
  )

# Compute scaled and derived variables
df_vars <- df_vars %>%
  mutate(
    # Outcome
    TA_scaled = ifelse(has_valid_denominator, (NI - CFO) / A_lag, NA),
    
    # Jones family
    inv_A_lag        = ifelse(has_valid_denominator, 1 / A_lag, NA),
    dREV_scaled      = ifelse(has_valid_denominator, (REV - REV_lag) / A_lag, NA),
    dREC_scaled      = ifelse(has_valid_denominator, (REC - REC_lag) / A_lag, NA),
    dREV_dREC_scaled = ifelse(has_valid_denominator, ((REV - REV_lag) - (REC - REC_lag)) / A_lag, NA),
    PPE_scaled       = ifelse(has_valid_denominator, PPE / A_lag, NA),
    
    # Performance
    ROA_curr         = ROA,
    
    # CFO mapping
    CFO_lag_scaled   = ifelse(has_valid_denominator, CFO_lag / A_lag, NA),
    CFO_curr_scaled  = ifelse(has_valid_denominator, CFO / A_lag, NA),
    CFO_lead_scaled  = ifelse(has_valid_denominator, CFO_lead / A_lag, NA),
    
    # Asymmetry
    NEG_CFO          = ifelse(is.na(CFO_curr_scaled), NA, ifelse(CFO_curr_scaled < 0, 1, 0)),
    NEG_EARN         = ifelse(has_valid_denominator, ifelse((NI / A_lag) < 0, 1, 0), NA),
    
    # Size
    Size             = log(A),
    
    # Temporary columns for rolling SD
    REV_scaled_temp = ifelse(has_valid_denominator, REV / A_lag, NA),
    CFO_scaled_temp = ifelse(has_valid_denominator, CFO / A_lag, NA)
  )

# Rolling standard deviation over current and prior 2 years (t, t-1, t-2)
df_vars <- df_vars %>%
  group_by(company) %>%
  mutate(
    sd_REV = sapply(seq_along(year), function(i) {
      if (i >= 3) {
        yrs <- year[(i-2):i]
        vals <- REV_scaled_temp[(i-2):i]
        if (all(yrs == (year[i] - c(2, 1, 0))) && all(!is.na(vals))) {
          return(sd(vals))
        }
      }
      return(NA_real_)
    }),
    sd_CFO = sapply(seq_along(year), function(i) {
      if (i >= 3) {
        yrs <- year[(i-2):i]
        vals <- CFO_scaled_temp[(i-2):i]
        if (all(yrs == (year[i] - c(2, 1, 0))) && all(!is.na(vals))) {
          return(sd(vals))
        }
      }
      return(NA_real_)
    })
  ) %>%
  ungroup()

# Operating cycle & Growth
df_vars <- df_vars %>%
  mutate(
    operating_cycle = ifelse(REV > 0 & COGS > 0, (REC / REV) + (INV / COGS), NA),
    revenue_growth  = ifelse(!is.na(REV_lag) & REV_lag > 0, (REV / REV_lag) - 1, NA),
    sales_growth    = revenue_growth
  )

# Add metadata (industry classification)
df_vars <- df_vars %>%
  left_join(df_metadata, by = c("company" = "Mã"))

# 3. CATEGORIZE ROW DROPS (WITH RESPECT TO CORE COMMON SAMPLES)
# Define core variable lists (excluding sd_REV and sd_CFO)
core_ex_post_vars <- c(
  "TA_scaled", "inv_A_lag", "dREV_scaled", "dREC_scaled", "dREV_dREC_scaled", "PPE_scaled", "ROA_lag",
  "CFO_lag_scaled", "CFO_curr_scaled", "CFO_lead_scaled", "NEG_CFO", "Size", "sales_growth"
)

core_realtime_vars <- c(
  "TA_scaled", "inv_A_lag", "dREV_scaled", "dREC_scaled", "dREV_dREC_scaled", "PPE_scaled", "ROA_lag",
  "CFO_lag_scaled", "CFO_curr_scaled", "NEG_CFO", "Size", "sales_growth"
)

secondary_operating_cycle_ex_post_vars <- c(core_ex_post_vars, "operating_cycle")
secondary_operating_cycle_realtime_vars <- c(core_realtime_vars, "operating_cycle")

# Categorize rows based on Core Ex-Post space requirements
categorize_row <- function(row) {
  if (is.na(row$A) || row$A == 0) {
    return("denominator_invalid")
  }
  if (is.na(row$A_lag) || row$A_lag == 0) {
    return("denominator_invalid")
  }
  if (is.na(row$REV_lag) || is.na(row$REC_lag) || is.na(row$CFO_lag) || is.na(row$ROA_lag)) {
    return("lag_missing")
  }
  if (is.na(row$PPE) || is.na(row$REC)) {
    return("data_invalid")
  }
  if (is.na(row$CFO_lead)) {
    return("lead_missing")
  }
  if (is.na(row$sales_growth)) {
    return("model_variable_missing")
  }
  # sd_REV and sd_CFO are optional for the core set
  if (is.na(row$sd_REV) || is.na(row$sd_CFO)) {
    return("optional_variable_missing")
  }
  return("eligible")
}

row_status <- sapply(1:nrow(df_vars), function(i) {
  categorize_row(df_vars[i, ])
})
df_vars$row_status <- row_status

# Print Core Dropped Observation Breakdown
dropped_stats <- as.data.frame(table(row_status))
colnames(dropped_stats) <- c("Category", "Count")
message("\nDropped Observation Breakdown for Core Ex-Post space:")
print(dropped_stats)

# 4. BUILD MAIN AND SECONDARY COMMON SAMPLES
# A. Core common ex_post sample
final_ex_post <- df_vars[complete.cases(df_vars[, core_ex_post_vars]), ]

# B. Core common realtime sample
final_realtime <- df_vars[complete.cases(df_vars[, core_realtime_vars]), ]

# C. Secondary operating-cycle samples for M10 robustness only
final_operating_cycle_ex_post <- df_vars[complete.cases(df_vars[, secondary_operating_cycle_ex_post_vars]), ]
final_operating_cycle_realtime <- df_vars[complete.cases(df_vars[, secondary_operating_cycle_realtime_vars]), ]

# D. Secondary samples (including sd_REV & sd_CFO for M08)
final_ex_post_m08 <- final_ex_post %>%
  filter(!is.na(sd_REV), !is.na(sd_CFO))

final_realtime_m08 <- final_realtime %>%
  filter(!is.na(sd_REV), !is.na(sd_CFO))

message("\nFinal Sample Sizes (Core):")
message("Main Common Ex-Post Sample N: ", nrow(final_ex_post))
message("Main Common No-Lookahead Sample N: ", nrow(final_realtime))
message("Secondary OperatingCycle Ex-Post Sample N: ", nrow(final_operating_cycle_ex_post))
message("Secondary OperatingCycle No-Lookahead Sample N: ", nrow(final_operating_cycle_realtime))

message("\nFinal Sample Sizes (Secondary/M08 Robustness):")
message("M08 Ex-Post Subsample N: ", nrow(final_ex_post_m08))
message("M08 Realtime Subsample N: ", nrow(final_realtime_m08))

# Assert N > 0
if (nrow(final_ex_post) == 0) {
  stop("[BLOCKER] Core Common Ex-Post Sample has 0 rows.")
}
if (nrow(final_realtime) == 0) {
  stop("[BLOCKER] Core Common Realtime Sample has 0 rows.")
}

# Confirm 2015 contributes zero rows and zero lags
if (any(final_ex_post$year == 2015) || any(final_realtime$year == 2015)) {
  stop("[BLOCKER] Year 2015 is present in final sample.")
}

# Export core samples
write.csv(final_ex_post, v3_baseline_table_path("final_v3_common_ex_post_sample.csv"), row.names = FALSE)
write.csv(final_realtime, v3_baseline_table_path("final_v3_common_realtime_sample.csv"), row.names = FALSE)
write.csv(final_operating_cycle_ex_post, v3_baseline_table_path("final_v3_secondary_operating_cycle_ex_post_sample.csv"), row.names = FALSE)
write.csv(final_operating_cycle_realtime, v3_baseline_table_path("final_v3_secondary_operating_cycle_realtime_sample.csv"), row.names = FALSE)
# Export secondary samples for M08 robustness
write.csv(final_ex_post_m08, v3_baseline_table_path("final_v3_M08_ex_post_subsample.csv"), row.names = FALSE)
write.csv(final_realtime_m08, v3_baseline_table_path("final_v3_M08_realtime_subsample.csv"), row.names = FALSE)
message("Saved core and secondary samples to: ", file.path(v3_original_root, "tables"))

# 5. DIAGNOSTIC: Model Specific Availability
model_availability <- data.frame(
  Model_ID = character(),
  Model_Name = character(),
  N_Available = integer(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(feasible_models)) {
  m <- feasible_models[i, ]
  vars_needed <- trimws(unlist(strsplit(m$Required_Variables, ",")))
  
  df_m <- df_vars %>% filter(!is.na(TA_scaled))
  for (v in vars_needed) {
    if (v %in% colnames(df_m)) {
      df_m <- df_m %>% filter(!is.na(.data[[v]]))
    }
  }
  
  model_availability <- rbind(model_availability, data.frame(
    Model_ID = m$Model_ID,
    Model_Name = m$Model_Name,
    N_Available = nrow(df_m)
  ))
}

write.csv(model_availability, v3_baseline_table_path("table_v3_missingness_by_model.csv"), row.names = FALSE)
message("Saved model specific availability.")

# Create sample construction table
sample_construction <- data.frame(
  Step = c("Initial raw rows", "Drop A == 0", "Drop year 2015", "Main Ex-Post Common", "Main No-Lookahead Common", "Secondary OperatingCycle Ex-Post", "Secondary OperatingCycle No-Lookahead", "M08 Ex-Post Subsample", "M08 No-Lookahead Subsample"),
  N_Rows = c(n_raw_initial, n_raw_initial - n_drop_A_zero, n_raw_initial - n_drop_A_zero - n_drop_2015, nrow(final_ex_post), nrow(final_realtime), nrow(final_operating_cycle_ex_post), nrow(final_operating_cycle_realtime), nrow(final_ex_post_m08), nrow(final_realtime_m08)),
  stringsAsFactors = FALSE
)
write.csv(sample_construction, v3_baseline_table_path("table_v3_sample_construction.csv"), row.names = FALSE)

# Variable coverage summary (for ex-post core and optional variables)
all_tracked_vars <- unique(c(core_ex_post_vars, "sd_REV", "sd_CFO"))
variable_coverage <- data.frame(
  Variable = all_tracked_vars,
  N_NonMissing = sapply(all_tracked_vars, function(v) sum(!is.na(df_vars[[v]]))),
  pct_NonMissing = sapply(all_tracked_vars, function(v) round(sum(!is.na(df_vars[[v]])) / nrow(df_vars) * 100, 2)),
  stringsAsFactors = FALSE
)
write.csv(variable_coverage, v3_baseline_table_path("table_v3_variable_coverage.csv"), row.names = FALSE)

# Common sample summary statistics
sample_summary_row <- function(df, sample_name, requires_oc, intended_use, notes) {
  data.frame(
    Sample_Name = sample_name,
    N_Obs = nrow(df),
    N_Firms = length(unique(df$company)),
    Min_Year = if (nrow(df) == 0) NA_integer_ else min(df$year),
    Max_Year = if (nrow(df) == 0) NA_integer_ else max(df$year),
    Requires_Operating_Cycle = requires_oc,
    Intended_Use = intended_use,
    Notes = notes,
    stringsAsFactors = FALSE
  )
}

common_summary <- bind_rows(
  sample_summary_row(final_ex_post, "Main Ex-Post Common", FALSE, "Main ex-post model comparison and stacking", "Excludes operating_cycle so COGS/INV availability does not restrict main models."),
  sample_summary_row(final_realtime, "Main No-Lookahead Common", FALSE, "Main no-lookahead model comparison and stacking", "Excludes operating_cycle so COGS/INV availability does not restrict main models."),
  sample_summary_row(final_operating_cycle_ex_post, "Secondary OperatingCycle Ex-Post", TRUE, "M10 operating-cycle robustness only", "Use only for secondary M10/comparator analyses on identical operating-cycle-available sample."),
  sample_summary_row(final_operating_cycle_realtime, "Secondary OperatingCycle No-Lookahead", TRUE, "M10 operating-cycle robustness only", "Use only for secondary M10/comparator analyses on identical operating-cycle-available sample."),
  sample_summary_row(final_ex_post_m08, "M08 Ex-Post Subsample", FALSE, "M08 rolling-volatility robustness only", "Requires sd_REV and sd_CFO rolling variables."),
  sample_summary_row(final_realtime_m08, "M08 No-Lookahead Subsample", FALSE, "M08 rolling-volatility robustness only", "Requires sd_REV and sd_CFO rolling variables.")
)
write.csv(common_summary, v3_baseline_table_path("table_v3_common_sample_summary.csv"), row.names = FALSE)

# 6. WRITE PHASE 1 LOG NOTES
phase1_notes <- sprintf("=============================================================================
Phase 1 Sample Construction Notes: Two-Tier Common Sample Structure
=============================================================================
Date: 2026-06-04
Author: Antigravity

[DECISION] Two-Tier Common Sample:
- M08 requires 3-year rolling volatility (sd_REV, sd_CFO) which has low coverage (requires 3 continuous years of data)
  and restricts outcomes to 2019+.
- M10 requires operating_cycle and is now treated as secondary robustness only.
- Main model-comparison samples exclude operating_cycle, so COGS/INV availability and operating-cycle tails do not restrict M01-M07/M09.
- To prevent M08 from crippling the sample size and throwing away 2017-2018 data, we define:
  1. MAIN Common Sample (M01-M07, M09 where space-feasible) starting in 2017.
  2. SECONDARY OperatingCycle Subsample for M10 only.
  3. SECONDARY / Robustness Subsample for M08 (fit separately on rolling-volatility window).

Summary of Year 2015 Drop & Lag Source Continuity:
- Year 2015 has been dropped completely from the active sample.
- 2016 serves only as a lag source; the first outcome year is 2017.

Sample Construction Process:
1. Raw initial rows: %d
2. Removed A == 0 rows: %d rows dropped.
3. Set 0 to NA for PPE, REC, COGS.
4. Dropped year 2015: %d rows dropped.
5. Main Common Ex-Post Sample N: %d (Spans: %d - %d)
6. Main Common No-Lookahead Sample N: %d (Spans: %d - %d)
7. Secondary OperatingCycle Ex-Post Sample N: %d (Spans: %d - %d)
8. Secondary OperatingCycle No-Lookahead Sample N: %d (Spans: %d - %d)
9. M08 Ex-Post Subsample N: %d (Spans: %d - %d)
10. M08 No-Lookahead Subsample N: %d (Spans: %d - %d)

Reason for Dropping rows (from Core Ex-Post):
- denominator_invalid: rows with A_lag == 0 or A_lag is NA (mostly 2016 rows)
- data_invalid: rows with PPE == 0 or REC == 0 which were set to NA for main sample variables
- lead_missing: rows with missing CFO_lead (mostly 2024 rows in ex-post sample)
- model_variable_missing: missing sales growth
- optional_variable_missing: missing rolling SDs (retained in core, only drops for M08)
", n_raw_initial, n_drop_A_zero, n_drop_2015, 
   nrow(final_ex_post), min(final_ex_post$year), max(final_ex_post$year),
   nrow(final_realtime), min(final_realtime$year), max(final_realtime$year),
   nrow(final_operating_cycle_ex_post), min(final_operating_cycle_ex_post$year), max(final_operating_cycle_ex_post$year),
   nrow(final_operating_cycle_realtime), min(final_operating_cycle_realtime$year), max(final_operating_cycle_realtime$year),
   nrow(final_ex_post_m08), min(final_ex_post_m08$year), max(final_ex_post_m08$year),
   nrow(final_realtime_m08), min(final_realtime_m08$year), max(final_realtime_m08$year))

writeLines(phase1_notes, con = v3_baseline_log_path("v3_phase1_sample_notes.txt"))
writeLines(c(
  "Phase 1 sample design after operating-cycle separation",
  "",
  "operating_cycle is no longer used to restrict the main common sample.",
  "M10 OperatingCycle is secondary robustness only.",
  "This avoids letting COGS/INV availability or operating-cycle outliers determine the main model-comparison sample.",
  "Main model weights and M10 secondary results should not be interpreted as one unified stacking space unless all models are run on the exact same sample.",
  sprintf("Main Ex-Post Common N: %d", nrow(final_ex_post)),
  sprintf("Main No-Lookahead Common N: %d", nrow(final_realtime)),
  sprintf("Secondary OperatingCycle Ex-Post N: %d", nrow(final_operating_cycle_ex_post)),
  sprintf("Secondary OperatingCycle No-Lookahead N: %d", nrow(final_operating_cycle_realtime))
), con = v3_baseline_log_path("v3_phase1_sample_design_after_operating_cycle_separation.txt"))
message("Saved Phase 1 sample notes.")

cat("\n[SUCCESS] Phase 1 Build Common Sample (Two-Tier) completed successfully.\n")
