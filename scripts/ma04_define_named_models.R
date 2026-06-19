# -----------------------------------------------------------------------------
# Script: 04_define_named_models.R
# Purpose: Define formulas, stacking tiers, space memberships, and heterogeneity levels for the active models.
# Author: Antigravity
# Date: 2026-06-04
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("ma04", "Define named models")

library(dplyr)

# Paths
ensure_baseline_dirs()
registry_path <- baseline_table_path("table_model_registry.csv")
if (!file.exists(registry_path)) {
  stop("[BLOCKER] Model registry CSV not found. Please run ma01 first.")
}

registry <- read.csv(registry_path, stringsAsFactors = FALSE)
feasible_models <- registry %>% filter(Feasible_With_This_Data == "TRUE")

# 1. ADD STACKING TIERS AND SPACE MEMBERSHIP
main_ex_post_ids <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07")
main_no_lookahead_ids <- c("M01", "M02", "M03", "M07", "M09")
secondary_operating_cycle_ids <- c("M10")
secondary_volatility_ids <- c("M08")

feasible_models <- feasible_models %>%
  mutate(
    Stacking_Tier = case_when(
      Model_ID == "M08" ~ "ROBUSTNESS_M08",
      Model_ID == "M10" ~ "ROBUSTNESS_OPERATING_CYCLE",
      TRUE ~ "CORE"
    ),
    Secondary_Robustness = Model_ID %in% c(secondary_volatility_ids, secondary_operating_cycle_ids),
    Main_Stack_Inclusion = Model_ID %in% c(main_ex_post_ids, main_no_lookahead_ids),
    Sample_Group = case_when(
      Model_ID == "M08" ~ "secondary_volatility",
      Model_ID == "M10" ~ "secondary_operating_cycle",
      TRUE ~ "main_common"
    ),
    Requires_Operating_Cycle = Model_ID == "M10",
    Requires_Rolling_Volatility = Model_ID == "M08",
    Reason = case_when(
      Model_ID == "M10" ~ "Requires operating_cycle; not used to restrict main common sample",
      Model_ID == "M08" ~ "Requires rolling volatility variables; secondary robustness only",
      TRUE ~ "Primary model-comparison candidate"
    )
  )

# Add explicit space membership columns for stacking
feasible_models <- feasible_models %>%
  mutate(
    In_ExPost_Stack = Model_ID %in% c(main_ex_post_ids, secondary_volatility_ids, secondary_operating_cycle_ids),
    In_RealTime_Stack = Model_ID %in% c(main_no_lookahead_ids, secondary_operating_cycle_ids)
  )

# For each feasible model, we expand with heterogeneity variants and space-specific fittings.
# If a model belongs to 'both' spaces, it will be fit twice: once for ExPost and once for RealTime.
# We must generate rows in the formulas table indicating the exact Space, Stacking Tier, and Target Sample.

model_formulas_expanded <- data.frame(
  Model_ID = character(),
  Model_Name = character(),
  Stacking_Tier = character(),
  Target_Space = character(), # ex_post or real_time
  Target_Sample = character(), # file name of target dataset
  In_ExPost_Stack = logical(),
  In_RealTime_Stack = logical(),
  Main_Stack_Inclusion = logical(),
  Secondary_Robustness = logical(),
  Sample_Group = character(),
  Requires_Operating_Cycle = logical(),
  Requires_Rolling_Volatility = logical(),
  Reason = character(),
  Heterogeneity_Variant = character(),
  Base_Formula = character(),
  brms_Formula = character(),
  stringsAsFactors = FALSE
)

# Helper function to add formula entries
add_formula_entries <- function(m, space, sample_file) {
  base_form_str <- m$Formula
  predictors <- trimws(unlist(strsplit(base_form_str, "~")))[2]
  
  # Pooled formula
  pooled_formula <- sprintf("TA_scaled ~ %s + factor(industry) + factor(year)", predictors)
  # Firm RE formula
  firm_re_formula <- sprintf("TA_scaled ~ %s + factor(year) + (1 | company)", predictors)
  
  # Return data frame with the two heterogeneity variants
  data.frame(
    Model_ID = rep(m$Model_ID, 2),
    Model_Name = rep(m$Model_Name, 2),
    Stacking_Tier = rep(m$Stacking_Tier, 2),
    Target_Space = rep(space, 2),
    Target_Sample = rep(sample_file, 2),
    In_ExPost_Stack = rep(m$In_ExPost_Stack, 2),
    In_RealTime_Stack = rep(m$In_RealTime_Stack, 2),
    Main_Stack_Inclusion = rep(m$Main_Stack_Inclusion && m$Sample_Group == "main_common", 2),
    Secondary_Robustness = rep(m$Secondary_Robustness, 2),
    Sample_Group = rep(m$Sample_Group, 2),
    Requires_Operating_Cycle = rep(m$Requires_Operating_Cycle, 2),
    Requires_Rolling_Volatility = rep(m$Requires_Rolling_Volatility, 2),
    Reason = rep(m$Reason, 2),
    Heterogeneity_Variant = c("Pooled (Industry + Year FE)", "Firm RE (Random Intercept + Year FE)"),
    Base_Formula = rep(base_form_str, 2),
    brms_Formula = c(pooled_formula, firm_re_formula),
    stringsAsFactors = FALSE
  )
}

for (i in 1:nrow(feasible_models)) {
  m <- feasible_models[i, ]
  
  # Ex-Post Space
  if (m$In_ExPost_Stack) {
    target_sample <- dplyr::case_when(
      m$Model_ID == "M08" ~ "final_M08_ex_post_subsample.csv",
      m$Model_ID == "M10" ~ "final_secondary_operating_cycle_ex_post_sample.csv",
      TRUE ~ "final_common_ex_post_sample.csv"
    )
    model_formulas_expanded <- rbind(model_formulas_expanded, add_formula_entries(m, "ex_post", target_sample))
  }
  
  # Real-Time Space
  if (m$In_RealTime_Stack) {
    target_sample <- dplyr::case_when(
      m$Model_ID == "M10" ~ "final_secondary_operating_cycle_realtime_sample.csv",
      TRUE ~ "final_common_realtime_sample.csv"
    )
    model_formulas_expanded <- rbind(model_formulas_expanded, add_formula_entries(m, "real_time", target_sample))
  }
}

# Export expanded formulas
model_formulas_out <- baseline_table_path("table_named_model_formulas.csv")
write.csv(model_formulas_expanded, model_formulas_out, row.names = FALSE)
message("Saved expanded named-model formulas to ", model_formulas_out)

# Export updated model feasibility table
feasibility_overview <- registry %>%
  select(Model_ID, Model_Name, Feasible_With_This_Data, Infeasible_Reason, Rationale) %>%
  mutate(
    Stacking_Tier = case_when(
      Model_ID == "M08" ~ "ROBUSTNESS_M08",
      Model_ID == "M10" ~ "ROBUSTNESS_OPERATING_CYCLE",
      TRUE ~ "CORE"
    ),
    In_ExPost_Stack = Model_ID %in% c(main_ex_post_ids, secondary_volatility_ids, secondary_operating_cycle_ids),
    In_RealTime_Stack = Model_ID %in% c(main_no_lookahead_ids, secondary_operating_cycle_ids),
    Main_Stack_Inclusion = Model_ID %in% c(main_ex_post_ids, main_no_lookahead_ids),
    Secondary_Robustness = Model_ID %in% c("M08", "M10"),
    Sample_Group = case_when(
      Model_ID == "M08" ~ "secondary_volatility",
      Model_ID == "M10" ~ "secondary_operating_cycle",
      TRUE ~ "main_common"
    )
  )
write.csv(feasibility_overview, baseline_table_path("table_model_feasibility.csv"), row.names = FALSE)

# Export required variables
model_req_vars <- registry %>%
  select(Model_ID, Model_Name, Required_Variables, Uses_CFO_lead) %>%
  mutate(
    In_ExPost_Stack = Model_ID %in% c(main_ex_post_ids, secondary_volatility_ids, secondary_operating_cycle_ids),
    In_RealTime_Stack = Model_ID %in% c(main_no_lookahead_ids, secondary_operating_cycle_ids),
    Main_Stack_Inclusion = Model_ID %in% c(main_ex_post_ids, main_no_lookahead_ids),
    Secondary_Robustness = Model_ID %in% c("M08", "M10")
  )
write.csv(model_req_vars, baseline_table_path("table_model_required_variables.csv"), row.names = FALSE)

# 2. WRITE ma04 LOG NOTES
phase2_notes <- "=============================================================================
ma04 Model Space definition: Space Membership & Two-Tier Stacking
=============================================================================
Date: 2026-06-04
Author: Antigravity

Space Membership & Sample Routing:
1. ex_post_measurement_space main stack (Target Sample: final_common_ex_post_sample.csv):
   - Feasible CORE models: M01, M02, M03, M04, M05, M06, M07.
   - M10 is excluded from the main stack because it requires operating_cycle.
   - Robustness Model: M08 (secondary_volatility on final_M08_ex_post_subsample.csv).
   - Robustness Model: M10 (secondary_operating_cycle on final_secondary_operating_cycle_ex_post_sample.csv).
   - M09 is EXCLUDED (not CFO_lead-capable by design).
   
2. real_time_prediction_space main stack (Target Sample: final_common_realtime_sample.csv):
   - Feasible CORE models: M01, M02, M03, M07, M09.
   - M10 is excluded from the main stack because it requires operating_cycle.
   - M04, M05, M06 are EXCLUDED (requires CFO_lead look-ahead).
   - M08 is EXCLUDED (restricted to ex-post space robustness only).
   - M10 is available only as secondary_operating_cycle robustness on final_secondary_operating_cycle_realtime_sample.csv.

Double Fitting:
- Models marked 'both' (M01, M02, M03, M07) are listed twice for main_common when space-feasible.
- M10 may also be listed in both spaces, but only with Sample_Group='secondary_operating_cycle' and Main_Stack_Inclusion=FALSE.
- Main model weights and secondary M10 results must not be read as one unified stacking space unless all models are rerun on the same secondary sample.
"

writeLines(phase2_notes, con = baseline_log_path("phase2_model_space_notes.txt"))
message("Saved ma04 model space notes.")

cat("\n[SUCCESS] ma04 define named-model space (with explicit Stacking Spaces) completed successfully.\n")
phase_end("ma04", "Define named models")
