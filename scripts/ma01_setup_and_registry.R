# -----------------------------------------------------------------------------
# Script: 01_setup_and_registry.R
# Purpose: Create the accrual uncertainty pipeline structure and model registry.
# Author: Antigravity
# Date: 2026-06-04
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("ma01", "Setup and model registry")

ensure_baseline_dirs()

# 2. Define the Model Registry data frame
registry_data <- data.frame(
  Model_ID = c(
    "M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10"
  ),
  Model_Name = c(
    "Jones", 
    "ModJones", 
    "PerfModJones", 
    "DD_TotalAccruals", 
    "McNichols", 
    "McNichols_Perf", 
    "BallShivakumar_Style", 
    "Extended_Perf_Vol", 
    "RealTime_NoLead", 
    "OperatingCycle"
  ),
  Literature_Family = c(
    "Jones (1991)", 
    "Dechow et al. (1995)", 
    "Kothari et al. (2005)", 
    "Dechow & Dichev (2002) / Breuer-Schutt (2023)", 
    "McNichols (2002)", 
    "McNichols (2002) / Kothari et al. (2005)", 
    "Ball & Shivakumar (2005)", 
    "Extended Performance & Volatility", 
    "Real-time No-lead", 
    "Operating-cycle Extended"
  ),
  Dependent_Variable = rep("TA_scaled", 10),
  Formula = c(
    "TA_scaled ~ inv_A_lag + dREV_scaled + PPE_scaled",
    "TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled",
    "TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag",
    "TA_scaled ~ CFO_lag_scaled + CFO_curr_scaled + CFO_lead_scaled",
    "TA_scaled ~ CFO_lag_scaled + CFO_curr_scaled + CFO_lead_scaled + dREV_scaled + PPE_scaled",
    "TA_scaled ~ CFO_lag_scaled + CFO_curr_scaled + CFO_lead_scaled + dREV_scaled + PPE_scaled + ROA_lag",
    "TA_scaled ~ CFO_curr_scaled + NEG_CFO + CFO_curr_scaled:NEG_CFO + dREV_scaled + PPE_scaled",
    "TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + Size + sd_REV + sd_CFO",
    "TA_scaled ~ CFO_lag_scaled + CFO_curr_scaled + dREV_scaled + PPE_scaled + ROA_lag + Size",
    "TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + operating_cycle + sales_growth"
  ),
  Required_Variables = c(
    "inv_A_lag, dREV_scaled, PPE_scaled",
    "inv_A_lag, dREV_dREC_scaled, PPE_scaled",
    "inv_A_lag, dREV_dREC_scaled, PPE_scaled, ROA_lag",
    "CFO_lag_scaled, CFO_curr_scaled, CFO_lead_scaled",
    "CFO_lag_scaled, CFO_curr_scaled, CFO_lead_scaled, dREV_scaled, PPE_scaled",
    "CFO_lag_scaled, CFO_curr_scaled, CFO_lead_scaled, dREV_scaled, PPE_scaled, ROA_lag",
    "CFO_curr_scaled, NEG_CFO, dREV_scaled, PPE_scaled",
    "inv_A_lag, dREV_dREC_scaled, PPE_scaled, ROA_lag, Size, sd_REV, sd_CFO",
    "CFO_lag_scaled, CFO_curr_scaled, dREV_scaled, PPE_scaled, ROA_lag, Size",
    "inv_A_lag, dREV_dREC_scaled, PPE_scaled, ROA_lag, operating_cycle, sales_growth"
  ),
  Uses_CFO_lead = c(
    "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE"
  ),
  Lookahead_Status = c(
    "No-lookahead", "No-lookahead", "No-lookahead", "Lookahead", "Lookahead", "Lookahead", "No-lookahead", "No-lookahead", "No-lookahead", "No-lookahead"
  ),
  Heterogeneity_Level = rep("Industry FE / Firm RE", 10),
  Intended_Space = c(
    "both", "both", "both", "ex_post_measurement_space", "ex_post_measurement_space", "ex_post_measurement_space", "both", "both", "real_time_prediction_space", "both"
  ),
  Feasible_With_This_Data = c(
    "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE"
  ),
  Infeasible_Reason = c(
    "", "", "", "", "", "", "", 
    "", # Note: M08 has leverage omitted to make it feasible
    "", ""
  ),
  Rationale = c(
    "Baseline Jones model separating normal/abnormal accruals via changes in revenue and PPE.",
    "Modified Jones model incorporating changes in receivables to reduce measurement error in credit sales.",
    "Performance-matched modified Jones model controlling for historical operating performance (ROA).",
    "Pure CFO mapping based on Dechow & Dichev (2002) total-accruals specification to capture cash flow matching.",
    "Integrated Dechow-Dichev and Jones framework mapping lagged, current, and lead cash flows along with Jones growth/PPE predictors.",
    "Integrated McNichols model controlling for historical performance (ROA).",
    "Incorporates asymmetric loss recognition by interacting current cash flows with negative cash flow indicator.",
    "Extended controls for size and volatility. Omitted Leverage to remain feasible.",
    "Real-time McNichols variant omitting CFO_lead to avoid look-ahead bias.",
    "Jones/McNichols performance-matched variant extended with a reduced proxy for operating cycle."
  ),
  Notes = c(
    "Fully feasible.",
    "Fully feasible.",
    "Fully feasible.",
    "Requires CFO_lead_scaled (only for ex_post space).",
    "Requires CFO_lead_scaled (only for ex_post space).",
    "Requires CFO_lead_scaled (only for ex_post space).",
    "Fully feasible.",
    "Modified version: Leverage is omitted to satisfy feasibility. Marked feasible with this modification.",
    "Designed specifically for real_time_prediction_space.",
    "Modified version: operating_cycle constructed as a reduced proxy omitting payables. Marked feasible."
  ),
  stringsAsFactors = FALSE
)

registry_data$Secondary_Robustness <- registry_data$Model_ID %in% c("M08", "M10")
registry_data$Main_Stack_Inclusion <- registry_data$Model_ID %in% c(
  "M01", "M02", "M03", "M04", "M05", "M06", "M07", "M09"
)
registry_data$Sample_Group <- ifelse(
  registry_data$Model_ID == "M08",
  "secondary_volatility",
  ifelse(registry_data$Model_ID == "M10", "secondary_operating_cycle", "main_common")
)
registry_data$Requires_Operating_Cycle <- registry_data$Model_ID == "M10"
registry_data$Requires_Rolling_Volatility <- registry_data$Model_ID == "M08"
registry_data$Main_Stack_Reason <- ifelse(
  registry_data$Model_ID == "M10",
  "Requires operating_cycle; not used to restrict main common sample.",
  ifelse(
    registry_data$Model_ID == "M08",
    "Requires rolling volatility variables; secondary robustness only.",
    ifelse(registry_data$Main_Stack_Inclusion, "Primary model-comparison candidate.", "Not included in feasible main stack.")
  )
)

# Save the registry to CSV
registry_out <- baseline_table_path("table_model_registry.csv")
write.csv(
  registry_data, 
  file = registry_out,
  row.names = FALSE
)
message("Saved model registry to: ", registry_out)

main_secondary_design <- data.frame(
  Model_ID = registry_data$Model_ID,
  Model_Name = registry_data$Model_Name,
  Family = registry_data$Literature_Family,
  Main_Stack_Inclusion = registry_data$Main_Stack_Inclusion,
  Secondary_Robustness = registry_data$Secondary_Robustness,
  Sample_Group = registry_data$Sample_Group,
  Requires_Operating_Cycle = registry_data$Requires_Operating_Cycle,
  Requires_Rolling_Volatility = registry_data$Requires_Rolling_Volatility,
  Reason = registry_data$Main_Stack_Reason,
  stringsAsFactors = FALSE
)
write.csv(
  main_secondary_design,
  file = baseline_table_path("table_main_vs_secondary_model_design.csv"),
  row.names = FALSE
)

method_note <- paste(
  "The main model-comparison sample excludes operating_cycle because this variable is required only by the operating-cycle extension.",
  "M10 is therefore treated as a secondary robustness specification on the operating-cycle-available subsample.",
  "This prevents COGS/INV availability and the heavy upper tail of INV/COGS from determining the common sample used to compare the primary Jones-family, cash-flow mapping, and asymmetric accrual models."
)
writeLines(method_note, con = baseline_log_path("method_note_operating_cycle_secondary_design.txt"))

# 3. Create the registry notes explaining the design choices
notes_content <- "=============================================================================
ma01 Registry Notes: Model Registry and Design Choices for accrual uncertainty pipeline
=============================================================================
Date: 2026-06-04
Author: Antigravity

(a) Why the unified accrual pipeline abandons separate TA/TCA BMA:
- Under the BMA setup in v1/v2, running separate models for Total Accruals (TA) and Working
  Capital Accruals (TCA) created inconsistent model spaces and disjoint predictive uncertainty
  estimates.
- Breuer-Schutt (2023) framework focuses on the posterior predictive distribution of normal
  accruals and the corresponding model uncertainty to scale abnormal accruals (DA_z_stacked).
  To obtain a single coherent uncertainty estimate and avoid mixing different dependent variables,
  all models in the unified model space must predict the EXACT same dependent variable: total accruals,
  TA_scaled.
- Dechow-Dichev (2002) logic (mapping current, lagged, and future CFO) is elegantly absorbed 
  directly into total-accrual models (e.g. McNichols 2002 specification), ensuring we keep the
  same dependent variable (TA_scaled) while preserving the cash-flow matching predictors.

(b) Feasibility Analysis of Candidate Models with data.xlsx:
- Active ten-model space (M01 - M10):
  - M01_Jones, M02_ModJones, M03_PerfModJones: Feasible using A, REV, REC, PPE, ROA.
  - M04_DD_TotalAccruals, M05_McNichols, M06_McNichols_Perf: Feasible using CFO (and CFO lags/leads),
    dREV, PPE, and ROA. CFO_lead restricts them to the ex_post_measurement_space.
  - M07_BallShivakumar_Style: Feasible using CFO and NEG_CFO interaction.
  - M08_Extended_Perf_Vol: Omitted Leverage because no debt data exists. Marked feasible under this
    reduced specification.
  - M09_RealTime_NoLead: Feasible using no-lead predictors, designed for real-time prediction.
  - M10_OperatingCycle: Feasible using a reduced proxy for operating_cycle (REC/REV + INV/COGS) due to
    the lack of payables/purchases. Marked feasible under this reduced specification.

(c) Conformity to Global Rules:
- Checked against R2 and R3. No book-to-market, Leverage, firm age, or returns are used.
- All candidate models use TA_scaled as the dependent variable.
"

registry_notes_out <- baseline_log_path("phase0_registry_notes.txt")
writeLines(notes_content, con = registry_notes_out)
message("Saved registry notes to: ", registry_notes_out)

cat("\n[SUCCESS] ma01 setup and registry completed successfully.\n")
phase_end("ma01", "Setup and model registry")
