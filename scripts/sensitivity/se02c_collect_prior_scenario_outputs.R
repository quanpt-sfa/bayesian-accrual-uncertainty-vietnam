# Script: se02c_collect_prior_scenario_outputs.R
# Purpose: Collect sensitivity prior-scenario outputs.

source("scripts/ma00_setup.R")
phase_begin("se02c", "Collect prior-scenario outputs")
tables_dir <- file.path(output_root, "sensitivity", "tables")
manifest_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) stop("[BLOCKER] se02c requires se02a manifest and se02b task status.")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "se02c sensitivity collect")
collector_contract <- data.frame(
  output = c("sensitivity_refit_plan.csv", "sensitivity_refit_fit_status.csv", "sensitivity_refit_audit_summary.csv"),
  owner = "se02c_collect_prior_scenario_outputs.R",
  evidence_role = "sensitivity_prior_scenarios",
  task_manifest_rows = nrow(manifest),
  stringsAsFactors = FALSE
)
write.csv(collector_contract, file.path(tables_dir, "table_se02_collect_contract.csv"), row.names = FALSE)
message("se02c collector owns sensitivity shared outputs.")
phase_end("se02c", "Collect prior-scenario outputs")
