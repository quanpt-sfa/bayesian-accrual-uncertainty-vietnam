# Script: si04c_collect_brms_parameter_recovery.R
# Purpose: Collect BRMS parameter recovery simulation outputs.

source("scripts/ma00_setup.R")
phase_begin("si04c", "Collect BRMS parameter recovery")
root <- file.path(output_root, "simulation", "brms_parameter_recovery")
manifest_path <- file.path(root, "tables", "table_si04_brms_recovery_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si04_brms_recovery_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) stop("[BLOCKER] si04c requires si04a manifest and si04b status.")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "si04c brms recovery collect")
write.csv(data.frame(output = c("table_brms_parameter_recovery_replications.csv", "table_brms_parameter_recovery_summary.csv"),
                     owner = "si04c_collect_brms_parameter_recovery.R", task_manifest_rows = nrow(manifest)),
          file.path(root, "tables", "table_si04_collect_contract.csv"), row.names = FALSE)
phase_end("si04c", "Collect BRMS parameter recovery")
