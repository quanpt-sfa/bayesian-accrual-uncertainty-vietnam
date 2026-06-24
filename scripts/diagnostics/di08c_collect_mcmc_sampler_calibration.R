# Script: di08c_collect_mcmc_sampler_calibration.R
# Purpose: Collect diagnostic MCMC sampler calibration outputs.

source("scripts/ma00_setup.R")
phase_begin("di08c", "Collect MCMC sampler calibration")
root <- file.path(output_root, "diagnostics", "mcmc_sampler_calibration")
manifest_path <- file.path(root, "tables", "table_di08_sampler_calibration_task_manifest.csv")
status_path <- file.path(root, "tables", "table_di08_sampler_calibration_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) stop("[BLOCKER] di08c requires di08a manifest and di08b status.")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "di08c calibration collect")
write.csv(data.frame(output = c("table_di08_mcmc_sampler_calibration_results.csv", "table_di08_mcmc_sampler_calibration_recommendations.csv"),
                     owner = "di08c_collect_mcmc_sampler_calibration.R", task_manifest_rows = nrow(manifest),
                     production_inference = FALSE),
          file.path(root, "tables", "table_di08_collect_contract.csv"), row.names = FALSE)
phase_end("di08c", "Collect MCMC sampler calibration")
