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
results <- do.call(rbind, lapply(manifest$diagnostic_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] di08c missing diagnostic result: ", path)
  readRDS(path)
}))
recommendations <- results[order(results$Max_Rhat), , drop = FALSE]
recommendations$recommended_rank <- seq_len(nrow(recommendations))
recommendations$production_inference <- FALSE
write_csv_safely(results, file.path(root, "tables", "table_di08_mcmc_sampler_calibration_results.csv"), row.names = FALSE)
write_csv_safely(recommendations, file.path(root, "tables", "table_di08_mcmc_sampler_calibration_recommendations.csv"), row.names = FALSE)
phase_end("di08c", "Collect MCMC sampler calibration")
