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
replications <- do.call(rbind, lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] si04c missing task result: ", path)
  readRDS(path)
}))
replications$error <- replications$estimate - replications$true_value
summary <- aggregate(error ~ parameter, replications, function(x) c(mean = mean(x), rmse = sqrt(mean(x^2))))
summary <- data.frame(parameter = summary$parameter, mean_error = summary$error[, "mean"],
                      rmse = summary$error[, "rmse"], stringsAsFactors = FALSE)
write.csv(replications, file.path(root, "tables", "table_brms_parameter_recovery_replications.csv"), row.names = FALSE)
write.csv(summary, file.path(root, "tables", "table_brms_parameter_recovery_summary.csv"), row.names = FALSE)
phase_end("si04c", "Collect BRMS parameter recovery")
