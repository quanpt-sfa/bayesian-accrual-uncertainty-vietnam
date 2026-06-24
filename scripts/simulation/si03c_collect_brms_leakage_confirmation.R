# Script: si03c_collect_brms_leakage_confirmation.R
# Purpose: Collect BRMS leakage confirmation simulation outputs.

source("scripts/ma00_setup.R")
phase_begin("si03c", "Collect BRMS leakage confirmation")
root <- file.path(output_root, "simulation", "brms_leakage")
manifest_path <- file.path(root, "tables", "table_si03_brms_leakage_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si03_brms_leakage_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) stop("[BLOCKER] si03c requires si03a manifest and si03b status.")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "si03c brms leakage collect")
replications <- do.call(rbind, lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] si03c missing task result: ", path)
  readRDS(path)
}))
summary <- aggregate(elpd_proxy ~ model_type, replications, mean)
names(summary)[names(summary) == "elpd_proxy"] <- "mean_elpd_proxy"
summary$n_replications <- as.integer(table(replications$model_type)[summary$model_type])
write.csv(replications, file.path(root, "tables", "table_brms_leakage_replications.csv"), row.names = FALSE)
write.csv(summary, file.path(root, "tables", "table_brms_leakage_summary.csv"), row.names = FALSE)
phase_end("si03c", "Collect BRMS leakage confirmation")
