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
required_failed <- merge(
  manifest[, c("Task_Key", "Required"), drop = FALSE],
  status[, intersect(c("Task_Key", "status", "reason"), names(status)), drop = FALSE],
  by = "Task_Key",
  all.x = TRUE
)
required_failed <- required_failed[
  required_failed$Required %in% c(TRUE, "TRUE", "true", "1", 1L) &
    (!(required_failed$status %in% c("SUCCESS")) | is.na(required_failed$status)),
  ,
  drop = FALSE
]
if (nrow(required_failed)) {
  stop("[BLOCKER] si04c required task failures remain: ", paste(required_failed$Task_Key, collapse = ", "))
}
replications <- do.call(rbind, lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] si04c missing task result: ", path)
  readRDS(path)
}))
replications$error <- replications$estimate - replications$true_value
summary <- aggregate(error ~ T + sigma_firm + parameter, replications, function(x) c(mean = mean(x), rmse = sqrt(mean(x^2))))
summary <- data.frame(
  T = summary$T,
  sigma_firm = summary$sigma_firm,
  parameter = summary$parameter,
  mean_error = summary$error[, "mean"],
  rmse = summary$error[, "rmse"],
  stringsAsFactors = FALSE
)
task_diag <- unique(replications[, c(
  "T", "sigma_firm", "Replication", "max_rhat", "min_ess_bulk", "min_ess_tail",
  "total_divergent", "max_treedepth_hits"
), drop = FALSE])
diagnostic_summary <- do.call(rbind, lapply(split(task_diag, paste(task_diag$T, task_diag$sigma_firm, sep = "|")), function(x) {
  data.frame(
    T = x$T[1],
    sigma_firm = x$sigma_firm[1],
    n_replications = length(unique(x$Replication)),
    max_rhat_max = suppressWarnings(max(x$max_rhat, na.rm = TRUE)),
    min_ess_bulk_min = suppressWarnings(min(x$min_ess_bulk, na.rm = TRUE)),
    min_ess_tail_min = suppressWarnings(min(x$min_ess_tail, na.rm = TRUE)),
    total_divergent = sum(x$total_divergent, na.rm = TRUE),
    max_treedepth_hits = sum(x$max_treedepth_hits, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
diagnostic_summary$max_rhat_max[!is.finite(diagnostic_summary$max_rhat_max)] <- NA_real_
diagnostic_summary$min_ess_bulk_min[!is.finite(diagnostic_summary$min_ess_bulk_min)] <- NA_real_
diagnostic_summary$min_ess_tail_min[!is.finite(diagnostic_summary$min_ess_tail_min)] <- NA_real_
has_key_cell <- any(diagnostic_summary$T == 15L & abs(diagnostic_summary$sigma_firm - 0.3) < 1e-8)
if (!has_key_cell) {
  stop("[BLOCKER] si04c diagnostic summary lacks required cell T=15, sigma_firm=0.3.")
}
write.csv(replications, file.path(root, "tables", "table_brms_parameter_recovery_replications.csv"), row.names = FALSE)
write.csv(summary, file.path(root, "tables", "table_brms_parameter_recovery_summary.csv"), row.names = FALSE)
write.csv(diagnostic_summary, file.path(root, "tables", "table_brms_parameter_recovery_diagnostic_summary.csv"), row.names = FALSE)
phase_end("si04c", "Collect BRMS parameter recovery")
