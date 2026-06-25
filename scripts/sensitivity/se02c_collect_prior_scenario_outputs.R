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
metadata <- lapply(manifest$metadata_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] se02c missing task metadata: ", path)
  read.csv(path, stringsAsFactors = FALSE)
})
fit_status <- do.call(rbind, metadata)
plan <- manifest[, intersect(c("Scenario", "Model_ID", "Model_Name", "Target_Space", "Sample_Group",
                               "Heterogeneity_Variant", "Target_Sample", "brms_Formula",
                               "fit_path", "draw_path", "metadata_path"), names(manifest)), drop = FALSE]
audit_summary <- data.frame(
  scenario = fit_status$Scenario,
  model_id = fit_status$Model_ID,
  status = fit_status$status,
  warning_count = if ("warning_count" %in% names(fit_status)) fit_status$warning_count else NA_integer_,
  error_message = fit_status$reason,
  elapsed_seconds = if ("runtime_seconds" %in% names(fit_status)) fit_status$runtime_seconds else NA_real_,
  fit_path = fit_status$fit_path,
  stringsAsFactors = FALSE
)
write_csv_safely(plan, file.path(tables_dir, "sensitivity_refit_plan.csv"), row.names = FALSE)
write_csv_safely(fit_status, file.path(tables_dir, "sensitivity_refit_fit_status.csv"), row.names = FALSE)
write_csv_safely(audit_summary, file.path(tables_dir, "sensitivity_refit_audit_summary.csv"), row.names = FALSE)
message("se02c collected sensitivity task metadata and wrote shared outputs.")
phase_end("se02c", "Collect prior-scenario outputs")
