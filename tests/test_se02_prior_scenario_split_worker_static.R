# Static contract for SE02 prior-scenario split-worker pipeline.

paths <- c(
  se02a = "scripts/sensitivity/se02a_plan_prior_scenario_refits.R",
  se02b = "scripts/sensitivity/se02b_fit_prior_scenario_workers.R",
  se02c = "scripts/sensitivity/se02c_collect_prior_scenario_outputs.R"
)
for (path in paths) {
  if (!file.exists(path)) stop("Missing SE02 split-worker script: ", path)
}

txt <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
se02a <- txt(paths[["se02a"]])
se02b <- txt(paths[["se02b"]])
se02c <- txt(paths[["se02c"]])

for (fragment in c(
  "selected_sensitivity_scenarios()",
  "table_named_model_formulas_winsor.csv",
  "main_model_ids_for_space",
  "Prior_Set_ID",
  "Likelihood_Family",
  "Model_Structure",
  "table_se02_prior_scenario_refit_task_manifest.csv",
  "fit_path",
  "draw_path",
  "metadata_path",
  "task_log_path",
  "RNG_Context",
  "RNG_Offset",
  "Canonical_Seed",
  "Effective_Seed",
  "RNG_Source",
  "Required = TRUE",
  "ACCRUAL_SENS_SCENARIO=ALL"
)) {
  if (!grepl(fragment, se02a, fixed = TRUE)) stop("SE02A missing manifest/scenario contract fragment: ", fragment)
}

for (fragment in c(
  "task$Prior_Set_ID",
  "task$Likelihood_Family",
  "task$Model_Structure",
  "default_prior_list(",
  "prior_set_id = task$Prior_Set_ID",
  "family = task$Likelihood_Family",
  "brms_family(task$Likelihood_Family)",
  "brms::posterior_epred(fit)",
  "brms::posterior_predict(fit)",
  "save_pars = brms::save_pars(all = TRUE)",
  "accrual_fit_worker_config(\"sensitivity\", max(as.integer(tasks$cores), na.rm = TRUE), \"se02b prior-scenario workers\")",
  "accrual_run_task_pool",
  "write_task_status(status_path, status)",
  "accrual_task_status_blocker(status, required_col = \"Required\", context = \"se02b prior-scenario workers\")",
  "ACCRUAL_FORCE_REFIT=TRUE"
)) {
  if (!grepl(fragment, se02b, fixed = TRUE)) stop("SE02B missing task-specific worker contract fragment: ", fragment)
}

for (bad in c(
  "prior_set_id = prior_set_id",
  "family = likelihood_family",
  "brms_family()",
  "sensitivity_refit_plan.csv"
)) {
  if (grepl(bad, se02b, fixed = TRUE)) stop("SE02B must not contain unsafe/shared-output fragment: ", bad)
}

for (fragment in c(
  "accrual_task_status_blocker(status, required_col = \"Required\", context = \"se02c sensitivity collect\")",
  "sensitivity_root(scenario)",
  "file.path(scenario_root, \"fits\", paste0(\"fit_\", model_key, \".rds\"))",
  "file.path(scenario_root, \"draws\", paste0(\"draws_\", model_key, \".rds\"))",
  "file.copy(from, to, overwrite = TRUE)",
  "sensitivity_refit_plan.csv",
  "sensitivity_refit_fit_status.csv",
  "sensitivity_refit_audit_summary.csv",
  "task_fit_path",
  "task_draw_path",
  "task_metadata_path",
  "prior_set_id",
  "family",
  "model_structure"
)) {
  if (!grepl(fragment, se02c, fixed = TRUE)) stop("SE02C missing collector/canonical contract fragment: ", fragment)
}

source("scripts/ma00_setup.R")
scenarios <- sensitivity_scenarios()
expected <- data.frame(
  Scenario = c("baseline", "tight", "wide"),
  Prior_Set_ID = c(
    "scale_aware_student_baseline_v1",
    "scale_aware_student_tight_v1",
    "scale_aware_student_wide_v1"
  ),
  Likelihood_Family = "student",
  Model_Structure = "pooled_random_intercept",
  stringsAsFactors = FALSE
)
merged <- merge(expected, scenarios, by = c("Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure"), all.x = TRUE)
if (nrow(merged) != nrow(expected)) {
  stop("sensitivity_scenarios() does not expose the expected baseline/tight/wide scenario-prior mapping.")
}

cat("test_se02_prior_scenario_split_worker_static.R passed\n")
