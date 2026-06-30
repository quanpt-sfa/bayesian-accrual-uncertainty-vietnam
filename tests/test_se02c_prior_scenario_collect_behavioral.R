# Lightweight behavioral smoke test for SE02C collector-only materialization.

root <- normalizePath(file.path(tempdir(), paste0("se02c_smoke_", Sys.getpid())), winslash = "/", mustWork = FALSE)
tables_dir <- file.path(root, "sensitivity", "tables")
task_root <- file.path(root, "sensitivity", "task_artifacts")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(task_root, recursive = TRUE, showWarnings = FALSE)

task_key <- "se02_baseline_M01_ex_post_pooled"
task_fit_path <- file.path(task_root, paste0(task_key, "_fit.rds"))
task_draw_path <- file.path(task_root, paste0(task_key, "_draws.rds"))
task_metadata_path <- file.path(task_root, paste0(task_key, "_metadata.csv"))
dir.create(dirname(task_fit_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(fake = "fit"), task_fit_path)
saveRDS(list(epred = matrix(1, 1, 1), predict = matrix(1, 1, 1)), task_draw_path)

manifest <- data.frame(
  Task_Key = task_key,
  Scenario = "baseline",
  Prior_Set_ID = "scale_aware_student_baseline_v1",
  Likelihood_Family = "student",
  Model_Structure = "pooled_random_intercept",
  Model_ID = "M01",
  Model_Name = "Modified Jones",
  Target_Space = "ex_post",
  Sample_Group = "main_common",
  Heterogeneity_Variant = "Pooled (Industry + Year FE)",
  Target_Sample = "final_common_ex_post_winsor.csv",
  brms_Formula = "TA_scaled ~ dREV_scaled + PPE_scaled",
  fit_path = task_fit_path,
  draw_path = task_draw_path,
  metadata_path = task_metadata_path,
  task_log_path = file.path(task_root, "logs", paste0(task_key, ".log")),
  chains = 4L,
  cores = 4L,
  iter = 3000L,
  warmup = 1000L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 500L,
  backend = "rstan",
  RNG_Context = "test_rng",
  RNG_Offset = 1L,
  Canonical_Seed = 20260629L,
  Effective_Seed = 20260630L,
  RNG_Source = "test",
  Required = TRUE,
  stringsAsFactors = FALSE
)

metadata <- data.frame(
  Scenario = manifest$Scenario,
  Prior_Set_ID = manifest$Prior_Set_ID,
  Likelihood_Family = manifest$Likelihood_Family,
  Model_Structure = manifest$Model_Structure,
  Model_ID = manifest$Model_ID,
  Model_Name = manifest$Model_Name,
  Target_Space = manifest$Target_Space,
  Sample_Group = manifest$Sample_Group,
  Heterogeneity_Variant = manifest$Heterogeneity_Variant,
  Target_Sample = manifest$Target_Sample,
  brms_Formula = manifest$brms_Formula,
  chains = manifest$chains,
  cores = manifest$cores,
  iter = manifest$iter,
  warmup = manifest$warmup,
  adapt_delta = manifest$adapt_delta,
  max_treedepth = manifest$max_treedepth,
  refresh = manifest$refresh,
  backend = manifest$backend,
  RNG_Context = manifest$RNG_Context,
  RNG_Offset = manifest$RNG_Offset,
  Canonical_Seed = manifest$Canonical_Seed,
  Effective_Seed = manifest$Effective_Seed,
  RNG_Source = manifest$RNG_Source,
  status = "SUCCESS",
  reason = NA_character_,
  warning_count = 0L,
  runtime_seconds = 0.1,
  fit_path = manifest$fit_path,
  draw_path = manifest$draw_path,
  metadata_path = manifest$metadata_path,
  stringsAsFactors = FALSE
)

status <- data.frame(
  Task_Key = task_key,
  status = "SUCCESS",
  reason = NA_character_,
  Required = TRUE,
  fit_path = task_fit_path,
  draw_path = task_draw_path,
  metadata_path = task_metadata_path,
  stringsAsFactors = FALSE
)

write.csv(manifest, file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv"), row.names = FALSE)
write.csv(status, file.path(tables_dir, "table_se02_prior_scenario_refit_task_status.csv"), row.names = FALSE)
write.csv(metadata, task_metadata_path, row.names = FALSE)

old_root <- Sys.getenv("ACCRUAL_OUTPUT_ROOT", unset = NA_character_)
old_input <- Sys.getenv("ACCRUAL_INPUT_WINSOR_ROOT", unset = NA_character_)
old_phase <- Sys.getenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG", unset = NA_character_)
on.exit({
  if (is.na(old_root)) Sys.unsetenv("ACCRUAL_OUTPUT_ROOT") else Sys.setenv(ACCRUAL_OUTPUT_ROOT = old_root)
  if (is.na(old_input)) Sys.unsetenv("ACCRUAL_INPUT_WINSOR_ROOT") else Sys.setenv(ACCRUAL_INPUT_WINSOR_ROOT = old_input)
  if (is.na(old_phase)) Sys.unsetenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG") else Sys.setenv(ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = old_phase)
}, add = TRUE)
Sys.setenv(
  ACCRUAL_OUTPUT_ROOT = root,
  ACCRUAL_INPUT_WINSOR_ROOT = root,
  ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = "TRUE"
)

source("scripts/sensitivity/se02c_collect_prior_scenario_outputs.R")

plan_path <- file.path(tables_dir, "sensitivity_refit_plan.csv")
fit_status_path <- file.path(tables_dir, "sensitivity_refit_fit_status.csv")
audit_path <- file.path(tables_dir, "sensitivity_refit_audit_summary.csv")
for (path in c(plan_path, fit_status_path, audit_path)) {
  if (!file.exists(path)) stop("SE02C smoke missing expected output: ", path)
}

plan <- read.csv(plan_path, stringsAsFactors = FALSE)
required_plan_cols <- c(
  "scenario", "prior_set_id", "family", "model_structure", "model_id",
  "model_name", "target_space", "sample_group", "heterogeneity_variant",
  "target_sample", "brms_formula", "fit_path", "draw_path", "metadata_path",
  "task_fit_path", "task_draw_path", "task_metadata_path", "status"
)
missing <- setdiff(required_plan_cols, names(plan))
if (length(missing)) stop("SE02C smoke plan missing columns: ", paste(missing, collapse = ", "))
if (!identical(plan$prior_set_id[1], "scale_aware_student_baseline_v1") ||
    !identical(plan$family[1], "student") ||
    !identical(plan$model_structure[1], "pooled_random_intercept")) {
  stop("SE02C smoke plan lost scenario-specific prior/family/model-structure metadata.")
}
if (identical(normalizePath(plan$fit_path[1], winslash = "/", mustWork = FALSE),
              normalizePath(task_fit_path, winslash = "/", mustWork = FALSE))) {
  stop("SE02C smoke plan must point fit_path to canonical scenario path, not task-local path.")
}
if (!file.exists(plan$fit_path[1]) || !file.exists(plan$draw_path[1]) || !file.exists(plan$metadata_path[1])) {
  stop("SE02C smoke failed to copy task-local artifacts to canonical scenario paths.")
}
if (!grepl("/sensitivity/baseline/fits/", normalizePath(plan$fit_path[1], winslash = "/", mustWork = FALSE), fixed = TRUE)) {
  stop("SE02C smoke canonical fit path is not under sensitivity_root(scenario)/fits.")
}
if (!grepl("/sensitivity/baseline/draws/", normalizePath(plan$draw_path[1], winslash = "/", mustWork = FALSE), fixed = TRUE)) {
  stop("SE02C smoke canonical draw path is not under sensitivity_root(scenario)/draws.")
}

cat("test_se02c_prior_scenario_collect_behavioral.R passed\n")
