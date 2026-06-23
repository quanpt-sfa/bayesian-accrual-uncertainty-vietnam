source("scripts/ma00_setup.R")

txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

run_txt <- txt("run.R")
ma07a <- txt("scripts/ma07a_fit_brms_named_models.R")
ma07b <- txt("scripts/ma07b_collect_brms_fit_outputs.R")
ma00 <- txt("scripts/ma00_setup.R")

dry <- system2("Rscript", c("run.R", "--dry-run"), stdout = TRUE, stderr = TRUE)
dry_text <- paste(dry, collapse = "\n")
pos_07a <- regexpr("ma07a scripts/ma07a_fit_brms_named_models.R", dry_text, fixed = TRUE)[1]
pos_07b <- regexpr("ma07b scripts/ma07b_collect_brms_fit_outputs.R", dry_text, fixed = TRUE)[1]
if (pos_07a < 0 || pos_07b < 0 || pos_07a > pos_07b) {
  stop("run.R --dry-run must include ma07a before ma07b.")
}

if (!grepl("ACCRUAL_ENABLE_MODEL_PARALLEL", ma07a, fixed = TRUE) &&
    !grepl("accrual_fit_worker_config", ma07a, fixed = TRUE)) {
  stop("ma07a must support ACCRUAL_ENABLE_MODEL_PARALLEL through central config.")
}
if (!grepl("validate_model_parallel_budget", ma00, fixed = TRUE) ||
    !grepl("workers \\* cores_per_fit", ma00)) {
  stop("ma00 must validate model parallel workers * cores_per_fit against total budget.")
}
if (!grepl("ACCRUAL_TOTAL_CORE_BUDGET", ma00, fixed = TRUE) ||
    !grepl("ACCRUAL_MODEL_PARALLEL_WORKERS", ma00, fixed = TRUE)) {
  stop("ma00 is missing model-parallel environment variable handling.")
}
if (grepl("split_tasks_for_workers", ma00, fixed = TRUE)) {
  stop("ma00 should not expose split_tasks_for_workers while ma07a uses parLapplyLB scheduling.")
}

blocked_shared_outputs <- c(
  "table_brms_diagnostics_winsor.csv",
  "table_coefficient_summary_winsor.csv",
  "table_ma07_fit_draw_artifact_audit.csv",
  "table_ma07_hard_gate_failures.csv",
  "baseline_manifest.csv",
  "LATEST_"
)
hits <- blocked_shared_outputs[vapply(blocked_shared_outputs, grepl, logical(1), x = ma07a, fixed = TRUE)]
if (length(hits)) {
  stop("ma07a fit worker stage must not write collector/shared outputs: ", paste(hits, collapse = ", "))
}
if (grepl("write\\.csv\\([^\\n]*table_named_model_formulas_winsor\\.csv", ma07a, perl = TRUE)) {
  stop("ma07a fit worker stage must not write the shared formula table.")
}
if (!grepl("table_ma07_fit_task_manifest.csv", ma07a, fixed = TRUE) ||
    !grepl("table_ma07_fit_task_status.csv", ma07a, fixed = TRUE)) {
  stop("ma07a parent must write task manifest and task status tables.")
}

if (!grepl("accrual_rng_metadata_list\\(rng_context, offset = i\\)", ma07a)) {
  stop("ma07a must use deterministic original row-index offsets for fit seeds.")
}
if (grepl("worker_id|cluster_id", ma07a)) {
  stop("ma07a seeds must not depend on worker identity.")
}
if (!grepl("seed\\s*=\\s*task\\$Effective_Seed", ma07a)) {
  stop("ma07a must pass the deterministic effective seed into the fit.")
}
if (!grepl("cores\\s*=\\s*task\\$cores", ma07a)) {
  stop("ma07a must pass explicit cores into the brms fit call.")
}
for (fragment in c(
  "ACCRUAL_ADOPT_LEGACY_MA07_FITS",
  "metadata_state_file",
  "legacy_metadata_missing",
  "SKIPPED_BACKFILL_EXISTING_FIT",
  "SKIPPED_ADOPTED_LEGACY_FIT",
  "BLOCKED_METADATA_MISSING",
  "metadata_status"
)) {
  if (!grepl(fragment, ma07a, fixed = TRUE)) {
    stop("ma07a missing legacy metadata handling fragment: ", fragment)
  }
}
if (!grepl("write_metadata_file\\(task\\$metadata_path, expected_meta\\)", ma07a)) {
  stop("ma07a must be able to adopt legacy ma07 fits by writing metadata without refitting.")
}
task_pool_match <- regmatches(
  ma07a,
  regexpr("accrual_run_task_pool\\([\\s\\S]*?context = \"ma07a baseline brms fit\"\\s*\\)", ma07a, perl = TRUE)
)
if (!length(task_pool_match) || !nzchar(task_pool_match)) {
  stop("ma07a must run workers through accrual_run_task_pool().")
}
for (symbol in c(
  "metadata_matches_file",
  "metadata_state_file",
  "write_metadata_file",
  "force_refit",
  "remediation_mode",
  "backfill_diagnostics_only",
  "adopt_legacy_ma07_fits"
)) {
  if (!grepl(symbol, task_pool_match, fixed = TRUE)) {
    stop("ma07a worker export_names missing worker dependency: ", symbol)
  }
}

if (grepl("\\bbrm\\s*\\(", ma07b, perl = TRUE)) {
  stop("ma07b must not contain a brm() fitting call.")
}
for (path_fragment in c(
  "table_brms_diagnostics_winsor.csv",
  "table_coefficient_summary_winsor.csv",
  "table_ma07_fit_draw_artifact_audit.csv",
  "table_ma07_hard_gate_failures.csv"
)) {
  if (!grepl(path_fragment, ma07b, fixed = TRUE)) {
    stop("ma07b must write downstream-compatible output: ", path_fragment)
  }
}
if (!grepl("posterior_epred", ma07b, fixed = TRUE) ||
    !grepl("posterior_predict", ma07b, fixed = TRUE)) {
  stop("ma07b must preserve downstream draw artifact generation.")
}

old_env <- Sys.getenv(c("ACCRUAL_ENABLE_MODEL_PARALLEL", "ACCRUAL_MODEL_PARALLEL_WORKERS", "ACCRUAL_TOTAL_CORE_BUDGET", "ACCRUAL_BASELINE_CORES"))
on.exit({
  for (nm in names(old_env)) {
    if (!nzchar(old_env[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, as.list(stats::setNames(old_env[[nm]], nm)))
  }
  Sys.unsetenv("ACCRUAL_ALLOW_NESTED_RSTAN_CORES")
}, add = TRUE)
Sys.setenv(
  ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE",
  ACCRUAL_MODEL_PARALLEL_WORKERS = "2",
  ACCRUAL_TOTAL_CORE_BUDGET = "4",
  ACCRUAL_BASELINE_CORES = "2",
  ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"
)
cfg <- accrual_model_parallel_config(cores_per_fit = 2L, context = "static test")
if (!isTRUE(cfg$enabled) || !identical(cfg$workers, 2L) || !identical(cfg$total_core_budget, 4L)) {
  stop("accrual_model_parallel_config did not parse enabled worker config as expected.")
}
Sys.setenv(ACCRUAL_MODEL_PARALLEL_WORKERS = "3", ACCRUAL_TOTAL_CORE_BUDGET = "4")
blocked <- tryCatch({
  accrual_model_parallel_config(cores_per_fit = 2L, context = "static overbudget test")
  FALSE
}, error = function(e) grepl("[BLOCKER]", conditionMessage(e), fixed = TRUE))
if (!isTRUE(blocked)) {
  stop("accrual_model_parallel_config must block workers * cores_per_fit above budget.")
}

cat("test_ma07_fit_collect_refactor_static.R passed\n")
