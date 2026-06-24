# -----------------------------------------------------------------------------
# Script: ma07c_collect_brms_fit_outputs.R
# Purpose: Final ma07 collector. Bind ma07b task-local extraction artifacts and
#          write downstream-compatible shared diagnostics, draws, manifests,
#          audit, notes, and coefficient tables.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma07c", "Collect extracted baseline brms fit outputs")
ensure_analysis_dirs()
validate_final_analysis_config("ma07c baseline fit output collection", final_mode = TRUE)

run_varying_slope_models <- identical(model_structure, "breuer_varying_slopes")
phase_root <- if (run_varying_slope_models) varyslopes_root else output_root
for (d in file.path(phase_root, c("", "tables", "draws", "logs", "manifests"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

fit_manifest_path <- file.path(phase_root, "tables", "table_ma07_fit_task_manifest.csv")
fit_status_path <- file.path(phase_root, "tables", "table_ma07_fit_task_status.csv")
collect_manifest_path <- file.path(phase_root, "tables", "table_ma07_collect_task_manifest.csv")
collect_status_path <- file.path(phase_root, "tables", "table_ma07_collect_task_status.csv")
for (path in c(fit_manifest_path, fit_status_path, collect_manifest_path, collect_status_path)) {
  if (!file.exists(path)) stop("[BLOCKER] Missing ma07 collection input: ", path)
}

fit_manifest <- read.csv(fit_manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
fit_status <- read.csv(fit_status_path, stringsAsFactors = FALSE, check.names = FALSE)
collect_manifest <- read.csv(collect_manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
collect_status <- read.csv(collect_status_path, stringsAsFactors = FALSE, check.names = FALSE)
accrual_task_status_blocker(fit_status, required_col = "Main_Stack_Inclusion", context = "ma07c fit status collection")
accrual_task_status_blocker(collect_status, required_col = "Required", context = "ma07c extracted output collection")

truthy <- function(x) {
  x %in% c(TRUE, "TRUE", "true", "True", "1", 1L)
}

empty_artifact_audit <- function() {
  data.frame(
    Model_ID = character(), Model_Name = character(), Target_Space = character(), Sample_Group = character(),
    Heterogeneity_Variant = character(), diagnostic_key = character(), Main_Stack_Inclusion = logical(),
    Secondary_Robustness = logical(), fit_path = character(), draws_path = character(),
    fit_exists_before = logical(), draws_exists_before = logical(), fit_exists_after = logical(),
    draws_exists_after = logical(), Fit_Status = character(), max_rhat = numeric(), divergences = numeric(),
    treedepth_warnings = numeric(), min_bulk_ess = numeric(), min_tail_ess = numeric(), converged = logical(),
    stacking_eligible = logical(), draw_generation_attempted = logical(), draw_generation_status = character(),
    draw_generation_skip_reason = character(), hard_gate_status = character(), hard_gate_reason = character(),
    recommended_remediation_key = character(), timestamp = character(), stringsAsFactors = FALSE
  )
}

empty_hard_gate_failures <- function() {
  data.frame(
    diagnostic_key = character(), Model_ID = character(), Model_Name = character(),
    Target_Space = character(), Sample_Group = character(), Heterogeneity_Variant = character(),
    Main_Stack_Inclusion = logical(), hard_gate_status = character(), hard_gate_reason = character(),
    max_rhat = numeric(), divergences = numeric(), min_bulk_ess = numeric(), min_tail_ess = numeric(),
    fit_path = character(), draws_path = character(), fit_exists_after = logical(),
    draws_exists_after = logical(), recommended_remediation_key = character(), timestamp = character(),
    stringsAsFactors = FALSE
  )
}

write_csv_utf8 <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

formulas_path <- file.path(phase_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) stop("[BLOCKER] Missing formula table for ma07c collection: ", formulas_path)
formula_out_cols <- intersect(
  c("Model_ID", "Model_Name", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
    "Target_Sample", "brms_Formula", "Main_Stack_Inclusion", "Secondary_Robustness", "Reason"),
  names(fit_manifest)
)
write_csv_utf8(fit_manifest[, formula_out_cols, drop = FALSE], file.path(phase_root, "tables", "table_named_model_formulas_winsor.csv"))
if (run_varying_slope_models) {
  write_csv_utf8(fit_manifest[, formula_out_cols, drop = FALSE], file.path(phase_root, "tables", "table_varyslopes_model_registry.csv"))
}

diag_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_varyslopes_diagnostics.csv")
} else {
  file.path(phase_root, "tables", "table_brms_diagnostics_winsor.csv")
}
coeff_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_varyslopes_coefficient_summary.csv")
} else {
  file.path(phase_root, "tables", "table_coefficient_summary_winsor.csv")
}
ma07_artifact_audit_path <- file.path(phase_root, "tables", "table_ma07_fit_draw_artifact_audit.csv")
ma07_hard_gate_failures_path <- file.path(phase_root, "tables", "table_ma07_hard_gate_failures.csv")
ma07_remediation_helper_path <- file.path(phase_root, "logs", "ma07_suggested_remediation_targets.ps1")

successful_status <- collect_status[collect_status$status == "SUCCESS", , drop = FALSE]
missing_bundle <- successful_status$bundle_path[!file.exists(successful_status$bundle_path)]
if (length(missing_bundle)) {
  stop("[BLOCKER] ma07c missing ma07b task-local bundle(s): ", paste(missing_bundle, collapse = "; "))
}

bundles <- lapply(successful_status$bundle_path, readRDS)

copy_draw_artifact <- function(bundle) {
  draw_task_path <- bundle$draw_task_path
  final_draw_path <- bundle$final_draw_path
  if (length(draw_task_path) && !is.na(draw_task_path) && nzchar(draw_task_path) && file.exists(draw_task_path)) {
    dir.create(dirname(final_draw_path), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(draw_task_path, final_draw_path, overwrite = TRUE)) {
      stop("[BLOCKER] ma07c could not publish task-local draw artifact to shared draws path: ", final_draw_path)
    }
  }
  invisible(TRUE)
}
invisible(lapply(bundles, copy_draw_artifact))

diagnostics_rows <- lapply(bundles, `[[`, "diagnostics")
audit_rows <- lapply(bundles, `[[`, "audit")
failure_rows <- lapply(bundles, `[[`, "failures")
coeff_rows <- lapply(bundles, `[[`, "coefficients")

diagnostics_df <- bind_rows(diagnostics_rows)
if (!nrow(diagnostics_df)) stop("[BLOCKER] ma07c found no diagnostics rows in ma07b task-local bundles.")
diagnostics_df <- diagnostics_df %>%
  arrange(match(paste(Model_ID, Target_Space, Sample_Group, Heterogeneity_Variant, sep = "|"), fit_manifest$task_key))

expected_diag_n <- nrow(successful_status)
if (nrow(diagnostics_df) != expected_diag_n) {
  stop(
    "[BLOCKER] ma07c diagnostics row count mismatch. Expected one row per successful ma07b task (",
    expected_diag_n, "), found ", nrow(diagnostics_df), "."
  )
}

audit_df <- if (length(audit_rows)) bind_rows(audit_rows) else empty_artifact_audit()
if (nrow(audit_df)) {
  audit_df$draws_exists_after <- vapply(audit_df$draws_path, file.exists, logical(1))
}
fail_df <- if (length(failure_rows)) bind_rows(failure_rows) else empty_hard_gate_failures()
if (nrow(fail_df)) {
  fail_df$draws_exists_after <- vapply(fail_df$draws_path, file.exists, logical(1))
}
coeff_df <- if (length(coeff_rows)) bind_rows(coeff_rows) else data.frame()

write_csv_utf8(diagnostics_df, diag_path)
write_csv_utf8(audit_df, ma07_artifact_audit_path)
write_csv_utf8(fail_df, ma07_hard_gate_failures_path)
write_csv_utf8(coeff_df, coeff_path)

if (nrow(fail_df) > 0) {
  failed_main_keys <- unique(fail_df$recommended_remediation_key[truthy(fail_df$Main_Stack_Inclusion)])
  failed_main_keys <- failed_main_keys[!is.na(failed_main_keys) & nzchar(failed_main_keys)]
  helper_lines <- c(
    "# Suggested ma07 remediation targets generated from hard-gate failures.",
    paste0("$env:ACCRUAL_MCMC_REMEDIATION_TARGETS = \"", paste(failed_main_keys, collapse = ";"), "\""),
    "Remove-Item Env:\\ACCRUAL_FORCE_REFIT -ErrorAction SilentlyContinue",
    "Rscript .\\scripts\\ma07a_fit_brms_named_models.R",
    "Rscript .\\scripts\\ma07b_extract_brms_fit_outputs_workers.R",
    "Rscript .\\scripts\\ma07c_collect_brms_fit_outputs.R",
    "Rscript .\\scripts\\ma08_mcmc_diagnostics.R"
  )
  writeLines(helper_lines, ma07_remediation_helper_path, useBytes = TRUE)
}

sampler_cfg <- accrual_sampler_config("baseline", varying_slopes = run_varying_slope_models)
baseline_rng_meta <- accrual_rng_metadata_list("baseline_fit_brms_named_models")
phase3_notes <- sprintf(
  paste0(
    "ma07 winsorized BRMS fit notes\n",
    "Fit-stage artifacts are created by scripts/ma07a_fit_brms_named_models.R.\n",
    "Task-local extraction artifacts are created by scripts/ma07b_extract_brms_fit_outputs_workers.R.\n",
    "Shared diagnostics, audit, draw, and coefficient outputs are published by scripts/ma07c_collect_brms_fit_outputs.R.\n",
    "Sampling settings: chains=%d, cores=%d, iter=%d, warmup=%d, adapt_delta=%.2f, max_treedepth=%d, canonical_seed=%d, effective_seed=%d.\n",
    "Prior_Set_ID: %s.\nLikelihood_Family: %s.\nModel_Structure: %s.\n"
  ),
  sampler_cfg$chains, sampler_cfg$cores, sampler_cfg$iter, sampler_cfg$warmup,
  sampler_cfg$adapt_delta, sampler_cfg$max_treedepth,
  baseline_rng_meta$Canonical_Seed, baseline_rng_meta$Effective_Seed,
  prior_set_id, likelihood_family, model_structure
)
notes_file <- if (run_varying_slope_models) file.path(phase_root, "logs", "varyslopes_notes.txt") else file.path(phase_root, "logs", "ma07_fit_notes_winsor.txt")
writeLines(phase3_notes, con = notes_file, useBytes = TRUE)

manifest_path <- file.path(phase_root, "manifests", "baseline_manifest.csv")
write_run_manifest(
  path = manifest_path,
  scenario = "baseline",
  prior_set_id = prior_set_id,
  family = likelihood_family,
  model_structure = model_structure,
  model_list = unique(fit_manifest$Model_ID),
  seed = baseline_rng_meta$Effective_Seed,
  sampling_config = sprintf("chains=%d;cores=%d;iter=%d;warmup=%d", sampler_cfg$chains, sampler_cfg$cores, sampler_cfg$iter, sampler_cfg$warmup),
  status = "SUCCESS",
  notes = "ma07 split fit/extract/collect run",
  input_paths = c(formulas_path, fit_manifest_path, fit_status_path, collect_manifest_path, collect_status_path),
  rng_context = baseline_rng_meta$RNG_Context,
  rng_offset = baseline_rng_meta$RNG_Offset
)

if (run_varying_slope_models) {
  empty_weights <- data.frame(
    Status = "NOT_COMPUTED_BY_ma07",
    Notes = "Varying-slope fits are a Breuer-structure robustness check. Run a separate varying-slope stacking analysis before using weights.",
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = phase_root,
    stringsAsFactors = FALSE
  )
  write_csv_utf8(empty_weights, file.path(phase_root, "tables", "table_varyslopes_loo_weights.csv"))
}

if (nrow(fail_df) > 0 && any(truthy(fail_df$Main_Stack_Inclusion))) {
  stop("[MA07 HARD GATE BLOCKER] Main-stack model failed diagnostics or draw artifact collection: ",
       paste(fail_df$diagnostic_key[truthy(fail_df$Main_Stack_Inclusion)], collapse = "; "))
}

cat("[SUCCESS] ma07c collection completed.\n")
phase_end("ma07c", "Collect extracted baseline brms fit outputs")
