source("scripts/ma00_setup.R")

pipeline <- write_pipeline_index()
index_path <- file.path(method_design_root, "pipeline_index.csv")
if (!file.exists(index_path)) {
  stop("write_pipeline_index() did not create pipeline_index.csv at: ", index_path)
}

written <- read.csv(index_path, stringsAsFactors = FALSE)
required_cols <- c("Order", "Script", "Role")
missing_cols <- setdiff(required_cols, names(written))
if (length(missing_cols)) {
  stop("pipeline_index.csv missing required columns: ", paste(missing_cols, collapse = ", "))
}

for (col in required_cols) {
  bad <- is.na(written[[col]]) | !nzchar(written[[col]])
  if (any(bad)) {
    stop("pipeline_index.csv has missing values in ", col, " at rows: ",
         paste(which(bad), collapse = ", "))
  }
}

if (nrow(pipeline) != nrow(written)) {
  stop("write_pipeline_index() returned ", nrow(pipeline),
       " rows but pipeline_index.csv has ", nrow(written), " rows.")
}

missing_scripts <- written$Script[!file.exists(written$Script)]
if (length(missing_scripts)) {
  stop("pipeline_index.csv references missing active scripts: ",
       paste(missing_scripts, collapse = ", "))
}

archived_or_legacy <- grep("archive/legacy_diagnostics|rv04|rv05|rv06", written$Script, value = TRUE)
if (length(archived_or_legacy)) {
  stop("pipeline_index.csv must not reference archived legacy diagnostics: ",
       paste(archived_or_legacy, collapse = ", "))
}

obsolete_mixed_stage <- c(
  "scripts/ma09_loo_stacking.R",
  "scripts/ma12_grouped_kfold_firm.R",
  "scripts/ma13_row_level_exact_kfold.R",
  "scripts/sensitivity/se02_refit_prior_scenarios.R",
  "scripts/simulation/si03_brms_leakage_confirmation.R",
  "scripts/simulation/si04_brms_parameter_recovery.R",
  "scripts/diagnostics/di08_mcmc_sampler_calibration.R"
)
bad_obsolete <- intersect(obsolete_mixed_stage, written$Script)
if (length(bad_obsolete)) {
  stop("pipeline_index.csv must not list obsolete mixed-stage scripts as active: ",
       paste(bad_obsolete, collapse = ", "))
}

for (fragment in c(
  "scripts/ma07a_fit_brms_named_models.R",
  "scripts/ma07b_extract_brms_fit_outputs_workers.R",
  "scripts/ma07c_collect_brms_fit_outputs.R",
  "scripts/ma09a_plan_loo_savepars_refits.R",
  "scripts/ma09b_fit_loo_savepars_refits.R",
  "scripts/ma09c_collect_loo_stacking.R",
  "scripts/ma12a_plan_grouped_kfold_firm.R",
  "scripts/ma12b_fit_grouped_kfold_firm_workers.R",
  "scripts/ma12c_collect_grouped_kfold_firm_scores.R",
  "scripts/ma13a_plan_row_level_exact_kfold.R",
  "scripts/ma13b_fit_row_level_exact_kfold_workers.R",
  "scripts/ma13c_collect_row_level_exact_kfold_scores.R",
  "scripts/sensitivity/se02a_plan_prior_scenario_refits.R",
  "scripts/sensitivity/se02b_fit_prior_scenario_workers.R",
  "scripts/sensitivity/se02c_collect_prior_scenario_outputs.R",
  "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R",
  "scripts/diagnostics/di03_exact_kfold_reclassification_audit.R",
  "scripts/diagnostics/di04_denominator_diagnostics.R",
  "scripts/diagnostics/di05_economic_validity_top_tail.R",
  "scripts/diagnostics/di07_section4_7_reviewer_package.R",
  "scripts/diagnostics/di09_temporal_dependence_robustness.R"
)) {
  if (!fragment %in% written$Script) {
    stop("pipeline_index.csv missing current pipeline script: ", fragment)
  }
}

cat("test_pipeline_index_static.R passed\n")
