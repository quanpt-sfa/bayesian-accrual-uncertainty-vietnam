# -----------------------------------------------------------------------------
# Method registries, prior specs, and design manifests
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

sensitivity_scenario_ids <- c("baseline", "tight", "wide")

main_model_ids_for_space <- function(target_space) {
  if (identical(target_space, "ex_post")) return(c("M01", "M02", "M03", "M04", "M05", "M06", "M07"))
  if (identical(target_space, "real_time")) return(c("M01", "M02", "M03", "M07", "M09"))
  character()
}

primary_model_ids_for_space <- function(target_space) main_model_ids_for_space(target_space)
exact_kfold_model_ids_for_space <- function(target_space) main_model_ids_for_space(target_space)

chapter3_prior_predictive_thresholds <- function() {
  list(
    abs_gt_1_pass = 0.05,
    abs_gt_2_pass = 0.01,
    range_ratio_pass = 3.00,
    abs_gt_1_review = 0.15,
    abs_gt_2_review = 0.02,
    range_ratio_review = 5.00,
    source = "doc/method_authority/chapter_3_method_authority.md"
  )
}

classify_chapter3_prior_predictive <- function(share_gt_1, share_gt_2, prior_p01, prior_p99, observed_p01, observed_p99) {
  thr <- chapter3_prior_predictive_thresholds()
  vals <- c(share_gt_1, share_gt_2, prior_p01, prior_p99, observed_p01, observed_p99)
  if (any(!is.finite(vals))) {
    return(list(status = "FAIL", reason = "non-finite prior predictive or observed summary", range_ratio = NA_real_))
  }
  empirical_range <- observed_p99 - observed_p01
  prior_range <- prior_p99 - prior_p01
  if (!is.finite(empirical_range) || empirical_range <= 0 || !is.finite(prior_range)) {
    return(list(status = "FAIL", reason = "invalid empirical or prior predictive 1st-to-99th percentile range", range_ratio = NA_real_))
  }
  range_ratio <- prior_range / empirical_range
  pass <- share_gt_1 <= thr$abs_gt_1_pass &&
    share_gt_2 <= thr$abs_gt_2_pass &&
    range_ratio <= thr$range_ratio_pass
  if (pass) {
    return(list(status = "PASS", reason = "meets Chapter 3 prior predictive gates", range_ratio = range_ratio))
  }
  review <- share_gt_1 <= thr$abs_gt_1_review &&
    share_gt_2 <= thr$abs_gt_2_review &&
    range_ratio <= thr$range_ratio_review
  if (review) {
    return(list(status = "REVIEW", reason = "misses Chapter 3 PASS gate but remains within derived REVIEW band", range_ratio = range_ratio))
  }
  list(status = "FAIL", reason = "fails Chapter 3 prior predictive gates", range_ratio = range_ratio)
}


accrual_test_parallel_scenarios <- function() {
  list(
    valid_budget = list(
      env = c(
        ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE",
        ACCRUAL_MODEL_PARALLEL_WORKERS = "2",
        ACCRUAL_TOTAL_CORE_BUDGET = "4",
        ACCRUAL_BASELINE_CORES = "2",
        ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"
      ),
      cores_per_fit = 2L
    ),
    over_budget = list(
      env = c(
        ACCRUAL_MODEL_PARALLEL_WORKERS = "3",
        ACCRUAL_TOTAL_CORE_BUDGET = "4"
      ),
      cores_per_fit = 2L
    )
  )
}

accrual_heavy_fit_stage_registry <- function() {
  data.frame(
    stage_id = c("ma09", "ma12", "ma13", "se02", "si03", "si04", "di08"),
    fit_script = c(
      "scripts/ma09b_fit_loo_savepars_refits.R",
      "scripts/ma12b_fit_grouped_kfold_firm_workers.R",
      "scripts/ma13b_fit_row_level_exact_kfold_workers.R",
      "scripts/sensitivity/se02b_fit_prior_scenario_workers.R",
      "scripts/simulation/si03b_fit_brms_leakage_confirmation_workers.R",
      "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R",
      "scripts/diagnostics/di08b_fit_mcmc_sampler_calibration_workers.R"
    ),
    collect_script = c(
      "scripts/ma09c_collect_loo_stacking.R",
      "scripts/ma12c_collect_grouped_kfold_firm_scores.R",
      "scripts/ma13c_collect_row_level_exact_kfold_scores.R",
      "scripts/sensitivity/se02c_collect_prior_scenario_outputs.R",
      "scripts/simulation/si03c_collect_brms_leakage_confirmation.R",
      "scripts/simulation/si04c_collect_brms_parameter_recovery.R",
      "scripts/diagnostics/di08c_collect_mcmc_sampler_calibration.R"
    ),
    original_script = c(
      "scripts/ma09_loo_stacking.R",
      "scripts/ma12_grouped_kfold_firm.R",
      "scripts/ma13_row_level_exact_kfold.R",
      "scripts/sensitivity/se02_refit_prior_scenarios.R",
      "scripts/simulation/si03_brms_leakage_confirmation.R",
      "scripts/simulation/si04_brms_parameter_recovery.R",
      "scripts/diagnostics/di08_mcmc_sampler_calibration.R"
    ),
    fit_kind = c(
      "loo_savepars",
      "grouped_kfold",
      "row_kfold",
      "sensitivity",
      "simulation",
      "simulation",
      "diagnostic_calibration"
    ),
    config_helper = c(
      "accrual_loo_config",
      "accrual_kfold_config",
      "accrual_kfold_config",
      "accrual_sampler_config",
      "accrual_simulation_runtime_config",
      "accrual_simulation_runtime_config",
      "accrual_calibration_profile_grid"
    ),
    worker_required = TRUE,
    shared_outputs_parent_only = TRUE,
    notes = c(
      "PSIS/LOO remains secondary evidence; save_pars refits are worker-owned and stacking outputs are collector-owned.",
      "Exact grouped firm K-fold remains primary RQ1 validation evidence; workers own fold fit artifacts only.",
      "Exact row-level K-fold remains primary RQ1 validation evidence; workers own fold fit artifacts only.",
      "Sensitivity prior-scenario model refits are worker-owned; sensitivity tables are collector-owned.",
      "BRMS leakage simulation fit replicates are worker-owned; simulation summaries are collector-owned.",
      "BRMS parameter recovery fit replicates are worker-owned; recovery summaries are collector-owned.",
      "Sampler calibration is diagnostic-only; workers own calibration fit artifacts only."
    ),
    stringsAsFactors = FALSE
  )
}


accrual_section47_reviewer_artifact_spec <- function(root = output_root) {
  data.frame(
    artifact_id = c(
      "denominator_sd_mu_distribution",
      "denominator_capped_jaccard",
      "da_z_est_vs_z_pred_comparison",
      "denominator_decision",
      "economic_validity",
      "economic_validity_decision",
      "economic_validity_means",
      "economic_validity_counts",
      "economic_validity_note",
      "temporal_firmre_premium",
      "temporal_decision"
    ),
    source_path = c(
      file.path(root, "diagnostics", "table_denominator_sd_mu_distribution.csv"),
      file.path(root, "diagnostics", "table_denominator_capped_jaccard.csv"),
      file.path(root, "diagnostics", "table_da_z_est_vs_z_pred_comparison.csv"),
      file.path(root, "diagnostics", "table_denominator_diagnostics_decision.csv"),
      file.path(root, "diagnostics", "table_top_tail_group_economic_validity.csv"),
      file.path(root, "diagnostics", "table_top_tail_group_economic_validity_decision.csv"),
      file.path(root, "diagnostics", "table_top_tail_group_outcome_means.csv"),
      file.path(root, "diagnostics", "table_top_tail_set_counts_exact_kfold.csv"),
      file.path(root, "diagnostics", "economic_validity_top_tail_reviewer_note.md"),
      file.path(root, "simulation", "temporal_dependence", "tables", "table_temporal_dependence_firmre_premium.csv"),
      file.path(root, "simulation", "temporal_dependence", "tables", "table_temporal_dependence_decision.csv")
    ),
    artifact_class = c(
      rep("di04_denominator", 4),
      rep("di05_economic_validity", 5),
      rep("temporal_dependence_optional", 2)
    ),
    required = c(rep(TRUE, 9), rep(FALSE, 2)),
    stringsAsFactors = FALSE
  )
}

accrual_section47_required_artifacts <- function(root = output_root) {
  spec <- accrual_section47_reviewer_artifact_spec(root)
  spec$source_path[spec$required]
}


accrual_simulation_dgp_config <- function(kind = c("brms_leakage", "brms_recovery")) {
  kind <- match.arg(kind)
  if (identical(kind, "brms_leakage")) {
    return(list(
      design_source = "scripts/ma00_setup.R::accrual_simulation_dgp_config",
      n_firms = 24L,
      years = 2016:2020,
      n_industries = 6L,
      beta_drev = 0.02,
      beta_ppe = -0.03,
      sigma_eps = 0.08,
      model_type = "firm_random_intercept"
    ))
  }
  list(
    design_source = "scripts/ma00_setup.R::accrual_simulation_dgp_config",
    beta_drev = 0.04,
    beta_ppe = -0.03,
    beta_roa = 0.02
  )
}

sensitivity_scenarios <- function() {
  data.frame(
    Scenario = sensitivity_scenario_ids,
    Prior_Set_ID = c(
      "scale_aware_student_baseline_v1",
      "scale_aware_student_tight_v1",
      "scale_aware_student_wide_v1"
    ),
    Likelihood_Family = "student",
    Model_Structure = "pooled_random_intercept",
    Manuscript_Use = "final_prior_sensitivity",
    Scenario_Description = c(
      "Baseline scale-aware weakly informative Student-t prior.",
      "Tight scale-aware Student-t prior with about half-scale slope and intercept shrinkage.",
      "Wide but still plausible scale-aware Student-t prior for final sensitivity."
    ),
    stringsAsFactors = FALSE
  )
}

prior_registry <- function() {
  row <- function(id, cls, dist, loc, scale, applies, family, role, use, notes) {
    data.frame(
      Prior_Set_ID = id,
      Parameter_Class = cls,
      Prior_Distribution = dist,
      Location = loc,
      Scale_or_Rate = as.character(scale),
      Applies_To = applies,
      Likelihood_Family = family,
      Prior_Set_Role = role,
      Manuscript_Use = use,
      Notes = notes,
      stringsAsFactors = FALSE
    )
  }

  bind_rows_if_available <- function(rows) {
    if (requireNamespace("dplyr", quietly = TRUE)) {
      return(dplyr::bind_rows(rows))
    }
    do.call(rbind, rows)
  }

  bind_rows_if_available(list(
    row("wide_original", "b", "normal(0, 2.5)", 0, 2.5, "All slope coefficients", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Old Gaussian wide-prior result preserved as diagnostic only."),
    row("wide_original", "Intercept", "normal(0, 2.5)", 0, 2.5, "Model intercept", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Old Gaussian wide-prior result preserved as diagnostic only."),
    row("wide_original", "sigma", "exponential(1)", NA, 1, "Residual standard deviation", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Old Gaussian wide-prior result preserved as diagnostic only."),
    row("wide_original", "sd", "exponential(1)", NA, 1, "Group-level standard deviations", "gaussian", "Diagnostic only; prior predictive checks failed; not manuscript baseline.", "Diagnostic only", "Applied to random-effect variants."),
    row("scale_aware_student_baseline_v1", "b", "normal(0, 0.10)", 0, 0.10, "All slope coefficients", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Scale-aware prior for accruals scaled by lagged assets."),
    row("scale_aware_student_baseline_v1", "Intercept", "normal(0, 0.10)", 0, 0.10, "Model intercept", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Scale-aware prior for centered accrual level."),
    row("scale_aware_student_baseline_v1", "sigma", "exponential(10)", NA, 10, "Residual standard deviation", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Favors residual scale plausible for TA scaled by lagged assets."),
    row("scale_aware_student_baseline_v1", "sd", "exponential(10)", NA, 10, "Group-level standard deviations", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Applied to random intercepts or varying slopes when present."),
    row("scale_aware_student_baseline_v1", "nu", "gamma(2, 0.1)", NA, "shape=2; rate=0.1", "Student-t degrees of freedom", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Lets the likelihood absorb heavier tails without making priors diffuse."),
    row("scale_aware_student_baseline_v1", "cor", "lkj(2)", NA, 2, "Correlated random effects", "student", "Candidate manuscript baseline; scale-aware priors and Student-t likelihood.", "Candidate baseline", "Used for optional Breuer-like varying-slope robustness."),
    row("scale_aware_student_tight_v1", "b", "normal(0, 0.05)", 0, 0.05, "All slope coefficients", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter slope shrinkage."),
    row("scale_aware_student_tight_v1", "Intercept", "normal(0, 0.05)", 0, 0.05, "Model intercept", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter intercept shrinkage."),
    row("scale_aware_student_tight_v1", "sigma", "exponential(20)", NA, 20, "Residual standard deviation", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter residual scale."),
    row("scale_aware_student_tight_v1", "sd", "exponential(20)", NA, 20, "Group-level standard deviations", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Tighter group-level scale."),
    row("scale_aware_student_tight_v1", "nu", "gamma(2, 0.1)", NA, "shape=2; rate=0.1", "Student-t degrees of freedom", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Same Student-t tail prior as baseline."),
    row("scale_aware_student_tight_v1", "cor", "lkj(2)", NA, 2, "Correlated random effects", "student", "Tight scale-aware Student-t sensitivity.", "Sensitivity", "Used for optional varying slopes."),
    row("scale_aware_student_wide_v1", "b", "normal(0, 0.25)", 0, 0.25, "All slope coefficients", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider but still scale-aware slope prior."),
    row("scale_aware_student_wide_v1", "Intercept", "normal(0, 0.25)", 0, 0.25, "Model intercept", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider but still scale-aware intercept prior."),
    row("scale_aware_student_wide_v1", "sigma", "exponential(5)", NA, 5, "Residual standard deviation", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider residual scale."),
    row("scale_aware_student_wide_v1", "sd", "exponential(5)", NA, 5, "Group-level standard deviations", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Wider group-level scale."),
    row("scale_aware_student_wide_v1", "nu", "gamma(2, 0.1)", NA, "shape=2; rate=0.1", "Student-t degrees of freedom", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Same Student-t tail prior as baseline."),
    row("scale_aware_student_wide_v1", "cor", "lkj(2)", NA, 2, "Correlated random effects", "student", "Wide scale-aware Student-t sensitivity.", "Sensitivity", "Used for optional varying slopes.")
  ))
}

default_prior_specification <- function() {
  prior_registry()
}

write_prior_registry <- function(root = output_root) {
  ensure_analysis_dirs()
  out <- file.path(root, "tables", "table_prior_sets.csv")
  write_csv_safely(prior_registry(), out, row.names = FALSE)
  out
}

prior_set_rows <- function(selected_prior_set_id = prior_set_id) {
  rows <- prior_registry()
  rows <- rows[rows$Prior_Set_ID == selected_prior_set_id, , drop = FALSE]
  if (nrow(rows) == 0) stop("[BLOCKER] Unknown ACCRUAL_PRIOR_SET_ID: ", selected_prior_set_id)
  rows
}

default_prior_list <- function(heterogeneity_variant = "", selected_model_structure = NULL,
                                  model_structure = NULL, selected_prior_set_id = NULL,
                                  prior_set_id = NULL, family = NULL) {
  helper_env <- environment(default_prior_list)
  if (is.null(selected_model_structure)) {
    selected_model_structure <- if (!is.null(model_structure)) model_structure else helper_env$model_structure
  }
  if (is.null(selected_prior_set_id)) {
    selected_prior_set_id <- if (!is.null(prior_set_id)) prior_set_id else helper_env$prior_set_id
  }
  if (is.null(family)) {
    family <- helper_env$likelihood_family
  }

  rows <- prior_set_rows(selected_prior_set_id)
  prior_for <- function(cls) rows$Prior_Distribution[rows$Parameter_Class == cls][1]
  prior_list <- c(
    brms::prior_string(prior_for("b"), class = "b"),
    brms::prior_string(prior_for("Intercept"), class = "Intercept"),
    brms::prior_string(prior_for("sigma"), class = "sigma")
  )
  has_group_effect <- grepl("Firm RE", heterogeneity_variant, fixed = TRUE) ||
    identical(selected_model_structure, "breuer_varying_slopes")
  if (has_group_effect && any(rows$Parameter_Class == "sd")) {
    prior_list <- c(prior_list, brms::prior_string(prior_for("sd"), class = "sd"))
  }
  if (identical(tolower(family), "student") && any(rows$Parameter_Class == "nu")) {
    prior_list <- c(prior_list, brms::prior_string(prior_for("nu"), class = "nu"))
  }
  if (identical(selected_model_structure, "breuer_varying_slopes") && any(rows$Parameter_Class == "cor")) {
    prior_list <- c(prior_list, brms::prior_string(prior_for("cor"), class = "cor"))
  }
  prior_list
}

brms_family <- function(family = likelihood_family) {
  if (identical(tolower(family), "student")) brms::student() else brms::gaussian()
}

metadata_columns <- function() {
  data.frame(
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
}

validate_final_analysis_config <- function(context = "final analysis", final_mode = TRUE) {
  is_invalid <- !identical(prior_set_id, "scale_aware_student_baseline_v1") ||
                !identical(likelihood_family, "student") ||
                !identical(model_structure, "pooled_random_intercept")
  if (!is_invalid) return(invisible(TRUE))

  msg <- paste0(
    "[CONFIG WARNING] ", context, " has a deviant/diagnostic configuration: ",
    "(ACCRUAL_PRIOR_SET_ID=", prior_set_id, ", ACCRUAL_FAMILY=", likelihood_family, ", ACCRUAL_MODEL_STRUCTURE=", model_structure, "). ",
    "The standard final-analysis config should have ACCRUAL_PRIOR_SET_ID='scale_aware_student_baseline_v1', ",
    "ACCRUAL_FAMILY='student', and ACCRUAL_MODEL_STRUCTURE='pooled_random_intercept'."
  )
  if (final_mode && !env_flag("ACCRUAL_ALLOW_DIAGNOSTIC_CONFIG", "FALSE")) {
    stop(msg, " Set ACCRUAL_ALLOW_DIAGNOSTIC_CONFIG=TRUE only for an intentional diagnostic run.")
  }
  warning(msg, call. = FALSE)
  invisible(FALSE)
}

selected_sensitivity_scenarios <- function() {
  requested <- env_value("ACCRUAL_SENS_SCENARIO", "")
  scenarios <- sensitivity_scenarios()
  if (!nzchar(requested) || toupper(requested) == "ALL") return(scenarios)
  keep <- trimws(unlist(strsplit(requested, ",", fixed = TRUE)))
  unknown <- setdiff(keep, scenarios$Scenario)
  if (length(unknown) > 0) stop("[BLOCKER] Unknown ACCRUAL_SENS_SCENARIO: ", paste(unknown, collapse = ", "))
  scenarios[scenarios$Scenario %in% keep, , drop = FALSE]
}

package_versions <- function(pkgs = c("brms", "rstan", "cmdstanr", "posterior", "loo", "bayesplot", "dplyr", "readr", "tibble", "ggplot2")) {
  vals <- vapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return("NOT_INSTALLED")
    as.character(utils::packageVersion(pkg))
  }, character(1))
  paste(paste(names(vals), vals, sep = "="), collapse = "; ")
}

file_fingerprint <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (requireNamespace("digest", quietly = TRUE)) {
    return(paste0("sha256:", digest::digest(path, algo = "sha256", file = TRUE)))
  }
  info <- file.info(path)
  paste0("mtime:", format(info$mtime[1], "%Y-%m-%d %H:%M:%S %z"), ";size:", as.numeric(info$size[1]))
}

session_info_string <- function() {
  paste(capture.output(sessionInfo()), collapse = "\n")
}

metadata_matches <- function(path, expected) {
  if (!file.exists(path)) return(FALSE)
  old <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(old) == 0) return(FALSE)
  for (nm in names(expected)) {
    if (!nm %in% names(old)) return(FALSE)
    old_val <- as.character(old[[nm]][1])
    new_val <- as.character(expected[[nm]][1])
    if (!identical(old_val, new_val)) return(FALSE)
  }
  TRUE
}

write_run_manifest <- function(path, scenario, prior_set_id, family, model_structure,
                                  model_list, seed, sampling_config, status,
                                  notes = "", input_paths = character(),
                                  rng_context = "manifest", rng_offset = 0L) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  input_hash <- if (length(input_paths) > 0) {
    paste(paste(input_paths, vapply(input_paths, file_fingerprint, character(1)), sep = "="), collapse = "; ")
  } else {
    NA_character_
  }
  rng_meta <- accrual_rng_metadata(rng_context, rng_offset)
  manifest <- data.frame(
    Scenario = scenario,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = family,
    Model_Structure = model_structure,
    Model_List = paste(model_list, collapse = ","),
    Seed = seed,
    Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    Input_Hash = input_hash,
    Package_Versions = package_versions(),
    R_Version = paste(R.version$major, R.version$minor, sep = "."),
    Sampling_Config = sampling_config,
    Status = status,
    Notes = notes,
    Session_Info = session_info_string(),
    stringsAsFactors = FALSE
  )
  manifest <- cbind(manifest, rng_meta)
  write_csv_safely(manifest, path, row.names = FALSE)
  path
}

accrual_pipeline_index_registry <- function() {
  rows <- list(
    c("ma00", "scripts/ma00_setup.R", "Setup helpers, runtime config, and shared registries"),
    c("ma01", "scripts/ma01_setup_and_registry.R", "Setup and ten-model registry"),
    c("ma02", "scripts/ma02_build_common_sample.R", "Build common samples"),
    c("ma03", "scripts/ma03_audit_data_integrity.R", "Data integrity audit"),
    c("ma04", "scripts/ma04_define_named_models.R", "Define named model formulas"),
    c("ma05", "scripts/ma05_winsorize_common_samples.R", "Winsorize common samples"),
    c("ma06", "scripts/ma06_prior_predictive_checks.R", "Prior predictive gate with task-worker support"),
    c("ma07a", "scripts/ma07a_fit_brms_named_models.R", "Full-sample baseline brms fit worker stage"),
    c("ma07b", "scripts/ma07b_extract_brms_fit_outputs_workers.R", "Extract baseline brms fit outputs with workers"),
    c("ma07c", "scripts/ma07c_collect_brms_fit_outputs.R", "Collect extracted baseline brms fit outputs"),
    c("ma08", "scripts/ma08_mcmc_diagnostics.R", "MCMC diagnostics for baseline fits"),
    c("ma09a", "scripts/ma09a_plan_loo_savepars_refits.R", "Plan PSIS/LOO save_pars refits"),
    c("ma09b", "scripts/ma09b_fit_loo_savepars_refits.R", "Fit PSIS/LOO save_pars refits with workers"),
    c("ma09c", "scripts/ma09c_collect_loo_stacking.R", "Collect PSIS/LOO stacking evidence"),
    c("ma10", "scripts/ma10_construct_psis_loo_DA.R", "Construct PSIS/LOO secondary uncertainty-adjusted DA"),
    c("ma11", "scripts/ma11_posterior_predictive_checks.R", "Posterior predictive checks for secondary PSIS/LOO DA"),
    c("ma12a", "scripts/ma12a_plan_grouped_kfold_firm.R", "Plan grouped exact firm K-fold run and fold assignments"),
    c("ma12b", "scripts/ma12b_fit_grouped_kfold_firm_workers.R", "Fit grouped exact firm K-fold tasks with workers"),
    c("ma12c", "scripts/ma12c_collect_grouped_kfold_firm_scores.R", "Collect grouped exact firm K-fold scores and completed-run contract"),
    c("ma13a", "scripts/ma13a_plan_row_level_exact_kfold.R", "Plan row-level exact K-fold run and fold assignments"),
    c("ma13b", "scripts/ma13b_fit_row_level_exact_kfold_workers.R", "Fit row-level exact K-fold tasks with workers"),
    c("ma13c", "scripts/ma13c_collect_row_level_exact_kfold_scores.R", "Collect row-level exact K-fold scores and completed-run contract"),
    c("ma14", "scripts/ma14_construct_exact_kfold_DA.R", "Primary exact-KFoldW DA construction from completed-run pins"),
    c("ma15", "scripts/ma15_audit_DA_finite_outputs.R", "Hard finite-output gate for exact-KFold DA"),
    c("ma16", "scripts/ma16_validate_outcomes.R", "Outcome validation on primary exact row-KFold DA"),
    c("di02", "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R", "New-firm predictive integration reporting gate"),
    c("di03", "scripts/diagnostics/di03_exact_kfold_reclassification_audit.R", "Exact K-fold reclassification/Jaccard diagnostics"),
    c("di04", "scripts/diagnostics/di04_denominator_diagnostics.R", "Denominator diagnostics for estimation-scaled exact-KFold DA"),
    c("di05", "scripts/diagnostics/di05_economic_validity_top_tail.R", "Economic-validity check for exact-KFold top-tail groups"),
    c("ma17", "scripts/ma17_export_tables_figures.R", "Chapter 3 manuscript table export"),
    c("ro01", "scripts/robustness/ro01_lofo_stacking.R", "Optional grouped PSIS-LOFO robustness"),
    c("se01", "scripts/sensitivity/se01_prior_predictive.R", "Sensitivity prior predictive gate"),
    c("se02a", "scripts/sensitivity/se02a_plan_prior_scenario_refits.R", "Plan sensitivity prior-scenario refits"),
    c("se02b", "scripts/sensitivity/se02b_fit_prior_scenario_workers.R", "Fit sensitivity prior-scenario models with workers"),
    c("se02c", "scripts/sensitivity/se02c_collect_prior_scenario_outputs.R", "Collect sensitivity prior-scenario outputs"),
    c("se03", "scripts/sensitivity/se03_mcmc_diagnostics.R", "Sensitivity MCMC diagnostics gate"),
    c("se04", "scripts/sensitivity/se04_stacking.R", "Sensitivity LOO/stacking by scenario"),
    c("se05", "scripts/sensitivity/se05_construct_DA.R", "Sensitivity DA reconstruction"),
    c("se06", "scripts/sensitivity/se06_validation.R", "Sensitivity validation/outcome tests"),
    c("se07", "scripts/sensitivity/se07_report.R", "Sensitivity report"),
    c("si00", "scripts/simulation/si00_helpers.R", "Simulation helper functions"),
    c("si01", "scripts/simulation/si01_lmer_pilot_run.R", "LMER leakage pilot simulation run"),
    c("si02", "scripts/simulation/si02_lmer_pilot_report.R", "LMER leakage pilot simulation report"),
    c("si03a", "scripts/simulation/si03a_plan_brms_leakage_confirmation.R", "Plan BRMS leakage confirmation simulation"),
    c("si03b", "scripts/simulation/si03b_fit_brms_leakage_confirmation_workers.R", "Fit BRMS leakage confirmation simulation with workers"),
    c("si03c", "scripts/simulation/si03c_collect_brms_leakage_confirmation.R", "Collect BRMS leakage confirmation simulation"),
    c("si04a", "scripts/simulation/si04a_plan_brms_parameter_recovery.R", "Plan BRMS parameter recovery simulation"),
    c("si04b", "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R", "Fit BRMS parameter recovery simulation with workers"),
    c("si04c", "scripts/simulation/si04c_collect_brms_parameter_recovery.R", "Collect BRMS parameter recovery simulation"),
    c("si05", "scripts/simulation/si05_lmer_temporal_dependence_run.R", "LMER temporal-dependence persistent-shock simulation"),
    c("si06", "scripts/simulation/si06_lmer_temporal_dependence_report.R", "Report temporal-dependence mechanism simulation"),
    c("di01", "scripts/diagnostics/di01_psis_reliability_gate.R", "Optional secondary PSIS reliability diagnostics"),
    c("di07", "scripts/diagnostics/di07_section4_7_reviewer_package.R", "Assemble Section 4.7 reviewer evidence package"),
    c("di08a", "scripts/diagnostics/di08a_plan_mcmc_sampler_calibration.R", "Plan diagnostic MCMC sampler calibration"),
    c("di08b", "scripts/diagnostics/di08b_fit_mcmc_sampler_calibration_workers.R", "Fit diagnostic MCMC sampler calibration with workers"),
    c("di08c", "scripts/diagnostics/di08c_collect_mcmc_sampler_calibration.R", "Collect diagnostic MCMC sampler calibration"),
    c("di09", "scripts/diagnostics/di09_temporal_dependence_robustness.R", "Temporal-dependence robustness for row-minus-grouped Firm-RE premium")
  )
  row_lengths <- lengths(rows)
  if (any(row_lengths != 3L)) {
    stop("[BLOCKER] Pipeline index registry rows must each contain Order, Script, and Role. Bad rows: ",
         paste(which(row_lengths != 3L), collapse = ", "))
  }
  matrix_rows <- do.call(rbind, rows)
  data.frame(
    Order = matrix_rows[, 1],
    Script = matrix_rows[, 2],
    Role = matrix_rows[, 3],
    stringsAsFactors = FALSE
  )
}

write_pipeline_index <- function() {
  dir.create(method_design_root, recursive = TRUE, showWarnings = FALSE)
  pipeline <- accrual_pipeline_index_registry()
  lengths <- c(Order = length(pipeline$Order), Script = length(pipeline$Script), Role = length(pipeline$Role))
  if (length(unique(lengths)) != 1L) {
    stop("[BLOCKER] Pipeline index registry has mismatched field lengths: ",
         paste(names(lengths), lengths, sep = "=", collapse = ", "))
  }
  if (any(is.na(pipeline$Order) | !nzchar(pipeline$Order)) ||
      any(is.na(pipeline$Script) | !nzchar(pipeline$Script)) ||
      any(is.na(pipeline$Role) | !nzchar(pipeline$Role))) {
    stop("[BLOCKER] Pipeline index registry contains missing Order, Script, or Role values.")
  }
  pipeline$Active <- TRUE
  write_csv_safely(pipeline, file.path(method_design_root, "pipeline_index.csv"), row.names = FALSE)

  readme_lines <- c(
    "# accrual uncertainty pipeline index",
    "",
    "Active scripts use the ma/ro/se/si/di reorg prefixes. The execution order is defined by `run.R`.",
    "",
    "| Order | Script | Role |",
    "|---|---|---|",
    sprintf("| %s | `%s` | %s |", pipeline$Order, pipeline$Script, pipeline$Role),
    "",
    "Sensitivity scripts se01-se07 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `ACCRUAL_DRY_RUN=FALSE` and the relevant script is launched intentionally.",
    "",
    "Sampler protocol: Chapter 3 baseline brms/Stan estimation uses the centralized sampler profiles in `scripts/ma00_setup.R`, with fixed seed 42 unless explicitly overridden and recorded in manifests. FAST_MODE/smoke runs are excluded from primary inference.",
    "",
    "Execution configuration is centralized in `scripts/ma00_setup.R`: `accrual_base_seed()` and `accrual_seed()` enforce one canonical seed (`ACCRUAL_SEED`, default 42) across baseline, grouped exact K-fold, row exact K-fold, sensitivity, and simulation branches; `accrual_seed_for()` derives deterministic context-specific offsets from that same canonical seed; `set_accrual_seed()` and `set_accrual_effective_seed()` are the only helpers that call base `set.seed()`; `accrual_sampler_config()` supplies sampler settings; `accrual_kfold_config()` supplies exact K-fold K/seed/sampler settings; and `main_model_ids_for_space()` supplies primary model IDs. Branch-specific seed env vars (`ACCRUAL_BASELINE_SEED`, `ACCRUAL_KFOLD_FIRM_SEED`, `ACCRUAL_ROW_KFOLD_SEED`, `ACCRUAL_SENS_SEED`, `ACCRUAL_SIM_SEED`) are deprecated and blocked if they differ from `ACCRUAL_SEED`. The helper writes `out/manifests/method_design/execution_config_registry.csv`.",
    "",
    "Production exact K-fold defaults are 4 chains, 4 rstan cores, 12000 iterations, 4000 warmup iterations, `adapt_delta = 0.99`, and `max_treedepth = 15` for both grouped-firm and row-level exact K-fold. Lower settings are light/test modes only and must be explicit in the K-fold run mode and task manifest sampler provenance.",
    "",
    "Primary model helpers return M01-M07 for ex-post and M01, M02, M03, M07, M09 for real-time/no-lookahead. M08/M10 remain secondary/robustness unless explicitly included through documented secondary flows, and M11/M12 remain excluded from active primary helpers.",
    "",
    "`Rscript run.R` runs the `main` target by default. The main target includes split grouped exact firm K-fold planning, worker fitting, and collection (`scripts/ma12a_plan_grouped_kfold_firm.R`, `scripts/ma12b_fit_grouped_kfold_firm_workers.R`, `scripts/ma12c_collect_grouped_kfold_firm_scores.R`) plus split row-level exact K-fold planning, worker fitting, and collection (`scripts/ma13a_plan_row_level_exact_kfold.R`, `scripts/ma13b_fit_row_level_exact_kfold_workers.R`, `scripts/ma13c_collect_row_level_exact_kfold_scores.R`) as adjacent primary RQ1 evidence steps. It then constructs primary exact-KFoldW DA (`scripts/ma14_construct_exact_kfold_DA.R`), applies the finite-output gate (`scripts/ma15_audit_DA_finite_outputs.R`), runs validation on the primary exact row-KFold DA, the new-firm predictive integration reporting gate, exact-KFold reclassification diagnostics, denominator/economic-validity diagnostics, and the corrected Chapter 3 manuscript export path `scripts/ma17_export_tables_figures.R`.",
    "",
    "`scripts/ma10_construct_psis_loo_DA.R` remains the PSIS/LOO secondary DA constructor, including secondary validation panels only. Scripts `ma12` and `ma13` write `LATEST_COMPLETED_RUN.txt` only for completed primary-eligible exact-refit runs, and script `ma14` uses those pins or explicit run-root environment variables instead of moving `LATEST_RUN.txt` for primary inference. `LATEST_RUN.txt` is operational only and should not be used as primary provenance. Scripts `ma12` and `ma13` write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.",
    "",
    "`scripts/ma14_construct_exact_kfold_DA.R` refuses completed-run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. It writes file-size/mtime/hash source manifests, draw-file hash manifests, and `table_model_primary_inclusion_gate.csv`. MCMC `FAIL`/`LOW_RELIABILITY` models are excluded from primary exact-KFold DA; `REVIEW`/`CAUTION` models can be retained only with `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS`.",
    "",
    "`scripts/ma15_audit_DA_finite_outputs.R` writes `table_DA_finite_gate_decision.csv` and is a hard RQ2/export gate. Script `di02` is a hard new-firm tail-suppression gate; if unverified Firm-RE out-of-firm posterior predictive tail quantities require suppression, export stops unless the explicit suppression override is set and the outputs are labelled non-primary. `Rscript run.R all --dry-run` de-duplicates `scripts/diagnostics/di02_new_firm_predictive_integration_audit.R` so the new-firm audit appears once.",
    "",
    "LOFO (`scripts/robustness/ro01_lofo_stacking.R`) is an opt-in robustness branch, not a default main step. Sensitivity scripts se01-se07, simulation scripts si00-si06, diagnostic sampler calibration di08a-di08c, and temporal robustness di09 are opt-in/heavy branches. PSIS reliability (`scripts/diagnostics/di01_psis_reliability_gate.R`) is secondary diagnostics, not the primary RQ1 comparison.",
    "",
    paste0("The machine-readable pipeline index is written to `", file.path(method_design_root, "pipeline_index.csv"), "`.")
  )
  readme_path <- file.path("doc", "pipeline_index.md")
  con <- file(readme_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0(paste(readme_lines, collapse = "\n"), "\n")), con)
  invisible(pipeline)
}


classify_model_family <- function(model_id, model_name) {
  if (model_id %in% c("M01", "M02", "M03")) return("Jones-family")
  if (model_id %in% c("M04", "M05", "M06")) return("Cash-flow/McNichols-family")
  if (model_id == "M07") return("Ball-Shivakumar/asymmetry")
  if (model_id == "M09") return("No-lookahead/real-time")
  if (model_id == "M08") return("Secondary volatility")
  if (model_id == "M10") return("Secondary operating-cycle")
  model_name
}

write_method_design_files <- function() {
  design_root <- method_design_root
  dir.create(design_root, recursive = TRUE, showWarnings = FALSE)
  differences <- data.frame(
    Dimension = c(
      "Scaling",
      "Model structure",
      "Outlier handling",
      "Model space",
      "Output",
      "Cross-validation",
      "Priors"
    ),
    Original_AccForUncertaintyCode = c(
      "Firm-demeaned, truncated, standardized variables.",
      "Hierarchical varying-coefficient / varying-slope model by group.",
      "Truncation after firm demeaning.",
      "Original AccForUncertaintyCode model set.",
      "NDA posterior mean and posterior SD.",
      "Observation-level PSIS-LOO stacking.",
      "Diffuse Gaussian prior used in the original implementation."
    ),
    This_Project = c(
      "Winsorized variables and scale-aware priors on accruals scaled by lagged assets.",
      "brms extension with pooled, random-intercept, and optional Breuer-like varying-slope variants.",
      "1/99 winsorization to preserve sample size in Vietnam.",
      "Vietnam-feasible two-tier model space, with M10/M08 secondary.",
      "NDA mean/estimation uncertainty plus posterior-predictive tail flags as an extension.",
      "Row-level LOO plus grouped PSIS-LOFO and optional exact grouped K-fold.",
      "Original diffuse prior retained only as diagnostic; manuscript baseline uses scale-aware priors after prior predictive checks."
    ),
    Implication = c(
      "The scale is tailored to Vietnamese listed-firm accrual variables.",
      "The Breuer-style structure is available as robustness, not default baseline.",
      "The corrected design avoids losing already limited Vietnam observations.",
      "The main stack remains feasible without imposing M08/M10 restrictions on all models.",
      "The output keeps the original NDA mean/uncertainty concept and adds predictive-tail diagnostics.",
      "Panel dependence is handled through grouped diagnostics and optional exact refits.",
      "Wide-prior Gaussian outputs are preserved as diagnostic only."
    ),
    stringsAsFactors = FALSE
  )
  write_csv_safely(differences, file.path(design_root, "differences_from_AccForUncertaintyCode.csv"), row.names = FALSE)
  writeLines(c(
    "This study adapts the Bayesian model-averaging framework of AccForUncertaintyCode to the Vietnamese listed-firm setting. It differs from the original implementation in sample construction, scaling, outlier handling, model space, posterior predictive abnormality classification, and panel-dependence robustness checks.",
    "",
    "The analysis is therefore positioned as an extension/adaptation, not a replication. The corrected design preserves corrected COGS/INV data, the two-tier sample design, the exclusion of M08 and M10 from main stacks, and the treatment of existing wide-prior Gaussian outputs as diagnostic only."
  ), file.path(design_root, "method_note_adaptation_not_replication.txt"))
  write_pipeline_index()
  write_execution_config_registry()
}
