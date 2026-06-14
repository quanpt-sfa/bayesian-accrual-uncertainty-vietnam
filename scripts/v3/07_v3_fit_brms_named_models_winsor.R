# -----------------------------------------------------------------------------
# Script: 07_v3_fit_brms_named_models_winsor.R
# Purpose: Fit v3 named brms models on winsorized samples.
# -----------------------------------------------------------------------------

library(dplyr)
library(brms)

source("scripts/v3/00_v3_winsor_helpers.R")
ensure_v3_winsor_dirs()
write_method_design_files_v3()
write_prior_registry_v3()
validate_v3_final_analysis_config("Phase 3b baseline brms fit", final_mode = TRUE)

# Check prior predictive check gatekeeper status
gate_csv_path <- file.path(v3_output_root, "prior_predictive_gate_status.csv")
if (!file.exists(gate_csv_path)) {
  stop("[BLOCKER] Prior predictive gate status file does not exist. Please run '06_v3_prior_predictive_checks_winsor.R' first.")
}
gate_df <- read.csv(gate_csv_path, stringsAsFactors = FALSE)
has_prior_pred_fail <- any(gate_df$status == "FAIL")
prior_pred_override_used <- FALSE

if (has_prior_pred_fail) {
  allow_fail <- env_flag_v3("V3_ALLOW_PRIOR_PREDICTIVE_FAIL", "FALSE")
  if (!allow_fail) {
    stop("[BLOCKER] Prior predictive check gate contains FAIL. Fitting blocked. Run 06_v3_prior_predictive_checks_winsor.R or set V3_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE.")
  } else {
    prior_pred_override_used <- TRUE
    message("[OVERRIDE] Prior predictive check FAIL bypassed via V3_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE.")
  }
}

options(mc.cores = parallel::detectCores())
set.seed(42)

run_varying_slope_models <- identical(v3_model_structure, "breuer_varying_slopes")
if (run_varying_slope_models && !v3_run_varying_slopes) {
  stop("[BLOCKER] V3_MODEL_STRUCTURE='breuer_varying_slopes' requires V3_RUN_VARYING_SLOPES='TRUE'.")
}

phase_root <- if (run_varying_slope_models) v3_varyslopes_root else v3_output_root
for (d in file.path(phase_root, c("", "tables", "models", "draws", "logs", "figures"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

formulas_path <- file.path(v3_input_winsor_root, "tables", "table_v3_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) {
  stop("[BLOCKER] Winsorized formula table not found. Run 05_v3_winsorize_common_samples.R first.")
}

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
if (run_varying_slope_models) {
  formulas_df <- formulas_df %>%
    filter(Main_Stack_Inclusion == TRUE) %>%
    filter(mapply(varying_slope_candidate_v3, Model_ID, Target_Space)) %>%
    group_by(Model_ID, Target_Space, Sample_Group) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      Heterogeneity_Variant = paste0("Breuer-like varying slopes (", v3_varyslope_group, ")"),
      brms_Formula = vapply(Base_Formula, varying_slope_formula_v3, character(1), group = v3_varyslope_group),
      Model_Structure = v3_model_structure,
      VarySlope_Group = v3_varyslope_group,
      VarySlope_Scope = v3_varyslope_scope
    )
  write.csv(formulas_df, file.path(phase_root, "tables", "table_v3_varyslopes_model_registry.csv"), row.names = FALSE)
}
write.csv(formulas_df, file.path(phase_root, "tables", "table_v3_named_model_formulas_winsor.csv"), row.names = FALSE)

diag_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_v3_varyslopes_diagnostics.csv")
} else {
  file.path(phase_root, "tables", "table_v3_brms_diagnostics_winsor.csv")
}
coeff_path <- if (run_varying_slope_models) {
  file.path(phase_root, "tables", "table_v3_varyslopes_coefficient_summary.csv")
} else {
  file.path(phase_root, "tables", "table_v3_coefficient_summary_winsor.csv")
}

if (file.exists(diag_path)) {
  diagnostics_df <- read.csv(diag_path, stringsAsFactors = FALSE)
  message("Resuming from existing winsor diagnostics table with ", nrow(diagnostics_df), " entries.")
} else {
  diagnostics_df <- data.frame(
    Model_ID = character(),
    Model_Name = character(),
    Target_Space = character(),
    Sample_Group = character(),
    Main_Stack_Inclusion = logical(),
    Secondary_Robustness = logical(),
    Heterogeneity_Variant = character(),
    N_Obs = integer(),
    N_Firms = integer(),
    Fit_Status = character(),
    Rhat_Max = double(),
    ESS_Min = double(),
    Divergences = integer(),
    converged = logical(),
    stacking_eligible = logical(),
    max_rhat = double(),
    divergences = integer(),
    treedepth_warnings = integer(),
    pareto_k_above_07 = integer(),
    random_intercept_sd = double(),
    elpd_loo = double(),
    error_message = character(),
    Prior_Set_ID = character(),
    Likelihood_Family = character(),
    Model_Structure = character(),
    Output_Root = character(),
    save_pars_all = logical(),
    stringsAsFactors = FALSE
  )
}

for (nm in names(metadata_columns_v3())) {
  if (!nm %in% names(diagnostics_df)) diagnostics_df[[nm]] <- metadata_columns_v3()[[nm]]
}
if (!"save_pars_all" %in% names(diagnostics_df)) {
  diagnostics_df$save_pars_all <- FALSE
}

chains <- 4
iter <- 4000
warmup <- 1000
adapt_delta <- if (run_varying_slope_models) 0.99 else 0.95
max_treedepth <- if (run_varying_slope_models) 15 else 12
seed <- 42

main_ex_post_ids <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07")
main_no_lookahead_ids <- c("M01", "M02", "M03", "M07", "M09")

formulas_df <- formulas_df %>%
  mutate(
    order_key = case_when(
      Model_ID %in% c("M01", "M02", "M03", "M04", "M05", "M06") ~ 1,
      Model_ID %in% c("M07", "M09", "M10") ~ 2,
      Model_ID == "M08" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  arrange(order_key, Model_ID, Target_Space, Heterogeneity_Variant)

write_failure_diag <- function(row, err) {
  fail_row <- data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Main_Stack_Inclusion = row$Main_Stack_Inclusion,
    Secondary_Robustness = row$Secondary_Robustness,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    N_Obs = NA_integer_,
    N_Firms = NA_integer_,
    Fit_Status = "FAILED",
    Rhat_Max = NA_real_,
    ESS_Min = NA_real_,
    Divergences = NA_integer_,
    converged = FALSE,
    stacking_eligible = FALSE,
    max_rhat = NA_real_,
    divergences = NA_integer_,
    treedepth_warnings = NA_integer_,
    pareto_k_above_07 = NA_integer_,
    random_intercept_sd = NA_real_,
    elpd_loo = NA_real_,
    error_message = err,
    Notes = row$Reason,
    Prior_Set_ID = v3_prior_set_id,
    Likelihood_Family = v3_likelihood_family,
    Model_Structure = v3_model_structure,
    Output_Root = phase_root,
    save_pars_all = FALSE,
    stringsAsFactors = FALSE
  )
  diagnostics_df <<- diagnostics_df %>%
    filter(!(Model_ID == row$Model_ID &
               Target_Space == row$Target_Space &
               Sample_Group == row$Sample_Group &
               Heterogeneity_Variant == row$Heterogeneity_Variant)) %>%
    bind_rows(fail_row)
  write.csv(diagnostics_df, diag_path, row.names = FALSE)
}

total_runs <- nrow(formulas_df)
message("Total winsorized configurations to fit/evaluate: ", total_runs)

for (i in seq_len(total_runs)) {
  row <- formulas_df[i, ]
  model_key <- model_key_v3_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
  model_filename <- file.path(phase_root, "models", paste0("fit_", model_key, ".rds"))
  draws_filename <- file.path(phase_root, "draws", paste0("draws_", model_key, ".rds"))

  message(sprintf("\n=== [%d/%d] Winsor model %s (%s) - %s ===",
                  i, total_runs, row$Model_Name, row$Target_Space, row$Heterogeneity_Variant))

  already_done <- !v3_force_refit && nrow(diagnostics_df) > 0 && any(
      diagnostics_df$Model_ID == row$Model_ID &
      diagnostics_df$Target_Space == row$Target_Space &
      diagnostics_df$Sample_Group == row$Sample_Group &
      diagnostics_df$Heterogeneity_Variant == row$Heterogeneity_Variant &
      is.na(diagnostics_df$error_message)
  )

  fit <- NULL
  if (file.exists(model_filename) && !v3_force_refit) {
    message("Loading pre-existing winsor model fit from: ", model_filename)
    fit <- tryCatch(readRDS(model_filename), error = function(e) {
      message("[ERROR] Could not read existing fit: ", e$message)
      NULL
    })
    if (file.exists(draws_filename)) {
      message("Draw file already exists; Phase 3b will not regenerate it unless V3_FORCE_REFIT='TRUE': ", draws_filename)
    }
  }

  if (is.null(fit) && !already_done) {
    df_scaled <- tryCatch(read_winsor_sample(row$Target_Sample), error = function(e) {
      write_failure_diag(row, e$message)
      stop(e)
    })
    if (run_varying_slope_models) {
      df_scaled <- prepare_varying_slope_data_v3(df_scaled)
    }

    formula_str <- fix_formula_v3(row$brms_Formula)
    brms_form <- bf(as.formula(formula_str))

    prior_list <- default_prior_list_v3(row$Heterogeneity_Variant, model_structure = v3_model_structure)

    fit <- tryCatch({
      brm(
        formula = brms_form,
        data = df_scaled,
        family = brms_family_v3(),
        prior = prior_list,
        chains = chains,
        iter = iter,
        warmup = warmup,
        control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
        seed = seed,
        save_pars = save_pars(all = TRUE),
        refresh = 500
      )
    }, error = function(e) {
      message("[ERROR] Winsor model fitting crashed: ", e$message)
      write_failure_diag(row, e$message)
      NULL
    })

    if (is.null(fit)) {
      if (isTRUE(row$Main_Stack_Inclusion)) {
        stop("[BLOCKER] Required main-stack model failed: ", model_key)
      }
      next
    }

    saveRDS(fit, model_filename)
    message("Saved winsor fit to: ", model_filename)
  }

  if (is.null(fit) && file.exists(model_filename)) {
    fit <- readRDS(model_filename)
  }
  if (is.null(fit)) next

  post_summary <- summary(fit)
  rhats <- post_summary$fixed[, "Rhat"]
  esses <- post_summary$fixed[, "Bulk_ESS"]
  if ("random" %in% names(post_summary) && !is.null(post_summary$random)) {
    for (group in names(post_summary$random)) {
      rhats <- c(rhats, post_summary$random[[group]][, "Rhat"])
      esses <- c(esses, post_summary$random[[group]][, "Bulk_ESS"])
    }
  }
  max_rhat <- suppressWarnings(max(rhats, na.rm = TRUE))
  min_ess <- suppressWarnings(min(esses, na.rm = TRUE))

  np <- nuts_params(fit)
  divergences <- sum(subset(np, Parameter == "divergent__")$Value)
  treedepths <- subset(np, Parameter == "treedepth__")$Value
  treedepth_warnings <- sum(treedepths >= max_treedepth)

  converged <- is.finite(max_rhat) && max_rhat <= 1.01 && divergences == 0
  stacking_eligible <- converged

  random_intercept_sd <- NA_real_
  if (grepl("Firm RE", row$Heterogeneity_Variant) && !is.null(post_summary$random$company)) {
    random_intercept_sd <- post_summary$random$company["sd(Intercept)", "Estimate"]
  }

  loo_res <- tryCatch(loo(fit), error = function(e) {
    message("[ERROR] LOO failed: ", e$message)
    NULL
  })
  pareto_k_above_07 <- NA_integer_
  elpd_loo <- NA_real_
  if (!is.null(loo_res)) {
    pareto_k_above_07 <- sum(loo_res$diagnostics$pareto_k > 0.7)
    elpd_loo <- loo_res$estimates["elpd_loo", "Estimate"]
  }

  diag_row <- data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Main_Stack_Inclusion = row$Main_Stack_Inclusion,
    Secondary_Robustness = row$Secondary_Robustness,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    N_Obs = nobs(fit),
    N_Firms = length(unique(fit$data$company)),
    Fit_Status = "SUCCESS",
    Rhat_Max = max_rhat,
    ESS_Min = min_ess,
    Divergences = divergences,
    converged = converged,
    stacking_eligible = stacking_eligible,
    max_rhat = max_rhat,
    divergences = divergences,
    treedepth_warnings = treedepth_warnings,
    pareto_k_above_07 = pareto_k_above_07,
    random_intercept_sd = random_intercept_sd,
    elpd_loo = elpd_loo,
    error_message = NA_character_,
    Notes = row$Reason,
    Prior_Set_ID = v3_prior_set_id,
    Likelihood_Family = v3_likelihood_family,
    Model_Structure = v3_model_structure,
    Output_Root = phase_root,
    save_pars_all = isTRUE(fit$save_pars$all) || (is.list(fit$save_pars) && "all" %in% names(fit$save_pars) && isTRUE(fit$save_pars$all)),
    stringsAsFactors = FALSE
  )

  diagnostics_df <- diagnostics_df %>%
    filter(!(Model_ID == row$Model_ID &
               Target_Space == row$Target_Space &
               Sample_Group == row$Sample_Group &
               Heterogeneity_Variant == row$Heterogeneity_Variant)) %>%
    bind_rows(diag_row)
  write.csv(diagnostics_df, diag_path, row.names = FALSE)

  if ((!file.exists(draws_filename) || v3_force_refit) && stacking_eligible) {
    message("Generating winsor posterior_epred and posterior_predict draws...")
    ep_draws <- posterior_epred(fit)
    pp_draws <- posterior_predict(fit)
    saveRDS(list(epred = ep_draws, predict = pp_draws), draws_filename)
    message("Saved winsor draws to: ", draws_filename)
  }
}

message("\nSaving winsor coefficient table...")
coeff_df <- data.frame(
  Model_ID = character(),
  Model_Name = character(),
  Target_Space = character(),
  Heterogeneity_Variant = character(),
  Parameter = character(),
  Estimate = double(),
  Est_Error = double(),
  CI_Lower = double(),
  CI_Upper = double(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(formulas_df))) {
  row <- formulas_df[i, ]
  model_key <- model_key_v3_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
  model_filename <- file.path(phase_root, "models", paste0("fit_", model_key, ".rds"))
  if (!file.exists(model_filename)) next
  fit <- readRDS(model_filename)
  fix_effects <- as.data.frame(fixef(fit))
  for (pname in rownames(fix_effects)) {
    coeff_df <- rbind(coeff_df, data.frame(
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Main_Stack_Inclusion = row$Main_Stack_Inclusion,
      Secondary_Robustness = row$Secondary_Robustness,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Parameter = pname,
      Estimate = fix_effects[pname, "Estimate"],
      Est_Error = fix_effects[pname, "Est.Error"],
      CI_Lower = fix_effects[pname, "Q2.5"],
      CI_Upper = fix_effects[pname, "Q97.5"],
      Prior_Set_ID = v3_prior_set_id,
      Likelihood_Family = v3_likelihood_family,
      Model_Structure = v3_model_structure,
      Output_Root = phase_root,
      stringsAsFactors = FALSE
    ))
  }
}
write.csv(coeff_df, coeff_path, row.names = FALSE)

phase3_notes <- sprintf(
  paste0(
    "Phase 3b winsorized BRMS fit notes\n",
    "Winsorized samples are read from %s/tables/.\n",
    "Outputs are written to %s/.\n",
    "Predictors are z-standardized after winsorization using winsorized sample moments.\n",
    "Sampling settings: chains=%d, iter=%d, warmup=%d, adapt_delta=%.2f, max_treedepth=%d, seed=%d.\n",
    "Prior_Set_ID: %s.\n",
    "Likelihood_Family: %s.\n",
    "Model_Structure: %s.\n",
    "Varying slopes are written separately under V3_OUTPUT_ROOT/varyslopes and are not mixed into baseline stacking weights.\n",
    "Stacking eligibility requires max Rhat <= 1.01 and divergences == 0.\n"
  ),
  v3_input_winsor_root, phase_root, chains, iter, warmup, adapt_delta, max_treedepth, seed,
  v3_prior_set_id, v3_likelihood_family, v3_model_structure
)
notes_file <- if (run_varying_slope_models) {
  file.path(phase_root, "logs", "v3_varyslopes_notes.txt")
} else {
  file.path(phase_root, "logs", "v3_phase3b_fit_notes_winsor.txt")
}
writeLines(phase3_notes, con = notes_file)

if (run_varying_slope_models) {
  empty_weights <- data.frame(
    Status = "NOT_COMPUTED_BY_PHASE_3B",
    Notes = "Varying-slope fits are a Breuer-structure robustness check. Run a separate varying-slope stacking analysis before using weights.",
    Prior_Set_ID = v3_prior_set_id,
    Likelihood_Family = v3_likelihood_family,
    Model_Structure = v3_model_structure,
    Output_Root = phase_root,
    stringsAsFactors = FALSE
  )
  write.csv(empty_weights, file.path(phase_root, "tables", "table_v3_varyslopes_loo_weights.csv"), row.names = FALSE)
}

# Write baseline run manifest
manifest_notes <- character()
if (prior_pred_override_used) {
  manifest_notes <- c(manifest_notes, "prior predictive check FAIL bypassed")
}
is_deviant_config <- !identical(v3_prior_set_id, "scale_aware_student_baseline_v1") || 
                     !identical(v3_likelihood_family, "student") || 
                     !identical(v3_model_structure, "pooled_random_intercept")
if (is_deviant_config && env_flag_v3("V3_ALLOW_DIAGNOSTIC_CONFIG", "FALSE")) {
  manifest_notes <- c(manifest_notes, sprintf("diagnostic config override used: actual prior_set_id=%s, actual family=%s, actual model_structure=%s", v3_prior_set_id, v3_likelihood_family, v3_model_structure))
}
notes_str <- if (length(manifest_notes) > 0) paste(manifest_notes, collapse = "; ") else "normal baseline run"

manifest_path <- file.path(phase_root, "manifests", "baseline_manifest.csv")
write_v3_run_manifest(
  path = manifest_path,
  scenario = "baseline",
  prior_set_id = v3_prior_set_id,
  family = v3_likelihood_family,
  model_structure = v3_model_structure,
  model_list = unique(formulas_df$Model_ID),
  seed = seed,
  sampling_config = sprintf("chains=%d;iter=%d;warmup=%d", chains, iter, warmup),
  status = "SUCCESS",
  notes = notes_str,
  input_paths = c(formulas_path, gate_csv_path)
)
message("Saved baseline manifest to: ", manifest_path)

cat("\n[SUCCESS] Phase 3b winsorized model fitting completed.\n")
