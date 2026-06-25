# -----------------------------------------------------------------------------
# Script: ma07b_extract_brms_fit_outputs_workers.R
# Purpose: Extract task-local diagnostics, summaries, LOO, and draw artifacts
#          from ma07a brms fits. Workers write only task-specific artifacts.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(brms)
  library(loo)
})

source("scripts/ma00_setup.R")
phase_begin("ma07b", "Extract baseline brms fit outputs with workers")
ensure_analysis_dirs()
validate_final_analysis_config("ma07b baseline fit extraction", final_mode = TRUE)

backfill_diagnostics_only <- env_flag("ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY", "FALSE")
remediation_targets_raw <- trimws(env_value("ACCRUAL_MCMC_REMEDIATION_TARGETS", ""))
remediation_targets <- if (nzchar(remediation_targets_raw)) trimws(unlist(strsplit(remediation_targets_raw, ";", fixed = TRUE))) else character()
ma07_strict_review_blocker <- env_flag("ACCRUAL_MA07_STRICT_REVIEW_BLOCKER", "FALSE")
run_varying_slope_models <- identical(model_structure, "breuer_varying_slopes")
phase_root <- if (run_varying_slope_models) varyslopes_root else output_root
for (d in file.path(phase_root, c("", "tables", "models", "draws", "logs", "manifests", "task_artifacts", "task_artifacts/ma07b"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

fit_manifest_path <- file.path(phase_root, "tables", "table_ma07_fit_task_manifest.csv")
fit_status_path <- file.path(phase_root, "tables", "table_ma07_fit_task_status.csv")
collect_manifest_path <- file.path(phase_root, "tables", "table_ma07_collect_task_manifest.csv")
collect_status_path <- file.path(phase_root, "tables", "table_ma07_collect_task_status.csv")
if (!file.exists(fit_manifest_path)) stop("[BLOCKER] Missing ma07a task manifest: ", fit_manifest_path)
if (!file.exists(fit_status_path)) stop("[BLOCKER] Missing ma07a task status table: ", fit_status_path)

fit_manifest <- read.csv(fit_manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
fit_status <- read.csv(fit_status_path, stringsAsFactors = FALSE, check.names = FALSE)
accrual_task_status_blocker(fit_status, required_col = "Main_Stack_Inclusion", context = "ma07b baseline fit extraction")

truthy <- function(x) {
  x %in% c(TRUE, "TRUE", "true", "True", "1", 1L)
}

ma07b_slug <- function(x) {
  gsub("[^A-Za-z0-9_]+", "_", as.character(x))
}

classify_ma07_mcmc_gate <- function(max_rhat, divergences, min_bulk_ess, min_tail_ess) {
  divergences <- suppressWarnings(as.numeric(divergences))
  if (!is.finite(max_rhat) || max_rhat > 1.01) return("FAIL")
  if (!is.finite(divergences) || divergences > 0) return("FAIL")
  if (!is.finite(min_bulk_ess) || min_bulk_ess < 400) return("FAIL")
  if (!is.finite(min_tail_ess) || min_tail_ess < 400) return("FAIL")
  if (min_bulk_ess < 1000 || min_tail_ess < 1000) return("REVIEW")
  "PASS"
}

ma07_mcmc_gate_reason <- function(max_rhat, divergences, min_bulk_ess, min_tail_ess) {
  divergences <- suppressWarnings(as.numeric(divergences))
  reasons <- c(
    if (!is.finite(max_rhat)) "max_rhat is non-finite" else if (max_rhat > 1.01) sprintf("max_rhat %.6f > 1.01", max_rhat),
    if (!is.finite(divergences)) "divergences is non-finite" else if (divergences > 0) sprintf("divergences=%d", as.integer(divergences)),
    if (!is.finite(min_bulk_ess)) "min_bulk_ess is non-finite" else if (min_bulk_ess < 400) sprintf("min_bulk_ess %.2f < 400", min_bulk_ess),
    if (!is.finite(min_tail_ess)) "min_tail_ess is non-finite" else if (min_tail_ess < 400) sprintf("min_tail_ess %.2f < 400", min_tail_ess),
    if (is.finite(min_bulk_ess) && min_bulk_ess >= 400 && min_bulk_ess < 1000) sprintf("min_bulk_ess %.2f below strict marker 1000", min_bulk_ess),
    if (is.finite(min_tail_ess) && min_tail_ess >= 400 && min_tail_ess < 1000) sprintf("min_tail_ess %.2f below strict marker 1000", min_tail_ess)
  )
  reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
  if (!length(reasons)) "MCMC diagnostics passed ma07 hard gate." else paste(reasons, collapse = "; ")
}

classify_loo_result <- function(fit) {
  loo_res <- tryCatch(loo::loo(fit), error = function(e) NULL)
  if (is.null(loo_res)) {
    return(list(
      pareto_k_above_07 = NA_integer_,
      elpd_loo = NA_real_,
      loo_status = "LOO_FAILED",
      loo_warning_reason = "loo() failed; elpd_loo unavailable"
    ))
  }
  pareto <- sum(loo_res$diagnostics$pareto_k > 0.7)
  list(
    pareto_k_above_07 = pareto,
    elpd_loo = loo_res$estimates["elpd_loo", "Estimate"],
    loo_status = if (pareto > 0) "PSIS_REVIEW_REQUIRED" else "PSIS_OK",
    loo_warning_reason = if (pareto > 0) "pareto_k_above_07 > 0; consider reloo, moment matching, or grouped K-fold before relying on PSIS-LOO" else NA_character_
  )
}

load_step7_sample_info <- function(row) {
  df_scaled <- read_winsor_sample(row$Target_Sample)
  company_values <- trimws(as.character(df_scaled$company))
  company_values[company_values == ""] <- NA_character_
  list(n_obs_fit = nrow(df_scaled), n_firms_fit = length(unique(company_values[!is.na(company_values)])))
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

build_missing_fit_bundle <- function(task, status_row, n_obs_fit, n_firms_fit) {
  diagnostics <- data.frame(
    Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group, Main_Stack_Inclusion = task$Main_Stack_Inclusion,
    Secondary_Robustness = task$Secondary_Robustness, Heterogeneity_Variant = task$Heterogeneity_Variant,
    N_Obs = as.integer(n_obs_fit), N_Firms = as.integer(n_firms_fit), Fit_Status = "FAILED",
    Rhat_Max = NA_real_, ESS_Min = NA_real_, Min_Tail_ESS = NA_real_, Divergences = NA_integer_,
    converged = FALSE, stacking_eligible = FALSE, max_rhat = NA_real_, divergences = NA_integer_,
    treedepth_warnings = NA_integer_, pareto_k_above_07 = NA_integer_, loo_status = "LOO_FAILED",
    loo_warning_reason = "fit artifact missing", random_intercept_sd = NA_real_, elpd_loo = NA_real_,
    error_message = if (nrow(status_row)) status_row$error_message[1] else "fit artifact missing",
    Notes = task$Reason, Prior_Set_ID = prior_set_id, Likelihood_Family = likelihood_family,
    Model_Structure = model_structure, Output_Root = phase_root, save_pars_all = FALSE,
    stringsAsFactors = FALSE
  )
  final_draw_path <- if (!is.null(task$final_draw_path)) task$final_draw_path else task$draw_path
  list(diagnostics = diagnostics, audit = empty_artifact_audit(), failures = empty_hard_gate_failures(),
       coefficients = data.frame(), draw_task_path = NA_character_, final_draw_path = final_draw_path)
}

collect_rows <- lapply(seq_len(nrow(fit_manifest)), function(i) {
  task <- fit_manifest[i, , drop = FALSE]
  key_slug <- ma07b_slug(task$task_key)
  artifact_dir <- file.path(phase_root, "task_artifacts", "ma07b", key_slug)
  data.frame(
    collect_task_index = i,
    collect_task_key = paste0("ma07b_extract_", key_slug),
    task_key = task$task_key,
    model_key = task$model_key,
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    Target_Sample = task$Target_Sample,
    Main_Stack_Inclusion = task$Main_Stack_Inclusion,
    Secondary_Robustness = task$Secondary_Robustness,
    Reason = if ("Reason" %in% names(task)) task$Reason else NA_character_,
    fit_path = task$fit_path,
    final_draw_path = task$draw_path,
    source_metadata_path = task$metadata_path,
    source_log_path = task$log_path,
    bundle_path = file.path(artifact_dir, paste0("ma07b_bundle_", key_slug, ".rds")),
    task_draw_path = file.path(artifact_dir, paste0("draws_", key_slug, ".rds")),
    task_log_path = file.path(artifact_dir, paste0("ma07b_extract_", key_slug, ".log")),
    chains = task$chains,
    cores = task$cores,
    iter = task$iter,
    warmup = task$warmup,
    adapt_delta = task$adapt_delta,
    max_treedepth = task$max_treedepth,
    refresh = if ("refresh" %in% names(task)) task$refresh else 0L,
    backend = if ("backend" %in% names(task)) task$backend else "rstan",
    RNG_Context = if ("RNG_Context" %in% names(task)) task$RNG_Context else NA_character_,
    RNG_Offset = if ("RNG_Offset" %in% names(task)) task$RNG_Offset else NA_integer_,
    Canonical_Seed = if ("Canonical_Seed" %in% names(task)) task$Canonical_Seed else NA_integer_,
    Effective_Seed = if ("Effective_Seed" %in% names(task)) task$Effective_Seed else NA_integer_,
    Required = task$Main_Stack_Inclusion,
    stringsAsFactors = FALSE
  )
})
collect_manifest <- bind_rows(collect_rows)
write_csv_safely(collect_manifest, collect_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")

fit_status_lookup <- split(fit_status, fit_status$task_key)

extract_ma07b_task_worker <- function(task) {
  suppressPackageStartupMessages({
    library(brms)
    library(loo)
  })
  source("scripts/ma00_setup.R")
  task <- as.list(task)
  started_at <- as.character(Sys.time())
  status <- "FAILED"
  reason <- NA_character_
  dir.create(dirname(task$bundle_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  log_lines <- c(
    "ma07b extraction task log",
    paste0("Task: ", task$task_key),
    paste0("Started: ", started_at),
    paste0("Fit path: ", task$fit_path),
    paste0("Bundle path: ", task$bundle_path)
  )
  result <- tryCatch({
    status_row <- fit_status_lookup[[task$task_key]]
    sample_info <- tryCatch(load_step7_sample_info(task), error = function(e) NULL)
    n_obs_fit <- if (is.null(sample_info)) NA_integer_ else sample_info$n_obs_fit
    n_firms_fit <- if (is.null(sample_info)) NA_integer_ else sample_info$n_firms_fit

    if (!file.exists(task$fit_path)) {
      bundle <- build_missing_fit_bundle(task, if (is.null(status_row)) data.frame() else status_row, n_obs_fit, n_firms_fit)
      saveRDS(bundle, task$bundle_path)
      if (truthy(task$Main_Stack_Inclusion)) {
        return(list(status = "BLOCKED_MISSING_FIT", reason = paste0("Required ma07 fit artifact is missing: ", task$fit_path), value = bundle))
      }
      return(list(status = "SUCCESS", reason = "optional fit artifact missing", value = bundle))
    }

    fit <- readRDS(task$fit_path)
    post_summary <- summary(fit)
    rhats <- post_summary$fixed[, "Rhat"]
    bulk_esses <- post_summary$fixed[, "Bulk_ESS"]
    tail_esses <- if ("Tail_ESS" %in% colnames(post_summary$fixed)) post_summary$fixed[, "Tail_ESS"] else rep(NA_real_, length(bulk_esses))
    if ("random" %in% names(post_summary) && !is.null(post_summary$random)) {
      for (group in names(post_summary$random)) {
        rhats <- c(rhats, post_summary$random[[group]][, "Rhat"])
        bulk_esses <- c(bulk_esses, post_summary$random[[group]][, "Bulk_ESS"])
        tail_esses <- c(tail_esses, if ("Tail_ESS" %in% colnames(post_summary$random[[group]])) post_summary$random[[group]][, "Tail_ESS"] else rep(NA_real_, nrow(post_summary$random[[group]])))
      }
    }
    max_rhat <- suppressWarnings(max(rhats, na.rm = TRUE)); if (!is.finite(max_rhat)) max_rhat <- NA_real_
    min_ess <- suppressWarnings(min(bulk_esses, na.rm = TRUE)); if (!is.finite(min_ess)) min_ess <- NA_real_
    min_tail_ess <- suppressWarnings(min(tail_esses, na.rm = TRUE)); if (!is.finite(min_tail_ess)) min_tail_ess <- NA_real_
    np <- brms::nuts_params(fit)
    divergences <- sum(subset(np, Parameter == "divergent__")$Value)
    treedepths <- subset(np, Parameter == "treedepth__")$Value
    treedepth_warnings <- sum(treedepths >= as.integer(task$max_treedepth))
    hard_gate_status <- classify_ma07_mcmc_gate(max_rhat, divergences, min_ess, min_tail_ess)
    hard_gate_reason <- ma07_mcmc_gate_reason(max_rhat, divergences, min_ess, min_tail_ess)
    converged <- hard_gate_status %in% c("PASS", "REVIEW")
    stacking_eligible <- converged
    random_intercept_sd <- NA_real_
    if (grepl("Firm RE", task$Heterogeneity_Variant) && !is.null(post_summary$random$company)) {
      random_intercept_sd <- post_summary$random$company["sd(Intercept)", "Estimate"]
    }
    loo_diag <- classify_loo_result(fit)
    n_obs_from_fit <- tryCatch(as.integer(stats::nobs(fit)), error = function(e) NA_integer_)
    if (is.finite(n_obs_from_fit)) n_obs_fit <- n_obs_from_fit

    blocker_for_mcmc <- truthy(task$Main_Stack_Inclusion) &&
      (identical(hard_gate_status, "FAIL") || (identical(hard_gate_status, "REVIEW") && isTRUE(ma07_strict_review_blocker)))

    draw_generation_attempted <- FALSE
    draw_generation_status <- "SKIPPED"
    draw_generation_skip_reason <- if (file.exists(task$final_draw_path)) "existing draws already present" else "draw generation not required by current mode"
    regenerate_draws <- !backfill_diagnostics_only && stacking_eligible &&
      (task$task_key %in% remediation_targets || !file.exists(task$final_draw_path) || force_refit)
    if (!blocker_for_mcmc && regenerate_draws) {
      draw_generation_attempted <- TRUE
      draw_error <- tryCatch({
        ep_draws <- brms::posterior_epred(fit)
        pp_draws <- brms::posterior_predict(fit)
        saveRDS(list(epred = ep_draws, predict = pp_draws), task$task_draw_path)
        NULL
      }, error = function(e) conditionMessage(e))
      if (is.null(draw_error) && file.exists(task$task_draw_path)) {
        draw_generation_status <- "GENERATED_TASK_LOCAL"
        draw_generation_skip_reason <- ""
      } else {
        draw_generation_status <- "FAILED"
        draw_generation_skip_reason <- paste0("draw generation failed: ", draw_error)
      }
    } else if (!stacking_eligible) {
      draw_generation_skip_reason <- "model is not stacking eligible"
    } else if (backfill_diagnostics_only) {
      draw_generation_skip_reason <- "diagnostics-only/backfill mode"
    }

    diagnostics <- data.frame(
      Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group, Main_Stack_Inclusion = task$Main_Stack_Inclusion,
      Secondary_Robustness = task$Secondary_Robustness, Heterogeneity_Variant = task$Heterogeneity_Variant,
      N_Obs = as.integer(n_obs_fit), N_Firms = as.integer(n_firms_fit), Fit_Status = "SUCCESS",
      Rhat_Max = max_rhat, ESS_Min = min_ess, Min_Tail_ESS = min_tail_ess, Divergences = divergences,
      converged = converged, stacking_eligible = stacking_eligible, max_rhat = max_rhat,
      divergences = divergences, treedepth_warnings = treedepth_warnings,
      pareto_k_above_07 = loo_diag$pareto_k_above_07, loo_status = loo_diag$loo_status,
      loo_warning_reason = loo_diag$loo_warning_reason, random_intercept_sd = random_intercept_sd,
      elpd_loo = loo_diag$elpd_loo, error_message = NA_character_, Notes = task$Reason,
      Prior_Set_ID = prior_set_id, Likelihood_Family = likelihood_family, Model_Structure = model_structure,
      Output_Root = phase_root,
      save_pars_all = isTRUE(fit$save_pars$all) || (is.list(fit$save_pars) && "all" %in% names(fit$save_pars) && isTRUE(fit$save_pars$all)),
      stringsAsFactors = FALSE
    )

    audit <- data.frame(
      Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group, Heterogeneity_Variant = task$Heterogeneity_Variant,
      diagnostic_key = task$task_key, Main_Stack_Inclusion = truthy(task$Main_Stack_Inclusion),
      Secondary_Robustness = truthy(task$Secondary_Robustness), fit_path = task$fit_path, draws_path = task$final_draw_path,
      fit_exists_before = if (!is.null(status_row) && nrow(status_row)) status_row$fit_exists_before[1] else TRUE,
      draws_exists_before = file.exists(task$final_draw_path), fit_exists_after = file.exists(task$fit_path),
      draws_exists_after = file.exists(task$final_draw_path) || file.exists(task$task_draw_path),
      Fit_Status = "SUCCESS", max_rhat = max_rhat, divergences = divergences,
      treedepth_warnings = treedepth_warnings, min_bulk_ess = min_ess, min_tail_ess = min_tail_ess,
      converged = converged, stacking_eligible = stacking_eligible,
      draw_generation_attempted = draw_generation_attempted, draw_generation_status = draw_generation_status,
      draw_generation_skip_reason = draw_generation_skip_reason, hard_gate_status = hard_gate_status,
      hard_gate_reason = hard_gate_reason, recommended_remediation_key = task$task_key, timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    failures <- empty_hard_gate_failures()
    if (blocker_for_mcmc || (truthy(task$Main_Stack_Inclusion) && hard_gate_status %in% c("PASS", "REVIEW") && !file.exists(task$final_draw_path) && !file.exists(task$task_draw_path))) {
      failures <- audit[, c(
        "diagnostic_key", "Model_ID", "Model_Name", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
        "Main_Stack_Inclusion", "hard_gate_status", "hard_gate_reason", "max_rhat", "divergences",
        "min_bulk_ess", "min_tail_ess", "fit_path", "draws_path", "fit_exists_after",
        "draws_exists_after", "recommended_remediation_key", "timestamp"
      )]
    }

    fix_effects <- as.data.frame(brms::fixef(fit))
    coefficients <- do.call(rbind, lapply(rownames(fix_effects), function(pname) {
      data.frame(
        Model_ID = task$Model_ID, Model_Name = task$Model_Name, Target_Space = task$Target_Space,
        Sample_Group = task$Sample_Group, Main_Stack_Inclusion = task$Main_Stack_Inclusion,
        Secondary_Robustness = task$Secondary_Robustness, Heterogeneity_Variant = task$Heterogeneity_Variant,
        Parameter = pname, Estimate = fix_effects[pname, "Estimate"], Est_Error = fix_effects[pname, "Est.Error"],
        CI_Lower = fix_effects[pname, "Q2.5"], CI_Upper = fix_effects[pname, "Q97.5"],
        Prior_Set_ID = prior_set_id, Likelihood_Family = likelihood_family, Model_Structure = model_structure,
        Output_Root = phase_root, stringsAsFactors = FALSE
      )
    }))
    bundle <- list(
      diagnostics = diagnostics,
      audit = audit,
      failures = failures,
      coefficients = coefficients,
      draw_task_path = if (file.exists(task$task_draw_path)) task$task_draw_path else NA_character_,
      final_draw_path = task$final_draw_path
    )
    saveRDS(bundle, task$bundle_path)
    list(status = "SUCCESS", reason = NA_character_, value = bundle)
  }, error = function(e) {
    list(status = "FAILED", reason = conditionMessage(e), value = NULL)
  })
  status <- result$status
  reason <- result$reason
  ended_at <- as.character(Sys.time())
  log_lines <- c(log_lines, paste0("Ended: ", ended_at), paste0("Status: ", status), paste0("Reason: ", reason))
  writeLines(log_lines, task$task_log_path, useBytes = TRUE)
  data.frame(
    collect_task_index = task$collect_task_index,
    collect_task_key = task$collect_task_key,
    task_key = task$task_key,
    model_key = task$model_key,
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    Main_Stack_Inclusion = task$Main_Stack_Inclusion,
    Secondary_Robustness = task$Secondary_Robustness,
    Required = task$Required,
    status = status,
    reason = reason,
    fit_path = task$fit_path,
    bundle_path = task$bundle_path,
    task_draw_path = task$task_draw_path,
    final_draw_path = task$final_draw_path,
    task_log_path = task$task_log_path,
    started_at = started_at,
    ended_at = ended_at,
    stringsAsFactors = FALSE
  )
}

task_list <- lapply(seq_len(nrow(collect_manifest)), function(i) as.list(collect_manifest[i, ]))
parallel_cfg <- accrual_fit_worker_config(
  "baseline_collect",
  cores_per_fit = if (nrow(collect_manifest)) max(as.integer(collect_manifest$cores), na.rm = TRUE) else 1L,
  context = "ma07b baseline fit output extraction"
)
statuses <- accrual_run_task_pool(
  tasks = task_list,
  worker_fun = extract_ma07b_task_worker,
  parallel_cfg = parallel_cfg,
  export_names = c(
    "truthy",
    "classify_ma07_mcmc_gate",
    "ma07_mcmc_gate_reason",
    "classify_loo_result",
    "load_step7_sample_info",
    "empty_artifact_audit",
    "empty_hard_gate_failures",
    "build_missing_fit_bundle",
    "fit_status_lookup",
    "backfill_diagnostics_only",
    "remediation_targets",
    "ma07_strict_review_blocker",
    "phase_root",
    "force_refit"
  ),
  packages = c("brms", "loo"),
  context = "ma07b baseline fit output extraction"
)
status_df <- bind_rows(statuses) %>% arrange(collect_task_index)
write_csv_safely(status_df, collect_status_path, row.names = FALSE, fileEncoding = "UTF-8")
accrual_task_status_blocker(status_df, required_col = "Required", context = "ma07b baseline fit output extraction")

cat("[SUCCESS] ma07b extraction completed. Task status: ", collect_status_path, "\n", sep = "")
phase_end("ma07b", "Extract baseline brms fit outputs with workers")
