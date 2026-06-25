# -----------------------------------------------------------------------------
# Sampler and runtime configuration helpers
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

validate_rstan_cores <- function(cores, chains, context = "unknown") {
  if (length(cores) != 1L || is.na(cores) || !is.finite(cores) || as.integer(cores) < 1L) {
    stop("[BLOCKER] rstan cores must be an integer >= 1 for ", context, ".")
  }
  if (length(chains) != 1L || is.na(chains) || !is.finite(chains) || as.integer(chains) < 1L) {
    stop("[BLOCKER] brms chains must be an integer >= 1 for ", context, ".")
  }
  cores <- as.integer(cores)
  chains <- as.integer(chains)
  detected <- parallel::detectCores(logical = TRUE)
  if (!is.na(detected) && cores > detected) {
    stop(
      "[BLOCKER] Requested rstan cores (", cores, ") exceed detected logical CPU cores (",
      detected, ") for ", context, "."
    )
  }
  if (cores > chains) {
    warning(
      "[WARNING] In the current rstan backend, brms parallelizes across chains. cores > chains may not improve speed.",
      call. = FALSE
    )
  }
  invisible(cores)
}


accrual_production_sampler_defaults <- function(kind = c("baseline", "grouped_kfold", "row_kfold", "sensitivity"),
                                                run_mode = "FULL_MODE") {
  kind <- match.arg(kind)
  run_mode <- toupper(run_mode)
  if (kind %in% c("grouped_kfold", "row_kfold") && !run_mode %in% c("FULL_MODE", "FAST_MODE")) {
    stop("[BLOCKER] K-fold run_mode must be FULL_MODE or FAST_MODE.")
  }
  if (kind %in% c("grouped_kfold", "row_kfold")) {
    if (identical(run_mode, "FULL_MODE")) {
      return(list(chains = 4L, cores = 4L, iter = 12000L, warmup = 4000L,
                  adapt_delta = 0.99, max_treedepth = 15L, refresh = 500L))
    }
    return(list(chains = 2L, cores = 2L, iter = 1000L, warmup = 500L,
                adapt_delta = 0.95, max_treedepth = 12L, refresh = 500L))
  }
  if (identical(kind, "baseline")) {
    return(list(chains = 4L, cores = 4L, iter = 12000L, warmup = 4000L,
                adapt_delta = 0.99, max_treedepth = 15L, refresh = 500L))
  }
  list(chains = 4L, cores = 4L, iter = 12000L, warmup = 4000L,
       adapt_delta = 0.99, max_treedepth = 15L, refresh = 500L)
}


accrual_sampler_config <- function(kind = c("baseline", "baseline_remediation", "prior_predictive",
                                            "grouped_kfold", "row_kfold", "sensitivity",
                                            "simulation", "diagnostic_calibration"),
                                   run_mode = "FULL_MODE", varying_slopes = FALSE) {
  kind <- match.arg(kind)
  run_mode <- toupper(run_mode)
  if (kind %in% c("grouped_kfold", "row_kfold") && !run_mode %in% c("FULL_MODE", "FAST_MODE")) {
    stop("[BLOCKER] K-fold run_mode must be FULL_MODE or FAST_MODE.")
  }

  profile <- paste(kind, run_mode, sep = "_")
  if (identical(kind, "baseline")) {
    chains <- env_int("ACCRUAL_BASELINE_CHAINS", 4L, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int("ACCRUAL_BASELINE_CORES", chains, min = 1L),
      iter = env_int("ACCRUAL_BASELINE_ITER", 3000L, min = 1L),
      warmup = env_int("ACCRUAL_BASELINE_WARMUP", 1000L, min = 0L),
      adapt_delta = env_num("ACCRUAL_BASELINE_ADAPT_DELTA", if (varying_slopes) 0.99 else 0.95, min = 0),
      max_treedepth = env_int(c("ACCRUAL_BASELINE_MAX_TREEDEPTH", "ACCRUAL_BASELINE_MAX_TREEDepth"), if (varying_slopes) 15L else 12L, min = 1L),
      refresh = env_int("ACCRUAL_BASELINE_REFRESH", 500L, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = "FULL_MODE",
      sampler_profile = if (varying_slopes) "baseline_varying_slopes" else "baseline",
      config_source = "scripts/ma00_setup.R:accrual_sampler_config"
    )
  } else if (identical(kind, "baseline_remediation")) {
    chains <- env_int("ACCRUAL_REMEDIATION_CHAINS", 4L, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int("ACCRUAL_REMEDIATION_CORES", chains, min = 1L),
      iter = env_int("ACCRUAL_REMEDIATION_ITER", 8000L, min = 1L),
      warmup = env_int("ACCRUAL_REMEDIATION_WARMUP", 2000L, min = 0L),
      adapt_delta = env_num("ACCRUAL_REMEDIATION_ADAPT_DELTA", 0.99, min = 0),
      max_treedepth = env_int("ACCRUAL_REMEDIATION_MAX_TREEDEPTH", 15L, min = 1L),
      refresh = env_int("ACCRUAL_REMEDIATION_REFRESH", 500L, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = "REMEDIATION",
      sampler_profile = "baseline_remediation",
      config_source = "scripts/ma00_setup.R:accrual_sampler_config"
    )
  } else if (identical(kind, "prior_predictive")) {
    chains <- env_int("ACCRUAL_PRIOR_PRED_CHAINS", 2L, min = 1L)
    iter <- env_int(c("ACCRUAL_PRIOR_PRED_ITER", "ACCRUAL_PRIOR_PRED_N_DRAWS"), prior_pred_n_draws, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int("ACCRUAL_PRIOR_PRED_CORES", chains, min = 1L),
      iter = iter,
      warmup = env_int("ACCRUAL_PRIOR_PRED_WARMUP", min(500L, floor(iter / 2)), min = 0L),
      adapt_delta = env_num("ACCRUAL_PRIOR_PRED_ADAPT_DELTA", NA_real_, min = 0, allow_na = TRUE),
      max_treedepth = env_int("ACCRUAL_PRIOR_PRED_MAX_TREEDEPTH", NA_integer_, min = 1L, allow_na = TRUE),
      refresh = env_int("ACCRUAL_PRIOR_PRED_REFRESH", 0L, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = "FULL_MODE",
      sampler_profile = "prior_predictive",
      config_source = "scripts/ma00_setup.R:accrual_sampler_config"
    )
  } else if (kind %in% c("grouped_kfold", "row_kfold")) {
    prefix <- if (identical(kind, "grouped_kfold")) "ACCRUAL_KFOLD_FIRM" else "ACCRUAL_ROW_KFOLD"
    defaults <- accrual_production_sampler_defaults(kind, run_mode)
    chains <- env_int(paste0(prefix, "_CHAINS"), defaults$chains, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int(paste0(prefix, "_CORES"), chains, min = 1L),
      iter = env_int(paste0(prefix, "_ITER"), defaults$iter, min = 1L),
      warmup = env_int(paste0(prefix, "_WARMUP"), defaults$warmup, min = 0L),
      adapt_delta = env_num(paste0(prefix, "_ADAPT_DELTA"), defaults$adapt_delta, min = 0),
      max_treedepth = env_int(paste0(prefix, "_MAX_TREEDEPTH"), defaults$max_treedepth, min = 1L),
      refresh = env_int(paste0(prefix, "_REFRESH"), defaults$refresh, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = run_mode,
      sampler_profile = profile,
      config_source = "scripts/ma00_setup.R:accrual_production_sampler_defaults/accrual_sampler_config"
    )
  } else if (identical(kind, "sensitivity")) {
    chains <- env_int("ACCRUAL_SENS_CHAINS", 4L, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int("ACCRUAL_SENS_CORES", chains, min = 1L),
      iter = env_int("ACCRUAL_SENS_ITER", 3000L, min = 1L),
      warmup = env_int("ACCRUAL_SENS_WARMUP", 1000L, min = 0L),
      adapt_delta = env_num("ACCRUAL_SENS_ADAPT_DELTA", 0.95, min = 0),
      max_treedepth = env_int("ACCRUAL_SENS_MAX_TREEDEPTH", 12L, min = 1L),
      refresh = env_int("ACCRUAL_SENS_REFRESH", 500L, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = run_mode,
      sampler_profile = "sensitivity",
      config_source = "scripts/ma00_setup.R:accrual_sampler_config"
    )
  } else if (identical(kind, "simulation")) {
    chains <- env_int("ACCRUAL_SIM_CHAINS", 2L, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int("ACCRUAL_SIM_CORES", chains, min = 1L),
      iter = env_int("ACCRUAL_SIM_ITER", 1000L, min = 1L),
      warmup = env_int("ACCRUAL_SIM_WARMUP", 500L, min = 0L),
      adapt_delta = env_num("ACCRUAL_SIM_ADAPT_DELTA", 0.95, min = 0),
      max_treedepth = env_int("ACCRUAL_SIM_MAX_TREEDEPTH", 12L, min = 1L),
      refresh = env_int("ACCRUAL_SIM_REFRESH", 0L, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = run_mode,
      sampler_profile = "simulation",
      config_source = "scripts/ma00_setup.R:accrual_sampler_config"
    )
  } else {
    chains <- env_int("ACCRUAL_CALIBRATION_CHAINS", 4L, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int("ACCRUAL_CALIBRATION_CORES", chains, min = 1L),
      iter = env_int("ACCRUAL_CALIBRATION_ITER", 8000L, min = 1L),
      warmup = env_int("ACCRUAL_CALIBRATION_WARMUP", 2000L, min = 0L),
      adapt_delta = env_num("ACCRUAL_CALIBRATION_ADAPT_DELTA", 0.99, min = 0),
      max_treedepth = env_int("ACCRUAL_CALIBRATION_MAX_TREEDEPTH", 15L, min = 1L),
      refresh = env_int("ACCRUAL_CALIBRATION_REFRESH", 500L, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = run_mode,
      sampler_profile = "diagnostic_calibration",
      config_source = "scripts/ma00_setup.R:accrual_sampler_config"
    )
  }
  if (cfg$warmup >= cfg$iter) stop("[BLOCKER] warmup must be smaller than iter for ", kind, ".")
  if (!identical(cfg$backend, "rstan")) stop("[BLOCKER] Only brms/rstan backend is allowed in this pipeline patch. Got: ", cfg$backend)
  validate_rstan_cores(cfg$cores, cfg$chains, paste(kind, run_mode))
  cfg
}

accrual_kfold_config <- function(kind = c("grouped_firm", "row"), run_mode = NULL) {
  kind <- match.arg(kind)
  if (is.null(run_mode)) {
    run_mode <- if (identical(kind, "grouped_firm")) {
      env_value("ACCRUAL_KFOLD_FIRM_MODE", "FULL_MODE")
    } else {
      env_value("ACCRUAL_ROW_KFOLD_MODE", "FULL_MODE")
    }
  }
  run_mode <- toupper(run_mode)
  if (identical(kind, "grouped_firm")) {
    sampler <- accrual_sampler_config("grouped_kfold", run_mode = run_mode)
    c(list(K = env_int("ACCRUAL_KFOLD_FIRM_K", 5L, min = 2L), seed = accrual_seed("grouped_kfold")), sampler)
  } else {
    sampler <- accrual_sampler_config("row_kfold", run_mode = run_mode)
    c(list(K = env_int("ACCRUAL_ROW_KFOLD_K", 5L, min = 2L), seed = accrual_seed("row_kfold")), sampler)
  }
}

accrual_sampler_manifest_columns <- function() {
  c("sampler_profile", "run_mode", "config_source", "chains", "cores", "iter",
    "warmup", "adapt_delta", "max_treedepth", "refresh", "backend")
}

accrual_sampler_value_equal <- function(actual, expected, name) {
  if (name %in% c("chains", "cores", "iter", "warmup", "max_treedepth", "refresh")) {
    return(identical(as.integer(actual), as.integer(expected)))
  }
  if (identical(name, "adapt_delta")) {
    return(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-12)))
  }
  identical(as.character(actual), as.character(expected))
}

accrual_assert_kfold_manifest_matches_config <- function(tasks, kind = c("grouped_firm", "row"),
                                                         context = "unknown") {
  kind <- match.arg(kind)
  if (env_flag("ACCRUAL_ALLOW_STALE_KFOLD_MANIFEST", "FALSE")) {
    warning("[WARNING] ACCRUAL_ALLOW_STALE_KFOLD_MANIFEST=TRUE; skipping current K-fold config guard for ", context, ".", call. = FALSE)
    return(invisible(TRUE))
  }
  current <- accrual_kfold_config(kind)
  cols <- accrual_sampler_manifest_columns()
  missing_cols <- setdiff(cols, names(tasks))
  if (length(missing_cols)) {
    stop("[BLOCKER] ", context, " task manifest is missing sampler provenance column(s): ",
         paste(missing_cols, collapse = ", "), ". Rerun the planning script before fitting.")
  }
  mismatches <- character()
  for (col in cols) {
    vals <- unique(tasks[[col]])
    vals <- vals[!is.na(vals)]
    if (length(vals) != 1L || !accrual_sampler_value_equal(vals[[1]], current[[col]], col)) {
      mismatches <- c(mismatches, sprintf("%s manifest=%s current=%s", col, paste(vals, collapse = "|"), current[[col]]))
    }
  }
  if (length(mismatches)) {
    plan_script <- if (identical(kind, "grouped_firm")) "scripts/ma12a_plan_grouped_kfold_firm.R" else "scripts/ma13a_plan_row_level_exact_kfold.R"
    stop("[BLOCKER] ", context, " task manifest sampler config does not match current ma00 K-fold config: ",
         paste(mismatches, collapse = "; "), ". Rerun ", plan_script,
         " or set ACCRUAL_ALLOW_STALE_KFOLD_MANIFEST=TRUE only for an explicitly documented legacy run.")
  }
  invisible(TRUE)
}

accrual_validate_existing_fit_metadata <- function(task, context = "unknown",
                                                   compare_cols = c("chains", "cores", "iter", "warmup", "adapt_delta", "max_treedepth")) {
  if (!file.exists(task$metadata_path)) {
    return(list(reusable = FALSE, reason = paste0("metadata missing: ", task$metadata_path)))
  }
  meta <- tryCatch(read.csv(task$metadata_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(meta) || nrow(meta) == 0) {
    return(list(reusable = FALSE, reason = paste0("metadata unreadable or empty: ", task$metadata_path)))
  }
  missing_cols <- setdiff(compare_cols, names(meta))
  if (length(missing_cols)) {
    return(list(reusable = FALSE, reason = paste0("metadata missing sampler column(s): ", paste(missing_cols, collapse = ", "))))
  }
  mismatches <- character()
  for (col in compare_cols) {
    if (!accrual_sampler_value_equal(meta[[col]][1], task[[col]], col)) {
      mismatches <- c(mismatches, sprintf("%s metadata=%s manifest=%s", col, meta[[col]][1], task[[col]]))
    }
  }
  if (length(mismatches)) {
    return(list(reusable = FALSE, reason = paste(mismatches, collapse = "; ")))
  }
  list(reusable = TRUE, reason = NA_character_)
}

accrual_assert_reusable_fit_metadata <- function(task, context = "unknown") {
  state <- accrual_validate_existing_fit_metadata(task, context = context)
  if (isTRUE(state$reusable)) return(invisible(TRUE))
  if (isTRUE(force_refit)) {
    warning("[WARNING] Existing fit metadata is not reusable for ", context, "; ACCRUAL_FORCE_REFIT=TRUE so the fit will be refit. Reason: ", state$reason, call. = FALSE)
    return(invisible(FALSE))
  }
  stop("[BLOCKER] Existing fit cannot be reused for ", context, " because sampler metadata does not match the task manifest. ",
       "Reason: ", state$reason, ". Set ACCRUAL_FORCE_REFIT=TRUE to refit or rerun the planning stage if the manifest is stale.")
}

accrual_orchestrator_config <- function() {
  list(
    dry_run = env_flag("ACCRUAL_DRY_RUN", "FALSE"),
    run_heavy = env_flag("ACCRUAL_RUN_HEAVY", "FALSE"),
    allow_suppressed_tail_flags = env_flag("ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS", "FALSE"),
    data_path = env_value("ACCRUAL_DATA_PATH", data_path),
    output_root = output_root,
    accruals_root = accruals_root,
    grouped_kfold_run_root = trimws(env_value("ACCRUAL_GROUPED_KFOLD_RUN_ROOT", "")),
    row_kfold_run_root = trimws(env_value("ACCRUAL_ROW_KFOLD_RUN_ROOT", ""))
  )
}

accrual_loo_config <- function() {
  sampler <- accrual_sampler_config("baseline")
  c(
    list(
      compare_original_weights = env_flag("ACCRUAL_COMPARE_ORIGINAL_WEIGHTS", "FALSE"),
      mc_cores = 1L,
      psis_role = "secondary_to_exact_kfold"
    ),
    sampler
  )
}

accrual_kfold_filter_config <- function(kind = c("grouped_firm", "row")) {
  kind <- match.arg(kind)
  if (identical(kind, "grouped_firm")) {
    fold_raw <- env_list("ACCRUAL_KFOLD_FOLDS")
    fold_filter <- if (length(fold_raw)) suppressWarnings(as.integer(fold_raw)) else integer()
    if (any(is.na(fold_filter))) stop("[BLOCKER] ACCRUAL_KFOLD_FOLDS must be comma-separated integers.")
    target_mode <- env_choice(
      "ACCRUAL_KFOLD_TARGET_MODE",
      "MAIN_STACK_FULL",
      c("PARETO_PROBLEM_ONLY", "MAIN_STACK_FULL"),
      case = "upper"
    )
    list(
      target_space_filter = env_list("ACCRUAL_KFOLD_TARGET_SPACE"),
      model_id_filter = env_list("ACCRUAL_KFOLD_MODEL_IDS"),
      fold_filter_raw = fold_raw,
      fold_filter = fold_filter,
      target_mode = target_mode,
      run_id = gsub("[^A-Za-z0-9_.-]", "_", trimws(env_value("ACCRUAL_KFOLD_FIRM_RUN_ID", "default")))
    )
  } else {
    fold_raw <- env_list("ACCRUAL_ROW_KFOLD_FOLDS")
    fold_filter <- if (length(fold_raw)) suppressWarnings(as.integer(fold_raw)) else integer()
    if (any(is.na(fold_filter))) stop("[BLOCKER] ACCRUAL_ROW_KFOLD_FOLDS must be comma-separated integers.")
    list(
      target_space_filter = env_list("ACCRUAL_ROW_KFOLD_TARGET_SPACE"),
      model_id_filter = env_list("ACCRUAL_ROW_KFOLD_MODEL_IDS"),
      fold_filter_raw = fold_raw,
      fold_filter = fold_filter,
      run_id = gsub("[^A-Za-z0-9_.-]", "_", trimws(env_value("ACCRUAL_ROW_KFOLD_RUN_ID", "default")))
    )
  }
}

accrual_exact_kfold_run_context <- function(kind = c("grouped_firm", "row"), script_version = "split") {
  kind <- match.arg(kind)
  cfg <- accrual_kfold_config(kind)
  filter_cfg <- accrual_kfold_filter_config(kind)
  run_id <- filter_cfg$run_id
  if (!nzchar(run_id)) run_id <- "default"
  run_id <- gsub("[^A-Za-z0-9_.-]", "_", run_id)
  partial_run <- length(filter_cfg$target_space_filter) > 0 ||
    length(filter_cfg$model_id_filter) > 0 ||
    length(filter_cfg$fold_filter) > 0
  config_tag <- paste0("K", cfg$K, "_", cfg$run_mode, "_modelset_primary_v", script_version, "_", run_id)
  base_root <- file.path(output_root, if (identical(kind, "grouped_firm")) "kfold_firm" else "row_exact_kfold")
  run_root <- file.path(base_root, config_tag)
  dirs <- list(
    base = base_root,
    run = run_root,
    tables = file.path(run_root, "tables"),
    logs = file.path(run_root, "logs"),
    models = file.path(run_root, "models"),
    cache = file.path(run_root, "cache"),
    task_artifacts = file.path(run_root, "task_artifacts"),
    figures = file.path(run_root, "figures")
  )
  list(
    kind = kind,
    cfg = cfg,
    filter_cfg = filter_cfg,
    run_id = run_id,
    config_tag = config_tag,
    partial_run = partial_run,
    base_root = base_root,
    run_root = run_root,
    dirs = dirs,
    latest_run_path = file.path(base_root, "LATEST_RUN.txt"),
    latest_completed_run_path = file.path(base_root, "LATEST_COMPLETED_RUN.txt")
  )
}

accrual_extract_brms_mcmc_diagnostics <- function(fit, max_treedepth) {
  out <- list(
    max_rhat = NA_real_,
    min_ess_bulk = NA_real_,
    min_ess_tail = NA_real_,
    ess_warning = TRUE,
    divergences = NA_integer_,
    treedepth_warnings = NA_integer_
  )
  s <- tryCatch(summary(fit), error = function(e) NULL)
  if (!is.null(s)) {
    rhats <- numeric()
    ess_bulk <- numeric()
    ess_tail <- numeric()
    add_diag <- function(x, col) {
      if (!is.null(x) && col %in% colnames(x)) x[, col] else numeric()
    }
    rhats <- c(rhats, add_diag(s$fixed, "Rhat"))
    ess_bulk <- c(ess_bulk, add_diag(s$fixed, "Bulk_ESS"))
    ess_tail <- c(ess_tail, add_diag(s$fixed, "Tail_ESS"))
    if (!is.null(s$random)) {
      for (group in names(s$random)) {
        rhats <- c(rhats, add_diag(s$random[[group]], "Rhat"))
        ess_bulk <- c(ess_bulk, add_diag(s$random[[group]], "Bulk_ESS"))
        ess_tail <- c(ess_tail, add_diag(s$random[[group]], "Tail_ESS"))
      }
    }
    out$max_rhat <- suppressWarnings(max(rhats, na.rm = TRUE))
    out$min_ess_bulk <- suppressWarnings(min(ess_bulk, na.rm = TRUE))
    out$min_ess_tail <- suppressWarnings(min(ess_tail, na.rm = TRUE))
    if (!is.finite(out$max_rhat)) out$max_rhat <- NA_real_
    if (!is.finite(out$min_ess_bulk)) out$min_ess_bulk <- NA_real_
    if (!is.finite(out$min_ess_tail)) out$min_ess_tail <- NA_real_
  }
  np <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  if (!is.null(np) && all(c("Parameter", "Value") %in% names(np))) {
    out$divergences <- as.integer(sum(np$Value[np$Parameter == "divergent__"], na.rm = TRUE))
    treedepths <- np$Value[np$Parameter == "treedepth__"]
    out$treedepth_warnings <- as.integer(sum(treedepths >= as.integer(max_treedepth), na.rm = TRUE))
  }
  out$ess_warning <- is.na(out$min_ess_bulk) || is.na(out$min_ess_tail) ||
    out$min_ess_bulk < 400 || out$min_ess_tail < 400
  out
}

accrual_simulation_runtime_config <- function(kind) {
  kind <- match.arg(kind, c("lmer_pilot", "brms_leakage", "brms_recovery", "lmer_temporal", "temporal_robustness"))
  if (identical(kind, "lmer_pilot")) {
    return(list(
      t_grid = env_num_list("ACCRUAL_SIM_T_GRID", c(3, 7, 15)),
      sigma_grid = env_num_list("ACCRUAL_SIM_SIGMA_FIRM_GRID", c(0, 0.10, 0.30)),
      R = env_int("ACCRUAL_SIM_REPLICATIONS", 20L, min = 1L),
      K = env_int("ACCRUAL_SIM_K", 5L, min = 1L),
      n_firms = env_int("ACCRUAL_SIM_N_FIRMS", 200L, min = 1L),
      n_industries = env_int("ACCRUAL_SIM_N_INDUSTRIES", 10L, min = 1L),
      sigma_eps = env_num_list("ACCRUAL_SIM_SIGMA_EPS", 0.08)[1]
    ))
  }
  if (identical(kind, "brms_leakage")) {
    chains <- env_int("ACCRUAL_SIM_BRMS_CHAINS", 2L, min = 1L)
    cores <- env_int(c("ACCRUAL_SIM_BRMS_CORES", "ACCRUAL_SIM_CORES"), chains, min = 1L)
    cfg <- list(
      t_grid = env_num_list("ACCRUAL_SIM_BRMS_T_GRID", c(3, 7, 15)),
      sigma_grid = env_num_list("ACCRUAL_SIM_BRMS_SIGMA_FIRM_GRID", c(0, 0.10, 0.30)),
      R = env_int("ACCRUAL_SIM_BRMS_REPLICATIONS", 2L, min = 1L),
      K = env_int("ACCRUAL_SIM_BRMS_K", 3L, min = 1L),
      n_firms = env_int("ACCRUAL_SIM_BRMS_N_FIRMS", 80L, min = 1L),
      n_industries = env_int("ACCRUAL_SIM_BRMS_N_INDUSTRIES", 10L, min = 1L),
      sigma_eps = env_num_list("ACCRUAL_SIM_BRMS_SIGMA_EPS", 0.08)[1],
      dgp_family = env_choice("ACCRUAL_SIM_BRMS_DGP_FAMILY", "student", c("gaussian", "student"), case = "lower"),
      prior_mode = env_choice("ACCRUAL_SIM_BRMS_PRIOR_MODE", "scale_aware", c("fixed", "scale_aware"), case = "lower"),
      dgp_nu = env_num_list("ACCRUAL_SIM_BRMS_DGP_NU", 7)[1],
      chains = chains,
      cores = cores,
      iter = env_int("ACCRUAL_SIM_BRMS_ITER", 1000L, min = 1L),
      warmup = env_int("ACCRUAL_SIM_BRMS_WARMUP", 500L, min = 0L),
      adapt_delta = env_num_list("ACCRUAL_SIM_BRMS_ADAPT_DELTA", 0.95)[1],
      max_treedepth = env_int("ACCRUAL_SIM_BRMS_MAX_TREEDEPTH", 12L, min = 1L)
    )
    if (!is.finite(cfg$dgp_nu) || cfg$dgp_nu <= 2) stop("[BLOCKER] ACCRUAL_SIM_BRMS_DGP_NU must be finite and > 2.")
    validate_rstan_cores(cfg$cores, cfg$chains, "si03 brms leakage confirmation")
    return(cfg)
  }
  if (identical(kind, "brms_recovery")) {
    chains <- env_int("ACCRUAL_SIM_RECOVERY_CHAINS", 2L, min = 1L)
    cores <- env_int(c("ACCRUAL_SIM_RECOVERY_CORES", "ACCRUAL_SIM_CORES"), chains, min = 1L)
    cfg <- list(
      t_grid = env_num_list("ACCRUAL_SIM_RECOVERY_T_GRID", c(3, 7, 15)),
      sigma_grid = env_num_list("ACCRUAL_SIM_RECOVERY_SIGMA_FIRM_GRID", c(0, 0.10, 0.30)),
      R = env_int("ACCRUAL_SIM_RECOVERY_REPLICATIONS", 2L, min = 1L),
      n_firms = env_int("ACCRUAL_SIM_RECOVERY_N_FIRMS", 80L, min = 1L),
      n_industries = env_int("ACCRUAL_SIM_RECOVERY_N_INDUSTRIES", 10L, min = 1L),
      sigma_eps = env_num_list("ACCRUAL_SIM_RECOVERY_SIGMA_EPS", 0.08)[1],
      nu = env_num_list("ACCRUAL_SIM_RECOVERY_NU", 7)[1],
      chains = chains,
      cores = cores,
      iter = env_int("ACCRUAL_SIM_RECOVERY_ITER", 1000L, min = 1L),
      warmup = env_int("ACCRUAL_SIM_RECOVERY_WARMUP", 500L, min = 0L),
      adapt_delta = env_num_list("ACCRUAL_SIM_RECOVERY_ADAPT_DELTA", 0.95)[1],
      max_treedepth = env_int("ACCRUAL_SIM_RECOVERY_MAX_TREEDEPTH", 12L, min = 1L),
      sd_zero_eps = env_num_list("ACCRUAL_SIM_RECOVERY_SD_ZERO_EPS", 0.01)[1]
    )
    if (!is.finite(cfg$sd_zero_eps) || cfg$sd_zero_eps <= 0) stop("[BLOCKER] ACCRUAL_SIM_RECOVERY_SD_ZERO_EPS must be positive.")
    validate_rstan_cores(cfg$cores, cfg$chains, "si04 brms parameter recovery")
    return(cfg)
  }
  if (identical(kind, "temporal_robustness")) {
    return(list(
      t_grid = env_num_list("ACCRUAL_TEMPORAL_T_GRID", env_num_list("ACCRUAL_SIM_TEMPORAL_T_GRID", c(3, 7, 15))),
      sigma_grid = env_num_list("ACCRUAL_TEMPORAL_SIGMA_FIRM_GRID", env_num_list("ACCRUAL_SIM_TEMPORAL_SIGMA_FIRM_GRID", c(0, 0.10, 0.30))),
      rho_grid = env_num_list("ACCRUAL_TEMPORAL_RHO_GRID", env_num_list("ACCRUAL_SIM_TEMPORAL_RHO_GRID", c(0, 0.30, 0.60, 0.80))),
      R = env_int(c("ACCRUAL_TEMPORAL_REPLICATIONS", "ACCRUAL_SIM_TEMPORAL_REPLICATIONS"), 100L, min = 1L),
      K = env_int(c("ACCRUAL_TEMPORAL_K", "ACCRUAL_SIM_TEMPORAL_K"), 5L, min = 2L),
      n_firms = env_int(c("ACCRUAL_TEMPORAL_N_FIRMS", "ACCRUAL_SIM_TEMPORAL_N_FIRMS"), 80L, min = 2L),
      n_industries = env_int(c("ACCRUAL_TEMPORAL_N_INDUSTRIES", "ACCRUAL_SIM_TEMPORAL_N_INDUSTRIES"), 10L, min = 1L),
      sigma_eps = env_num_list("ACCRUAL_TEMPORAL_SIGMA_EPS", env_num_list("ACCRUAL_SIM_TEMPORAL_SIGMA_EPS", 0.08))[1],
      seed = env_int(c("ACCRUAL_TEMPORAL_SEED", "ACCRUAL_SIM_TEMPORAL_SEED", "ACCRUAL_SIM_SEED", "ACCRUAL_SEED"), accrual_seed("simulation"), min = 0L)
    ))
  }
  list(
    t_grid = env_num_list("ACCRUAL_SIM_TEMPORAL_T_GRID", c(3, 7, 15)),
    sigma_grid = env_num_list("ACCRUAL_SIM_TEMPORAL_SIGMA_FIRM_GRID", c(0, 0.10, 0.30)),
    rho_grid = env_num_list("ACCRUAL_SIM_TEMPORAL_RHO_GRID", c(0, 0.30, 0.60)),
    shock_duration_grid = env_num_list("ACCRUAL_SIM_TEMPORAL_SHOCK_DURATION_GRID", c(1, 2, 3)),
    R = env_int("ACCRUAL_SIM_TEMPORAL_REPLICATIONS", 20L, min = 1L),
    K = env_int("ACCRUAL_SIM_TEMPORAL_K", 5L, min = 1L),
    n_firms = env_int("ACCRUAL_SIM_TEMPORAL_N_FIRMS", 200L, min = 1L),
    n_industries = env_int("ACCRUAL_SIM_TEMPORAL_N_INDUSTRIES", 10L, min = 1L),
    sigma_eps = env_num_list("ACCRUAL_SIM_TEMPORAL_SIGMA_EPS", 0.08)[1],
    shock_size = env_num_list("ACCRUAL_SIM_TEMPORAL_SHOCK_SIZE", 0.20)[1]
  )
}


accrual_calibration_profile_grid <- function() {
  grid <- data.frame(
    sampler_profile = c("baseline_current", "remediation_default", "longer_warmup", "very_long_if_needed"),
    chains = c(4L, 4L, 4L, 4L),
    cores = c(4L, 4L, 4L, 4L),
    iter = c(3000L, 8000L, 12000L, 16000L),
    warmup = c(1000L, 2000L, 4000L, 6000L),
    adapt_delta = c(0.95, 0.99, 0.99, 0.99),
    max_treedepth = c(12L, 15L, 15L, 15L),
    cost_order = c(0L, 1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
  requested_profiles <- env_list("ACCRUAL_CALIBRATION_PROFILES")
  if (length(requested_profiles)) {
    missing_profiles <- setdiff(requested_profiles, grid$sampler_profile)
    if (length(missing_profiles)) {
      stop("[DI08 INPUT BLOCKER] Unknown ACCRUAL_CALIBRATION_PROFILES value(s): ",
           paste(missing_profiles, collapse = ", "))
    }
    grid <- grid[grid$sampler_profile %in% requested_profiles, , drop = FALSE]
  }
  grid$cores <- vapply(grid$chains, function(chains) env_int("ACCRUAL_CALIBRATION_CORES", chains, min = 1L), integer(1))
  invisible(mapply(validate_rstan_cores, cores = grid$cores, chains = grid$chains,
                   context = paste("di08", grid$sampler_profile)))
  grid
}

write_execution_config_registry <- function(path = file.path(method_design_root, "execution_config_registry.csv")) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  seed_note <- "All branches use canonical ACCRUAL_SEED; branch-specific seed env vars are deprecated and blocked if different."
  row <- function(scope, parameter, value, env_names, notes = "") {
    data.frame(
      Scope = scope,
      Parameter = parameter,
      Value = paste(value, collapse = ","),
      Env_Names = paste(env_names, collapse = ","),
      Notes = notes,
      stringsAsFactors = FALSE
    )
  }
  sampler_rows <- function(scope, cfg) {
    rstan_note <- paste(cfg$config_source, "rstan between-chain parallelization only; no cmdstanr/threading.")
    do.call(rbind, list(
      row(scope, "chains", cfg$chains, paste0("see_", cfg$sampler_profile), cfg$config_source),
      row(scope, "cores", cfg$cores, paste0("see_", cfg$sampler_profile), rstan_note),
      row(scope, "iter", cfg$iter, paste0("see_", cfg$sampler_profile), cfg$config_source),
      row(scope, "warmup", cfg$warmup, paste0("see_", cfg$sampler_profile), cfg$config_source),
      row(scope, "adapt_delta", cfg$adapt_delta, paste0("see_", cfg$sampler_profile), cfg$config_source),
      row(scope, "max_treedepth", cfg$max_treedepth, paste0("see_", cfg$sampler_profile), cfg$config_source),
      row(scope, "refresh", cfg$refresh, paste0("see_", cfg$sampler_profile), cfg$config_source),
      row(scope, "backend", cfg$backend, paste0("see_", cfg$sampler_profile), rstan_note),
      row(scope, "sampler_profile", cfg$sampler_profile, "", cfg$config_source)
    ))
  }
  registry <- do.call(rbind, c(
    list(
      row("baseline", "seed", accrual_seed("baseline"), c("ACCRUAL_SEED", "ACCRUAL_BASELINE_SEED"), seed_note),
      row("grouped_kfold", "seed", accrual_seed("grouped_kfold"), c("ACCRUAL_SEED", "ACCRUAL_KFOLD_FIRM_SEED"), seed_note),
      row("row_kfold", "seed", accrual_seed("row_kfold"), c("ACCRUAL_SEED", "ACCRUAL_ROW_KFOLD_SEED"), seed_note),
      row("sensitivity", "seed", accrual_seed("sensitivity"), c("ACCRUAL_SEED", "ACCRUAL_SENS_SEED"), seed_note),
      row("simulation", "seed", accrual_seed("simulation"), c("ACCRUAL_SEED", "ACCRUAL_SIM_SEED"), seed_note),
      row("model_parallel", "enabled", is_model_parallel_enabled(), "ACCRUAL_ENABLE_MODEL_PARALLEL", "Outer model-level worker pool; disabled by default."),
      row("model_parallel", "workers", env_int("ACCRUAL_MODEL_PARALLEL_WORKERS", 1L, min = 1L), "ACCRUAL_MODEL_PARALLEL_WORKERS", "Number of independent model/fold/scenario fit workers."),
      row("model_parallel", "total_core_budget", env_int("ACCRUAL_TOTAL_CORE_BUDGET", default_total_core_budget(), min = 1L), "ACCRUAL_TOTAL_CORE_BUDGET", "Budget checked against workers times cores_per_fit."),
      row("model_parallel", "backend", env_value("ACCRUAL_PARALLEL_BACKEND", "base_parallel"), "ACCRUAL_PARALLEL_BACKEND", "Allowed backend: base_parallel."),
      row("model_parallel", "retry_failed", env_flag("ACCRUAL_TASK_RETRY_FAILED", "FALSE"), "ACCRUAL_TASK_RETRY_FAILED", "Reserved task retry flag for split fit stages."),
      row("ma07_legacy_fit_adoption", "enabled", env_flag("ACCRUAL_ADOPT_LEGACY_MA07_FITS", "FALSE"), "ACCRUAL_ADOPT_LEGACY_MA07_FITS", "If TRUE, ma07a writes metadata for legacy fit artifacts with missing metadata without refitting."),
      row("grouped_kfold", "K", accrual_kfold_config("grouped_firm")$K, "ACCRUAL_KFOLD_FIRM_K"),
      row("row_kfold", "K", accrual_kfold_config("row")$K, "ACCRUAL_ROW_KFOLD_K"),
      row("model_space", "ex_post_primary_models", main_model_ids_for_space("ex_post"), "", "M08/M10 secondary; M11/M12 excluded."),
      row("model_space", "real_time_primary_models", main_model_ids_for_space("real_time"), "", "M08/M10 secondary; M11/M12 excluded.")
    ),
    list(
      sampler_rows("baseline", accrual_sampler_config("baseline")),
      sampler_rows("baseline_remediation", accrual_sampler_config("baseline_remediation")),
      sampler_rows("prior_predictive", accrual_sampler_config("prior_predictive")),
      sampler_rows("grouped_kfold_FULL_MODE", accrual_sampler_config("grouped_kfold", "FULL_MODE")),
      sampler_rows("grouped_kfold_FAST_MODE", accrual_sampler_config("grouped_kfold", "FAST_MODE")),
      sampler_rows("row_kfold_FULL_MODE", accrual_sampler_config("row_kfold", "FULL_MODE")),
      sampler_rows("row_kfold_FAST_MODE", accrual_sampler_config("row_kfold", "FAST_MODE")),
      sampler_rows("sensitivity", accrual_sampler_config("sensitivity")),
      sampler_rows("simulation", accrual_sampler_config("simulation")),
      sampler_rows("diagnostic_calibration", accrual_sampler_config("diagnostic_calibration"))
    )
  ))
  write_csv_safely(registry, path, row.names = FALSE)
  invisible(path)
}
