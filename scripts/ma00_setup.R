# -----------------------------------------------------------------------------
# Script: ma00_setup.R
# Purpose: Shared setup/config + phase logging for the accrual uncertainty pipeline.
# -----------------------------------------------------------------------------

env_value <- function(name, default) {
  val <- Sys.getenv(name, unset = default)
  if (!nzchar(val)) default else val
}

env_flag <- function(name, default = "FALSE") {
  toupper(env_value(name, default)) %in% c("TRUE", "1", "YES", "Y")
}

env_list <- function(name, sep = ",", default = character()) {
  raw <- trimws(env_value(name, ""))
  if (!nzchar(raw)) return(default)
  out <- trimws(strsplit(raw, sep, fixed = TRUE)[[1]])
  out[nzchar(out)]
}

env_choice <- function(name, default, allowed, case = c("upper", "lower", "asis")) {
  case <- match.arg(case)
  value <- env_value(name, default)
  value <- switch(
    case,
    upper = toupper(value),
    lower = tolower(value),
    asis = value
  )
  allowed_cmp <- switch(
    case,
    upper = toupper(allowed),
    lower = tolower(allowed),
    asis = allowed
  )
  if (!value %in% allowed_cmp) {
    stop("[BLOCKER] ", name, " must be one of: ", paste(allowed_cmp, collapse = ", "), ".")
  }
  value
}

env_first <- function(names, default) {
  for (nm in names) {
    val <- Sys.getenv(nm, unset = "")
    if (nzchar(val)) return(val)
  }
  default
}

# --- Phase logging & timing (single source for all pipeline lines) ---
.phase_clock <- new.env(parent = emptyenv())

phase_begin <- function(phase_id, phase_label = "") {
  t0 <- Sys.time()
  assign(phase_id, t0, envir = .phase_clock)
  message(sprintf("[%s] BEGIN  %s | start=%s",
                  phase_id, phase_label, format(t0, "%Y-%m-%d %H:%M:%S %Z")))
  invisible(t0)
}

phase_end <- function(phase_id, phase_label = "") {
  t1 <- Sys.time()
  t0 <- get0(phase_id, envir = .phase_clock, ifnotfound = t1)
  secs <- as.numeric(difftime(t1, t0, units = "secs"))
  message(sprintf("[%s] END    %s | end=%s | elapsed=%.1fs (%.2f min)",
                  phase_id, phase_label, format(t1, "%H:%M:%S"), secs, secs / 60))
  log_path <- file.path(env_value("ACCRUAL_LOG_ROOT", file.path("out", "logs")), "phase_runtime_log.csv")
  tryCatch({
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    row <- data.frame(phase_id = phase_id, phase_label = phase_label,
      start_time = format(t0, "%Y-%m-%d %H:%M:%S"), end_time = format(t1, "%Y-%m-%d %H:%M:%S"),
      elapsed_seconds = round(secs, 1), elapsed_minutes = round(secs / 60, 2),
      run_date = format(Sys.Date()), stringsAsFactors = FALSE)
    write.table(row, log_path, sep = ",", row.names = FALSE,
                col.names = !file.exists(log_path), append = file.exists(log_path))
  }, error = function(e) invisible(NULL))
  invisible(secs)
}

env_int <- function(name, default, min = NULL, allow_na = FALSE) {
  raw <- if (length(name) > 1) env_first(name, as.character(default)) else env_value(name, as.character(default))
  out <- suppressWarnings(as.integer(raw))
  if (is.na(out)) {
    if (allow_na) return(NA_integer_)
    stop("[BLOCKER] Invalid integer environment value for ", paste(name, collapse = "/"), ": ", raw)
  }
  if (!is.null(min) && out < min) {
    stop("[BLOCKER] Environment value for ", paste(name, collapse = "/"), " must be >= ", min, ". Got: ", out)
  }
  out
}

env_num <- function(name, default, min = NULL, allow_na = FALSE) {
  raw <- if (length(name) > 1) env_first(name, as.character(default)) else env_value(name, as.character(default))
  out <- suppressWarnings(as.numeric(raw))
  if (is.na(out)) {
    if (allow_na) return(NA_real_)
    stop("[BLOCKER] Invalid numeric environment value for ", paste(name, collapse = "/"), ": ", raw)
  }
  if (!is.null(min) && out < min) {
    stop("[BLOCKER] Environment value for ", paste(name, collapse = "/"), " must be >= ", min, ". Got: ", out)
  }
  out
}

env_num_list <- function(name, default, sep = ",") {
  raw <- env_list(name, sep = sep, default = character())
  if (!length(raw)) return(as.numeric(default))
  out <- suppressWarnings(as.numeric(raw))
  if (any(is.na(out))) stop("[BLOCKER] ", name, " must be ", sep, "-separated numeric values.")
  out
}

env_int_list <- function(name, default, sep = ",", min = NULL) {
  raw <- env_list(name, sep = sep, default = character())
  if (!length(raw)) return(as.integer(default))
  out <- suppressWarnings(as.integer(raw))
  if (any(is.na(out))) stop("[BLOCKER] ", name, " must be ", sep, "-separated integer values.")
  if (!is.null(min) && any(out < min)) stop("[BLOCKER] ", name, " values must be >= ", min, ".")
  out
}

data_path <- env_value("ACCRUAL_DATA_PATH", file.path("data", "raw", "data.xlsx"))
baseline_root <- env_value("ACCRUAL_BASELINE_ROOT", file.path("out", "interim", "baseline"))
output_root <- env_value("ACCRUAL_OUTPUT_ROOT", file.path("out", "interim", "winsor"))
input_winsor_root <- env_value("ACCRUAL_INPUT_WINSOR_ROOT", file.path("out", "interim", "winsor"))
reports_root <- env_value("ACCRUAL_REPORTS_ROOT", "reports")
accruals_root <- env_value("ACCRUAL_ACCRUALS_ROOT", "accruals")
method_design_root <- env_value("ACCRUAL_METHOD_DESIGN_ROOT", file.path("out", "manifests", "method_design"))
prior_set_id <- env_value("ACCRUAL_PRIOR_SET_ID", "scale_aware_student_baseline_v1")
likelihood_family <- tolower(env_value("ACCRUAL_FAMILY", "student"))
model_structure <- env_value("ACCRUAL_MODEL_STRUCTURE", "pooled_random_intercept")
run_varying_slopes <- env_flag("ACCRUAL_RUN_VARYING_SLOPES", "FALSE")
varyslope_scope <- toupper(env_value("ACCRUAL_VARYSLOPE_SCOPE", "LEADING_ONLY"))
varyslope_group <- env_value("ACCRUAL_VARYSLOPE_GROUP", "industry_year")
force_refit <- env_flag("ACCRUAL_FORCE_REFIT", "FALSE")
prior_predictive_mode <- toupper(env_value("ACCRUAL_PRIOR_PREDICTIVE_MODE", "REPRESENTATIVE"))
prior_pred_n_draws <- as.integer(env_value("ACCRUAL_PRIOR_PRED_N_DRAWS", "1000"))
stacking_mixture_draws <- as.integer(env_value("ACCRUAL_STACKING_MIXTURE_DRAWS", "8000"))

if (is.na(prior_pred_n_draws) || prior_pred_n_draws <= 0) prior_pred_n_draws <- 1000L
if (is.na(stacking_mixture_draws) || stacking_mixture_draws <= 0) stacking_mixture_draws <- 8000L
if (!likelihood_family %in% c("gaussian", "student")) {
  stop("[BLOCKER] ACCRUAL_FAMILY must be 'gaussian' or 'student'.")
}

winsor_root <- output_root
varyslopes_root <- file.path(output_root, "varyslopes")

baseline_dirs <- file.path(
  baseline_root,
  c("", "tables", "models", "draws", "figures", "logs", "validation", "appendix")
)

winsor_dirs <- file.path(
  winsor_root,
  c(
    "",
    "tables",
    "models",
    "draws",
    "draws/loo_cache",
    "figures",
    "logs",
    "validation",
    "lofo",
    "lofo/tables",
    "lofo/logs",
    "lofo/figures",
    "lofo/cache",
    "kfold_firm",
    "sensitivity",
    "sensitivity/tables",
    "sensitivity/logs",
    "sensitivity/manifests",
    "sensitivity/reports",
    "sensitivity/cache",
    "varyslopes",
    "varyslopes/tables",
    "varyslopes/models",
    "varyslopes/draws",
    "varyslopes/logs",
    "varyslopes/figures",
    "varyslopes/cache"
  )
)

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
    source = "reports/chapter_3_method_only_reviewer_final_journal_style_transitions.md"
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

accrual_base_seed <- function() {
  env_int("ACCRUAL_SEED", 42L, min = 0L)
}

accrual_seed <- function(kind = c("baseline", "grouped_kfold", "row_kfold", "sensitivity", "simulation"), default = NULL) {
  kind <- match.arg(kind)
  base <- accrual_base_seed()
  legacy_env <- switch(
    kind,
    baseline = "ACCRUAL_BASELINE_SEED",
    grouped_kfold = "ACCRUAL_KFOLD_FIRM_SEED",
    row_kfold = "ACCRUAL_ROW_KFOLD_SEED",
    sensitivity = "ACCRUAL_SENS_SEED",
    simulation = "ACCRUAL_SIM_SEED"
  )
  if (!is.null(default)) {
    default_value <- suppressWarnings(as.integer(default))
    if (is.na(default_value)) {
      stop("[BLOCKER] Invalid deprecated default override for accrual_seed(", kind, "): ", default)
    }
    if (!identical(default_value, base)) {
      stop("[BLOCKER] Deprecated default override for accrual_seed(", kind, ")=", default_value,
           " differs from canonical ACCRUAL_SEED=", base, ".")
    }
  }

  legacy_raw <- Sys.getenv(legacy_env, unset = "")
  if (nzchar(legacy_raw)) {
    legacy_value <- suppressWarnings(as.integer(legacy_raw))
    if (is.na(legacy_value)) {
      stop("[BLOCKER] Invalid integer seed in deprecated ", legacy_env, ": ", legacy_raw)
    }
    if (!identical(legacy_value, base)) {
      stop("[BLOCKER] Branch-specific seed ", legacy_env, "=", legacy_value,
           " differs from canonical ACCRUAL_SEED=", base,
           ". Use one common seed to avoid branch-specific tuning/cherry-picking risk.")
    }
    warning("[WARNING] ", legacy_env, " is deprecated. Use ACCRUAL_SEED only.", call. = FALSE)
  }

  base
}

normalize_accrual_seed_offset <- function(offset = 0L, context = "unknown") {
  offset_value <- suppressWarnings(as.integer(offset))
  if (length(offset_value) != 1 || is.na(offset_value)) {
    stop("[BLOCKER] Seed offset for ", context, " must be one integer value. Got: ", paste(offset, collapse = ", "))
  }
  offset_value
}

accrual_seed_for <- function(context, offset = 0L) {
  if (missing(context) || !nzchar(trimws(as.character(context)))) {
    stop("[BLOCKER] accrual_seed_for() requires a non-empty context label.")
  }
  accrual_base_seed() + normalize_accrual_seed_offset(offset, context)
}

set_accrual_seed <- function(context, offset = 0L) {
  seed_value <- accrual_seed_for(context, offset = offset)
  base::set.seed(seed_value)
  invisible(seed_value)
}

accrual_rng_metadata_list <- function(context = "global", offset = 0L) {
  list(
    RNG_Context = context,
    RNG_Offset = normalize_accrual_seed_offset(offset, context),
    Canonical_Seed = accrual_base_seed(),
    Effective_Seed = accrual_seed_for(context, offset),
    RNG_Source = "scripts/ma00_setup.R"
  )
}

accrual_rng_metadata <- function(context = "global", offset = 0L) {
  as.data.frame(accrual_rng_metadata_list(context, offset), stringsAsFactors = FALSE)
}

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

is_model_parallel_enabled <- function() {
  env_flag("ACCRUAL_ENABLE_MODEL_PARALLEL", "FALSE")
}

stable_task_key <- function(...) {
  parts <- vapply(list(...), as.character, character(1))
  paste(parts, collapse = "|")
}

default_total_core_budget <- function() {
  physical <- parallel::detectCores(logical = FALSE)
  if (!is.na(physical) && is.finite(physical) && physical >= 1L) return(as.integer(physical))
  logical <- parallel::detectCores(logical = TRUE)
  if (!is.na(logical) && is.finite(logical) && logical >= 1L) return(as.integer(logical))
  1L
}

validate_model_parallel_budget <- function(workers, cores_per_fit, total_core_budget, context = "unknown") {
  workers <- as.integer(workers)
  cores_per_fit <- as.integer(cores_per_fit)
  total_core_budget <- as.integer(total_core_budget)
  if (is.na(workers) || workers < 1L) stop("[BLOCKER] ACCRUAL_MODEL_PARALLEL_WORKERS must be >= 1 for ", context, ".")
  if (is.na(cores_per_fit) || cores_per_fit < 1L) stop("[BLOCKER] cores_per_fit must be >= 1 for ", context, ".")
  if (is.na(total_core_budget) || total_core_budget < 1L) stop("[BLOCKER] ACCRUAL_TOTAL_CORE_BUDGET must be >= 1 for ", context, ".")
  requested <- workers * cores_per_fit
  if (requested > total_core_budget) {
    stop(
      "[BLOCKER] Model-parallel core request exceeds budget for ", context, ": workers * cores_per_fit = ",
      workers, " * ", cores_per_fit, " = ", requested, " > ACCRUAL_TOTAL_CORE_BUDGET=", total_core_budget, "."
    )
  }
  logical <- parallel::detectCores(logical = TRUE)
  if (!is.na(logical) && is.finite(logical) && requested >= 0.9 * logical) {
    warning(
      "[WARNING] Model-parallel core request is close to detected logical cores for ", context,
      ": requested=", requested, ", logical_cores=", logical, ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

accrual_model_parallel_config <- function(cores_per_fit, context = "unknown") {
  backend <- env_value("ACCRUAL_PARALLEL_BACKEND", "base_parallel")
  if (!backend %in% c("base_parallel")) {
    stop("[BLOCKER] ACCRUAL_PARALLEL_BACKEND must be one of: base_parallel.")
  }
  enabled <- is_model_parallel_enabled()
  workers <- env_int("ACCRUAL_MODEL_PARALLEL_WORKERS", 1L, min = 1L)
  if (!enabled) workers <- 1L
  total_core_budget <- env_int("ACCRUAL_TOTAL_CORE_BUDGET", default_total_core_budget(), min = 1L)
  validate_model_parallel_budget(workers, cores_per_fit, total_core_budget, context)
  if (enabled && workers > 1L && identical(.Platform$OS.type, "windows") &&
      as.integer(cores_per_fit) > 1L) {
    if (!env_flag("ACCRUAL_ALLOW_NESTED_RSTAN_CORES", "FALSE")) {
      stop(
        "[BLOCKER] Model-level PSOCK workers with rstan cores_per_fit > 1 on Windows require ",
        "ACCRUAL_ALLOW_NESTED_RSTAN_CORES=TRUE for ", context, ". ",
        "Set cores_per_fit=1 or explicitly opt in after confirming the nested rstan chain parallelism is stable."
      )
    }
    warning(
      "[WARNING] ACCRUAL_ALLOW_NESTED_RSTAN_CORES=TRUE is enabled for ", context,
      ". Total active cores are workers * cores_per_fit; monitor rstan/PSOCK stability on Windows.",
      call. = FALSE
    )
  }
  list(
    enabled = enabled,
    workers = workers,
    cores_per_fit = as.integer(cores_per_fit),
    total_core_budget = total_core_budget,
    backend = backend,
    retry_failed = env_flag("ACCRUAL_TASK_RETRY_FAILED", "FALSE")
  )
}

accrual_fit_worker_config <- function(kind, cores_per_fit, context = "unknown") {
  cfg <- accrual_model_parallel_config(cores_per_fit = cores_per_fit, context = context)
  cfg$context <- context
  cfg$fit_kind <- kind
  cfg
}

accrual_run_task_pool <- function(tasks, worker_fun, parallel_cfg,
                                  export_names = character(), packages = character(),
                                  context = "unknown") {
  if (!length(tasks)) return(list())
  if (!isTRUE(parallel_cfg$enabled) || as.integer(parallel_cfg$workers) <= 1L) {
    return(lapply(tasks, worker_fun))
  }
  if (!identical(parallel_cfg$backend, "base_parallel")) {
    stop("[BLOCKER] Unsupported model-parallel backend for ", context, ": ", parallel_cfg$backend)
  }
  message("[WORKER POOL] ", context, ": workers=", parallel_cfg$workers,
          ", cores_per_fit=", parallel_cfg$cores_per_fit,
          ", total_core_budget=", parallel_cfg$total_core_budget)
  cl <- parallel::makeCluster(as.integer(parallel_cfg$workers))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    source("scripts/ma00_setup.R")
    NULL
  })
  if (length(packages)) {
    parallel::clusterCall(cl, function(pkgs) {
      for (pkg in pkgs) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      }
      NULL
    }, packages)
  }
  worker_env <- environment()
  parallel::clusterExport(cl, varlist = "worker_fun", envir = worker_env)
  if (length(export_names)) {
    parallel::clusterExport(cl, varlist = unique(export_names), envir = parent.frame())
  }
  parallel::parLapplyLB(cl, tasks, worker_fun)
}

accrual_task_status_blocker <- function(status_df, required_col = "Main_Stack_Inclusion",
                                        context = "unknown") {
  if (!nrow(status_df)) return(invisible(TRUE))
  status_col <- if ("status" %in% names(status_df)) {
    "status"
  } else if ("Status" %in% names(status_df)) {
    "Status"
  } else {
    stop("[BLOCKER] Task status table for ", context, " has no status column.")
  }
  blocked_statuses <- c(
    "FAILED",
    "BLOCKED_METADATA_MISSING",
    "BLOCKED_METADATA_MISMATCH",
    "BLOCKED_MISSING_FIT",
    "BLOCKED_MISSING_NON_TARGET_FIT",
    "BLOCKED_BACKFILL_MISSING_FIT"
  )
  required <- if (required_col %in% names(status_df)) {
    status_df[[required_col]] %in% c(TRUE, "TRUE", "true", "1", 1L)
  } else {
    rep(TRUE, nrow(status_df))
  }
  blocked <- status_df[[status_col]] %in% blocked_statuses
  if (any(required & blocked, na.rm = TRUE)) {
    key_col <- intersect(c("task_key", "Task_Key", "model_key", "Model_ID"), names(status_df))[1]
    keys <- if (!is.na(key_col)) status_df[[key_col]][required & blocked] else which(required & blocked)
    stop("[BLOCKER] Required task(s) failed or were blocked in ", context, ": ",
         paste(keys, collapse = "; "))
  }
  invisible(TRUE)
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
    full_defaults <- run_mode == "FULL_MODE"
    chains <- env_int(paste0(prefix, "_CHAINS"), if (full_defaults) 4L else 2L, min = 1L)
    cfg <- list(
      chains = chains,
      cores = env_int(paste0(prefix, "_CORES"), chains, min = 1L),
      iter = env_int(paste0(prefix, "_ITER"), if (full_defaults) 3000L else 1000L, min = 1L),
      warmup = env_int(paste0(prefix, "_WARMUP"), if (full_defaults) 1000L else 500L, min = 0L),
      adapt_delta = env_num(paste0(prefix, "_ADAPT_DELTA"), 0.95, min = 0),
      max_treedepth = env_int(paste0(prefix, "_MAX_TREEDEPTH"), 12L, min = 1L),
      refresh = env_int(paste0(prefix, "_REFRESH"), 500L, min = 0L),
      backend = env_value("ACCRUAL_BRMS_BACKEND", "rstan"),
      run_mode = run_mode,
      sampler_profile = profile,
      config_source = "scripts/ma00_setup.R:accrual_sampler_config"
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

accrual_kfold_config <- function(kind = c("grouped_firm", "row"), run_mode = "FULL_MODE") {
  kind <- match.arg(kind)
  if (identical(kind, "grouped_firm")) {
    sampler <- accrual_sampler_config("grouped_kfold", run_mode = run_mode)
    c(list(K = env_int("ACCRUAL_KFOLD_FIRM_K", 5L, min = 2L), seed = accrual_seed("grouped_kfold")), sampler)
  } else {
    sampler <- accrual_sampler_config("row_kfold", run_mode = run_mode)
    c(list(K = env_int("ACCRUAL_ROW_KFOLD_K", 5L, min = 2L), seed = accrual_seed("row_kfold")), sampler)
  }
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
      fold_filter = fold_filter
    )
  }
}

accrual_simulation_runtime_config <- function(kind) {
  kind <- match.arg(kind, c("lmer_pilot", "brms_leakage", "brms_recovery", "lmer_temporal"))
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
  write.csv(registry, path, row.names = FALSE)
  invisible(path)
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

sensitivity_root <- function(scenario = NULL, root = output_root) {
  base <- file.path(root, "sensitivity")
  if (is.null(scenario) || !nzchar(scenario)) return(base)
  file.path(base, scenario)
}

ensure_sensitivity_dirs <- function(scenario = NULL, root = output_root) {
  base_dirs <- file.path(sensitivity_root(NULL, root), c("", "tables", "logs", "manifests", "reports", "cache"))
  for (d in base_dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  scenarios <- if (is.null(scenario) || !nzchar(scenario)) sensitivity_scenario_ids else scenario
  subdirs <- c("", "prior_predictive", "fits", "models", "draws", "diagnostics", "stacking", "DA", "validation", "logs", "manifests", "cache", "tables")
  for (sc in scenarios) {
    for (d in file.path(sensitivity_root(sc, root), subdirs)) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(TRUE)
}

ensure_baseline_dirs <- function() {
  for (d in baseline_dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

baseline_table_path <- function(file_name) {
  file.path(baseline_root, "tables", file_name)
}

baseline_log_path <- function(file_name) {
  file.path(baseline_root, "logs", file_name)
}

normalize_column_names_safely <- function(column_names) {
  text <- as.character(column_names)
  text <- enc2utf8(text)
  text[is.na(text)] <- ""
  transliterated <- suppressWarnings(iconv(text, from = "", to = "ASCII//TRANSLIT"))
  transliterated[is.na(transliterated)] <- text[is.na(transliterated)]
  normalized <- tolower(trimws(transliterated))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("_+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized
}

format_available_columns <- function(column_names) {
  paste(as.character(column_names), collapse = ", ")
}

normalize_join_key_values <- function(values) {
  normalized <- trimws(as.character(values))
  normalized[normalized == ""] <- NA_character_
  normalized
}

detect_column_from_candidates <- function(column_names, candidates, context_label = "required column") {
  available_columns <- as.character(column_names)
  available_normalized <- normalize_column_names_safely(available_columns)
  candidate_normalized <- normalize_column_names_safely(candidates)

  for (candidate in candidates) {
    exact_hits <- which(trimws(available_columns) == trimws(candidate))
    if (length(exact_hits) == 1) {
      return(available_columns[[exact_hits]])
    }
    if (length(exact_hits) > 1) {
      stop(
        "[BLOCKER] Ambiguous ", context_label, ": multiple exact matches for '", candidate,
        "'. Available columns: ", format_available_columns(available_columns)
      )
    }
  }

  for (i in seq_along(candidates)) {
    hits <- which(available_normalized == candidate_normalized[[i]])
    if (length(hits) == 1) {
      return(available_columns[[hits]])
    }
    if (length(hits) > 1) {
      stop(
        "[BLOCKER] Ambiguous ", context_label, ": candidate '", candidates[[i]],
        "' matched multiple columns after normalization. Available columns: ",
        format_available_columns(available_columns)
      )
    }
  }

  stop(
    "[BLOCKER] Could not identify ", context_label, ". Available columns: ",
    format_available_columns(available_columns)
  )
}

detect_metadata_company_column <- function(column_names) {
  candidates <- c(
    "company", "Company", "ticker", "Ticker", "symbol", "Symbol", "code", "Code",
    "Ma", "MÃ£", "MÃ£ CK", "Ma CK", "StockCode", "Stock_Code"
  )
  detect_column_from_candidates(
    column_names = column_names,
    candidates = candidates,
    context_label = "metadata company-code column"
  )
}

reports_path <- function(...) {
  file.path(reports_root, ...)
}

baseline_accruals_path <- function(file_name = "final_uncertainty_adjusted_accruals_winsor.csv") {
  file.path(accruals_root, "baseline", file_name)
}

sensitivity_accruals_path <- function(scenario, file_name = NULL) {
  if (is.null(file_name) || !nzchar(file_name)) {
    file_name <- paste0("final_sensitivity_uncertainty_adjusted_accruals_", scenario, ".csv")
  }
  file.path(accruals_root, "sensitivity", scenario, file_name)
}

continuous_vars_to_winsor <- c(
  "TA_scaled",
  "inv_A_lag",
  "dREV_scaled",
  "dREC_scaled",
  "dREV_dREC_scaled",
  "PPE_scaled",
  "ROA_lag",
  "ROA_curr",
  "CFO_lag_scaled",
  "CFO_curr_scaled",
  "CFO_lead_scaled",
  "Size",
  "operating_cycle",
  "sales_growth",
  "revenue_growth",
  "sd_REV",
  "sd_CFO"
)

binary_vars_do_not_winsor <- c("NEG_CFO", "NEG_EARN")

pred_vars <- c(
  "inv_A_lag", "dREV_scaled", "dREC_scaled", "dREV_dREC_scaled", "PPE_scaled",
  "ROA_lag", "CFO_lag_scaled", "CFO_curr_scaled", "CFO_lead_scaled", "Size",
  "operating_cycle", "sales_growth", "sd_REV", "sd_CFO"
)

appendix1_vars <- c(
  "TA_scaled",
  "dREV_scaled",
  "dREC_scaled",
  "dREV_dREC_scaled",
  "PPE_scaled",
  "CFO_lag_scaled",
  "CFO_curr_scaled",
  "CFO_lead_scaled",
  "ROA_lag",
  "Size",
  "operating_cycle",
  "sales_growth"
)

ensure_analysis_dirs <- function() {
  for (d in winsor_dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
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
  write.csv(prior_registry(), out, row.names = FALSE)
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
  write.csv(manifest, path, row.names = FALSE)
  path
}

write_pipeline_index <- function() {
  dir.create(method_design_root, recursive = TRUE, showWarnings = FALSE)
  pipeline <- data.frame(
    Order = c(sprintf("ma%02d", 0:6), "ma07a", "ma07b", sprintf("ma%02d", 8:16), "di02", "ma17", "ro01", sprintf("se%02d", 1:7), sprintf("si%02d", 0:4), "di01"),
    Script = c(
      "scripts/ma00_setup.R",
      "scripts/ma01_setup_and_registry.R",
      "scripts/ma02_build_common_sample.R",
      "scripts/ma03_audit_data_integrity.R",
      "scripts/ma04_define_named_models.R",
      "scripts/ma05_winsorize_common_samples.R",
      "scripts/ma06_prior_predictive_checks.R",
      "scripts/ma07a_fit_brms_named_models.R",
      "scripts/ma07b_collect_brms_fit_outputs.R",
      "scripts/ma08_mcmc_diagnostics.R",
      "scripts/ma09_loo_stacking.R",
      "scripts/ma10_construct_psis_loo_DA.R",
      "scripts/ma11_posterior_predictive_checks.R",
      "scripts/ma12_grouped_kfold_firm.R",
      "scripts/ma13_row_level_exact_kfold.R",
      "scripts/ma14_construct_exact_kfold_DA.R",
      "scripts/ma15_audit_DA_finite_outputs.R",
      "scripts/ma16_validate_outcomes.R",
      "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R",
      "scripts/ma17_export_tables_figures.R",
      "scripts/robustness/ro01_lofo_stacking.R",
      "scripts/sensitivity/se01_prior_predictive.R",
      "scripts/sensitivity/se02_refit_prior_scenarios.R",
      "scripts/sensitivity/se03_mcmc_diagnostics.R",
      "scripts/sensitivity/se04_stacking.R",
      "scripts/sensitivity/se05_construct_DA.R",
      "scripts/sensitivity/se06_validation.R",
      "scripts/sensitivity/se07_report.R",
      "scripts/simulation/si00_helpers.R",
      "scripts/simulation/si01_lmer_pilot_run.R",
      "scripts/simulation/si02_lmer_pilot_report.R",
      "scripts/simulation/si03_brms_leakage_confirmation.R",
      "scripts/simulation/si04_brms_parameter_recovery.R",
      "scripts/diagnostics/di01_psis_reliability_gate.R"
    ),
    Role = c(
      "Shared helpers and registries",
      "Setup and model registry",
      "Build common samples",
      "COGS/INV audit",
      "Define model formulas",
      "Winsorize common samples",
      "Baseline prior predictive checks",
      "Baseline brms fit worker stage",
      "Collect baseline brms fit outputs",
      "Baseline MCMC diagnostics",
      "Baseline LOO stacking",
      "Secondary PSIS/LOO uncertainty-adjusted DA",
      "Posterior predictive checks for secondary PSIS/LOO DA",
      "Baseline exact grouped K-fold",
      "Main row-level exact K-fold method-matching arm",
      "Primary exact-KFoldW DA construction from completed-run pins",
      "Hard finite-output gate for exact-KFold DA",
      "Primary validation on exact row-KFold DA",
      "Main new-firm predictive integration reporting gate",
      "Chapter 3 manuscript table export",
      "Optional grouped PSIS-LOFO robustness",
      "Sensitivity prior predictive gate",
      "Sensitivity full refits by prior scenario",
      "Sensitivity MCMC diagnostics gate",
      "Sensitivity LOO/stacking by scenario",
      "Sensitivity DA reconstruction",
      "Sensitivity validation/outcome tests",
      "Sensitivity report",
      "Simulation helper functions for leakage pilot scripts",
      "LMER leakage pilot simulation run",
      "LMER leakage pilot simulation report",
      "BRMS leakage confirmation simulation",
      "BRMS parameter recovery simulation",
      "Optional secondary PSIS reliability diagnostics"
    ),
    Active = TRUE,
    stringsAsFactors = FALSE
  )
  write.csv(pipeline, file.path(method_design_root, "pipeline_index.csv"), row.names = FALSE)

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
    "Sampler protocol: Chapter 3 specifies 4 chains, 3000 iterations, 1000 warmup iterations, fixed seed 42, adapt_delta = 0.95, and max_treedepth = 12 for brms/Stan estimation. Baseline full-sample fits, exact K-fold refits, and sensitivity refits use those defaults unless explicitly overridden and recorded in manifests. FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference.",
    "",
    "Execution configuration is centralized in `scripts/ma00_setup.R`: `accrual_base_seed()` and `accrual_seed()` enforce one canonical seed (`ACCRUAL_SEED`, default 42) across baseline, grouped exact K-fold, row exact K-fold, sensitivity, and simulation branches; `accrual_seed_for()` derives deterministic context-specific offsets from that same canonical seed; `set_accrual_seed()` is the only helper that calls base `set.seed()`; `accrual_sampler_config()` supplies sampler settings; `accrual_kfold_config()` supplies exact K-fold K/seed/sampler settings; and `main_model_ids_for_space()` supplies primary model IDs. Branch-specific seed env vars (`ACCRUAL_BASELINE_SEED`, `ACCRUAL_KFOLD_FIRM_SEED`, `ACCRUAL_ROW_KFOLD_SEED`, `ACCRUAL_SENS_SEED`, `ACCRUAL_SIM_SEED`) are deprecated and blocked if they differ from `ACCRUAL_SEED`. The helper writes `out/manifests/method_design/execution_config_registry.csv`.",
    "",
    "Primary model helpers return M01-M07 for ex-post and M01, M02, M03, M07, M09 for real-time/no-lookahead. M08/M10 remain secondary/robustness unless explicitly included through documented secondary flows, and M11/M12 remain excluded from active primary helpers.",
    "",
    "`Rscript run.R` runs the `main` target by default. The main target includes grouped exact firm K-fold (`scripts/ma12_grouped_kfold_firm.R`) and row-level exact K-fold (`scripts/ma13_row_level_exact_kfold.R`) as adjacent primary RQ1 evidence steps, then constructs primary exact-KFoldW DA (`scripts/ma14_construct_exact_kfold_DA.R`), applies the finite-output gate (`scripts/ma15_audit_DA_finite_outputs.R`), runs validation on the primary exact row-KFold DA, the new-firm predictive integration reporting gate, and the corrected Chapter 3 manuscript export path `scripts/ma17_export_tables_figures.R`.",
    "",
    "`scripts/ma10_construct_psis_loo_DA.R` remains the PSIS/LOO secondary DA constructor, including secondary validation panels only. Scripts `ma12` and `ma13` write `LATEST_COMPLETED_RUN.txt` only for completed primary-eligible exact-refit runs, and script `ma14` uses those pins or explicit run-root environment variables instead of moving `LATEST_RUN.txt` for primary inference. `LATEST_RUN.txt` is operational only and should not be used as primary provenance. Scripts `ma12` and `ma13` write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.",
    "",
    "`scripts/ma14_construct_exact_kfold_DA.R` refuses completed-run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. It writes file-size/mtime/hash source manifests, draw-file hash manifests, and `table_model_primary_inclusion_gate.csv`. MCMC `FAIL`/`LOW_RELIABILITY` models are excluded from primary exact-KFold DA; `REVIEW`/`CAUTION` models can be retained only with `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS`.",
    "",
    "`scripts/ma15_audit_DA_finite_outputs.R` writes `table_DA_finite_gate_decision.csv` and is a hard RQ2/export gate. Script `di02` is a hard new-firm tail-suppression gate; if unverified Firm-RE out-of-firm posterior predictive tail quantities require suppression, export stops unless the explicit suppression override is set and the outputs are labelled non-primary. `Rscript run.R all --dry-run` de-duplicates `scripts/diagnostics/di02_new_firm_predictive_integration_audit.R` so the new-firm audit appears once.",
    "",
    "LOFO (`scripts/robustness/ro01_lofo_stacking.R`) is an opt-in robustness branch, not a default main step. Sensitivity scripts se01-se07 and simulation scripts si00-si04 are opt-in branches. PSIS reliability (`scripts/diagnostics/di01_psis_reliability_gate.R`) is secondary diagnostics, not the primary RQ1 comparison.",
    "",
    paste0("The machine-readable pipeline index is written to `", file.path(method_design_root, "pipeline_index.csv"), "`.")
  )
  readme_path <- file.path("doc", "pipeline_index.md")
  con <- file(readme_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0(paste(readme_lines, collapse = "\n"), "\n")), con)
  invisible(pipeline)
}

get_lag_contiguous <- function(x, year, n = 1) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) stop("[BLOCKER] n must be a positive integer for get_lag_contiguous().")
  if (length(x) != length(year)) stop("[BLOCKER] x and year must have equal length for get_lag_contiguous().")
  if (n >= length(x)) return(rep(NA, length(x)))
  lag_val <- c(rep(NA, n), head(x, -n))
  lag_year <- c(rep(NA, n), head(year, -n))
  ifelse(!is.na(lag_year) & lag_year == (year - n), lag_val, NA)
}

get_lead_contiguous <- function(x, year, n = 1) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) stop("[BLOCKER] n must be a positive integer for get_lead_contiguous().")
  if (length(x) != length(year)) stop("[BLOCKER] x and year must have equal length for get_lead_contiguous().")
  if (n >= length(x)) return(rep(NA, length(x)))
  lead_val <- c(tail(x, -n), rep(NA, n))
  lead_year <- c(tail(year, -n), rep(NA, n))
  ifelse(!is.na(lead_year) & lead_year == (year + n), lead_val, NA)
}

rolling_sd_contiguous_3 <- function(x, year) {
  out <- rep(NA_real_, length(x))
  if (length(x) < 3L) return(out)
  for (i in seq_along(x)) {
    if (i >= 3L) {
      idx <- (i - 2L):i
      yrs <- year[idx]
      vals <- x[idx]
      if (all(yrs == (year[i] - c(2, 1, 0))) && all(is.finite(vals))) {
        out[i] <- stats::sd(vals)
      }
    }
  }
  out
}

winsorize_vec <- function(x, probs = c(0.01, 0.99), na.rm = TRUE) {
  qs <- stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE, type = 7)
  pmin(pmax(x, qs[1]), qs[2])
}

winsorize_with_cutoffs <- function(x, probs = c(0.01, 0.99), na.rm = TRUE) {
  qs <- stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE, type = 7)
  list(values = pmin(pmax(x, qs[1]), qs[2]), cutoffs = qs)
}

optimize_stacking_from_lpd <- function(lpd_matrix) {
  lpd_matrix <- as.matrix(lpd_matrix)
  if (is.null(colnames(lpd_matrix))) {
    colnames(lpd_matrix) <- paste0("model_", seq_len(ncol(lpd_matrix)))
  }
  if (ncol(lpd_matrix) == 1) {
    out <- 1
    names(out) <- colnames(lpd_matrix)
    return(out)
  }

  softmax <- function(theta) {
    z <- c(theta, 0)
    z <- z - max(z)
    exp(z) / sum(exp(z))
  }
  log_sum_exp <- function(vals) {
    m <- max(vals)
    m + log(sum(exp(vals - m)))
  }
  mixture_objective_value <- function(w) {
    log_w <- log(pmax(w, .Machine$double.eps))
    adjusted <- sweep(lpd_matrix, 2, log_w, "+")
    sum(apply(adjusted, 1, log_sum_exp))
  }
  objective <- function(theta) -mixture_objective_value(softmax(theta))

  singleton_elpd <- colSums(lpd_matrix)
  best_singleton <- which.max(singleton_elpd)
  singleton_w <- rep(0, ncol(lpd_matrix))
  singleton_w[best_singleton] <- 1
  names(singleton_w) <- colnames(lpd_matrix)

  starts <- list(rep(0, ncol(lpd_matrix) - 1))
  for (j in seq_len(ncol(lpd_matrix))) {
    z <- rep(-8, ncol(lpd_matrix))
    z[j] <- 8
    starts[[length(starts) + 1]] <- z[-ncol(lpd_matrix)]
  }
  fits <- lapply(starts, function(st) {
    tryCatch(
      stats::optim(st, objective, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12)),
      error = function(e) NULL
    )
  })
  fits <- Filter(Negate(is.null), fits)
  if (length(fits) == 0) {
    warning("Stacking optimizer failed for all starts; falling back to best singleton elpd model.")
    return(singleton_w)
  }

  vals <- vapply(fits, function(f) -f$value, numeric(1))
  best_fit <- fits[[which.max(vals)]]
  w <- softmax(best_fit$par)
  names(w) <- colnames(lpd_matrix)
  if (mixture_objective_value(w) + 1e-6 < mixture_objective_value(singleton_w)) {
    warning("Stacking optimizer returned a solution worse than the best singleton; falling back to best singleton elpd model.")
    return(singleton_w)
  }
  w
}

assert_training_factor_level_coverage <- function(train, test, factor_cols = c("industry", "year"), context = "unknown") {
  for (col in factor_cols) {
    if (col %in% names(train) && col %in% names(test)) {
      train_levels <- unique(as.character(train[[col]][!is.na(train[[col]])]))
      test_levels <- unique(as.character(test[[col]][!is.na(test[[col]])]))
      missing_levels <- setdiff(test_levels, train_levels)
      if (length(missing_levels)) {
        stop("[BLOCKER] Missing training factor-level coverage for ", context,
             "; column=", col, "; missing levels=", paste(missing_levels, collapse = ", "))
      }
    }
  }
  invisible(TRUE)
}

safe_variant_name <- function(x) {
  gsub(" ", "_", gsub("[()|]", "", x))
}

model_key <- function(model_id, target_space, heterogeneity_variant, suffix = NULL) {
  key <- sprintf("%s_%s_%s", model_id, target_space, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

model_key_sampled <- function(model_id, target_space, sample_group, heterogeneity_variant, suffix = NULL) {
  if (is.null(sample_group) || is.na(sample_group) || !nzchar(sample_group)) sample_group <- "main_common"
  key <- sprintf("%s_%s_%s_%s", model_id, target_space, sample_group, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

standardize_predictors <- function(df, predictor_vars = pred_vars) {
  for (v in predictor_vars) {
    if (v %in% colnames(df)) {
      m <- mean(df[[v]], na.rm = TRUE)
      s <- sd(df[[v]], na.rm = TRUE)
      df[[paste0(v, "_std")]] <- if (!is.na(s) && s > 0) (df[[v]] - m) / s else 0
    }
  }
  df
}

fix_formula <- function(formula_str, predictor_vars = pred_vars, prefactor = FALSE) {
  if (prefactor) {
    formula_str <- gsub("factor\\(industry\\)", "industry_f", formula_str)
    formula_str <- gsub("factor\\(year\\)", "year_f", formula_str)
  }
  for (v in predictor_vars) {
    formula_str <- gsub(paste0("\\b", v, "\\b"), paste0(v, "_std"), formula_str)
  }
  formula_str
}

read_winsor_sample <- function(sample_file, prefactor = FALSE, root = input_winsor_root) {
  path <- file.path(root, "tables", sample_file)
  if (!file.exists(path)) stop("[BLOCKER] Winsorized sample file missing: ", path)
  df <- read.csv(path, stringsAsFactors = FALSE)
  df <- standardize_predictors(df)
  if (prefactor) {
    df$industry_f <- factor(df$industry)
    df$year_f <- factor(df$year)
  }
  df
}

prepare_varying_slope_data <- function(df, group = varyslope_group) {
  if (identical(group, "industry_year")) {
    if (!all(c("industry", "year") %in% names(df))) {
      stop("[BLOCKER] industry_year varying slopes require industry and year columns.")
    }
    df$industry_year_id <- interaction(df$industry, df$year, drop = TRUE)
  }
  df
}

varying_slope_formula <- function(formula_str, group = varyslope_group) {
  parts <- strsplit(formula_str, "~", fixed = TRUE)[[1]]
  if (length(parts) != 2) stop("[BLOCKER] Cannot parse formula for varying slopes: ", formula_str)
  rhs <- trimws(parts[2])
  rhs <- gsub("\\+\\s*factor\\(industry\\)", "", rhs)
  rhs <- gsub("\\+\\s*factor\\(year\\)", "", rhs)
  rhs <- gsub("\\+\\s*\\(1\\s*\\|\\s*company\\)", "", rhs)
  rhs <- trimws(gsub("\\s+", " ", rhs))
  group_var <- if (identical(group, "firm")) "company" else "industry_year_id"
  sprintf("TA_scaled ~ 1 + %s + (1 + %s | %s)", rhs, rhs, group_var)
}

varying_slope_candidate <- function(model_id, target_space) {
  if (identical(varyslope_scope, "FULL")) return(TRUE)
  paste(model_id, target_space) %in% c(
    "M06 ex_post",
    "M07 ex_post",
    "M07 real_time",
    "M09 real_time",
    "M01 ex_post",
    "M01 real_time"
  )
}

describe_numeric <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(c(N = 0, Mean = NA, SD = NA, Min = NA, P01 = NA, P05 = NA, P25 = NA,
             Median = NA, P75 = NA, P95 = NA, P99 = NA, Max = NA))
  }
  qs <- quantile(x, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99),
                 na.rm = TRUE, names = FALSE, type = 7)
  c(
    N = length(x),
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    P01 = qs[1],
    P05 = qs[2],
    P25 = qs[3],
    Median = qs[4],
    P75 = qs[5],
    P95 = qs[6],
    P99 = qs[7],
    Max = max(x)
  )
}

extract_weight_variant <- function(model_name, heterogeneity_variant = NULL) {
  if (!is.null(heterogeneity_variant) && !is.na(heterogeneity_variant) && nzchar(heterogeneity_variant)) {
    return(heterogeneity_variant)
  }
  if (grepl("Firm RE", model_name)) return("Firm RE (Random Intercept + Year FE)")
  if (grepl("Pooled", model_name)) return("Pooled (Industry + Year FE)")
  NA_character_
}

extract_base_model_name <- function(model_name) {
  sub(" \\((Firm RE|Pooled).*$", "", model_name)
}

read_original_weight_file <- function(space) {
  if (space == "ex_post") {
    candidates <- c(
      file.path(baseline_root, "tables", "table_stacking_weights_ex_post_corrected.csv"),
      file.path(baseline_root, "tables", "table_stacking_weights_ex_post.csv")
    )
  } else {
    candidates <- c(
      file.path(baseline_root, "tables", "table_stacking_weights_real_time_corrected.csv"),
      file.path(baseline_root, "tables", "table_stacking_weights_real_time.csv")
    )
  }
  source_path <- candidates[file.exists(candidates)][1]
  if (is.na(source_path)) stop("[BLOCKER] No original weight file found for space: ", space)
  df <- read.csv(source_path, stringsAsFactors = FALSE)
  df$Original_Weight_Source <- source_path
  df
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
  write.csv(differences, file.path(design_root, "differences_from_AccForUncertaintyCode.csv"), row.names = FALSE)
  writeLines(c(
    "This study adapts the Bayesian model-averaging framework of AccForUncertaintyCode to the Vietnamese listed-firm setting. It differs from the original implementation in sample construction, scaling, outlier handling, model space, posterior predictive abnormality classification, and panel-dependence robustness checks.",
    "",
    "The analysis is therefore positioned as an extension/adaptation, not a replication. The corrected design preserves corrected COGS/INV data, the two-tier sample design, the exclusion of M08 and M10 from main stacks, and the treatment of existing wide-prior Gaussian outputs as diagnostic only."
  ), file.path(design_root, "method_note_adaptation_not_replication.txt"))
  write_pipeline_index()
  write_execution_config_registry()
}
