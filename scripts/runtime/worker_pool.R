# -----------------------------------------------------------------------------
# Worker pool and task-status runtime helpers
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

is_model_parallel_enabled <- function() {
  env_flag("ACCRUAL_ENABLE_MODEL_PARALLEL", "FALSE")
}

stable_task_key <- function(...) {
  parts <- vapply(list(...), as.character, character(1))
  paste(parts, collapse = "|")
}

accrual_task_cache_key <- function(...) {
  raw_key <- stable_task_key(...)
  gsub("[^A-Za-z0-9_.=-]+", "_", raw_key)
}


safe_task_artifact_path <- function(root, task_key, suffix) {
  safe_key <- accrual_task_cache_key(task_key)
  file.path(root, paste0(safe_key, suffix))
}

safe_task_log_path <- function(root, task_key) {
  safe_task_artifact_path(root, task_key, ".log")
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
  if (enabled && workers <= 1L && !env_flag("ACCRUAL_ALLOW_SINGLE_WORKER_MODEL_PARALLEL", "FALSE")) {
    stop(
      "[BLOCKER] ACCRUAL_ENABLE_MODEL_PARALLEL=TRUE but effective workers=1 for ", context, ". ",
      "Set ACCRUAL_MODEL_PARALLEL_WORKERS > 1 or explicitly set ",
      "ACCRUAL_ALLOW_SINGLE_WORKER_MODEL_PARALLEL=TRUE to run model-parallel stages with one worker."
    )
  }
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
  message("[WORKER POOL] ", context, ": workers=", parallel_cfg$workers,
          ", cores_per_fit=", parallel_cfg$cores_per_fit,
          ", total_core_budget=", parallel_cfg$total_core_budget,
          ", backend=", parallel_cfg$backend,
          ", fit_kind=", if (!is.null(parallel_cfg$fit_kind)) parallel_cfg$fit_kind else "unknown",
          ", context=", if (!is.null(parallel_cfg$context)) parallel_cfg$context else context)
  if (!isTRUE(parallel_cfg$enabled) || as.integer(parallel_cfg$workers) <= 1L) {
    return(lapply(tasks, worker_fun))
  }
  if (!identical(parallel_cfg$backend, "base_parallel")) {
    stop("[BLOCKER] Unsupported model-parallel backend for ", context, ": ", parallel_cfg$backend)
  }
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
    blocked_idx <- which(required & blocked)
    keys <- if (!is.na(key_col)) status_df[[key_col]][blocked_idx] else blocked_idx
    detail_col <- intersect(c("reason", "error_message", "Reason", "Failure_Reason"), names(status_df))[1]
    if (!is.na(detail_col)) {
      details <- paste0(keys, ": ", status_df[[detail_col]][blocked_idx])
      details <- details[!is.na(details) & nzchar(details)]
      if (length(details)) keys <- details
    }
    stop("[BLOCKER] Required task(s) failed or were blocked in ", context, ": ",
         paste(keys, collapse = "; "))
  }
  invisible(TRUE)
}
