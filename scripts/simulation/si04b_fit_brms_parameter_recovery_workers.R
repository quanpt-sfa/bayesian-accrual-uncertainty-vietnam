# Script: si04b_fit_brms_parameter_recovery_workers.R
# Purpose: Fit BRMS parameter recovery simulation tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("si04b", "Fit BRMS parameter recovery workers")
root <- file.path(output_root, "simulation", "brms_parameter_recovery")
manifest_path <- file.path(root, "tables", "table_si04_brms_recovery_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si04_brms_recovery_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing si04a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
sim_cfg <- accrual_simulation_runtime_config("brms_recovery")
dgp_cfg <- accrual_simulation_dgp_config("brms_recovery")

read_rds_checked <- function(path) {
  tryCatch(
    list(ok = TRUE, value = readRDS(path), reason = NA_character_),
    error = function(e) list(ok = FALSE, value = NULL, reason = conditionMessage(e))
  )
}

reconcile_si04b_task_artifacts <- function(task, result_status, result_reason) {
  task <- as.list(task)
  fit_exists <- file.exists(task$fit_path)
  result_exists <- file.exists(task$result_path)
  fit_read <- if (fit_exists) read_rds_checked(task$fit_path) else list(ok = FALSE, value = NULL, reason = "fit_path_missing")
  result_read <- if (result_exists) read_rds_checked(task$result_path) else list(ok = FALSE, value = NULL, reason = "result_path_missing")
  fit_class <- if (isTRUE(fit_read$ok)) paste(class(fit_read$value), collapse = "|") else NA_character_
  result_internal_status <- NA_character_
  result_status_ok <- FALSE
  if (isTRUE(result_read$ok) && is.data.frame(result_read$value) && "status" %in% names(result_read$value)) {
    internal <- unique(as.character(result_read$value$status[!is.na(result_read$value$status)]))
    result_internal_status <- if (length(internal)) paste(internal, collapse = "|") else NA_character_
    result_status_ok <- length(internal) > 0L && all(internal == "SUCCESS")
  }
  artifacts_valid <- isTRUE(fit_read$ok) &&
    isTRUE(result_read$ok) &&
    inherits(fit_read$value, "brmsfit") &&
    is.data.frame(result_read$value) &&
    result_status_ok
  original_failed <- is.na(result_status) || !identical(as.character(result_status), "SUCCESS")
  artifact_reconciled <- isTRUE(original_failed && artifacts_valid)
  status <- if (artifact_reconciled) "SUCCESS" else as.character(result_status)
  reason <- if (artifact_reconciled) "RECOVERED_FROM_VALID_ARTIFACTS" else as.character(result_reason)
  if (!nzchar(status) || is.na(status)) status <- "FAILED"
  if (!nzchar(reason) || is.na(reason)) reason <- NA_character_
  list(
    status = status,
    reason = reason,
    artifact_reconciled = artifact_reconciled,
    reconciliation_reason = if (artifact_reconciled) "valid_fit_and_success_result_artifacts" else NA_character_,
    fit_exists = fit_exists,
    result_exists = result_exists,
    fit_readable = isTRUE(fit_read$ok),
    result_readable = isTRUE(result_read$ok),
    fit_class = fit_class,
    result_internal_status = result_internal_status
  )
}

si04b_status_row <- function(task, result_status, result_reason) {
  rec <- reconcile_si04b_task_artifacts(task, result_status, result_reason)
  data.frame(
    Task_Key = task$Task_Key,
    status = rec$status,
    reason = rec$reason,
    Required = task$Required,
    fit_path = task$fit_path,
    result_path = task$result_path,
    metadata_path = task$metadata_path,
    task_log_path = task$task_log_path,
    artifact_reconciled = rec$artifact_reconciled,
    reconciliation_reason = rec$reconciliation_reason,
    fit_readable = rec$fit_readable,
    result_readable = rec$result_readable,
    result_internal_status = rec$result_internal_status,
    stringsAsFactors = FALSE
  )
}

reconcile_si04b_status_table <- function(status_df, tasks_df) {
  if (!nrow(status_df)) return(status_df)
  rows <- lapply(seq_len(nrow(status_df)), function(i) {
    status_row <- status_df[i, , drop = FALSE]
    if ("status" %in% names(status_row) && identical(as.character(status_row$status[[1]]), "SUCCESS")) {
      return(status_row)
    }
    task_idx <- match(status_row$Task_Key[[1]], tasks_df$Task_Key)
    if (is.na(task_idx)) return(status_row)
    task <- as.list(tasks_df[task_idx, , drop = FALSE])
    si04b_status_row(
      task,
      result_status = status_row$status[[1]],
      result_reason = if ("reason" %in% names(status_row)) status_row$reason[[1]] else NA_character_
    )
  })
  do.call(rbind, rows)
}

merge_si04b_status_rows <- function(new_status, prior_status, manifest_tasks) {
  if (is.null(prior_status) || !nrow(prior_status)) {
    merged <- new_status
  } else if (is.null(new_status) || !nrow(new_status)) {
    merged <- prior_status
  } else {
    # Filtered retry/reconcile runs replace rows for Task_Key values present in new_status,
    # keep prior rows for untouched tasks, and append any new task rows.
    all_cols <- unique(c(names(prior_status), names(new_status)))
    align_cols <- function(df) {
      missing <- setdiff(all_cols, names(df))
      for (col in missing) df[[col]] <- NA
      df[, all_cols, drop = FALSE]
    }
    prior_status <- align_cols(prior_status)
    new_status <- align_cols(new_status)
    keep_prior <- !prior_status$Task_Key %in% new_status$Task_Key
    merged <- rbind(prior_status[keep_prior, , drop = FALSE], new_status)
  }
  if (nrow(merged) && "Task_Key" %in% names(merged) && "Task_Key" %in% names(manifest_tasks)) {
    missing_keys <- setdiff(manifest_tasks$Task_Key, merged$Task_Key)
    if (length(missing_keys)) {
      missing_rows <- lapply(missing_keys, function(key) {
        task <- as.list(manifest_tasks[match(key, manifest_tasks$Task_Key), , drop = FALSE])
        si04b_status_row(task, "BLOCKED_MISSING_FIT", "MISSING_STATUS_ROW")
      })
      missing_status <- do.call(rbind, missing_rows)
      all_cols <- unique(c(names(merged), names(missing_status)))
      align_cols <- function(df) {
        missing <- setdiff(all_cols, names(df))
        for (col in missing) df[[col]] <- NA
        df[, all_cols, drop = FALSE]
      }
      merged <- rbind(align_cols(merged), align_cols(missing_status))
    }
    manifest_order <- match(merged$Task_Key, manifest_tasks$Task_Key)
    merged <- merged[order(is.na(manifest_order), manifest_order, merged$Task_Key), , drop = FALSE]
    rownames(merged) <- NULL
  }
  merged
}

select_si04b_tasks <- function(tasks_df) {
  selected <- tasks_df
  task_filter <- env_list("ACCRUAL_TASK_FILTER")
  if (length(task_filter)) {
    selected <- selected[selected$Task_Key %in% task_filter, , drop = FALSE]
  }
  status_filter_raw <- env_value("ACCRUAL_TASK_STATUS_FILTER", "")
  if (nzchar(status_filter_raw)) {
    status_filter <- env_choice("ACCRUAL_TASK_STATUS_FILTER", "FAILED", c("FAILED"), case = "upper")
    if (!file.exists(status_path)) {
      stop("[BLOCKER] ACCRUAL_TASK_STATUS_FILTER=", status_filter,
           " requires an existing status table: ", status_path)
    }
    prior_status <- read.csv(status_path, stringsAsFactors = FALSE)
    prior_status$status <- as.character(prior_status$status)
    retry_keys <- prior_status$Task_Key[is.na(prior_status$status) | prior_status$status != "SUCCESS" | prior_status$status == status_filter]
    selected <- selected[selected$Task_Key %in% retry_keys, , drop = FALSE]
  }
  if (length(task_filter) || nzchar(status_filter_raw)) {
    message("[si04b] Selected task count after filters: ", nrow(selected))
    message("[si04b] Selected Task_Key values: ", paste(selected$Task_Key, collapse = ", "))
  }
  selected
}

recovery_prior <- function() {
  c(
    brms::set_prior("normal(0, 0.10)", class = "b"),
    brms::set_prior("normal(0, 0.10)", class = "Intercept"),
    brms::set_prior("exponential(10)", class = "sigma"),
    brms::set_prior("exponential(10)", class = "sd")
  )
}

extract_si04b_diagnostics <- function(fit, max_treedepth) {
  draws <- posterior::as_draws_df(fit)
  draw_summary <- as.data.frame(posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail"))
  np <- brms::nuts_params(fit)
  treedepths <- np$Value[np$Parameter == "treedepth__"]
  max_rhat <- suppressWarnings(max(draw_summary$rhat, na.rm = TRUE))
  min_ess_bulk <- suppressWarnings(min(draw_summary$ess_bulk, na.rm = TRUE))
  min_ess_tail <- suppressWarnings(min(draw_summary$ess_tail, na.rm = TRUE))
  if (!is.finite(max_rhat)) max_rhat <- NA_real_
  if (!is.finite(min_ess_bulk)) min_ess_bulk <- NA_real_
  if (!is.finite(min_ess_tail)) min_ess_tail <- NA_real_
  list(
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    total_divergent = sum(np$Parameter == "divergent__" & np$Value > 0, na.rm = TRUE),
    max_treedepth_hits = sum(treedepths >= max_treedepth, na.rm = TRUE)
  )
}

manifest_tasks <- tasks
selected_tasks <- select_si04b_tasks(manifest_tasks)
if (!nrow(selected_tasks)) stop("[BLOCKER] si04b selected zero tasks after task filters.")

if (env_flag("ACCRUAL_RECONCILE_ONLY", "FALSE")) {
  message("[si04b] ACCRUAL_RECONCILE_ONLY=TRUE; reconciling existing task artifacts without fitting.")
  prior <- if (file.exists(status_path)) read.csv(status_path, stringsAsFactors = FALSE) else data.frame()
  rows <- lapply(seq_len(nrow(selected_tasks)), function(i) {
    task <- as.list(selected_tasks[i, , drop = FALSE])
    prior_idx <- if (nrow(prior) && "Task_Key" %in% names(prior)) match(task$Task_Key, prior$Task_Key) else NA_integer_
    prior_status <- if (!is.na(prior_idx) && "status" %in% names(prior)) prior$status[[prior_idx]] else "FAILED"
    prior_reason <- if (!is.na(prior_idx) && "reason" %in% names(prior)) prior$reason[[prior_idx]] else "RECONCILE_ONLY"
    si04b_status_row(task, prior_status, prior_reason)
  })
  status <- merge_si04b_status_rows(do.call(rbind, rows), prior, manifest_tasks)
  status <- reconcile_si04b_status_table(status, manifest_tasks)
  write_task_status(status_path, status)
  accrual_task_status_blocker(status, required_col = "Required", context = "si04b brms recovery reconcile-only")
  phase_end("si04b", "Fit BRMS parameter recovery workers")
  quit(save = "no", status = 0)
}

fit_si04b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  writeLines(c("si04b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  result <- tryCatch({
    set_accrual_effective_seed(task$Effective_Seed, context = task$Task_Key)
    T_val <- as.integer(task$T)
    sigma_firm <- as.numeric(task$sigma_firm)
    n_firms <- as.integer(sim_cfg$n_firms)
    n_industries <- as.integer(sim_cfg$n_industries)
    firms <- paste0("F", seq_len(n_firms))
    years <- seq_len(T_val)
    df <- expand.grid(company = firms, year = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    df$industry <- paste0("I", ((match(df$company, firms) - 1L) %% n_industries) + 1L)
    for (v in pred_vars) df[[v]] <- rnorm(nrow(df))
    beta_drev <- dgp_cfg$beta_drev
    beta_ppe <- dgp_cfg$beta_ppe
    beta_roa <- dgp_cfg$beta_roa
    firm_effect <- rnorm(n_firms, mean = 0, sd = sigma_firm)
    names(firm_effect) <- firms
    df$TA_scaled <- beta_drev * df$dREV_scaled +
      beta_ppe * df$PPE_scaled +
      beta_roa * df$ROA_lag +
      firm_effect[df$company] +
      rnorm(nrow(df), sd = sim_cfg$sigma_eps)
    df <- standardize_predictors(df)
    fit <- brms::brm(
      formula = brms::bf(TA_scaled ~ dREV_scaled_std + PPE_scaled_std + ROA_lag_std + (1 | company)),
      data = df,
      family = brms::student(),
      prior = recovery_prior(),
      chains = as.integer(task$chains),
      cores = as.integer(task$cores),
      iter = as.integer(task$iter),
      warmup = as.integer(task$warmup),
      control = list(adapt_delta = as.numeric(task$adapt_delta), max_treedepth = as.integer(task$max_treedepth)),
      seed = as.integer(task$Effective_Seed),
      save_pars = brms::save_pars(all = TRUE),
      refresh = 0L
    )
    saveRDS(fit, task$fit_path)
    fx <- brms::fixef(fit)
    fit_diag <- extract_si04b_diagnostics(fit, as.integer(task$max_treedepth))
    out <- data.frame(
      T = T_val,
      sigma_firm = sigma_firm,
      Replication = as.integer(task$Replication),
      dgp_design_source = dgp_cfg$design_source,
      dgp_n_firms = n_firms,
      dgp_n_industries = n_industries,
      dgp_sigma_eps = sim_cfg$sigma_eps,
      dgp_beta_roa = beta_roa,
      parameter = c("dREV_scaled_std", "PPE_scaled_std"),
      true_value = c(beta_drev, beta_ppe),
      estimate = c(fx["dREV_scaled_std", "Estimate"], fx["PPE_scaled_std", "Estimate"]),
      n_obs = stats::nobs(fit),
      max_rhat = fit_diag$max_rhat,
      min_ess_bulk = fit_diag$min_ess_bulk,
      min_ess_tail = fit_diag$min_ess_tail,
      total_divergent = fit_diag$total_divergent,
      max_treedepth_hits = fit_diag$max_treedepth_hits,
      fit_path = task$fit_path,
      status = "SUCCESS",
      stringsAsFactors = FALSE
    )
    saveRDS(out, task$result_path)
    list(status = "SUCCESS", reason = NA_character_, value = out)
  }, error = function(e) {
    list(status = "FAILED", reason = conditionMessage(e), value = NULL)
  })
  status <- result$status
  reason <- result$reason
  rec <- reconcile_si04b_task_artifacts(task, status, reason)
  status <- rec$status
  reason <- rec$reason
  ended <- Sys.time()
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason,
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       chains = task$chains, cores = task$cores, iter = task$iter, warmup = task$warmup,
                       adapt_delta = task$adapt_delta, max_treedepth = task$max_treedepth,
                       dgp_design_source = dgp_cfg$design_source,
                       artifact_reconciled = rec$artifact_reconciled,
                       reconciliation_reason = rec$reconciliation_reason,
                       fit_exists = rec$fit_exists,
                       result_exists = rec$result_exists,
                       fit_readable = rec$fit_readable,
                       result_readable = rec$result_readable,
                       fit_class = rec$fit_class,
                       result_internal_status = rec$result_internal_status,
                       runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  si04b_status_row(task, status, reason)
}
tasks <- selected_tasks
parallel_cfg <- accrual_fit_worker_config("simulation", max(as.integer(tasks$cores), na.rm = TRUE), "si04b brms recovery workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_si04b_task_worker, parallel_cfg,
                                 export_names = c("fit_si04b_task_worker", "sim_cfg", "dgp_cfg", "recovery_prior",
                                                  "extract_si04b_diagnostics", "read_rds_checked",
                                                  "reconcile_si04b_task_artifacts", "si04b_status_row"),
                                 packages = c("brms", "posterior"),
                                 context = "si04b brms recovery workers")
new_status <- do.call(rbind, results)
prior_status <- if (file.exists(status_path)) read.csv(status_path, stringsAsFactors = FALSE) else data.frame()
status <- merge_si04b_status_rows(new_status, prior_status, manifest_tasks)
status <- reconcile_si04b_status_table(status, manifest_tasks)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "si04b brms recovery workers")
phase_end("si04b", "Fit BRMS parameter recovery workers")
