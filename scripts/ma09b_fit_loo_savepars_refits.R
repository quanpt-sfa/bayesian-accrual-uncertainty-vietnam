# Script: ma09b_fit_loo_savepars_refits.R
# Purpose: Fit secondary PSIS/LOO save_pars refit tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("ma09b", "Fit LOO save_pars refits")

tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma09_savepars_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma09_savepars_refit_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing ma09a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)

fit_ma09b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(task$result_path) || is.na(task$result_path) || !nzchar(task$result_path)) {
    task$result_path <- sub("_metadata\\.csv$", "_loo_result.rds", task$metadata_path)
  }
  dir.create(dirname(task$result_path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  writeLines(c(
    "ma09b task log",
    paste("Task_Key:", task$Task_Key),
    paste("Effective_Seed:", task$Effective_Seed)
  ), task$task_log_path)
  fit <- if (file.exists(task$fit_path)) tryCatch(readRDS(task$fit_path), error = function(e) NULL) else NULL
  result <- tryCatch({
    if (is.null(fit)) {
      df_scaled <- read_winsor_sample(task$Target_Sample, prefactor = TRUE)
      formula_str <- fix_formula(task$brms_Formula, prefactor = TRUE)
      prior_list <- default_prior_list(task$Heterogeneity_Variant)
      fit <- brms::brm(
        formula = brms::bf(stats::as.formula(formula_str)),
        data = df_scaled,
        family = brms_family(),
        prior = prior_list,
        chains = as.integer(task$chains),
        cores = as.integer(task$cores),
        iter = as.integer(task$iter),
        warmup = as.integer(task$warmup),
        control = list(adapt_delta = as.numeric(task$adapt_delta), max_treedepth = as.integer(task$max_treedepth)),
        seed = as.integer(task$Effective_Seed),
        save_pars = brms::save_pars(all = TRUE),
        refresh = if ("refresh" %in% names(task)) as.integer(task$refresh) else 0L
      )
      saveRDS(fit, task$fit_path)
    }
    loo_raw <- loo::loo(fit, cores = 1)
    raw_k <- sum(loo_raw$diagnostics$pareto_k > 0.7)
    loo_corrected <- loo_raw
    mm_applied <- FALSE
    mm_note <- "No high Pareto-k observations"
    if (raw_k > 0) {
      loo_corrected <- loo::loo(fit, moment_match = TRUE, cores = 1)
      mm_applied <- TRUE
      mm_note <- sprintf("Moment matching applied; high-k before=%d after=%d",
                         raw_k, sum(loo_corrected$diagnostics$pareto_k > 0.7))
    }
    out <- list(
      task = task,
      loo_raw = loo_raw,
      loo_corrected = loo_corrected,
      n_obs = brms::nobs(fit),
      refit_raw_elpd = loo_raw$estimates["elpd_loo", "Estimate"],
      refit_raw_k_above_07 = raw_k,
      corrected_elpd = loo_corrected$estimates["elpd_loo", "Estimate"],
      corrected_k_above_07 = sum(loo_corrected$diagnostics$pareto_k > 0.7),
      moment_match_applied = mm_applied,
      moment_match_note = mm_note
    )
    saveRDS(out, task$result_path)
    status <<- "SUCCESS"
    out
  }, error = function(e) {
    reason <<- conditionMessage(e)
    NULL
  })
  ended <- Sys.time()
  metadata <- data.frame(
    Task_Key = task$Task_Key,
    status = status,
    reason = reason,
    RNG_Context = task$RNG_Context,
    Effective_Seed = task$Effective_Seed,
    chains = task$chains,
    cores = task$cores,
    iter = task$iter,
    warmup = task$warmup,
    adapt_delta = task$adapt_delta,
    max_treedepth = task$max_treedepth,
    backend = "rstan",
    started_at = as.character(started),
    ended_at = as.character(ended),
    runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
    stringsAsFactors = FALSE
  )
  write.csv(metadata, task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, result_path = task$result_path,
             metadata_path = task$metadata_path, stringsAsFactors = FALSE)
}

parallel_cfg <- accrual_fit_worker_config(
  "loo_savepars",
  cores_per_fit = if (nrow(tasks)) max(as.integer(tasks$cores), na.rm = TRUE) else 1L,
  context = "ma09b loo save_pars refits"
)
results <- accrual_run_task_pool(
  split(tasks, seq_len(nrow(tasks))),
  fit_ma09b_task_worker,
  parallel_cfg,
  export_names = "fit_ma09b_task_worker",
  packages = c("brms", "loo"),
  context = "ma09b loo save_pars refits"
)
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "ma09b loo save_pars refits")
phase_end("ma09b", "Fit LOO save_pars refits")
