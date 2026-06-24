# Script: di08b_fit_mcmc_sampler_calibration_workers.R
# Purpose: Fit diagnostic MCMC sampler calibration tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("di08b", "Fit MCMC sampler calibration workers")
root <- file.path(output_root, "diagnostics", "mcmc_sampler_calibration")
manifest_path <- file.path(root, "tables", "table_di08_sampler_calibration_task_manifest.csv")
status_path <- file.path(root, "tables", "table_di08_sampler_calibration_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing di08a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
formula_path <- file.path(output_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formula_path)) formula_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formula_path)) stop("[BLOCKER] Missing di08 formula table: ", formula_path)
formula_rows <- read.csv(formula_path, stringsAsFactors = FALSE)
formula_rows <- formula_rows[formula_rows$Sample_Group == "main_common", , drop = FALSE]
if (!nrow(formula_rows)) stop("[BLOCKER] di08 requires at least one main_common formula row.")
calibration_formula_row <- as.list(formula_rows[1, , drop = FALSE])
fit_di08b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  writeLines(c("di08b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  result <- tryCatch({
    row <- calibration_formula_row
    df_scaled <- read_winsor_sample(row$Target_Sample)
    formula_str <- fix_formula(row$brms_Formula)
    fit <- brms::brm(
      formula = brms::bf(stats::as.formula(formula_str)),
      data = df_scaled,
      family = brms_family(),
      prior = default_prior_list(row$Heterogeneity_Variant, model_structure = model_structure),
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
    diag <- data.frame(
      Task_Key = task$Task_Key,
      Profile_ID = task$Profile_ID,
      Model_ID = row$Model_ID,
      Target_Space = row$Target_Space,
      N_Obs = stats::nobs(fit),
      Max_Rhat = suppressWarnings(max(posterior::summarise_draws(posterior::as_draws_df(fit))$rhat, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
    saveRDS(diag, task$diagnostic_path)
    list(status = "SUCCESS", reason = NA_character_, value = diag)
  }, error = function(e) {
    list(status = "FAILED", reason = conditionMessage(e), value = NULL)
  })
  status <- result$status
  reason <- result$reason
  ended <- Sys.time()
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason,
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       chains = task$chains, cores = task$cores, iter = task$iter, warmup = task$warmup,
                       adapt_delta = task$adapt_delta, max_treedepth = task$max_treedepth,
                       runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, diagnostic_path = task$diagnostic_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("diagnostic_calibration", max(as.integer(tasks$cores), na.rm = TRUE), "di08b calibration workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_di08b_task_worker, parallel_cfg,
                                 export_names = c("fit_di08b_task_worker", "calibration_formula_row"),
                                 packages = c("brms", "posterior"), context = "di08b calibration workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "di08b calibration workers")
phase_end("di08b", "Fit MCMC sampler calibration workers")
