# Script: si03b_fit_brms_leakage_confirmation_workers.R
# Purpose: Fit BRMS leakage confirmation simulation tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("si03b", "Fit BRMS leakage confirmation workers")
root <- file.path(output_root, "simulation", "brms_leakage")
manifest_path <- file.path(root, "tables", "table_si03_brms_leakage_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si03_brms_leakage_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing si03a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_si03b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  writeLines(c("si03b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  result <- tryCatch({
    set.seed(as.integer(task$Effective_Seed))
    n_firms <- 24L
    years <- 2016:2020
    df <- expand.grid(company = paste0("F", seq_len(n_firms)), year = years, KEEP.OUT.ATTRS = FALSE)
    df$industry <- paste0("I", ((seq_len(nrow(df)) - 1L) %% 6L) + 1L)
    for (v in pred_vars) df[[v]] <- rnorm(nrow(df))
    df$TA_scaled <- 0.02 * df$dREV_scaled - 0.03 * df$PPE_scaled + rnorm(nrow(df), sd = 0.08)
    df <- standardize_predictors(df)
    fit <- brms::brm(
      formula = brms::bf(TA_scaled ~ dREV_scaled_std + PPE_scaled_std + ROA_lag_std + (1 | company)),
      data = df,
      family = brms_family(),
      prior = default_prior_list("firm_random_intercept", model_structure = model_structure),
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
    out <- data.frame(
      Replication = as.integer(task$Replication),
      model_type = "firm_random_intercept",
      n_obs = stats::nobs(fit),
      elpd_proxy = sum(colMeans(brms::log_lik(fit))),
      status = "SUCCESS",
      stringsAsFactors = FALSE
    )
    saveRDS(out, task$result_path)
    status <<- "SUCCESS"
    out
  }, error = function(e) {
    reason <<- conditionMessage(e)
    NULL
  })
  ended <- Sys.time()
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason,
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       chains = task$chains, cores = task$cores, iter = task$iter, warmup = task$warmup,
                       adapt_delta = task$adapt_delta, max_treedepth = task$max_treedepth,
                       runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, result_path = task$result_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("simulation", max(as.integer(tasks$cores), na.rm = TRUE), "si03b brms leakage workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_si03b_task_worker, parallel_cfg,
                                 export_names = "fit_si03b_task_worker", packages = "brms",
                                 context = "si03b brms leakage workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "si03b brms leakage workers")
phase_end("si03b", "Fit BRMS leakage confirmation workers")
