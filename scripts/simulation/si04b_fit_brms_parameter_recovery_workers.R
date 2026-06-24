# Script: si04b_fit_brms_parameter_recovery_workers.R
# Purpose: Fit BRMS parameter recovery simulation tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("si04b", "Fit BRMS parameter recovery workers")
root <- file.path(output_root, "simulation", "brms_parameter_recovery")
manifest_path <- file.path(root, "tables", "table_si04_brms_recovery_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si04_brms_recovery_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing si04a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_si04b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  writeLines(c("si04b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  result <- tryCatch({
    set.seed(as.integer(task$Effective_Seed))
    n <- 160L
    df <- data.frame(company = paste0("F", rep(seq_len(32L), each = 5L)), year = rep(2016:2020, 32L))
    df$industry <- paste0("I", ((seq_len(nrow(df)) - 1L) %% 5L) + 1L)
    for (v in pred_vars) df[[v]] <- rnorm(nrow(df))
    beta_drev <- 0.04
    beta_ppe <- -0.03
    df$TA_scaled <- beta_drev * df$dREV_scaled + beta_ppe * df$PPE_scaled + rnorm(nrow(df), sd = 0.06)
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
    fx <- brms::fixef(fit)
    out <- data.frame(
      Replication = as.integer(task$Replication),
      parameter = c("dREV_scaled_std", "PPE_scaled_std"),
      true_value = c(beta_drev, beta_ppe),
      estimate = c(fx["dREV_scaled_std", "Estimate"], fx["PPE_scaled_std", "Estimate"]),
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
parallel_cfg <- accrual_fit_worker_config("simulation", max(as.integer(tasks$cores), na.rm = TRUE), "si04b brms recovery workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_si04b_task_worker, parallel_cfg,
                                 export_names = "fit_si04b_task_worker", packages = "brms",
                                 context = "si04b brms recovery workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "si04b brms recovery workers")
phase_end("si04b", "Fit BRMS parameter recovery workers")
