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
    T_val <- as.integer(task$T)
    sigma_firm <- as.numeric(task$sigma_firm)
    n_firms <- as.integer(sim_cfg$n_firms)
    n_industries <- as.integer(sim_cfg$n_industries)
    firms <- paste0("F", seq_len(n_firms))
    years <- seq_len(T_val)
    df <- expand.grid(company = firms, year = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    df$industry <- paste0("I", ((match(df$company, firms) - 1L) %% n_industries) + 1L)
    for (v in pred_vars) df[[v]] <- rnorm(nrow(df))
    beta_drev <- 0.04
    beta_ppe <- -0.03
    beta_roa <- 0.02
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
                                 export_names = c("fit_si04b_task_worker", "sim_cfg", "recovery_prior", "extract_si04b_diagnostics"),
                                 packages = c("brms", "posterior"),
                                 context = "si04b brms recovery workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "si04b brms recovery workers")
phase_end("si04b", "Fit BRMS parameter recovery workers")
