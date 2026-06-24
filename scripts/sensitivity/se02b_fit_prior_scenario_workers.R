# Script: se02b_fit_prior_scenario_workers.R
# Purpose: Fit sensitivity prior-scenario tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("se02b", "Fit prior-scenario workers")
tables_dir <- file.path(output_root, "sensitivity", "tables")
manifest_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing se02a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_se02b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  warning_count <- 0L
  writeLines(c(
    "se02b task log",
    paste("Task_Key:", task$Task_Key),
    paste("Scenario:", task$Scenario),
    paste("Effective_Seed:", task$Effective_Seed)
  ), task$task_log_path)
  fit <- tryCatch({
    df_scaled <- read_winsor_sample(task$Target_Sample)
    formula_str <- fix_formula(task$brms_Formula)
    prior_list <- default_prior_list(
      task$Heterogeneity_Variant,
      model_structure = model_structure,
      prior_set_id = prior_set_id,
      family = likelihood_family
    )
    captured_warnings <- character()
    out <- withCallingHandlers(
      brms::brm(
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
      ),
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    warning_count <<- length(captured_warnings)
    out
  }, error = function(e) {
    reason <<- conditionMessage(e)
    NULL
  })
  if (!is.null(fit)) {
    saveRDS(fit, task$fit_path)
    draws <- tryCatch(list(
      epred = brms::posterior_epred(fit),
      predict = brms::posterior_predict(fit)
    ), error = function(e) list(error = conditionMessage(e)))
    saveRDS(draws, task$draw_path)
    status <- if (warning_count > 0L) "WARNING" else "SUCCESS"
  }
  ended <- Sys.time()
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason, backend = "rstan",
                       Scenario = task$Scenario, Model_ID = task$Model_ID,
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       chains = task$chains, cores = task$cores, iter = task$iter, warmup = task$warmup,
                       adapt_delta = task$adapt_delta, max_treedepth = task$max_treedepth,
                       warning_count = warning_count,
                       runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
                       fit_path = task$fit_path, draw_path = task$draw_path,
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, draw_path = task$draw_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("sensitivity", max(as.integer(tasks$cores), na.rm = TRUE), "se02b prior-scenario workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_se02b_task_worker, parallel_cfg,
                                 export_names = "fit_se02b_task_worker", packages = "brms",
                                 context = "se02b prior-scenario workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "se02b prior-scenario workers")
phase_end("se02b", "Fit prior-scenario workers")
