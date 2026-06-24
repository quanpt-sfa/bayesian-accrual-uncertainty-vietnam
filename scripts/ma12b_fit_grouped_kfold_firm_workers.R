# Script: ma12b_fit_grouped_kfold_firm_workers.R
# Purpose: Run grouped-firm exact K-fold fit tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("ma12b", "Fit grouped-firm exact K-fold workers")

tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma12_grouped_kfold_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma12_grouped_kfold_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing ma12a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
accrual_assert_kfold_manifest_matches_config(tasks, "grouped_firm", "ma12b grouped K-fold workers")
run_status_path <- if ("Kfold_Run_Root" %in% names(tasks)) {
  file.path(tasks$Kfold_Run_Root[1], "tables", "table_ma12_grouped_kfold_task_status.csv")
} else {
  status_path
}

fit_ma12b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(task$result_path) || is.na(task$result_path) || !nzchar(task$result_path)) task$result_path <- task$prediction_path
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  writeLines(c("ma12b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  result <- tryCatch({
    df <- read_winsor_sample(task$Target_Sample, prefactor = TRUE)
    if (!"Fold_Assignment_Path" %in% names(task) || is.na(task$Fold_Assignment_Path) || !nzchar(task$Fold_Assignment_Path)) {
      stop("Task manifest is missing Fold_Assignment_Path.")
    }
    if (!file.exists(task$Fold_Assignment_Path)) stop("Missing planned grouped K-fold assignment: ", task$Fold_Assignment_Path)
    fold_map <- read.csv(task$Fold_Assignment_Path, stringsAsFactors = FALSE)
    if (anyDuplicated(fold_map$company)) stop("Grouped K-fold assignment has duplicate company rows.")
    df <- merge(df, fold_map[, c("company", "Fold_ID"), drop = FALSE], by = "company", all.x = TRUE, sort = FALSE)
    if (any(is.na(df$Fold_ID))) stop("Grouped K-fold assignment does not cover every company in task sample.")
    train_df <- df[df$Fold_ID != as.integer(task$Fold_ID), , drop = FALSE]
    test_df <- df[df$Fold_ID == as.integer(task$Fold_ID), , drop = FALSE]
    if (!nrow(train_df) || !nrow(test_df)) stop("Empty grouped K-fold train/test split.")
    assert_training_factor_level_coverage(train_df, test_df, c("industry", "year"),
                                          paste("ma12b", task$Target_Space, task$Model_ID, "fold", task$Fold_ID))
    formula_str <- fix_formula(task$brms_Formula, prefactor = TRUE)
    fit <- NULL
    if (file.exists(task$fit_path)) {
      if (isTRUE(accrual_assert_reusable_fit_metadata(task, paste("ma12b", task$Task_Key)))) {
        fit <- tryCatch(readRDS(task$fit_path), error = function(e) NULL)
      }
    }
    if (is.null(fit)) {
      fit <- brms::brm(
        formula = brms::bf(stats::as.formula(formula_str)),
        data = train_df,
        family = brms_family(),
        prior = default_prior_list(task$Heterogeneity_Variant, model_structure = model_structure),
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
    ll <- brms::log_lik(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE)
    ep <- brms::posterior_epred(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE)
    pred <- tryCatch(brms::posterior_predict(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE), error = function(e) NULL)
    lpd <- apply(ll, 2, log_mean_exp)
    pred_mean <- colMeans(ep)
    pred_sd <- if (is.null(pred)) rep(NA_real_, nrow(test_df)) else apply(pred, 2, stats::sd)
    obs <- data.frame(
      Target_Space = task$Target_Space, Sample_Group = task$Sample_Group, Fold_ID = as.integer(task$Fold_ID),
      Obs_ID = paste(task$Target_Space, test_df$company, test_df$year, sep = "::"),
      company = test_df$company, year = test_df$year, Model_ID = task$Model_ID,
      Model_Name = task$Model_Name, Heterogeneity_Variant = task$Heterogeneity_Variant,
      Config_Tag = if ("Config_Tag" %in% names(task)) task$Config_Tag else NA_character_,
      Run_Mode = if ("run_mode" %in% names(task)) task$run_mode else NA_character_,
      K = if ("K" %in% names(task)) task$K else NA_integer_,
      Run_ID = if ("Run_ID" %in% names(task)) task$Run_ID else NA_character_,
      lpd_obs = lpd, y_actual = test_df$TA_scaled, pred_mean = pred_mean, pred_sd = pred_sd,
      abs_error = abs(test_df$TA_scaled - pred_mean),
      squared_error = (test_df$TA_scaled - pred_mean)^2,
      Prediction_Rule = "grouped_firm_log_lik_re_formula_NA_population_level",
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      Output_Root = output_root,
      stringsAsFactors = FALSE
    )
    fit_diag <- tryCatch(
      accrual_extract_brms_mcmc_diagnostics(fit, as.integer(task$max_treedepth)),
      error = function(e) list(max_rhat = NA_real_, min_ess_bulk = NA_real_, min_ess_tail = NA_real_,
                               ess_warning = TRUE, divergences = NA_integer_, treedepth_warnings = NA_integer_)
    )
    fold_diag <- data.frame(
      Target_Space = task$Target_Space, Sample_Group = task$Sample_Group,
      Fold_ID = as.integer(task$Fold_ID), Model_ID = task$Model_ID,
      Model_Name = task$Model_Name, Heterogeneity_Variant = task$Heterogeneity_Variant,
      N_Train_Obs = nrow(train_df), N_Test_Obs = nrow(test_df), Completed = TRUE,
      Failure_Reason = NA_character_,
      Max_Rhat = fit_diag$max_rhat,
      Min_ESS_Bulk = fit_diag$min_ess_bulk,
      Min_ESS_Tail = fit_diag$min_ess_tail,
      ESS_Warning = fit_diag$ess_warning,
      Divergences = fit_diag$divergences,
      Treedepth_Warnings = fit_diag$treedepth_warnings,
      Runtime_Seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      stringsAsFactors = FALSE
    )
    out <- list(fold_diag = fold_diag, obs_scores = obs, standardization_audit = data.frame())
    saveRDS(out, task$result_path)
    list(status = "SUCCESS", reason = NA_character_, value = out)
  }, error = function(e) {
    list(status = "FAILED", reason = conditionMessage(e), value = NULL)
  })
  status <- result$status
  reason <- result$reason
  ended <- Sys.time()
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason,
                       backend = if ("backend" %in% names(task)) task$backend else "rstan",
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       chains = task$chains, cores = task$cores, iter = task$iter, warmup = task$warmup,
                       adapt_delta = task$adapt_delta, max_treedepth = task$max_treedepth,
                       refresh = if ("refresh" %in% names(task)) task$refresh else NA_integer_,
                       sampler_profile = if ("sampler_profile" %in% names(task)) task$sampler_profile else NA_character_,
                       run_mode = if ("run_mode" %in% names(task)) task$run_mode else NA_character_,
                       config_source = if ("config_source" %in% names(task)) task$config_source else NA_character_,
                       runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
                       fit_path = task$fit_path,
                       result_path = task$result_path,
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE, fileEncoding = "UTF-8")
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, prediction_path = task$prediction_path,
             result_path = task$result_path, stringsAsFactors = FALSE)
}

parallel_cfg <- accrual_fit_worker_config("grouped_kfold", max(as.integer(tasks$cores), na.rm = TRUE), "ma12b grouped K-fold workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_ma12b_task_worker, parallel_cfg,
                                 export_names = "fit_ma12b_task_worker", packages = "brms",
                                 context = "ma12b grouped K-fold workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
if (!identical(normalizePath(status_path, winslash = "/", mustWork = FALSE),
               normalizePath(run_status_path, winslash = "/", mustWork = FALSE))) {
  write_task_status(run_status_path, status)
}
accrual_task_status_blocker(status, required_col = "Required", context = "ma12b grouped K-fold workers")
phase_end("ma12b", "Fit grouped-firm exact K-fold workers")
