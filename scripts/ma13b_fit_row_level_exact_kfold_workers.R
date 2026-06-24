# Script: ma13b_fit_row_level_exact_kfold_workers.R
# Purpose: Run row-level exact K-fold fit tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("ma13b", "Fit row-level exact K-fold workers")
tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma13_row_kfold_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma13_row_kfold_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing ma13a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)
fit_ma13b_task_worker <- function(task) {
  task <- as.list(task)
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(task$result_path) || is.na(task$result_path) || !nzchar(task$result_path)) task$result_path <- task$prediction_path
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  writeLines(c("ma13b task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  result <- tryCatch({
    df <- read_winsor_sample(task$Target_Sample, prefactor = TRUE)
    df$row_id <- seq_len(nrow(df))
    set.seed(as.integer(task$Canonical_Seed) + 13000L)
    fold_vec <- integer(nrow(df))
    for (cc in sort(unique(df$company))) {
      idx <- which(df$company == cc)
      idx <- sample(idx, length(idx))
      local_folds <- rep(seq_len(min(max(as.integer(tasks$Fold_ID)), length(idx))), length.out = length(idx))
      fold_vec[idx] <- sample(local_folds, length(local_folds))
    }
    df$Fold_ID <- fold_vec
    train_df <- df[df$Fold_ID != as.integer(task$Fold_ID), , drop = FALSE]
    test_df <- df[df$Fold_ID == as.integer(task$Fold_ID), , drop = FALSE]
    if (!nrow(train_df) || !nrow(test_df)) stop("Empty row K-fold train/test split.")
    assert_training_factor_level_coverage(train_df, test_df, c("industry", "year"),
                                          paste("ma13b", task$Target_Space, task$Model_ID, "fold", task$Fold_ID))
    formula_str <- fix_formula(task$brms_Formula, prefactor = TRUE)
    fit <- if (file.exists(task$fit_path)) tryCatch(readRDS(task$fit_path), error = function(e) NULL) else NULL
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
    same_firm_history <- test_df$company %in% train_df$company
    ll <- if (any(!same_firm_history)) {
      brms::log_lik(
        fit,
        newdata = test_df,
        re_formula = NULL,
        allow_new_levels = TRUE,
        sample_new_levels = "uncertainty"
      )
    } else {
      brms::log_lik(
        fit,
        newdata = test_df,
        re_formula = NULL,
        allow_new_levels = FALSE
      )
    }
    obs <- data.frame(
      target_space = task$Target_Space, model_id = task$Model_ID, model_name = task$Model_Name,
      heterogeneity_variant = task$Heterogeneity_Variant, sample_group = task$Sample_Group,
      fold = as.integer(task$Fold_ID), company = test_df$company, year = test_df$year,
      row_id = test_df$row_id, observation_id = paste(task$Target_Space, test_df$row_id, test_df$company, test_df$year, sep = ":"),
      observed_TA_scaled = test_df$TA_scaled, log_predictive_density = apply(ll, 2, log_mean_exp),
      prediction_rule = ifelse(same_firm_history, "heldout_log_lik_re_formula_NULL_same_firm_history", "heldout_log_lik_re_formula_NULL_new_level_uncertainty_fallback"),
      same_firm_history_available = same_firm_history,
      primary_row_target_inclusion = same_firm_history,
      stringsAsFactors = FALSE
    )
    fold_diag <- data.frame(
      Target_Space = task$Target_Space, Fold_ID = as.integer(task$Fold_ID), Model_ID = task$Model_ID,
      Model_Name = task$Model_Name, Heterogeneity_Variant = task$Heterogeneity_Variant,
      N_Train_Obs = nrow(train_df), N_Test_Obs = nrow(test_df),
      N_Test_Obs_No_Same_Firm_History = sum(!same_firm_history),
      Any_New_Company_In_Row_Fold = any(!same_firm_history),
      Completed = TRUE, Failure_Reason = NA_character_, stringsAsFactors = FALSE
    )
    out <- list(fold_diag = fold_diag, obs_scores = obs, standardization_audit = data.frame())
    saveRDS(out, task$result_path)
    status <<- "SUCCESS"
    out
  }, error = function(e) {
    reason <<- conditionMessage(e)
    NULL
  })
  ended <- Sys.time()
  write.csv(data.frame(Task_Key = task$Task_Key, status = status, reason = reason, backend = "rstan",
                       RNG_Context = task$RNG_Context, Effective_Seed = task$Effective_Seed,
                       chains = task$chains, cores = task$cores, iter = task$iter, warmup = task$warmup,
                       adapt_delta = task$adapt_delta, max_treedepth = task$max_treedepth,
                       runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
                       stringsAsFactors = FALSE), task$metadata_path, row.names = FALSE)
  data.frame(Task_Key = task$Task_Key, status = status, reason = reason, Required = task$Required,
             fit_path = task$fit_path, prediction_path = task$prediction_path,
             result_path = task$result_path, stringsAsFactors = FALSE)
}
parallel_cfg <- accrual_fit_worker_config("row_kfold", max(as.integer(tasks$cores), na.rm = TRUE), "ma13b row K-fold workers")
results <- accrual_run_task_pool(split(tasks, seq_len(nrow(tasks))), fit_ma13b_task_worker, parallel_cfg,
                                 export_names = c("fit_ma13b_task_worker", "tasks"), packages = "brms",
                                 context = "ma13b row K-fold workers")
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "ma13b row K-fold workers")
phase_end("ma13b", "Fit row-level exact K-fold workers")
