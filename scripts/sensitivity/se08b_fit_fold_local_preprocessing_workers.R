# Script: se08b_fit_fold_local_preprocessing_workers.R
# Purpose: Fit fold-local preprocessing exact K-fold sensitivity worker tasks.

source("scripts/ma00_setup.R")
phase_begin("se08b", "Fit fold-local preprocessing exact K-fold sensitivity workers")

se08_root <- file.path(output_root, "sensitivity", "fold_local_preprocessing")
tables_dir <- file.path(se08_root, "tables")
manifest_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing se08a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE)

compute_train_winsor_cutoffs <- function(train_df, vars, probs = c(0.01, 0.99)) {
  vars <- intersect(vars, names(train_df))
  rows <- lapply(vars, function(v) {
    vals <- suppressWarnings(as.numeric(train_df[[v]]))
    vals <- vals[is.finite(vals)]
    qs <- if (length(vals)) stats::quantile(vals, probs = probs, na.rm = TRUE, names = FALSE, type = 7) else c(NA_real_, NA_real_)
    data.frame(variable = v, train_cutoff_p01 = qs[1], train_cutoff_p99 = qs[2],
               n_train_nonmissing = length(vals), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

apply_winsor_cutoffs <- function(df, cutoffs) {
  for (i in seq_len(nrow(cutoffs))) {
    v <- cutoffs$variable[i]
    if (!v %in% names(df)) next
    lo <- as.numeric(cutoffs$train_cutoff_p01[i])
    hi <- as.numeric(cutoffs$train_cutoff_p99[i])
    if (is.finite(lo) && is.finite(hi)) df[[v]] <- pmin(pmax(df[[v]], lo), hi)
  }
  df
}

compute_train_standardization_params <- function(train_df, pred_vars) {
  vars <- intersect(pred_vars, names(train_df))
  rows <- lapply(vars, function(v) {
    vals <- suppressWarnings(as.numeric(train_df[[v]]))
    data.frame(variable = v, train_mean = mean(vals, na.rm = TRUE),
               train_sd = stats::sd(vals, na.rm = TRUE),
               n_train_nonmissing = sum(is.finite(vals)), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

apply_standardization_params <- function(df, params) {
  for (i in seq_len(nrow(params))) {
    v <- params$variable[i]
    if (!v %in% names(df)) next
    m <- as.numeric(params$train_mean[i])
    s <- as.numeric(params$train_sd[i])
    df[[paste0(v, "_std")]] <- if (is.finite(s) && s > 0) (df[[v]] - m) / s else 0
  }
  df
}

read_global_cutoffs <- function(path, target_space) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(data.frame())
  x <- read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("Variable", "P01_Cutoff", "P99_Cutoff") %in% names(x))) return(data.frame())
  if ("Sample" %in% names(x)) {
    sample_pat <- if (identical(target_space, "ex_post")) "Ex-Post|ex_post" else "Realtime|Real-Time|real_time"
    x <- x[grepl(sample_pat, x$Sample, ignore.case = TRUE), , drop = FALSE]
  }
  data.frame(variable = x$Variable,
             global_cutoff_p01 = suppressWarnings(as.numeric(x$P01_Cutoff)),
             global_cutoff_p99 = suppressWarnings(as.numeric(x$P99_Cutoff)),
             stringsAsFactors = FALSE)
}

prepare_fold_local_train_test <- function(sample_df, fold_assignment, fold_id, validation_scheme, target_space, primary_winsor_target_sample) {
  sample_df$row_id <- seq_len(nrow(sample_df))
  if (identical(validation_scheme, "grouped_firm_kfold")) {
    fold_assignment$company <- normalize_join_key_values(fold_assignment$company)
    sample_df$company <- normalize_join_key_values(sample_df$company)
    joined <- merge(sample_df, fold_assignment[, c("company", "Fold_ID"), drop = FALSE], by = "company", all.x = TRUE, sort = FALSE)
    if (any(is.na(joined$Fold_ID))) stop("Grouped fold assignment does not cover every firm in the raw sample.")
  } else {
    fa <- fold_assignment
    if ("Target_Space" %in% names(fa)) fa <- fa[fa$Target_Space == target_space, , drop = FALSE]
    if ("Target_Sample" %in% names(fa)) fa <- fa[fa$Target_Sample == primary_winsor_target_sample, , drop = FALSE]
    if (!"row_id" %in% names(fa)) stop("Row fold assignment lacks row_id.")
    joined <- merge(sample_df, fa[, c("row_id", "Fold_ID"), drop = FALSE], by = "row_id", all.x = TRUE, sort = FALSE)
    if (any(is.na(joined$Fold_ID))) stop("Row fold assignment does not cover every raw-sample row.")
  }
  train_df <- joined[joined$Fold_ID != as.integer(fold_id), , drop = FALSE]
  test_df <- joined[joined$Fold_ID == as.integer(fold_id), , drop = FALSE]
  if (!nrow(train_df) || !nrow(test_df)) stop("Empty fold-local train/test split.")
  list(train = train_df, test = test_df)
}

audit_fold_local_preprocessing <- function(train_df_raw, test_df_raw, train_df_processed, test_df_processed,
                                           cutoffs, params, task, global_cutoffs) {
  winsor_rows <- lapply(seq_len(nrow(cutoffs)), function(i) {
    v <- cutoffs$variable[i]
    lo <- cutoffs$train_cutoff_p01[i]
    hi <- cutoffs$train_cutoff_p99[i]
    g <- global_cutoffs[global_cutoffs$variable == v, , drop = FALSE]
    data.frame(
      validation_scheme = task$Validation_Scheme,
      target_space = task$Target_Space,
      fold_id = as.integer(task$Fold_ID),
      model_id = task$Model_ID,
      heterogeneity_variant = task$Heterogeneity_Variant,
      variable = v,
      preprocessing_step = "winsorization",
      train_cutoff_p01 = lo,
      train_cutoff_p99 = hi,
      train_mean = NA_real_,
      train_sd = NA_real_,
      n_train_nonmissing = sum(is.finite(suppressWarnings(as.numeric(train_df_raw[[v]])))),
      n_test_nonmissing = sum(is.finite(suppressWarnings(as.numeric(test_df_raw[[v]])))),
      n_train_capped_low = sum(suppressWarnings(as.numeric(train_df_raw[[v]])) < lo, na.rm = TRUE),
      n_train_capped_high = sum(suppressWarnings(as.numeric(train_df_raw[[v]])) > hi, na.rm = TRUE),
      n_test_capped_low = sum(suppressWarnings(as.numeric(test_df_raw[[v]])) < lo, na.rm = TRUE),
      n_test_capped_high = sum(suppressWarnings(as.numeric(test_df_raw[[v]])) > hi, na.rm = TRUE),
      share_test_capped_low = mean(suppressWarnings(as.numeric(test_df_raw[[v]])) < lo, na.rm = TRUE),
      share_test_capped_high = mean(suppressWarnings(as.numeric(test_df_raw[[v]])) > hi, na.rm = TRUE),
      global_cutoff_p01 = if (nrow(g)) g$global_cutoff_p01[1] else NA_real_,
      global_cutoff_p99 = if (nrow(g)) g$global_cutoff_p99[1] else NA_real_,
      delta_train_vs_global_p01 = if (nrow(g)) lo - g$global_cutoff_p01[1] else NA_real_,
      delta_train_vs_global_p99 = if (nrow(g)) hi - g$global_cutoff_p99[1] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  std_rows <- lapply(seq_len(nrow(params)), function(i) {
    v <- params$variable[i]
    data.frame(
      validation_scheme = task$Validation_Scheme,
      target_space = task$Target_Space,
      fold_id = as.integer(task$Fold_ID),
      model_id = task$Model_ID,
      heterogeneity_variant = task$Heterogeneity_Variant,
      variable = v,
      preprocessing_step = "standardization",
      train_cutoff_p01 = NA_real_,
      train_cutoff_p99 = NA_real_,
      train_mean = params$train_mean[i],
      train_sd = params$train_sd[i],
      n_train_nonmissing = sum(is.finite(suppressWarnings(as.numeric(train_df_processed[[v]])))),
      n_test_nonmissing = sum(is.finite(suppressWarnings(as.numeric(test_df_processed[[v]])))),
      n_train_capped_low = NA_integer_,
      n_train_capped_high = NA_integer_,
      n_test_capped_low = NA_integer_,
      n_test_capped_high = NA_integer_,
      share_test_capped_low = NA_real_,
      share_test_capped_high = NA_real_,
      global_cutoff_p01 = NA_real_,
      global_cutoff_p99 = NA_real_,
      delta_train_vs_global_p01 = NA_real_,
      delta_train_vs_global_p99 = NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(winsor_rows, std_rows))
}

fit_se08_task_worker <- function(task) {
  task <- as.list(task)
  started <- Sys.time()
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$result_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$task_log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("se08 task log", paste("Task_Key:", task$Task_Key), paste("Effective_Seed:", task$Effective_Seed)), task$task_log_path)
  result <- tryCatch({
    sample_df <- read.csv(task$Source_Sample_Path, stringsAsFactors = FALSE)
    fold_assignment <- read.csv(task$Fold_Assignment_Path, stringsAsFactors = FALSE)
    split <- prepare_fold_local_train_test(sample_df, fold_assignment, task$Fold_ID, task$Validation_Scheme,
                                           task$Target_Space, task$Primary_Winsor_Target_Sample)
    train_raw <- split$train
    test_raw <- split$test
    cutoffs <- compute_train_winsor_cutoffs(train_raw, continuous_vars_to_winsor, probs = c(0.01, 0.99))
    train_win <- apply_winsor_cutoffs(train_raw, cutoffs)
    test_win <- apply_winsor_cutoffs(test_raw, cutoffs)
    params <- compute_train_standardization_params(train_win, pred_vars)
    train_df <- apply_standardization_params(train_win, params)
    test_df <- apply_standardization_params(test_win, params)
    train_df$industry_f <- factor(train_df$industry)
    train_df$year_f <- factor(train_df$year)
    test_df$industry_f <- factor(test_df$industry, levels = levels(train_df$industry_f))
    test_df$year_f <- factor(test_df$year, levels = levels(train_df$year_f))
    assert_training_factor_level_coverage(train_df, test_df, c("industry", "year"),
                                          paste("se08", task$Validation_Scheme, task$Target_Space, task$Model_ID, "fold", task$Fold_ID))
    formula_str <- fix_formula(task$brms_Formula, prefactor = TRUE)
    fit <- if (file.exists(task$fit_path) && !force_refit) tryCatch(readRDS(task$fit_path), error = function(e) NULL) else NULL
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
    if (identical(task$Validation_Scheme, "grouped_firm_kfold")) {
      ll <- brms::log_lik(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE)
      ep <- brms::posterior_epred(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE)
      obs <- data.frame(
        Target_Space = task$Target_Space, Sample_Group = task$Sample_Group, Fold_ID = as.integer(task$Fold_ID),
        Obs_ID = paste(task$Target_Space, test_df$company, test_df$year, sep = "::"),
        company = test_df$company, year = test_df$year, Model_ID = task$Model_ID,
        Model_Name = task$Model_Name, Heterogeneity_Variant = task$Heterogeneity_Variant,
        lpd_obs = apply(ll, 2, log_mean_exp), y_actual = test_df$TA_scaled, pred_mean = colMeans(ep),
        Prediction_Rule = "fold_local_grouped_firm_log_lik_re_formula_NA_population_level",
        stringsAsFactors = FALSE
      )
    } else {
      same_firm_history <- test_df$company %in% train_df$company
      ll <- if (any(!same_firm_history)) {
        brms::log_lik(fit, newdata = test_df, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "uncertainty")
      } else {
        brms::log_lik(fit, newdata = test_df, re_formula = NULL, allow_new_levels = FALSE)
      }
      obs <- data.frame(
        target_space = task$Target_Space, model_id = task$Model_ID, model_name = task$Model_Name,
        heterogeneity_variant = task$Heterogeneity_Variant, sample_group = task$Sample_Group,
        fold = as.integer(task$Fold_ID), company = test_df$company, year = test_df$year,
        row_id = test_df$row_id, observation_id = paste(task$Target_Space, test_df$row_id, test_df$company, test_df$year, sep = ":"),
        observed_TA_scaled = test_df$TA_scaled, log_predictive_density = apply(ll, 2, log_mean_exp),
        prediction_rule = ifelse(same_firm_history, "fold_local_heldout_log_lik_re_formula_NULL_same_firm_history", "fold_local_heldout_log_lik_re_formula_NULL_new_level_uncertainty_fallback"),
        same_firm_history_available = same_firm_history,
        new_company_in_row_fold = !same_firm_history,
        primary_row_target_inclusion = same_firm_history,
        stringsAsFactors = FALSE
      )
    }
    fit_diag <- tryCatch(
      accrual_extract_brms_mcmc_diagnostics(fit, as.integer(task$max_treedepth)),
      error = function(e) list(max_rhat = NA_real_, min_ess_bulk = NA_real_, min_ess_tail = NA_real_,
                               ess_warning = TRUE, divergences = NA_integer_, treedepth_warnings = NA_integer_)
    )
    fold_diag <- data.frame(
      Validation_Scheme = task$Validation_Scheme,
      Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group,
      Fold_ID = as.integer(task$Fold_ID),
      Model_ID = task$Model_ID,
      Model_Name = task$Model_Name,
      Heterogeneity_Variant = task$Heterogeneity_Variant,
      N_Train_Obs = nrow(train_df),
      N_Test_Obs = nrow(test_df),
      N_Test_Obs_No_Same_Firm_History = if (identical(task$Validation_Scheme, "row_exact_kfold")) sum(!same_firm_history) else 0L,
      Any_New_Company_In_Row_Fold = if (identical(task$Validation_Scheme, "row_exact_kfold")) any(!same_firm_history) else FALSE,
      Completed = TRUE,
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
    audit <- audit_fold_local_preprocessing(
      train_raw, test_raw, train_df, test_df, cutoffs, params, task,
      read_global_cutoffs(task$Global_Winsor_Cutoff_Path, task$Target_Space)
    )
    out <- list(fold_diag = fold_diag, obs_scores = obs, preprocessing_audit = audit)
    saveRDS(out, task$result_path)
    list(status = "SUCCESS", reason = NA_character_)
  }, error = function(e) {
    list(status = "FAILED", reason = conditionMessage(e))
  })
  ended <- Sys.time()
  write_csv_safely(data.frame(
    Task_Key = task$Task_Key,
    status = result$status,
    reason = result$reason,
    backend = if ("backend" %in% names(task)) task$backend else "rstan",
    RNG_Context = task$RNG_Context,
    Effective_Seed = task$Effective_Seed,
    chains = task$chains,
    cores = task$cores,
    iter = task$iter,
    warmup = task$warmup,
    adapt_delta = task$adapt_delta,
    max_treedepth = task$max_treedepth,
    runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
    fit_path = task$fit_path,
    result_path = task$result_path,
    stringsAsFactors = FALSE
  ), task$metadata_path, row.names = FALSE, fileEncoding = "UTF-8")
  data.frame(Task_Key = task$Task_Key, status = result$status, reason = result$reason,
             Required = task$Required, fit_path = task$fit_path, result_path = task$result_path,
             stringsAsFactors = FALSE)
}

parallel_cfg <- accrual_fit_worker_config("se08_fold_local_preprocessing", max(as.integer(tasks$cores), na.rm = TRUE),
                                          "se08 fold-local preprocessing sensitivity workers")
results <- accrual_run_task_pool(
  split(tasks, seq_len(nrow(tasks))),
  fit_se08_task_worker,
  parallel_cfg,
  export_names = c(
    "fit_se08_task_worker",
    "compute_train_winsor_cutoffs",
    "apply_winsor_cutoffs",
    "compute_train_standardization_params",
    "apply_standardization_params",
    "prepare_fold_local_train_test",
    "audit_fold_local_preprocessing",
    "read_global_cutoffs"
  ),
  packages = "brms",
  context = "se08 fold-local preprocessing sensitivity workers"
)
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "se08 fold-local preprocessing sensitivity workers")
phase_end("se08b", "Fit fold-local preprocessing exact K-fold sensitivity workers")
