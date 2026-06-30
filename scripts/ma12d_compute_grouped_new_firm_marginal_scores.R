# Script: ma12d_compute_grouped_new_firm_marginal_scores.R
# Purpose: Rescore grouped-firm exact K-fold Firm-RE candidates with a
#          marginal new-firm random-intercept predictive density.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma12d", "Compute grouped-firm marginal new-firm exact K-fold scores")

script_name <- "scripts/ma12d_compute_grouped_new_firm_marginal_scores.R"
script_version <- "marginal-new-firm-rescore-v1"
script_start_time <- Sys.time()

ma12d_env <- list(
  new_firm_draws = env_int("ACCRUAL_MA12D_NEW_FIRM_DRAWS", 20L, min = 1L),
  max_posterior_draws = env_int("ACCRUAL_MA12D_MAX_POSTERIOR_DRAWS", 2000L, min = 1L),
  seed = env_int("ACCRUAL_MA12D_SEED", accrual_seed_for("ma12d_grouped_new_firm_marginal"), min = 0L),
  force_recompute = env_flag("ACCRUAL_MA12D_FORCE_RECOMPUTE", "FALSE"),
  material_weight_change = env_num("ACCRUAL_MA12D_WEIGHT_CHANGE_MATERIAL", 0.05, min = 0),
  source_kfold_run_root = trimws(env_value("ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT", "")),
  source_row_kfold_run_root = trimws(env_value("ACCRUAL_MA12D_SOURCE_ROW_KFOLD_RUN_ROOT", "")),
  output_run_root = trimws(env_value("ACCRUAL_MA12D_OUTPUT_RUN_ROOT", ""))
)

firm_re_indicator <- function(variant) {
  grepl("Firm RE|Random Intercept|firm_RE|firmre", as.character(variant), ignore.case = TRUE)
}

nonempty <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
}

single_line <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  x <- trimws(readLines(path, warn = FALSE))
  x <- x[nzchar(x)]
  if (!length(x)) NA_character_ else x[[1]]
}

file_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

resolve_source_kfold_run_root <- function() {
  explicit <- ma12d_env$source_kfold_run_root
  if (nonempty(explicit)) {
    root <- normalizePath(explicit, winslash = "/", mustWork = FALSE)
    return(list(root = root, resolution = "ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT"))
  }

  completed_pin <- file.path(output_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt")
  pinned <- single_line(completed_pin)
  if (nonempty(pinned)) {
    root <- normalizePath(pinned, winslash = "/", mustWork = FALSE)
    return(list(root = root, resolution = completed_pin))
  }

  stop(
    "[BLOCKER] MA12D could not resolve a completed grouped K-fold source run. ",
    "Set ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT or create ",
    completed_pin, "."
  )
}

require_source_run_contract <- function(source) {
  root <- source$root
  if (!dir.exists(root)) {
    stop("[BLOCKER] MA12D source grouped K-fold run root does not exist: ", root)
  }
  manifest_path <- file.path(root, "tables", "table_ma12_grouped_kfold_task_manifest.csv")
  status_path <- file.path(root, "tables", "table_ma12_grouped_kfold_task_status.csv")
  if (!file.exists(manifest_path) || !file.exists(status_path)) {
    stop(
      "[BLOCKER] MA12D source grouped K-fold run root lacks required task artifacts. ",
      "Expected ", manifest_path, " and ", status_path, "."
    )
  }
  obs_path <- file.path(root, "tables", "table_winsor_kfold_observation_scores.csv")
  model_path <- file.path(root, "tables", "table_winsor_kfold_model_scores.csv")
  weights_ep_path <- file.path(root, "tables", "table_winsor_kfold_weights_ex_post.csv")
  weights_rt_path <- file.path(root, "tables", "table_winsor_kfold_weights_no_lookahead.csv")
  if (!file.exists(obs_path)) {
    stop("[BLOCKER] MA12D requires source MA12C observation scores for baseline comparison/copying: ", obs_path)
  }
  list(
    root = root,
    resolution = source$resolution,
    manifest_path = manifest_path,
    status_path = status_path,
    observation_scores_path = obs_path,
    model_scores_path = model_path,
    weights_ex_post_path = weights_ep_path,
    weights_no_lookahead_path = weights_rt_path
  )
}

make_output_run_root <- function() {
  if (nonempty(ma12d_env$output_run_root)) {
    return(normalizePath(ma12d_env$output_run_root, winslash = "/", mustWork = FALSE))
  }
  run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file.path(output_root, "grouped_new_firm_marginal", paste0("ma12d_", run_id))
}

write_dual_csv <- function(x, file_name, tables_dir, compat_tables_dir = file.path(output_root, "tables")) {
  run_path <- file.path(tables_dir, file_name)
  compat_path <- file.path(compat_tables_dir, file_name)
  write_csv_safely(x, run_path, row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safely(x, compat_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(run_path)
}

write_blocker_decision <- function(decision_code, interpretation, source_contract, manifest = NULL, tables_dir) {
  target_spaces <- if (!is.null(manifest) && "Target_Space" %in% names(manifest)) {
    unique(as.character(manifest$Target_Space))
  } else {
    NA_character_
  }
  if (!length(target_spaces)) target_spaces <- NA_character_
  source_fold_path <- if (!is.null(manifest) && "Fold_Assignment_Path" %in% names(manifest)) {
    paste(unique(as.character(manifest$Fold_Assignment_Path)), collapse = ";")
  } else {
    NA_character_
  }
  decision <- data.frame(
    Target_Space = target_spaces,
    Top_Cell_Population_Level = NA_character_,
    Top_Cell_Marginal_New_Firm = NA_character_,
    Aggregate_FirmRE_Weight_Population_Level = NA_real_,
    Aggregate_FirmRE_Weight_Marginal_New_Firm = NA_real_,
    Absolute_FirmRE_Weight_Change = NA_real_,
    Top_Cell_Changed = NA,
    Conclusion_Changed = NA,
    Decision = decision_code,
    Interpretation = interpretation,
    Source_KFold_Run_Root = source_contract$root,
    Source_KFold_Run_Root_Resolution = source_contract$resolution,
    Source_KFold_Manifest_Path = source_contract$manifest_path,
    Source_KFold_Status_Path = source_contract$status_path,
    Source_KFold_Fold_Assignment_Path = source_fold_path,
    stringsAsFactors = FALSE
  )
  write_dual_csv(decision, "table_grouped_marginal_new_firm_decision.csv", tables_dir)
  invisible(decision)
}

required_columns <- function(df, cols, context) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop("[BLOCKER] ", context, " missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

normalize_task_paths <- function(tasks) {
  for (nm in c("fit_path", "result_path", "prediction_path", "Fold_Assignment_Path")) {
    if (nm %in% names(tasks)) tasks[[nm]] <- normalizePath(tasks[[nm]], winslash = "/", mustWork = FALSE)
  }
  if (!"result_path" %in% names(tasks)) tasks$result_path <- tasks$prediction_path
  tasks
}

find_draw_column <- function(draws, candidates, required = TRUE, label = "") {
  names_draws <- names(draws)
  hit <- intersect(candidates, names_draws)
  if (length(hit) == 1L) return(hit[[1]])
  if (length(hit) > 1L) {
    stop("[BLOCKER] Ambiguous posterior draw column for ", label, ": ", paste(hit, collapse = ", "))
  }
  if (required) {
    stop("[BLOCKER] Missing posterior draw column for ", label, ". Tried: ", paste(candidates, collapse = ", "))
  }
  NA_character_
}

extract_group_terms <- function(formula_text) {
  txt <- paste(as.character(formula_text), collapse = " ")
  matches <- gregexpr("\\([^|()]*\\|\\s*([^()]+?)\\)", txt, perl = TRUE)
  raw <- regmatches(txt, matches)[[1]]
  if (!length(raw) || identical(raw, character(0))) return(character())
  terms <- sub("^\\([^|()]*\\|\\s*", "", raw, perl = TRUE)
  terms <- sub("\\)$", "", terms)
  terms <- sub("^gr\\(([^,\\)]+).*$", "\\1", terms)
  terms <- gsub("[^A-Za-z0-9_.]", "", terms)
  unique(terms[nzchar(terms)])
}

extract_firm_intercept_sd_draws <- function(fit, task, draws) {
  sd_cols <- grep("^sd_.*__Intercept$", names(draws), value = TRUE)
  if (!length(sd_cols)) {
    stop("[BLOCKER] Firm-RE task has no group-level intercept posterior SD column matching ^sd_.*__Intercept$: ", task$Task_Key)
  }

  formula_terms <- unique(c(
    extract_group_terms(if ("brms_Formula" %in% names(task)) task$brms_Formula else ""),
    tryCatch(extract_group_terms(as.character(stats::formula(fit))), error = function(e) character())
  ))
  preferred_terms <- unique(c(formula_terms[grepl("company|firm", formula_terms, ignore.case = TRUE)], "company", "firm"))
  preferred_terms <- preferred_terms[nzchar(preferred_terms)]

  preferred_cols <- character()
  for (term in preferred_terms) {
    pattern <- paste0("^sd_", gsub("([.])", "\\\\\\1", term), "__Intercept$")
    preferred_cols <- unique(c(preferred_cols, grep(pattern, sd_cols, value = TRUE)))
  }
  if (length(preferred_cols) == 1L) return(as.numeric(draws[[preferred_cols[[1]]]]))
  if (length(preferred_cols) > 1L) {
    stop("[BLOCKER] Ambiguous firm random-intercept SD columns for ", task$Task_Key, ": ",
         paste(preferred_cols, collapse = ", "))
  }
  if (length(sd_cols) == 1L) return(as.numeric(draws[[sd_cols[[1]]]]))

  stop(
    "[BLOCKER] Could not identify firm random-intercept SD column for ", task$Task_Key,
    ". Candidate columns: ", paste(sd_cols, collapse = ", "),
    ". Formula group terms: ", paste(formula_terms, collapse = ", ")
  )
}

existing_score_key <- function(df) {
  paste(
    as.character(df$Obs_ID),
    as.character(df$Model_ID),
    as.character(df$Heterogeneity_Variant),
    as.character(df$Target_Space),
    as.integer(df$Fold_ID),
    sep = "\r"
  )
}

lookup_existing_scores <- function(existing_obs, task, test_df) {
  obs_id <- paste(task$Target_Space, test_df$company, test_df$year, sep = "::")
  key_df <- data.frame(
    Obs_ID = obs_id,
    Model_ID = task$Model_ID,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    Target_Space = task$Target_Space,
    Fold_ID = as.integer(task$Fold_ID),
    stringsAsFactors = FALSE
  )
  idx <- match(existing_score_key(key_df), existing_score_key(existing_obs))
  if (any(is.na(idx))) {
    missing_examples <- utils::head(obs_id[is.na(idx)], 10)
    stop("[BLOCKER] Existing MA12 grouped observation scores do not match task ",
         task$Task_Key, ". Missing Obs_ID examples: ", paste(missing_examples, collapse = ", "))
  }
  existing_obs[idx, , drop = FALSE]
}

validate_grouped_split <- function(df, fold_map, task) {
  required_columns(fold_map, c("company", "Fold_ID"), "MA12D fold assignment")
  fold_map$company <- normalize_join_key_values(fold_map$company)
  if (anyDuplicated(fold_map$company)) stop("[BLOCKER] Grouped K-fold assignment has duplicate company rows.")
  df$company <- normalize_join_key_values(df$company)
  merged <- merge(df, fold_map[, c("company", "Fold_ID"), drop = FALSE], by = "company", all.x = TRUE, sort = FALSE)
  if (any(is.na(merged$Fold_ID))) {
    missing_firms <- unique(merged$company[is.na(merged$Fold_ID)])
    stop("[BLOCKER] Fold assignment does not cover every company in ", task$Target_Sample,
         ". Missing examples: ", paste(utils::head(missing_firms, 10), collapse = ", "))
  }
  train_df <- merged[merged$Fold_ID != as.integer(task$Fold_ID), , drop = FALSE]
  test_df <- merged[merged$Fold_ID == as.integer(task$Fold_ID), , drop = FALSE]
  if (!nrow(train_df) || !nrow(test_df)) stop("[BLOCKER] Empty grouped K-fold train/test split for ", task$Task_Key)
  overlap <- intersect(unique(train_df$company), unique(test_df$company))
  if (length(overlap)) {
    stop("[BLOCKER] Grouped K-fold split leaks held-out firms into training for ", task$Task_Key,
         ". Examples: ", paste(utils::head(overlap, 10), collapse = ", "))
  }
  list(train_df = train_df, test_df = test_df)
}

subsample_draws <- function(n_draws, max_draws, seed, task_key) {
  if (n_draws <= 0L) stop("[BLOCKER] Posterior draw count is zero for ", task_key)
  if (n_draws <= max_draws) return(seq_len(n_draws))
  set.seed(seed + abs(sum(utf8ToInt(as.character(task_key)))) %% 100000L)
  sort(sample(seq_len(n_draws), max_draws, replace = FALSE))
}

student_log_density_matrix <- function(y, mu, sigma, nu) {
  z <- sweep(y - mu, 1L, sigma, "/")
  stats::dt(z, df = nu, log = TRUE) - log(sigma)
}

compute_marginal_new_firm_scores <- function(fit, task, test_df, seed) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("[BLOCKER] MA12D requires the posterior package to inspect brms posterior draws.")
  }
  eta_pop <- brms::posterior_linpred(
    fit,
    newdata = test_df,
    re_formula = NA,
    transform = FALSE,
    allow_new_levels = TRUE
  )
  if (is.null(dim(eta_pop)) || length(dim(eta_pop)) != 2L) {
    stop("[BLOCKER] posterior_linpred did not return a draws-by-observation matrix for ", task$Task_Key)
  }
  draws <- posterior::as_draws_df(fit)
  sigma_col <- find_draw_column(draws, c("sigma"), required = TRUE, label = "Student-t residual scale sigma")
  nu_col <- find_draw_column(draws, c("nu"), required = TRUE, label = "Student-t degrees of freedom nu")
  sigma_u_all <- extract_firm_intercept_sd_draws(fit, task, draws)

  n_draws <- min(nrow(eta_pop), nrow(draws), length(sigma_u_all))
  if (n_draws <= 0L) stop("[BLOCKER] No posterior draws available for ", task$Task_Key)
  eta_pop <- eta_pop[seq_len(n_draws), , drop = FALSE]
  sigma <- as.numeric(draws[[sigma_col]])[seq_len(n_draws)]
  nu <- as.numeric(draws[[nu_col]])[seq_len(n_draws)]
  sigma_u <- sigma_u_all[seq_len(n_draws)]
  keep_draws <- subsample_draws(n_draws, ma12d_env$max_posterior_draws, seed, task$Task_Key)
  eta_pop <- eta_pop[keep_draws, , drop = FALSE]
  sigma <- sigma[keep_draws]
  nu <- nu[keep_draws]
  sigma_u <- sigma_u[keep_draws]
  if (nrow(eta_pop) != length(sigma) || length(sigma) != length(nu) || length(nu) != length(sigma_u)) {
    stop("[BLOCKER] Inconsistent posterior draw lengths after subsampling for ", task$Task_Key)
  }
  if (!all(is.finite(sigma)) || !all(sigma > 0) || !all(is.finite(nu)) || !all(nu > 0) ||
      !all(is.finite(sigma_u)) || !all(sigma_u >= 0)) {
    stop("[BLOCKER] Non-finite or invalid sigma/nu/sigma_u posterior draws for ", task$Task_Key)
  }

  y <- as.numeric(test_df$TA_scaled)
  company <- normalize_join_key_values(test_df$company)
  n_obs <- length(y)
  n_s <- nrow(eta_pop)
  n_r <- ma12d_env$new_firm_draws
  lpd <- rep(NA_real_, n_obs)
  pred_mean <- rep(NA_real_, n_obs)
  pred_sd <- rep(NA_real_, n_obs)

  set.seed(seed + abs(sum(utf8ToInt(as.character(task$Task_Key)))) %% 100000L)
  for (firm in unique(company)) {
    obs_idx <- which(company == firm)
    u_new <- matrix(
      stats::rnorm(n_s * n_r, mean = 0, sd = rep(sigma_u, times = n_r)),
      nrow = n_s,
      ncol = n_r
    )
    for (j in obs_idx) {
      mu <- eta_pop[, j] + u_new
      log_density <- student_log_density_matrix(y[j], mu, sigma, nu)
      lpd[j] <- log_mean_exp(as.vector(log_density))
      mu_vec <- as.vector(mu)
      pred_mean[j] <- mean(mu_vec)
      pred_sd[j] <- stats::sd(mu_vec)
    }
  }

  if (!all(is.finite(lpd))) {
    stop("[BLOCKER] MA12D produced non-finite marginal lpd values for ", task$Task_Key)
  }
  list(
    lpd_obs = lpd,
    pred_mean = pred_mean,
    pred_sd = pred_sd,
    n_posterior_draws_used = n_s,
    n_new_firm_draws = n_r
  )
}

score_one_task <- function(task_row, existing_obs, task_output_dir, force_recompute = FALSE) {
  task <- as.list(task_row)
  result_path <- file.path(task_output_dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", task$Task_Key), "_ma12d_result.rds"))
  expected_meta <- list(
    Task_Key = task$Task_Key,
    Source_KFold_Run_Root = task$Source_KFold_Run_Root,
    N_New_Firm_Draws = ma12d_env$new_firm_draws,
    Max_Posterior_Draws = ma12d_env$max_posterior_draws,
    Prediction_Rule = "grouped_firm_marginal_new_firm_integrated"
  )
  if (file.exists(result_path) && !force_recompute) {
    cached <- tryCatch(readRDS(result_path), error = function(e) NULL)
    if (!is.null(cached) && is.list(cached) && !is.null(cached$metadata)) {
      ok <- all(vapply(names(expected_meta), function(nm) {
        nm %in% names(cached$metadata) && identical(as.character(cached$metadata[[nm]][1]), as.character(expected_meta[[nm]]))
      }, logical(1)))
      if (ok) return(cached)
    }
  }

  if (!file.exists(task$fit_path)) stop("[BLOCKER] Missing required MA12 fit object: ", task$fit_path)
  if (!file.exists(task$Fold_Assignment_Path)) stop("[BLOCKER] Missing grouped fold assignment: ", task$Fold_Assignment_Path)
  df <- read_winsor_sample(task$Target_Sample, prefactor = TRUE)
  fold_map <- read.csv(task$Fold_Assignment_Path, stringsAsFactors = FALSE, check.names = FALSE)
  split <- validate_grouped_split(df, fold_map, task)
  test_df <- split$test_df
  existing <- lookup_existing_scores(existing_obs, task, test_df)
  is_firm_re <- isTRUE(as.logical(task$Requires_Marginal_New_Firm))
  started <- Sys.time()

  marginal <- NULL
  if (is_firm_re) {
    fit <- readRDS(task$fit_path)
    marginal <- compute_marginal_new_firm_scores(fit, task, test_df, ma12d_env$seed)
    lpd_obs <- marginal$lpd_obs
    pred_mean_marginal <- marginal$pred_mean
    pred_sd_marginal <- marginal$pred_sd
    n_post <- marginal$n_posterior_draws_used
    n_new <- marginal$n_new_firm_draws
  } else {
    lpd_obs <- as.numeric(existing$lpd_obs)
    pred_mean_marginal <- rep(NA_real_, nrow(test_df))
    pred_sd_marginal <- rep(NA_real_, nrow(test_df))
    n_post <- NA_integer_
    n_new <- 0L
  }

  population_pred_mean <- if ("pred_mean" %in% names(existing)) as.numeric(existing$pred_mean) else
    if ("pred_mean_population_level_existing" %in% names(existing)) as.numeric(existing$pred_mean_population_level_existing) else NA_real_
  population_pred_sd <- if ("pred_sd" %in% names(existing)) as.numeric(existing$pred_sd) else NA_real_
  lpd_existing <- as.numeric(existing$lpd_obs)
  if (!is_firm_re && !isTRUE(all.equal(lpd_obs, lpd_existing, tolerance = 0, check.attributes = FALSE))) {
    stop("[BLOCKER] Pooled copied MA12D scores do not exactly match existing MA12 population-level scores for ", task$Task_Key)
  }

  obs <- data.frame(
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Fold_ID = as.integer(task$Fold_ID),
    Obs_ID = paste(task$Target_Space, test_df$company, test_df$year, sep = "::"),
    company = test_df$company,
    year = test_df$year,
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    lpd_obs = lpd_obs,
    lpd_obs_population_level_existing = lpd_existing,
    lpd_obs_marginal_new_firm = if (is_firm_re) lpd_obs else NA_real_,
    marginal_score_used = is_firm_re,
    y_actual = as.numeric(test_df$TA_scaled),
    pred_mean_population_level_existing = population_pred_mean,
    pred_mean_marginal_new_firm = pred_mean_marginal,
    pred_sd_marginal_new_firm = pred_sd_marginal,
    pred_sd_population_level_existing = population_pred_sd,
    N_New_Firm_Draws = n_new,
    N_Posterior_Draws_Used = n_post,
    Prediction_Rule = ifelse(is_firm_re,
                             "grouped_firm_marginal_new_firm_integrated",
                             "grouped_firm_log_lik_re_formula_NA_population_level_copied"),
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    Source_KFold_Run_Root = task$Source_KFold_Run_Root,
    Source_KFold_Run_Root_Resolution = task$Source_KFold_Run_Root_Resolution,
    Source_KFold_Manifest_Path = task$Source_KFold_Manifest_Path,
    Source_KFold_Status_Path = task$Source_KFold_Status_Path,
    Source_KFold_Fold_Assignment_Path = task$Fold_Assignment_Path,
    Task_Key = task$Task_Key,
    stringsAsFactors = FALSE
  )
  if (!all(is.finite(obs$lpd_obs))) {
    stop("[BLOCKER] MA12D observation scores contain non-finite lpd_obs values for ", task$Task_Key)
  }
  fold_diag <- data.frame(
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Fold_ID = as.integer(task$Fold_ID),
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    N_Train_Obs = nrow(split$train_df),
    N_Test_Obs = nrow(split$test_df),
    N_Test_Firms = length(unique(split$test_df$company)),
    Requires_Marginal_New_Firm = is_firm_re,
    N_New_Firm_Draws = n_new,
    N_Posterior_Draws_Used = n_post,
    Completed = TRUE,
    Failure_Reason = NA_character_,
    Runtime_Seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    stringsAsFactors = FALSE
  )
  out <- list(
    obs_scores = obs,
    fold_diag = fold_diag,
    metadata = as.data.frame(expected_meta, stringsAsFactors = FALSE)
  )
  saveRDS(out, result_path)
  out
}

source_contract <- require_source_run_contract(resolve_source_kfold_run_root())
output_run_root <- make_output_run_root()
tables_dir <- file.path(output_run_root, "tables")
logs_dir <- file.path(output_run_root, "logs")
task_output_dir <- file.path(output_run_root, "task_results")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(task_output_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(output_run_root, file.path(output_root, "grouped_new_firm_marginal", "LATEST_RUN.txt"))

manifest <- read.csv(source_contract$manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
status <- read.csv(source_contract$status_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns(manifest, c("Task_Key", "Target_Sample", "Fold_Assignment_Path", "Fold_ID", "fit_path",
                             "Model_ID", "Model_Name", "Heterogeneity_Variant", "Target_Space",
                             "Sample_Group"), "MA12D source task manifest")
required_columns(status, c("Task_Key", "status"), "MA12D source task status")
if (!"Required" %in% names(status) && "Required" %in% names(manifest)) {
  status <- merge(status, manifest[, c("Task_Key", "Required"), drop = FALSE], by = "Task_Key", all.x = TRUE, sort = FALSE)
}
if (!"Required" %in% names(status)) status$Required <- TRUE
accrual_task_status_blocker(status, required_col = "Required", context = "ma12d source grouped K-fold tasks")
required_source <- status$Required %in% c(TRUE, "TRUE", "true", "1", 1L)
not_success <- required_source & !status$status %in% "SUCCESS"
if (any(not_success, na.rm = TRUE)) {
  stop("[BLOCKER] MA12D requires all required MA12 grouped K-fold tasks to be completed successfully. Non-success tasks: ",
       paste(status$Task_Key[not_success], collapse = "; "))
}

manifest <- normalize_task_paths(manifest)
manifest$Requires_Marginal_New_Firm <- firm_re_indicator(manifest$Heterogeneity_Variant)
manifest$Source_KFold_Run_Root <- source_contract$root
manifest$Source_KFold_Run_Root_Resolution <- source_contract$resolution
manifest$Source_KFold_Manifest_Path <- source_contract$manifest_path
manifest$Source_KFold_Status_Path <- source_contract$status_path
manifest$Source_KFold_Fold_Assignment_Path <- manifest$Fold_Assignment_Path
manifest$MA12D_Result_Path <- file.path(task_output_dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", manifest$Task_Key), "_ma12d_result.rds"))
manifest$N_New_Firm_Draws <- ma12d_env$new_firm_draws
manifest$Max_Posterior_Draws <- ma12d_env$max_posterior_draws
manifest$MA12D_Seed <- ma12d_env$seed

write_dual_csv(manifest, "table_ma12d_grouped_new_firm_marginal_task_manifest.csv", tables_dir)
write_dual_csv(data.frame(), "table_ma12d_grouped_new_firm_marginal_task_status.csv", tables_dir)

missing_fits <- manifest$fit_path[!file.exists(manifest$fit_path)]
if (length(missing_fits)) {
  write_blocker_decision(
    "BLOCKED_MISSING_FITS",
    paste("Required MA12 fitted brms object(s) were unavailable; MA12D did not refit. Missing examples:",
          paste(utils::head(unique(missing_fits), 10), collapse = "; ")),
    source_contract,
    manifest,
    tables_dir
  )
  stop("[BLOCKER] MA12D found missing required fitted brms object(s): ",
       paste(utils::head(unique(missing_fits), 10), collapse = "; "))
}
missing_folds <- manifest$Fold_Assignment_Path[!file.exists(manifest$Fold_Assignment_Path)]
if (length(missing_folds)) {
  stop("[BLOCKER] MA12D found missing grouped fold assignment(s): ",
       paste(utils::head(unique(missing_folds), 10), collapse = "; "))
}

existing_obs <- read.csv(source_contract$observation_scores_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns(existing_obs, c("Obs_ID", "Model_ID", "Heterogeneity_Variant", "Target_Space", "Fold_ID", "lpd_obs"),
                 "MA12D source observation scores")
if (anyDuplicated(existing_score_key(existing_obs))) {
  stop("[BLOCKER] Existing MA12 grouped observation scores have duplicate matching keys.")
}

task_results <- vector("list", nrow(manifest))
status_rows <- vector("list", nrow(manifest))
for (i in seq_len(nrow(manifest))) {
  task_key <- manifest$Task_Key[[i]]
  message("ma12d rescoring task ", i, "/", nrow(manifest), ": ", task_key)
  started <- Sys.time()
  res <- tryCatch(
    score_one_task(manifest[i, , drop = FALSE], existing_obs, task_output_dir, ma12d_env$force_recompute),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    status_rows[[i]] <- data.frame(
      Task_Key = task_key,
      status = "FAILED",
      reason = conditionMessage(res),
      Required = if ("Required" %in% names(manifest)) manifest$Required[[i]] else TRUE,
      result_path = manifest$MA12D_Result_Path[[i]],
      stringsAsFactors = FALSE
    )
  } else {
    task_results[[i]] <- res
    status_rows[[i]] <- data.frame(
      Task_Key = task_key,
      status = "SUCCESS",
      reason = NA_character_,
      Required = if ("Required" %in% names(manifest)) manifest$Required[[i]] else TRUE,
      result_path = manifest$MA12D_Result_Path[[i]],
      runtime_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      stringsAsFactors = FALSE
    )
  }
  task_status <- bind_rows(status_rows)
  write_dual_csv(task_status, "table_ma12d_grouped_new_firm_marginal_task_status.csv", tables_dir)
}

task_status <- bind_rows(status_rows)
failed_required <- task_status$Required %in% c(TRUE, "TRUE", "true", "1", 1L) &
  task_status$status %in% "FAILED"
if (any(failed_required, na.rm = TRUE) &&
    any(grepl("random-intercept SD|sd_\\.\\*__Intercept|sigma_u|Firm-RE task has no group-level intercept",
              task_status$reason[failed_required], ignore.case = TRUE))) {
  write_blocker_decision(
    "BLOCKED_UNVERIFIED_FIRMRE_SD",
    paste("A Firm-RE model could not identify the firm random-intercept standard deviation:",
          paste(task_status$reason[failed_required], collapse = " | ")),
    source_contract,
    manifest,
    tables_dir
  )
}
accrual_task_status_blocker(task_status, required_col = "Required", context = "ma12d grouped new-firm rescoring")

successful_results <- Filter(Negate(is.null), task_results)
obs_scores <- bind_rows(lapply(successful_results, `[[`, "obs_scores"))
fold_diagnostics <- bind_rows(lapply(successful_results, `[[`, "fold_diag"))
if (!nrow(obs_scores)) stop("[BLOCKER] MA12D produced no observation scores.")
if (!all(is.finite(obs_scores$lpd_obs))) stop("[BLOCKER] MA12D exported non-finite lpd_obs values.")

write_dual_csv(obs_scores, "table_winsor_kfold_observation_scores_marginal_new_firm.csv", tables_dir)

fold_scores <- obs_scores %>%
  group_by(.data$Target_Space, .data$Sample_Group, .data$Fold_ID, .data$Model_ID,
           .data$Model_Name, .data$Heterogeneity_Variant) %>%
  summarise(
    N_Test_Obs = n(),
    N_Test_Firms = n_distinct(.data$company),
    elpd_fold = sum(.data$lpd_obs),
    mean_lpd_obs = mean(.data$lpd_obs),
    RMSE = sqrt(mean((.data$y_actual - dplyr::coalesce(.data$pred_mean_marginal_new_firm,
                                                       .data$pred_mean_population_level_existing))^2, na.rm = TRUE)),
    MAE = mean(abs(.data$y_actual - dplyr::coalesce(.data$pred_mean_marginal_new_firm,
                                                    .data$pred_mean_population_level_existing)), na.rm = TRUE),
    marginal_score_used_any = any(.data$marginal_score_used),
    N_New_Firm_Draws = max(.data$N_New_Firm_Draws, na.rm = TRUE),
    N_Posterior_Draws_Used = suppressWarnings(max(.data$N_Posterior_Draws_Used, na.rm = TRUE)),
    Scoring_Rule = "marginal_new_firm_integrated",
    .groups = "drop"
  ) %>%
  mutate(N_Posterior_Draws_Used = ifelse(is.infinite(.data$N_Posterior_Draws_Used), NA_real_, .data$N_Posterior_Draws_Used))
write_dual_csv(fold_scores, "table_winsor_kfold_fold_scores_marginal_new_firm.csv", tables_dir)

K <- suppressWarnings(as.integer(manifest$K[1]))
partial_run <- if ("Partial_Run" %in% names(manifest)) isTRUE(as.logical(manifest$Partial_Run[1])) else FALSE
model_scores <- fold_diagnostics %>%
  group_by(.data$Target_Space, .data$Sample_Group, .data$Model_ID,
           .data$Model_Name, .data$Heterogeneity_Variant) %>%
  summarise(
    N_Folds_Attempted = n(),
    N_Folds_Completed = sum(.data$Completed, na.rm = TRUE),
    N_New_Firm_Draws = max(.data$N_New_Firm_Draws, na.rm = TRUE),
    N_Posterior_Draws_Used = suppressWarnings(max(.data$N_Posterior_Draws_Used, na.rm = TRUE)),
    Runtime_Seconds = sum(.data$Runtime_Seconds, na.rm = TRUE),
    exclusion_reason = paste(na.omit(unique(.data$Failure_Reason)), collapse = " | "),
    .groups = "drop"
  ) %>%
  left_join(
    fold_scores %>%
      group_by(.data$Target_Space, .data$Sample_Group, .data$Model_ID,
               .data$Model_Name, .data$Heterogeneity_Variant) %>%
      summarise(
        N_Test_Obs_Total = sum(.data$N_Test_Obs),
        N_Test_Firms_Total = sum(.data$N_Test_Firms),
        elpd_kfold = sum(.data$elpd_fold),
        mean_lpd_obs = weighted.mean(.data$mean_lpd_obs, .data$N_Test_Obs),
        se_elpd_fold = stats::sd(.data$elpd_fold),
        RMSE = sqrt(weighted.mean(.data$RMSE^2, .data$N_Test_Obs)),
        MAE = weighted.mean(.data$MAE, .data$N_Test_Obs),
        .groups = "drop"
      ),
    by = c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant")
  ) %>%
  mutate(
    N_Posterior_Draws_Used = ifelse(is.infinite(.data$N_Posterior_Draws_Used), NA_real_, .data$N_Posterior_Draws_Used),
    reliability_flag = ifelse(N_Folds_Completed > 0 & (partial_run | is.na(K) | N_Folds_Completed == K), "OK", "LOW_RELIABILITY"),
    included_in_stack = reliability_flag == "OK",
    Scoring_Rule = "marginal_new_firm_integrated",
    exclusion_reason = ifelse(included_in_stack, NA_character_, exclusion_reason)
  )
write_dual_csv(model_scores, "table_winsor_kfold_model_scores_marginal_new_firm.csv", tables_dir)

build_kfold_weights <- function(target_space) {
  included <- model_scores %>%
    filter(.data$Target_Space == target_space, .data$Sample_Group == "main_common", .data$included_in_stack == TRUE) %>%
    arrange(.data$Model_ID, .data$Heterogeneity_Variant)
  if (!nrow(included)) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, ]
    key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_kfold_marginal_new_firm")
    one <- obs_scores %>%
      filter(.data$Target_Space == target_space, .data$Sample_Group == row$Sample_Group,
             .data$Model_ID == row$Model_ID, .data$Heterogeneity_Variant == row$Heterogeneity_Variant) %>%
      arrange(.data$company, .data$year)
    if (nrow(one) != row$N_Test_Obs_Total) next
    score_list[[key]] <- one$lpd_obs
    meta_keys <- c(meta_keys, key)
  }
  if (!length(score_list)) return(data.frame())
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) {
    stop("[BLOCKER] MA12D exact grouped K-fold score vectors have unequal lengths for ", target_space)
  }
  lpd_matrix <- do.call(cbind, score_list)
  colnames(lpd_matrix) <- names(score_list)
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  singleton_elpd <- colSums(lpd_matrix)
  meta_idx <- match(names(weights), meta_keys)
  best_elpd_key <- names(singleton_elpd)[which.max(singleton_elpd)]
  top_weight_key <- names(weights)[which.max(weights)]
  included[meta_idx, ] %>%
    mutate(
      Model_Key_KFold = names(weights),
      Weight_KFold = as.numeric(weights),
      Rank_KFold = rank(-as.numeric(weights), ties.method = "first"),
      Singleton_ELPD = as.numeric(singleton_elpd[names(weights)]),
      Best_Singleton_ELPD_Key = best_elpd_key,
      Top_Weight_Key = top_weight_key,
      Top_Weight_Not_Best_Singleton = max(weights) > 0.999 && !identical(top_weight_key, best_elpd_key),
      M10_Included = FALSE,
      Scoring_Rule = "marginal_new_firm_integrated"
    ) %>%
    arrange(desc(.data$Weight_KFold)) %>%
    select(Target_Space, Sample_Group, M10_Included, Model_ID,
           Model_Name, Heterogeneity_Variant, Model_Key_KFold,
           Weight_KFold, Rank_KFold, elpd_kfold, Singleton_ELPD,
           mean_lpd_obs, RMSE, MAE, reliability_flag,
           Best_Singleton_ELPD_Key, Top_Weight_Key, Top_Weight_Not_Best_Singleton,
           Scoring_Rule, N_New_Firm_Draws, N_Posterior_Draws_Used)
}

weights_ep <- build_kfold_weights("ex_post")
weights_rt <- build_kfold_weights("real_time")
write_dual_csv(weights_ep, "table_winsor_kfold_weights_ex_post_marginal_new_firm.csv", tables_dir)
write_dual_csv(weights_rt, "table_winsor_kfold_weights_no_lookahead_marginal_new_firm.csv", tables_dir)

read_weight_file <- function(path, target_space) {
  if (!file.exists(path)) return(data.frame())
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(x)) return(data.frame())
  x$Target_Space <- target_space
  x
}

population_weights <- bind_rows(
  read_weight_file(source_contract$weights_ex_post_path, "ex_post"),
  read_weight_file(source_contract$weights_no_lookahead_path, "real_time")
)
marginal_weights <- bind_rows(weights_ep, weights_rt)

standard_weights <- function(x, weight_col) {
  if (!nrow(x)) return(data.frame())
  data.frame(
    Target_Space = as.character(x$Target_Space),
    Model_ID = as.character(x$Model_ID),
    Model_Name = as.character(x$Model_Name),
    Heterogeneity_Variant = as.character(x$Heterogeneity_Variant),
    Weight = suppressWarnings(as.numeric(x[[weight_col]])),
    Rank = if ("Rank_KFold" %in% names(x)) suppressWarnings(as.integer(x$Rank_KFold)) else NA_integer_,
    stringsAsFactors = FALSE
  )
}

pop_std <- standard_weights(population_weights, "Weight_KFold")
mar_std <- standard_weights(marginal_weights, "Weight_KFold")
if (!nrow(pop_std) && !nrow(mar_std)) {
  stop("[BLOCKER] MA12D could not build population-vs-marginal comparison because both weight tables are empty.")
}
comparison <- full_join(
  pop_std %>% rename(Weight_Population_Level = Weight, Rank_Population_Level = Rank),
  mar_std %>% rename(Weight_Marginal_New_Firm = Weight, Rank_Marginal_New_Firm = Rank),
  by = c("Target_Space", "Model_ID", "Model_Name", "Heterogeneity_Variant")
) %>%
  mutate(
    Weight_Population_Level = ifelse(is.na(.data$Weight_Population_Level), 0, .data$Weight_Population_Level),
    Weight_Marginal_New_Firm = ifelse(is.na(.data$Weight_Marginal_New_Firm), 0, .data$Weight_Marginal_New_Firm),
    Weight_Difference = .data$Weight_Marginal_New_Firm - .data$Weight_Population_Level,
    is_firm_re = firm_re_indicator(.data$Heterogeneity_Variant)
  )

top_pop <- comparison %>%
  group_by(.data$Target_Space) %>%
  slice_max(.data$Weight_Population_Level, n = 1, with_ties = FALSE) %>%
  transmute(Target_Space,
            Top_Cell_Population_Level = paste(.data$Model_ID, .data$Heterogeneity_Variant, sep = "::"))
top_mar <- comparison %>%
  group_by(.data$Target_Space) %>%
  slice_max(.data$Weight_Marginal_New_Firm, n = 1, with_ties = FALSE) %>%
  transmute(Target_Space,
            Top_Cell_Marginal_New_Firm = paste(.data$Model_ID, .data$Heterogeneity_Variant, sep = "::"))
firmre_summary <- comparison %>%
  group_by(.data$Target_Space) %>%
  summarise(
    FirmRE_Weight_Population_Level = sum(.data$Weight_Population_Level[.data$is_firm_re], na.rm = TRUE),
    FirmRE_Weight_Marginal_New_Firm = sum(.data$Weight_Marginal_New_Firm[.data$is_firm_re], na.rm = TRUE),
    FirmRE_Weight_Difference = FirmRE_Weight_Marginal_New_Firm - FirmRE_Weight_Population_Level,
    .groups = "drop"
  )
comparison <- comparison %>%
  left_join(top_pop, by = "Target_Space") %>%
  left_join(top_mar, by = "Target_Space") %>%
  left_join(firmre_summary, by = "Target_Space") %>%
  select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant,
         Weight_Population_Level, Weight_Marginal_New_Firm, Weight_Difference,
         Rank_Population_Level, Rank_Marginal_New_Firm,
         Top_Cell_Population_Level, Top_Cell_Marginal_New_Firm,
         FirmRE_Weight_Population_Level, FirmRE_Weight_Marginal_New_Firm,
         FirmRE_Weight_Difference)
write_dual_csv(comparison, "table_grouped_population_vs_marginal_new_firm_weight_comparison.csv", tables_dir)

decision <- firmre_summary %>%
  left_join(top_pop, by = "Target_Space") %>%
  left_join(top_mar, by = "Target_Space") %>%
  mutate(
    Aggregate_FirmRE_Weight_Population_Level = .data$FirmRE_Weight_Population_Level,
    Aggregate_FirmRE_Weight_Marginal_New_Firm = .data$FirmRE_Weight_Marginal_New_Firm,
    Absolute_FirmRE_Weight_Change = abs(.data$FirmRE_Weight_Difference),
    Top_Cell_Changed = .data$Top_Cell_Population_Level != .data$Top_Cell_Marginal_New_Firm,
    Conclusion_Changed = (.data$Aggregate_FirmRE_Weight_Population_Level >= 0.5) !=
      (.data$Aggregate_FirmRE_Weight_Marginal_New_Firm >= 0.5),
    Decision = case_when(
      .data$Top_Cell_Changed | .data$Conclusion_Changed ~ "REVISE_PRIMARY_RESULT",
      .data$Absolute_FirmRE_Weight_Change >= ma12d_env$material_weight_change ~ "QUALIFIES_PRIMARY_RESULT",
      TRUE ~ "PASS_PRIMARY_ALIGNMENT"
    ),
    Interpretation = case_when(
      .data$Decision == "PASS_PRIMARY_ALIGNMENT" ~ "Marginal new-firm Firm-RE scoring preserves the original grouped population-level conclusion.",
      .data$Decision == "QUALIFIES_PRIMARY_RESULT" ~ "The qualitative conclusion remains aligned, but the aggregate Firm-RE weight changes materially.",
      TRUE ~ "The top grouped benchmark cell or aggregate Firm-RE conclusion changes under marginal new-firm scoring."
    ),
    Source_KFold_Run_Root = source_contract$root,
    Source_KFold_Run_Root_Resolution = source_contract$resolution,
    Source_KFold_Manifest_Path = source_contract$manifest_path,
    Source_KFold_Status_Path = source_contract$status_path,
    Source_KFold_Fold_Assignment_Path = paste(unique(manifest$Fold_Assignment_Path), collapse = ";")
  ) %>%
  select(Target_Space, Top_Cell_Population_Level, Top_Cell_Marginal_New_Firm,
         Aggregate_FirmRE_Weight_Population_Level, Aggregate_FirmRE_Weight_Marginal_New_Firm,
         Absolute_FirmRE_Weight_Change, Top_Cell_Changed, Conclusion_Changed,
         Decision, Interpretation, Source_KFold_Run_Root,
         Source_KFold_Run_Root_Resolution, Source_KFold_Manifest_Path,
         Source_KFold_Status_Path, Source_KFold_Fold_Assignment_Path)
write_dual_csv(decision, "table_grouped_marginal_new_firm_decision.csv", tables_dir)

run_manifest <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = as.character(script_start_time),
  End_Time = as.character(Sys.time()),
  Runtime_Seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  Output_Run_Root = output_run_root,
  Source_KFold_Run_Root = source_contract$root,
  Source_KFold_Run_Root_Resolution = source_contract$resolution,
  Source_KFold_Manifest_Path = source_contract$manifest_path,
  Source_KFold_Status_Path = source_contract$status_path,
  Source_KFold_Observation_Scores_Path = source_contract$observation_scores_path,
  Source_Row_KFold_Run_Root_Reserved = ma12d_env$source_row_kfold_run_root,
  N_New_Firm_Draws = ma12d_env$new_firm_draws,
  Max_Posterior_Draws = ma12d_env$max_posterior_draws,
  MA12D_Seed = ma12d_env$seed,
  Force_Recompute = ma12d_env$force_recompute,
  Weight_Change_Material_Threshold = ma12d_env$material_weight_change,
  Prior_Set_ID = prior_set_id,
  Likelihood_Family = likelihood_family,
  Model_Structure = model_structure,
  Prediction_Rule = "grouped_firm_marginal_new_firm_integrated",
  Refits_Performed = FALSE,
  Source_Manifest_MD5 = file_md5(source_contract$manifest_path),
  Source_Status_MD5 = file_md5(source_contract$status_path),
  stringsAsFactors = FALSE
)
write_csv_safely(run_manifest, file.path(logs_dir, "run_config_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))
writeLines(output_run_root, file.path(output_root, "grouped_new_firm_marginal", "LATEST_COMPLETED_RUN.txt"))

cat("\n[SUCCESS] MA12D grouped-firm marginal new-firm rescoring completed.\n")
cat("Output run root:", output_run_root, "\n")
cat("Decision table:", file.path(tables_dir, "table_grouped_marginal_new_firm_decision.csv"), "\n")
cat("Refits performed: FALSE\n")
phase_end("ma12d", "Compute grouped-firm marginal new-firm exact K-fold scores")
