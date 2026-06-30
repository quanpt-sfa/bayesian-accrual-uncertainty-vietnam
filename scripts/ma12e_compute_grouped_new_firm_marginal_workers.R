# Script: ma12e_compute_grouped_new_firm_marginal_workers.R
# Purpose: Workerized MA12D v1.1 marginal-new-firm rescoring without refits.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma12e", "Compute grouped-firm marginal new-firm rescoring workers")

script_name <- "scripts/ma12e_compute_grouped_new_firm_marginal_workers.R"
script_version <- "marginal-new-firm-rescore-v1.1-worker"

ma12d_env <- list(
  new_firm_draws = env_int("ACCRUAL_MA12D_NEW_FIRM_DRAWS", 20L, min = 1L),
  max_posterior_draws = env_int("ACCRUAL_MA12D_MAX_POSTERIOR_DRAWS", 2000L, min = 1L),
  seed = env_int("ACCRUAL_MA12D_SEED", accrual_seed_for("ma12d_grouped_new_firm_marginal"), min = 0L),
  force_recompute = env_flag("ACCRUAL_MA12D_FORCE_RECOMPUTE", "FALSE"),
  allow_restack_excluded = env_flag("ACCRUAL_MA12D_ALLOW_RESTACK_EXCLUDED", "FALSE")
)

nonempty <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
}

single_line <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  x <- trimws(readLines(path, warn = FALSE))
  x <- x[nzchar(x)]
  if (!length(x)) NA_character_ else x[[1]]
}

resolve_output_run_root <- function() {
  explicit <- trimws(env_value("ACCRUAL_MA12D_OUTPUT_RUN_ROOT", ""))
  if (nonempty(explicit)) return(normalizePath(explicit, winslash = "/", mustWork = FALSE))
  pinned <- single_line(file.path(output_root, "grouped_new_firm_marginal", "LATEST_RUN.txt"))
  if (nonempty(pinned)) return(normalizePath(pinned, winslash = "/", mustWork = FALSE))
  stop("[BLOCKER] MA12E cannot resolve MA12D output run root. Run ma12d_prepare first or set ACCRUAL_MA12D_OUTPUT_RUN_ROOT.")
}

required_columns <- function(df, cols, context) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop("[BLOCKER] ", context, " missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

as_bool <- function(x) {
  if (is.logical(x)) return(x)
  as.character(x) %in% c("TRUE", "true", "True", "1", "yes", "YES")
}

file_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
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

metadata_matches <- function(cached, expected_meta) {
  if (is.null(cached) || !is.list(cached) || is.null(cached$metadata)) return(FALSE)
  all(vapply(names(expected_meta), function(nm) {
    nm %in% names(cached$metadata) &&
      identical(as.character(cached$metadata[[nm]][1]), as.character(expected_meta[[nm]]))
  }, logical(1)))
}

compute_ma12e_task_worker <- function(task) {
  task <- as.list(task)
  started <- Sys.time()
  task_output_dir <- dirname(task$MA12D_Result_Path)
  task_log_dir <- file.path(task$Output_Run_Root, "logs", "tasks")
  dir.create(task_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(task_log_dir, recursive = TRUE, showWarnings = FALSE)
  result_path <- task$MA12D_Result_Path
  task_log_path <- file.path(task_log_dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", task$Task_Key), ".log"))
  writeLines(c("ma12e task log", paste("Task_Key:", task$Task_Key)), task_log_path)

  expected_meta <- list(
    Task_Key = task$Task_Key,
    Source_KFold_Run_Root = task$Source_KFold_Run_Root,
    Source_Model_Scores_MD5 = task$Source_Model_Scores_MD5,
    Source_Observation_Scores_MD5 = task$Source_Observation_Scores_MD5,
    N_New_Firm_Draws = ma12d_env$new_firm_draws,
    Max_Posterior_Draws = ma12d_env$max_posterior_draws,
    Prediction_Rule = task$Prediction_Rule,
    script_version = script_version
  )

  status <- "FAILED"
  reason <- NA_character_
  out <- NULL
  result <- tryCatch({
    cached_ok <- FALSE
    cached <- NULL
    if (file.exists(result_path) && !isTRUE(ma12d_env$force_recompute)) {
      cached <- tryCatch(readRDS(result_path), error = function(e) NULL)
      if (metadata_matches(cached, expected_meta)) {
        cached_ok <- TRUE
      }
    }
    if (cached_ok) {
      cached
    } else {

    if (!file.exists(task$fit_path)) stop("[BLOCKER] Missing required MA12 fit object: ", task$fit_path)
    if (!file.exists(task$Fold_Assignment_Path)) stop("[BLOCKER] Missing grouped fold assignment: ", task$Fold_Assignment_Path)
    if (!file.exists(task$Source_KFold_Observation_Scores_Path)) {
      stop("[BLOCKER] Missing source MA12C observation scores: ", task$Source_KFold_Observation_Scores_Path)
    }
    existing_obs <- read.csv(task$Source_KFold_Observation_Scores_Path, stringsAsFactors = FALSE, check.names = FALSE)
    required_columns(existing_obs, c("Obs_ID", "Model_ID", "Heterogeneity_Variant", "Target_Space", "Fold_ID", "lpd_obs"),
                     "MA12D source observation scores")
    if (anyDuplicated(existing_score_key(existing_obs))) {
      stop("[BLOCKER] Existing MA12 grouped observation scores have duplicate matching keys.")
    }

    df <- read_winsor_sample(task$Target_Sample, prefactor = TRUE)
    fold_map <- read.csv(task$Fold_Assignment_Path, stringsAsFactors = FALSE, check.names = FALSE)
    split <- validate_grouped_split(df, fold_map, task)
    test_df <- split$test_df
    existing <- lookup_existing_scores(existing_obs, task, test_df)
    is_firm_re <- isTRUE(as_bool(task$Requires_Marginal_New_Firm))

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
      Scoring_Rule = "marginal_new_firm_integrated",
      Source_Reliability_Flag = task$Source_Reliability_Flag,
      Source_Included_In_Stack = as_bool(task$Source_Included_In_Stack),
      MA12D_Primary_Stack_Eligible = as_bool(task$MA12D_Primary_Stack_Eligible),
      Restack_Excluded_Allowed = as_bool(task$Restack_Excluded_Allowed),
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      Output_Root = output_root,
      Source_KFold_Run_Root = task$Source_KFold_Run_Root,
      Source_KFold_Run_Root_Resolution = task$Source_KFold_Run_Root_Resolution,
      Source_KFold_Manifest_Path = task$Source_KFold_Manifest_Path,
      Source_KFold_Status_Path = task$Source_KFold_Status_Path,
      Source_KFold_Model_Scores_Path = task$Source_KFold_Model_Scores_Path,
      Source_KFold_Observation_Scores_Path = task$Source_KFold_Observation_Scores_Path,
      Source_KFold_Fold_Assignment_Path = task$Fold_Assignment_Path,
      Output_Run_Root = task$Output_Run_Root,
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
      Source_Reliability_Flag = task$Source_Reliability_Flag,
      Source_Included_In_Stack = as_bool(task$Source_Included_In_Stack),
      MA12D_Primary_Stack_Eligible = as_bool(task$MA12D_Primary_Stack_Eligible),
      Restack_Excluded_Allowed = as_bool(task$Restack_Excluded_Allowed),
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
  }, error = function(e) {
    reason <<- conditionMessage(e)
    NULL
  })

  if (!is.null(result)) {
    status <- "SUCCESS"
    reason <- NA_character_
    out <- result
  }
  ended <- Sys.time()
  data.frame(
    Task_Key = task$Task_Key,
    status = status,
    reason = reason,
    Required = if ("Required" %in% names(task)) task$Required else TRUE,
    MA12D_Primary_Stack_Eligible = as_bool(task$MA12D_Primary_Stack_Eligible),
    Source_Reliability_Flag = task$Source_Reliability_Flag,
    Source_Included_In_Stack = as_bool(task$Source_Included_In_Stack),
    fit_path = task$fit_path,
    result_path = result_path,
    runtime_seconds = as.numeric(difftime(ended, started, units = "secs")),
    stringsAsFactors = FALSE
  )
}

output_run_root <- resolve_output_run_root()
tables_dir <- file.path(output_run_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma12d_grouped_new_firm_marginal_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma12d_grouped_new_firm_marginal_task_status.csv")
compat_status_path <- file.path(output_root, "tables", "table_ma12d_grouped_new_firm_marginal_task_status.csv")
if (!file.exists(manifest_path)) stop("[BLOCKER] Missing MA12D task manifest: ", manifest_path)

tasks <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns(tasks, c("Task_Key", "fit_path", "Fold_Assignment_Path", "Source_KFold_Run_Root",
                          "Source_Model_Scores_MD5", "Source_Observation_Scores_MD5",
                          "N_New_Firm_Draws", "Max_Posterior_Draws", "Prediction_Rule",
                          "MA12D_Result_Path", "Output_Run_Root",
                          "Source_Reliability_Flag", "Source_Included_In_Stack",
                          "MA12D_Primary_Stack_Eligible"), "MA12E task manifest")

cores_per_task <- if ("cores" %in% names(tasks)) suppressWarnings(max(as.integer(tasks$cores), na.rm = TRUE)) else 1L
if (is.na(cores_per_task) || cores_per_task < 1L) cores_per_task <- 1L
parallel_cfg <- accrual_fit_worker_config("grouped_new_firm_marginal", cores_per_task, "ma12e grouped new-firm marginal workers")
results <- accrual_run_task_pool(
  split(tasks, seq_len(nrow(tasks))),
  compute_ma12e_task_worker,
  parallel_cfg,
  export_names = c(
    "compute_ma12e_task_worker", "ma12d_env", "script_version",
    "required_columns", "as_bool", "find_draw_column", "extract_group_terms",
    "extract_firm_intercept_sd_draws", "existing_score_key", "lookup_existing_scores",
    "validate_grouped_split", "subsample_draws", "student_log_density_matrix",
    "compute_marginal_new_firm_scores", "metadata_matches"
  ),
  packages = c("brms", "posterior"),
  context = "ma12e grouped new-firm marginal workers"
)
status <- do.call(rbind, results)
write_task_status(status_path, status)
write_task_status(compat_status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "ma12e grouped new-firm marginal workers")

cat("\n[SUCCESS] MA12E worker stage completed.\n")
cat("Task status:", status_path, "\n")
phase_end("ma12e", "Compute grouped-firm marginal new-firm rescoring workers")
