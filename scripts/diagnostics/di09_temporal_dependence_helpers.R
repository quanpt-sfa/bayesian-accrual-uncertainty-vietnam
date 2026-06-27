# -----------------------------------------------------------------------------
# Script: scripts/diagnostics/di09_temporal_dependence_helpers.R
# Purpose: Shared helpers for split-worker DI09 temporal-dependence robustness.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

di09_script_version <- function() "2026-06-27-v1-split-worker-di09"

di09_temporal_dirs <- function(root = output_root) {
  temporal_root <- file.path(root, "simulation", "temporal_dependence")
  dirs <- list(
    root = temporal_root,
    tables = file.path(temporal_root, "tables"),
    logs = file.path(temporal_root, "logs"),
    task_results = file.path(temporal_root, "task_artifacts", "results"),
    task_status = file.path(temporal_root, "task_artifacts", "status"),
    task_logs = file.path(temporal_root, "task_artifacts", "logs")
  )
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d)) stop("[BLOCKER] Could not create DI09 directory: ", d)
  }
  dirs
}

di09_runtime_config <- function() {
  cfg <- accrual_simulation_runtime_config("temporal_robustness")
  list(
    t_grid = as.integer(cfg$t_grid),
    rho_grid = as.numeric(cfg$rho_grid),
    sigma_grid = as.numeric(cfg$sigma_grid),
    R = as.integer(cfg$R),
    K = as.integer(cfg$K),
    n_firms = as.integer(cfg$n_firms),
    n_industries = as.integer(cfg$n_industries),
    sigma_eps = as.numeric(cfg$sigma_eps),
    seed = as.integer(cfg$seed)
  )
}

di09_task_key <- function(T, rho, sigma_firm) {
  key <- paste0(
    "T", as.integer(T),
    "__rho", formatC(as.numeric(rho), format = "f", digits = 4),
    "__sigma", formatC(as.numeric(sigma_firm), format = "f", digits = 4)
  )
  gsub("[^A-Za-z0-9_.=-]+", "_", key)
}

di09_git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}

di09_file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
di09_mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
di09_file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

di09_safe_seed <- function(base_seed, offset) {
  seed <- (as.integer(base_seed) + as.integer(offset)) %% .Machine$integer.max
  set_accrual_effective_seed(seed, context = "di09_temporal_dependence")
  seed
}

di09_simulate_panel_ar1 <- function(T, rho, sigma_firm, replication,
                                    n_firms, n_industries, sigma_eps, base_seed) {
  offset <- 700000L + as.integer(replication) +
    as.integer(T) * 1000L +
    as.integer(round(rho * 1000)) * 10L +
    as.integer(round(sigma_firm * 10000))
  seed <- di09_safe_seed(base_seed, offset)
  firms <- sprintf("F%04d", seq_len(n_firms))
  years <- seq_len(T)
  industries <- sprintf("IND%02d", seq_len(n_industries))
  industry_map <- sample(rep(industries, length.out = n_firms), n_firms)
  names(industry_map) <- firms
  df <- expand.grid(company = firms, year_num = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$industry <- industry_map[df$company]
  firm_effect <- stats::rnorm(n_firms, 0, sigma_firm)
  names(firm_effect) <- firms
  industry_year <- stats::rnorm(n_industries * T, 0, 0.020)
  dim(industry_year) <- c(n_industries, T)
  rownames(industry_year) <- industries
  colnames(industry_year) <- as.character(years)
  df$inv_A_lag <- stats::rnorm(nrow(df), 0.020, 0.010)
  df$dREV_dREC_scaled <- stats::rnorm(nrow(df), 0.050, 0.100)
  df$PPE_scaled <- stats::rnorm(nrow(df), 0.450, 0.200)
  df$ROA_lag <- stats::rnorm(nrow(df), 0.040, 0.080)
  epsilon <- numeric(nrow(df))
  for (firm in firms) {
    idx <- which(df$company == firm)
    idx <- idx[order(df$year_num[idx])]
    innovation <- stats::rnorm(length(idx), 0, sigma_eps)
    previous <- innovation[[1]] / sqrt(max(1 - rho^2, .Machine$double.eps))
    epsilon[idx[[1]]] <- previous
    if (length(idx) > 1) {
      for (pos in seq.int(2L, length(idx))) {
        previous <- rho * previous + innovation[[pos]]
        epsilon[idx[[pos]]] <- previous
      }
    }
  }
  mu <- 0.030 * df$inv_A_lag + 0.060 * df$dREV_dREC_scaled -
    0.040 * df$PPE_scaled + 0.080 * df$ROA_lag +
    firm_effect[df$company] +
    industry_year[cbind(df$industry, as.character(df$year_num))]
  df$epsilon <- epsilon
  df$TA_scaled <- mu + epsilon
  df$company <- factor(df$company, levels = firms)
  df$industry <- factor(df$industry, levels = industries)
  df$year <- factor(df$year_num, levels = as.character(years))
  attr(df, "seed") <- seed
  df
}

di09_make_row_folds <- function(df, K, offset, base_seed) {
  di09_safe_seed(base_seed, offset)
  sample(rep(seq_len(K), length.out = nrow(df)))
}

di09_make_grouped_folds <- function(df, K, offset, base_seed) {
  di09_safe_seed(base_seed, offset)
  firms <- sort(unique(as.character(df$company)))
  assignment <- data.frame(company = sample(firms), fold = rep(seq_len(K), length.out = length(firms)), stringsAsFactors = FALSE)
  fold_map <- assignment$fold
  names(fold_map) <- assignment$company
  unname(fold_map[as.character(df$company)])
}

di09_normal_lpd <- function(y, mu, sigma) stats::dnorm(y, mu, max(sigma, .Machine$double.eps), log = TRUE)

di09_score_fold <- function(train, test, validation_target, lme4_available) {
  pooled <- stats::lm(
    TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(industry) + factor(year),
    data = train
  )
  pred_pooled <- as.numeric(stats::predict(pooled, newdata = test))
  lpd_pooled <- di09_normal_lpd(test$TA_scaled, pred_pooled, stats::sigma(pooled))
  if (!lme4_available) {
    return(data.frame(
      lpd_pooled = lpd_pooled,
      lpd_firmre = NA_real_,
      warning = "INSUFFICIENT_DEPENDENCY_lme4",
      stringsAsFactors = FALSE
    ))
  }
  firmre <- lme4::lmer(
    TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(year) + (1 | company),
    data = train,
    REML = FALSE,
    control = lme4::lmerControl(check.conv.singular = "ignore", check.conv.grad = "ignore", check.conv.hess = "ignore")
  )
  pred_firmre <- if (identical(validation_target, "row_level_kfold")) {
    as.numeric(stats::predict(firmre, newdata = test, re.form = NULL, allow.new.levels = TRUE))
  } else {
    as.numeric(stats::predict(firmre, newdata = test, re.form = NA, allow.new.levels = TRUE))
  }
  data.frame(
    lpd_pooled = lpd_pooled,
    lpd_firmre = di09_normal_lpd(test$TA_scaled, pred_firmre, stats::sigma(firmre)),
    warning = ifelse(lme4::isSingular(firmre, tol = 1e-5), "singular_firmre_fit", NA_character_),
    stringsAsFactors = FALSE
  )
}

di09_score_cv <- function(df, validation_target, K, scenario_offset, lme4_available, base_seed) {
  folds <- if (identical(validation_target, "row_level_kfold")) {
    di09_make_row_folds(df, K, scenario_offset + 11L, base_seed)
  } else {
    di09_make_grouped_folds(df, K, scenario_offset + 29L, base_seed)
  }
  fold_scores <- lapply(seq_len(K), function(fold) {
    di09_score_fold(df[folds != fold, , drop = FALSE], df[folds == fold, , drop = FALSE], validation_target, lme4_available)
  })
  scored <- bind_rows(fold_scores)
  data.frame(
    validation_target = validation_target,
    score_pooled = sum(scored$lpd_pooled, na.rm = TRUE),
    score_firmre = if (all(is.na(scored$lpd_firmre))) NA_real_ else sum(scored$lpd_firmre, na.rm = TRUE),
    firmre_premium = if (all(is.na(scored$lpd_firmre))) NA_real_ else sum(scored$lpd_firmre, na.rm = TRUE) - sum(scored$lpd_pooled, na.rm = TRUE),
    firmre_weight = NA_real_,
    warning = paste(unique(na.omit(scored$warning)), collapse = ";"),
    stringsAsFactors = FALSE
  )
}

di09_failure_rows <- function(T, rho, sigma_firm, replication, K, n_firms, e) {
  bind_rows(lapply(c("row_level_kfold", "grouped_firm_kfold"), function(target) {
    data.frame(
      Replication = as.integer(replication),
      n_firms = as.integer(n_firms),
      T = as.integer(T),
      rho = as.numeric(rho),
      sigma_firm = as.numeric(sigma_firm),
      K = as.integer(K),
      validation_target = target,
      score_pooled = NA_real_,
      score_firmre = NA_real_,
      firmre_premium = NA_real_,
      firmre_weight = NA_real_,
      row_minus_grouped_firmre_premium = NA_real_,
      row_minus_grouped_firmre_weight_premium = NA_real_,
      seed = NA_integer_,
      fit_status = "FAILED",
      warning = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  }))
}

di09_run_replication <- function(T, rho, sigma_firm, replication,
                                 K, n_firms, n_industries, sigma_eps,
                                 base_seed, lme4_available) {
  scenario_offset <- 800000L + as.integer(replication) +
    as.integer(T) * 1000L + as.integer(round(rho * 1000)) * 10L +
    as.integer(round(sigma_firm * 10000))
  df <- di09_simulate_panel_ar1(
    T = T, rho = rho, sigma_firm = sigma_firm, replication = replication,
    n_firms = n_firms, n_industries = n_industries, sigma_eps = sigma_eps,
    base_seed = base_seed
  )
  seed <- attr(df, "seed")
  row_score <- di09_score_cv(df, "row_level_kfold", K, scenario_offset, lme4_available, base_seed)
  grouped_score <- di09_score_cv(df, "grouped_firm_kfold", K, scenario_offset, lme4_available, base_seed)
  pair <- bind_rows(row_score, grouped_score)
  row_premium <- pair$firmre_premium[pair$validation_target == "row_level_kfold"]
  grouped_premium <- pair$firmre_premium[pair$validation_target == "grouped_firm_kfold"]
  replication_cols <- c(
    "Replication", "n_firms", "T", "rho", "sigma_firm", "K",
    "validation_target", "score_pooled", "score_firmre", "firmre_premium",
    "firmre_weight", "row_minus_grouped_firmre_premium",
    "row_minus_grouped_firmre_weight_premium", "seed", "fit_status", "warning"
  )
  pair %>%
    mutate(
      Replication = as.integer(replication),
      n_firms = as.integer(n_firms),
      T = as.integer(T),
      rho = as.numeric(rho),
      sigma_firm = as.numeric(sigma_firm),
      K = as.integer(K),
      row_minus_grouped_firmre_premium = row_premium - grouped_premium,
      row_minus_grouped_firmre_weight_premium = NA_real_,
      seed = seed,
      fit_status = ifelse(is.finite(.data$firmre_premium), "SUCCESS", "INSUFFICIENT_DEPENDENCY"),
      warning = ifelse(nzchar(.data$warning), .data$warning, NA_character_)
    ) %>%
    select(all_of(replication_cols))
}

di09_successful_replication_pairs <- function(x) {
  if (!nrow(x) || !all(c("Replication", "validation_target", "fit_status") %in% names(x))) return(0L)
  reps <- split(x, as.integer(x$Replication))
  sum(vapply(reps, function(z) {
    all(c("row_level_kfold", "grouped_firm_kfold") %in% z$validation_target) && all(z$fit_status == "SUCCESS")
  }, logical(1)), na.rm = TRUE)
}

di09_expected_target_pair_ok <- function(x) {
  if (!nrow(x) || !all(c("Replication", "validation_target") %in% names(x))) return(FALSE)
  reps <- split(x, as.integer(x$Replication))
  all(vapply(reps, function(z) {
    identical(sort(as.character(z$validation_target)), c("grouped_firm_kfold", "row_level_kfold"))
  }, logical(1)))
}

di09_existing_result_state <- function(path, task) {
  expected_reps <- seq.int(as.integer(task$Rep_Start), as.integer(task$Rep_End))
  if (!file.exists(path)) {
    return(list(reusable = FALSE, reason = "missing_result_file", n_rows = 0L,
                n_success_pairs = 0L, n_failed_rows = 0L))
  }
  existing <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
  if (inherits(existing, "error")) {
    return(list(reusable = FALSE, reason = paste0("unreadable_result_file: ", conditionMessage(existing)),
                n_rows = NA_integer_, n_success_pairs = NA_integer_, n_failed_rows = NA_integer_))
  }
  required_cols <- c("Replication", "T", "rho", "sigma_firm", "validation_target", "fit_status")
  missing_cols <- setdiff(required_cols, names(existing))
  n_failed_rows <- if ("fit_status" %in% names(existing)) sum(existing$fit_status != "SUCCESS", na.rm = TRUE) else NA_integer_
  n_success_pairs <- di09_successful_replication_pairs(existing)
  if (length(missing_cols)) {
    return(list(reusable = FALSE, reason = paste0("missing_columns: ", paste(missing_cols, collapse = ",")),
                n_rows = nrow(existing), n_success_pairs = n_success_pairs, n_failed_rows = n_failed_rows))
  }
  actual_reps <- sort(unique(as.integer(existing$Replication)))
  missing_reps <- setdiff(expected_reps, actual_reps)
  extra_reps <- setdiff(actual_reps, expected_reps)
  duplicate_pair_count <- sum(duplicated(existing[, c("Replication", "validation_target"), drop = FALSE]))
  design_match <- nrow(existing) > 0 &&
    all(as.integer(existing$T) == as.integer(task$T), na.rm = TRUE) &&
    all(abs(as.numeric(existing$rho) - as.numeric(task$rho)) < 1e-12, na.rm = TRUE) &&
    all(abs(as.numeric(existing$sigma_firm) - as.numeric(task$sigma_firm)) < 1e-12, na.rm = TRUE)
  target_pair_ok <- di09_expected_target_pair_ok(existing)
  reusable <- length(missing_reps) == 0L && length(extra_reps) == 0L &&
    identical(as.integer(duplicate_pair_count), 0L) && isTRUE(design_match) &&
    isTRUE(target_pair_ok) && n_failed_rows == 0L && n_success_pairs == length(expected_reps)
  reason <- if (reusable) {
    "complete_successful_result"
  } else {
    paste(c(
      if (length(missing_reps)) paste0("missing_replications=", length(missing_reps)) else NULL,
      if (length(extra_reps)) paste0("extra_replications=", length(extra_reps)) else NULL,
      if (!identical(as.integer(duplicate_pair_count), 0L)) paste0("duplicate_replication_target_pairs=", duplicate_pair_count) else NULL,
      if (!isTRUE(design_match)) "design_mismatch" else NULL,
      if (!isTRUE(target_pair_ok)) "target_pair_incomplete" else NULL,
      if (is.finite(n_failed_rows) && n_failed_rows > 0) paste0("failed_or_insufficient_rows=", n_failed_rows) else NULL,
      if (n_success_pairs != length(expected_reps)) paste0("successful_pairs=", n_success_pairs, "_expected=", length(expected_reps)) else NULL
    ), collapse = "; ")
  }
  list(reusable = reusable, reason = reason, n_rows = nrow(existing),
       n_success_pairs = n_success_pairs, n_failed_rows = n_failed_rows)
}
