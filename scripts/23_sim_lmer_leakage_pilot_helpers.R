# -----------------------------------------------------------------------------
# Script: 23_sim_lmer_leakage_pilot_helpers.R
# Purpose: Helpers for lmer leakage-pilot simulation.
# -----------------------------------------------------------------------------

check_sim_packages <- function(pkgs = c("lme4", "dplyr", "ggplot2")) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("[BLOCKER] Install package(s): ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

parse_num_env <- function(name, default) {
  x <- trimws(Sys.getenv(name, ""))
  if (!nzchar(x)) return(default)
  y <- suppressWarnings(as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]])))
  if (any(is.na(y))) stop("[BLOCKER] ", name, " must be comma-separated numeric values.")
  y
}

parse_int_env <- function(name, default) {
  x <- trimws(Sys.getenv(name, ""))
  if (!nzchar(x)) return(as.integer(default))
  y <- suppressWarnings(as.integer(x))
  if (is.na(y) || y <= 0) stop("[BLOCKER] ", name, " must be a positive integer.")
  y
}

sim_root <- function(root = output_root) file.path(root, "simulation", "lmer_leakage_pilot")

ensure_sim_dirs <- function(root = output_root) {
  r <- sim_root(root)
  for (d in file.path(r, c("", "tables", "figures", "logs"))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  r
}

simulate_accrual_panel <- function(n_firms = 200, T = 7, sigma_firm = 0.10,
                                   sigma_eps = 0.08, n_industries = 10, seed = accrual_seed("simulation")) {
  set.seed(seed)
  firms <- sprintf("F%04d", seq_len(n_firms))
  years <- seq_len(T)
  inds <- sprintf("IND%02d", seq_len(n_industries))
  ind_map <- sample(rep(inds, length.out = n_firms), n_firms)
  names(ind_map) <- firms
  df <- expand.grid(company = firms, year = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$industry <- ind_map[df$company]
  a_i <- stats::rnorm(n_firms, 0, sigma_firm); names(a_i) <- firms
  y_t <- stats::rnorm(T, 0, 0.015); names(y_t) <- as.character(years)
  g_j <- stats::rnorm(n_industries, 0, 0.020); names(g_j) <- inds
  df$inv_A_lag <- stats::rnorm(nrow(df), 0.020, 0.010)
  df$dREV_dREC_scaled <- stats::rnorm(nrow(df), 0.050, 0.100)
  df$PPE_scaled <- stats::rnorm(nrow(df), 0.450, 0.200)
  df$ROA_lag <- stats::rnorm(nrow(df), 0.040, 0.080)
  mu <- 0.000 + 0.030 * df$inv_A_lag + 0.060 * df$dREV_dREC_scaled -
    0.040 * df$PPE_scaled + 0.080 * df$ROA_lag +
    a_i[df$company] + y_t[as.character(df$year)] + g_j[df$industry]
  df$TA_scaled <- mu + stats::rnorm(nrow(df), 0, sigma_eps)
  df$company <- factor(df$company, levels = firms)
  df$industry <- factor(df$industry, levels = inds)
  df$year <- factor(df$year, levels = as.character(years))
  df
}

make_row_folds <- function(df, K = 5, seed = accrual_seed("simulation")) {
  set.seed(seed)
  sample(rep(seq_len(K), length.out = nrow(df)))
}

make_firm_folds <- function(df, K = 5, seed = accrual_seed("simulation")) {
  info <- unique(df[, c("company", "industry")])
  info$company <- as.character(info$company); info$industry <- as.character(info$industry)
  set.seed(seed); info$u <- stats::runif(nrow(info))
  info <- info[order(info$industry, info$u), ]
  info$fold <- NA_integer_
  for (idx in split(seq_len(nrow(info)), info$industry)) info$fold[idx] <- rep(seq_len(K), length.out = length(idx))
  mp <- info$fold; names(mp) <- info$company
  unname(mp[as.character(df$company)])
}

log_sum_exp <- function(x) { m <- max(x); m + log(sum(exp(x - m))) }
normal_lpd <- function(y, mu, s) stats::dnorm(y, mu, max(s, .Machine$double.eps), log = TRUE)

stacking_weight_2model <- function(lpd_pooled, lpd_firmre) {
  lpd <- cbind(pooled = as.numeric(lpd_pooled), firmre = as.numeric(lpd_firmre))
  if (any(!is.finite(lpd))) stop("[BLOCKER] Non-finite lpd in stacking.")
  obj <- function(theta) {
    wf <- stats::plogis(theta); w <- c(1 - wf, wf)
    -sum(apply(lpd, 1, function(z) log_sum_exp(log(w) + z)))
  }
  wf <- stats::plogis(stats::optimize(obj, c(-10, 10))$minimum)
  c(pooled = 1 - wf, firmre = wf)
}

score_fold <- function(train, test, fold_type = c("row", "group")) {
  fold_type <- match.arg(fold_type)
  pooled <- stats::lm(
    TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(industry) + factor(year),
    data = train
  )
  firmre <- lme4::lmer(
    TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(year) + (1 | company),
    data = train, REML = FALSE,
    control = lme4::lmerControl(check.conv.singular = "ignore", check.conv.grad = "ignore", check.conv.hess = "ignore")
  )
  mu_pool <- as.numeric(stats::predict(pooled, newdata = test))
  mu_re <- if (fold_type == "row") {
    as.numeric(stats::predict(firmre, newdata = test, re.form = NULL, allow.new.levels = TRUE))
  } else {
    as.numeric(stats::predict(firmre, newdata = test, re.form = NA, allow.new.levels = TRUE))
  }
  data.frame(
    lpd_pooled = normal_lpd(test$TA_scaled, mu_pool, stats::sigma(pooled)),
    lpd_firmre = normal_lpd(test$TA_scaled, mu_re, stats::sigma(firmre)),
    singular_firmre = lme4::isSingular(firmre, tol = 1e-5)
  )
}

score_cv <- function(df, fold_type = c("row", "group"), K = 5, seed = 1) {
  fold_type <- match.arg(fold_type)
  folds <- if (fold_type == "row") make_row_folds(df, K, seed) else make_firm_folds(df, K, seed)
  fold_scores <- lapply(seq_len(K), function(k) score_fold(df[folds != k, ], df[folds == k, ], fold_type))
  singular_folds <- sum(vapply(fold_scores, function(z) isTRUE(z$singular_firmre[1]), logical(1)))
  sc <- do.call(rbind, fold_scores)
  w <- stacking_weight_2model(sc$lpd_pooled, sc$lpd_firmre)
  list(
    elpd_pooled = sum(sc$lpd_pooled),
    elpd_firmre = sum(sc$lpd_firmre),
    weight_pooled = unname(w["pooled"]),
    weight_firmre = unname(w["firmre"]),
    singular_folds = singular_folds,
    n_obs = nrow(sc)
  )
}

run_one_replication <- function(T, sigma_firm, rep_id, K = 5, n_firms = 200,
                                n_industries = 10, sigma_eps = 0.08) {
  base_seed <- accrual_seed("simulation")
  seed <- base_seed + 100000 + as.integer(rep_id) + as.integer(T) * 1000 + round(sigma_firm * 10000)
  df <- simulate_accrual_panel(n_firms, T, sigma_firm, sigma_eps, n_industries, seed)
  row <- score_cv(df, "row", K, seed + 11)
  grp <- score_cv(df, "group", K, seed + 29)
  drow <- row$elpd_firmre - row$elpd_pooled
  dgrp <- grp$elpd_firmre - grp$elpd_pooled
  data.frame(
    T = T, sigma_firm = sigma_firm, sigma_eps = sigma_eps, rep_id = rep_id,
    n_firms = n_firms, n_obs = nrow(df), K = K,
    elpd_row_pooled = row$elpd_pooled, elpd_row_firmre = row$elpd_firmre,
    elpd_group_pooled = grp$elpd_pooled, elpd_group_firmre = grp$elpd_firmre,
    delta_row = drow, delta_group = dgrp, elpd_leakage_premium = drow - dgrp,
    weight_row_pooled = row$weight_pooled, weight_row_firmre = row$weight_firmre,
    weight_group_pooled = grp$weight_pooled, weight_group_firmre = grp$weight_firmre,
    weight_leakage_premium = row$weight_firmre - grp$weight_firmre,
    singular_row_firmre_folds = row$singular_folds,
    singular_group_firmre_folds = grp$singular_folds,
    error = NA_character_, stringsAsFactors = FALSE
  )
}

summarise_leakage <- function(x) {
  if (!"error" %in% names(x)) x$error <- NA_character_
  ok <- x[is.na(x$error) | x$error == "", ]
  if (!nrow(ok)) stop("[BLOCKER] No successful replication.")
  dplyr::as_tibble(ok) |>
    dplyr::group_by(.data$T, .data$sigma_firm) |>
    dplyr::summarise(
      n_rep = dplyr::n(),
      mean_weight_row_firmre = mean(.data$weight_row_firmre, na.rm = TRUE),
      mean_weight_group_firmre = mean(.data$weight_group_firmre, na.rm = TRUE),
      mean_weight_premium = mean(.data$weight_leakage_premium, na.rm = TRUE),
      median_weight_premium = stats::median(.data$weight_leakage_premium, na.rm = TRUE),
      mean_delta_row = mean(.data$delta_row, na.rm = TRUE),
      mean_delta_group = mean(.data$delta_group, na.rm = TRUE),
      mean_elpd_premium = mean(.data$elpd_leakage_premium, na.rm = TRUE),
      median_elpd_premium = stats::median(.data$elpd_leakage_premium, na.rm = TRUE),
      prob_positive_weight_premium = mean(.data$weight_leakage_premium > 0, na.rm = TRUE),
      prob_positive_elpd_premium = mean(.data$elpd_leakage_premium > 0, na.rm = TRUE),
      mean_singular_row_folds = mean(.data$singular_row_firmre_folds, na.rm = TRUE),
      mean_singular_group_folds = mean(.data$singular_group_firmre_folds, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$sigma_firm, .data$T)
}

pilot_decision <- function(s, metric = "mean_weight_premium", tol = 0.02) {
  by_sig <- s |> dplyr::arrange(.data$sigma_firm, .data$T) |> dplyr::group_by(.data$sigma_firm) |>
    dplyr::summarise(monotone_T = all(diff(.data[[metric]]) >= -tol), .groups = "drop")
  by_T <- s |> dplyr::arrange(.data$T, .data$sigma_firm) |> dplyr::group_by(.data$T) |>
    dplyr::summarise(monotone_sigma = all(diff(.data[[metric]]) >= -tol), .groups = "drop")
  hi <- s |> dplyr::filter(.data$T == max(.data$T), .data$sigma_firm == max(.data$sigma_firm)) |> dplyr::slice(1)
  lo <- s |> dplyr::filter(.data$T == min(.data$T), .data$sigma_firm == min(.data$sigma_firm)) |> dplyr::slice(1)
  hml <- hi[[metric]] - lo[[metric]]
  hp <- hi$prob_positive_weight_premium
  dec <- if (mean(by_sig$monotone_T) >= 2/3 && mean(by_T$monotone_sigma) >= 2/3 && hml > 0.10 && hp >= 0.70) {
    "PILOT_PASS__RUN_FULL_GRID"
  } else {
    "PILOT_STOP__DO_NOT_RUN_FULL_SIMULATION"
  }
  list(decision = dec, metric = metric, monotone_T_rate = mean(by_sig$monotone_T),
       monotone_sigma_rate = mean(by_T$monotone_sigma), high_minus_low = hml,
       high_prob_positive = hp, by_sigma = by_sig, by_T = by_T)
}
