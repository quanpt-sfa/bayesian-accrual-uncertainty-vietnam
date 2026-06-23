# -----------------------------------------------------------------------------
# Script: si03_brms_leakage_confirmation.R
# Purpose: Bayesian MCMC bridge check for the Firm-RE row-LOO leakage mechanism.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si03", "Simulation: BRMS leakage confirmation")
source("scripts/simulation/si00_helpers.R")
check_sim_packages(c("brms", "loo", "dplyr", "posterior"))
suppressPackageStartupMessages({
  library(brms)
  library(loo)
  library(dplyr)
  library(posterior)
})

brms_sim_root <- function(root = output_root) {
  file.path(root, "simulation", "brms_leakage_confirmation")
}

ensure_brms_sim_dirs <- function(root = output_root) {
  r <- brms_sim_root(root)
  for (d in file.path(r, c("", "tables", "logs"))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  r
}

log_mean_exp <- function(x) {
  m <- max(x)
  m + log(mean(exp(x - m)))
}

lpd_from_log_lik <- function(log_lik_matrix) {
  apply(log_lik_matrix, 2, log_mean_exp)
}

simulate_accrual_panel_bridge <- function(n_firms = 80, T = 7, sigma_firm = 0.10,
                                          sigma_eps = 0.08, n_industries = 10,
                                          rng_context = "sim_brms_bridge_panel", rng_offset = 0L,
                                          dgp_family = c("gaussian", "student"),
                                          dgp_nu = 7) {
  dgp_family <- match.arg(dgp_family)
  if (!is.finite(dgp_nu) || dgp_nu <= 2) {
    stop("[BLOCKER] ACCRUAL_SIM_BRMS_DGP_NU must be finite and > 2.")
  }
  set_accrual_seed(rng_context, offset = rng_offset)

  firms <- sprintf("F%04d", seq_len(n_firms))
  years <- seq_len(T)
  inds <- sprintf("IND%02d", seq_len(n_industries))
  ind_map <- sample(rep(inds, length.out = n_firms), n_firms)
  names(ind_map) <- firms

  df <- expand.grid(
    company = firms,
    year = years,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  df$industry <- ind_map[df$company]

  a_i <- stats::rnorm(n_firms, 0, sigma_firm)
  names(a_i) <- firms
  y_t <- stats::rnorm(T, 0, 0.015)
  names(y_t) <- as.character(years)
  g_j <- stats::rnorm(n_industries, 0, 0.020)
  names(g_j) <- inds

  df$inv_A_lag <- stats::rnorm(nrow(df), 0.020, 0.010)
  df$dREV_dREC_scaled <- stats::rnorm(nrow(df), 0.050, 0.100)
  df$PPE_scaled <- stats::rnorm(nrow(df), 0.450, 0.200)
  df$ROA_lag <- stats::rnorm(nrow(df), 0.040, 0.080)

  mu <- 0.000 +
    0.030 * df$inv_A_lag +
    0.060 * df$dREV_dREC_scaled -
    0.040 * df$PPE_scaled +
    0.080 * df$ROA_lag +
    a_i[df$company] +
    y_t[as.character(df$year)] +
    g_j[df$industry]

  eps <- if (dgp_family == "gaussian") {
    stats::rnorm(nrow(df), 0, sigma_eps)
  } else {
    sigma_eps * stats::rt(nrow(df), df = dgp_nu)
  }

  df$TA_scaled <- mu + eps
  df$company <- factor(df$company, levels = firms)
  df$industry <- factor(df$industry, levels = inds)
  df$year <- factor(df$year, levels = as.character(years))

  df
}

brms_prior <- function(model_type = c("pooled", "firmre"),
                       prior_mode = c("fixed", "scale_aware"),
                       df = NULL) {
  model_type <- match.arg(model_type)
  prior_mode <- match.arg(prior_mode)

  if (prior_mode == "fixed") {
    p <- c(
      brms::set_prior("normal(0, 0.10)", class = "b"),
      brms::set_prior("normal(0, 0.10)", class = "Intercept"),
      brms::set_prior("exponential(10)", class = "sigma"),
      brms::set_prior("gamma(2, 0.1)", class = "nu")
    )
    if (model_type == "firmre") {
      p <- c(p, brms::set_prior("exponential(10)", class = "sd"))
    }
    return(p)
  }

  y_sd <- if (!is.null(df) && "TA_scaled" %in% names(df)) {
    stats::sd(df$TA_scaled, na.rm = TRUE)
  } else {
    NA_real_
  }
  if (!is.finite(y_sd) || y_sd <= 0) y_sd <- 0.08

  slope_scale <- max(0.10, 2.5 * y_sd)
  sigma_rate <- 1 / max(0.02, y_sd)

  p <- c(
    brms::set_prior(sprintf("normal(0, %.10f)", slope_scale), class = "b"),
    brms::set_prior(sprintf("normal(0, %.10f)", slope_scale), class = "Intercept"),
    brms::set_prior(sprintf("exponential(%.10f)", sigma_rate), class = "sigma"),
    brms::set_prior("gamma(2, 0.1)", class = "nu")
  )
  if (model_type == "firmre") {
    p <- c(p, brms::set_prior(sprintf("exponential(%.10f)", sigma_rate), class = "sd"))
  }
  p
}

brms_formula <- function(model_type = c("pooled", "firmre")) {
  model_type <- match.arg(model_type)
  if (model_type == "pooled") {
    return(brms::bf(TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag +
                      factor(industry) + factor(year)))
  }
  brms::bf(TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag +
             factor(year) + (1 | company))
}

fit_brms_model <- function(df, model_type, prior_mode, chains, iter, warmup, cores,
                           rng_context, rng_offset, adapt_delta, max_treedepth) {
  brms::brm(
    formula = brms_formula(model_type),
    data = df,
    family = brms::student(),
    prior = brms_prior(model_type, prior_mode = prior_mode, df = df),
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = accrual_seed_for(rng_context, offset = rng_offset),
    refresh = 0,
    silent = 2,
    save_pars = brms::save_pars(all = TRUE),
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
  )
}

safe_diag <- function(fit) {
  draws <- posterior::as_draws_df(fit)
  s <- posterior::summarise_draws(draws)
  np <- brms::nuts_params(fit)
  data.frame(
    max_rhat = suppressWarnings(max(s$rhat, na.rm = TRUE)),
    min_ess_bulk = suppressWarnings(min(s$ess_bulk, na.rm = TRUE)),
    n_divergent = sum(np$Parameter == "divergent__" & np$Value == 1, na.rm = TRUE)
  )
}

row_loo_score <- function(fit_pooled, fit_firmre) {
  loo_p <- brms::loo(fit_pooled, moment_match = FALSE)
  loo_r <- brms::loo(fit_firmre, moment_match = FALSE)
  w <- tryCatch(
    loo::loo_model_weights(list(pooled = loo_p, firmre = loo_r), method = "stacking"),
    error = function(e) loo::loo_model_weights(loo_p, loo_r, method = "stacking")
  )
  data.frame(
    elpd_row_pooled = loo_p$estimates["elpd_loo", "Estimate"],
    elpd_row_firmre = loo_r$estimates["elpd_loo", "Estimate"],
    weight_row_pooled = unname(w["pooled"]),
    weight_row_firmre = unname(w["firmre"]),
    max_pareto_k_pooled = max(loo_p$diagnostics$pareto_k, na.rm = TRUE),
    max_pareto_k_firmre = max(loo_r$diagnostics$pareto_k, na.rm = TRUE)
  )
}

group_kfold_score <- function(df, K, prior_mode, chains, iter, warmup, cores,
                              rng_context, rng_offset, adapt_delta, max_treedepth) {
  folds <- make_firm_folds(df, K = K, rng_context = paste0(rng_context, "_folds"), rng_offset = rng_offset)
  lpd_p <- numeric(0)
  lpd_r <- numeric(0)
  for (k in seq_len(K)) {
    train <- df[folds != k, , drop = FALSE]
    test <- df[folds == k, , drop = FALSE]
    fit_p <- fit_brms_model(train, "pooled", prior_mode, chains, iter, warmup, cores,
                            paste0(rng_context, "_pooled"), rng_offset + 1000L + k, adapt_delta, max_treedepth)
    fit_r <- fit_brms_model(train, "firmre", prior_mode, chains, iter, warmup, cores,
                            paste0(rng_context, "_firmre"), rng_offset + 2000L + k, adapt_delta, max_treedepth)
    lpd_p <- c(lpd_p, lpd_from_log_lik(brms::log_lik(fit_p, newdata = test, allow_new_levels = TRUE)))
    lpd_r <- c(lpd_r, lpd_from_log_lik(brms::log_lik(fit_r, newdata = test, re_formula = NA,
                                                     allow_new_levels = TRUE)))
  }
  w <- stacking_weight_2model(lpd_p, lpd_r)
  data.frame(
    elpd_group_pooled = sum(lpd_p),
    elpd_group_firmre = sum(lpd_r),
    weight_group_pooled = unname(w["pooled"]),
    weight_group_firmre = unname(w["firmre"])
  )
}

run_one_brms_replication <- function(T, sigma_firm, rep_id, K, n_firms, n_industries,
                                     sigma_eps, dgp_family, dgp_nu, prior_mode,
                                     chains, iter, warmup, cores, adapt_delta,
                                     max_treedepth) {
  replication_offset <- 900000L + as.integer(rep_id) + as.integer(T) * 1000L + as.integer(round(sigma_firm * 10000))
  df <- simulate_accrual_panel_bridge(
    n_firms = n_firms,
    T = T,
    sigma_firm = sigma_firm,
    sigma_eps = sigma_eps,
    n_industries = n_industries,
    rng_context = "sim_brms_bridge_panel",
    rng_offset = replication_offset,
    dgp_family = dgp_family,
    dgp_nu = dgp_nu
  )
  fit_p <- fit_brms_model(df, "pooled", prior_mode, chains, iter, warmup, cores,
                          "sim_brms_bridge_pooled", replication_offset + 11L, adapt_delta, max_treedepth)
  fit_r <- fit_brms_model(df, "firmre", prior_mode, chains, iter, warmup, cores,
                          "sim_brms_bridge_firmre", replication_offset + 29L, adapt_delta, max_treedepth)
  row <- row_loo_score(fit_p, fit_r)
  grp <- group_kfold_score(df, K, prior_mode, chains, iter, warmup, cores,
                           "sim_brms_bridge_group_kfold", replication_offset + 47L, adapt_delta, max_treedepth)
  diag_p <- safe_diag(fit_p)
  diag_r <- safe_diag(fit_r)
  drow <- row$elpd_row_firmre - row$elpd_row_pooled
  dgrp <- grp$elpd_group_firmre - grp$elpd_group_pooled
  data.frame(
    dgp_family = dgp_family,
    dgp_nu = dgp_nu,
    prior_mode = prior_mode,
    test_quantity_primary = "weight_leakage_premium",
    test_quantity_secondary = "elpd_leakage_premium",
    T = T,
    sigma_firm = sigma_firm,
    sigma_eps = sigma_eps,
    rep_id = rep_id,
    n_firms = n_firms,
    n_obs = nrow(df),
    K = K,
    chains = chains,
    iter = iter,
    warmup = warmup,
    row,
    grp,
    delta_row = drow,
    delta_group = dgrp,
    elpd_leakage_premium = drow - dgrp,
    weight_leakage_premium = row$weight_row_firmre - grp$weight_group_firmre,
    pooled_max_rhat = diag_p$max_rhat,
    firmre_max_rhat = diag_r$max_rhat,
    pooled_min_ess_bulk = diag_p$min_ess_bulk,
    firmre_min_ess_bulk = diag_r$min_ess_bulk,
    pooled_n_divergent = diag_p$n_divergent,
    firmre_n_divergent = diag_r$n_divergent,
    error = NA_character_,
    stringsAsFactors = FALSE
  )
}

brms_error_row <- function(g, K, n_firms, sigma_eps, dgp_family, dgp_nu, prior_mode,
                           chains, iter, warmup, err) {
  data.frame(
    dgp_family = dgp_family,
    dgp_nu = dgp_nu,
    prior_mode = prior_mode,
    test_quantity_primary = "weight_leakage_premium",
    test_quantity_secondary = "elpd_leakage_premium",
    T = g$T,
    sigma_firm = g$sigma_firm,
    sigma_eps = sigma_eps,
    rep_id = g$rep_id,
    n_firms = n_firms,
    n_obs = n_firms * g$T,
    K = K,
    chains = chains,
    iter = iter,
    warmup = warmup,
    error = conditionMessage(err),
    stringsAsFactors = FALSE
  )
}

summarise_brms_leakage <- function(x) {
  ok <- x[is.na(x$error) | x$error == "", , drop = FALSE]
  if (!nrow(ok)) stop("[BLOCKER] No successful Bayesian replication.")
  dplyr::as_tibble(ok) |>
    dplyr::group_by(.data$dgp_family, .data$dgp_nu, .data$prior_mode,
                    .data$T, .data$sigma_firm) |>
    dplyr::summarise(
      test_quantity_primary = "weight_leakage_premium",
      test_quantity_secondary = "elpd_leakage_premium",
      n_rep = dplyr::n(),
      mean_weight_row_firmre = mean(.data$weight_row_firmre, na.rm = TRUE),
      mean_weight_group_firmre = mean(.data$weight_group_firmre, na.rm = TRUE),
      mean_weight_premium = mean(.data$weight_leakage_premium, na.rm = TRUE),
      median_weight_premium = stats::median(.data$weight_leakage_premium, na.rm = TRUE),
      mean_abs_weight_premium = mean(abs(.data$weight_leakage_premium), na.rm = TRUE),
      mean_delta_row = mean(.data$delta_row, na.rm = TRUE),
      mean_delta_group = mean(.data$delta_group, na.rm = TRUE),
      mean_elpd_premium = mean(.data$elpd_leakage_premium, na.rm = TRUE),
      prob_positive_weight_premium = mean(.data$weight_leakage_premium > 0, na.rm = TRUE),
      prob_positive_elpd_premium = mean(.data$elpd_leakage_premium > 0, na.rm = TRUE),
      max_pareto_k_pooled = max(.data$max_pareto_k_pooled, na.rm = TRUE),
      max_pareto_k_firmre = max(.data$max_pareto_k_firmre, na.rm = TRUE),
      max_rhat = max(c(.data$pooled_max_rhat, .data$firmre_max_rhat), na.rm = TRUE),
      total_divergent = sum(.data$pooled_n_divergent, .data$firmre_n_divergent, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$dgp_family, .data$dgp_nu, .data$prior_mode,
                   .data$sigma_firm, .data$T)
}

start_time <- Sys.time()
root <- ensure_brms_sim_dirs()
tables_dir <- file.path(root, "tables")
logs_dir <- file.path(root, "logs")

sim_cfg <- accrual_simulation_runtime_config("brms_leakage")
t_grid <- sim_cfg$t_grid
sigma_grid <- sim_cfg$sigma_grid
R <- sim_cfg$R
K <- sim_cfg$K
n_firms <- sim_cfg$n_firms
n_industries <- sim_cfg$n_industries
sigma_eps <- sim_cfg$sigma_eps
dgp_family <- sim_cfg$dgp_family
prior_mode <- sim_cfg$prior_mode
dgp_nu <- sim_cfg$dgp_nu
chains <- sim_cfg$chains
iter <- sim_cfg$iter
warmup <- sim_cfg$warmup
cores <- sim_cfg$cores
options(mc.cores = cores)
adapt_delta <- sim_cfg$adapt_delta
max_treedepth <- sim_cfg$max_treedepth

grid <- expand.grid(
  T = as.integer(t_grid),
  sigma_firm = sigma_grid,
  rep_id = seq_len(R),
  KEEP.OUT.ATTRS = FALSE
)

rep_path <- file.path(tables_dir, "table_brms_leakage_confirmation_rep_results.csv")
sum_path <- file.path(tables_dir, "table_brms_leakage_confirmation_grid_summary.csv")
manifest_path <- file.path(logs_dir, "brms_leakage_confirmation_manifest.csv")

message(
  "Bayesian MCMC leakage confirmation: ", nrow(grid), " replications. ",
  "DGP=", dgp_family, ", prior_mode=", prior_mode,
  ", primary test quantity=weight_leakage_premium. This can be slow."
)
message(
  "brms/rstan sampler controls: chains=", chains,
  ", cores=", cores,
  ", iter=", iter,
  ", warmup=", warmup,
  ", adapt_delta=", adapt_delta,
  ", max_treedepth=", max_treedepth
)

out <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf(
    "[%d/%d] DGP=%s prior=%s T=%d sigma=%.2f rep=%d",
    i, nrow(grid), dgp_family, prior_mode, g$T, g$sigma_firm, g$rep_id
  ))
  out[[i]] <- tryCatch(
    run_one_brms_replication(
      g$T, g$sigma_firm, g$rep_id, K, n_firms, n_industries, sigma_eps,
      dgp_family, dgp_nu, prior_mode, chains, iter, warmup, cores,
      adapt_delta, max_treedepth
    ),
    error = function(e) {
      brms_error_row(g, K, n_firms, sigma_eps, dgp_family, dgp_nu, prior_mode,
                     chains, iter, warmup, e)
    }
  )
  write.csv(dplyr::bind_rows(out[seq_len(i)]), rep_path, row.names = FALSE)
}

results <- dplyr::bind_rows(out)
write.csv(results, rep_path, row.names = FALSE)
summary_df <- summarise_brms_leakage(results)
write.csv(summary_df, sum_path, row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))

manifest <- data.frame(
  script = "scripts/simulation/si03_brms_leakage_confirmation.R",
  script_role = "bayesian_bridge_check_not_parameter_recovery",
  start_time = as.character(start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(t_grid, collapse = ","),
  sigma_firm_grid = paste(sigma_grid, collapse = ","),
  replications = R,
  K = K,
  n_firms = n_firms,
  n_industries = n_industries,
  sigma_eps = sigma_eps,
  dgp_family = dgp_family,
  dgp_nu = dgp_nu,
  prior_mode = prior_mode,
  prior_set_id = prior_set_id,
  likelihood_family = "student",
  test_quantity_primary = "weight_leakage_premium",
  test_quantity_secondary = "elpd_leakage_premium",
  chains = chains,
  cores = cores,
  iter = iter,
  warmup = warmup,
  output_root = root,
  successful_replications = sum(is.na(results$error) | results$error == ""),
  failed_replications = sum(!(is.na(results$error) | results$error == "")),
  stringsAsFactors = FALSE
)
write.csv(manifest, manifest_path, row.names = FALSE)

cat("\n[SUCCESS] Bayesian MCMC leakage confirmation completed.\n")
cat("Results:", rep_path, "\n")
cat("Summary:", sum_path, "\n")
phase_end("si03", "Simulation: BRMS leakage confirmation")
