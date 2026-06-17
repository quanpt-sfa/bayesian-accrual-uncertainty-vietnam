# -----------------------------------------------------------------------------
# Script: 26_sim_brms_leakage_confirmation.R
# Purpose: Bayesian MCMC confirmation subset for the Firm-RE row-LOO leakage
#          mechanism detected by the fast lmer simulation.
# -----------------------------------------------------------------------------

source("scripts/00_helpers.R")
source("scripts/23_sim_lmer_leakage_pilot_helpers.R")

check_sim_packages(c("brms", "loo", "dplyr", "posterior"))
suppressPackageStartupMessages({
  library(brms)
  library(loo)
  library(dplyr)
  library(posterior)
})

brms_sim_root <- function(root = output_root) file.path(root, "simulation", "brms_leakage_confirmation")
ensure_brms_sim_dirs <- function(root = output_root) {
  r <- brms_sim_root(root)
  for (d in file.path(r, c("", "tables", "logs"))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  r
}

log_mean_exp <- function(x) {
  m <- max(x)
  m + log(mean(exp(x - m)))
}

lpd_from_log_lik <- function(log_lik_matrix) {
  apply(log_lik_matrix, 2, log_mean_exp)
}

brms_prior <- function(model_type = c("pooled", "firmre")) {
  model_type <- match.arg(model_type)
  p <- c(
    brms::set_prior("normal(0, 0.10)", class = "b"),
    brms::set_prior("normal(0, 0.10)", class = "Intercept"),
    brms::set_prior("exponential(10)", class = "sigma"),
    brms::set_prior("gamma(2, 0.1)", class = "nu")
  )
  if (model_type == "firmre") p <- c(p, brms::set_prior("exponential(10)", class = "sd"))
  p
}

brms_formula <- function(model_type = c("pooled", "firmre")) {
  model_type <- match.arg(model_type)
  if (model_type == "pooled") {
    return(brms::bf(TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(industry) + factor(year)))
  }
  brms::bf(TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(year) + (1 | company))
}

fit_brms_model <- function(df, model_type, chains, iter, warmup, cores, seed, adapt_delta, max_treedepth) {
  brms::brm(
    formula = brms_formula(model_type),
    data = df,
    family = brms::student(),
    prior = brms_prior(model_type),
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
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

group_kfold_score <- function(df, K, chains, iter, warmup, cores, seed, adapt_delta, max_treedepth) {
  folds <- make_firm_folds(df, K = K, seed = seed)
  lpd_p <- numeric(0)
  lpd_r <- numeric(0)
  for (k in seq_len(K)) {
    train <- df[folds != k, , drop = FALSE]
    test <- df[folds == k, , drop = FALSE]
    fit_p <- fit_brms_model(train, "pooled", chains, iter, warmup, cores, seed + 1000 + k, adapt_delta, max_treedepth)
    fit_r <- fit_brms_model(train, "firmre", chains, iter, warmup, cores, seed + 2000 + k, adapt_delta, max_treedepth)
    ll_p <- brms::log_lik(fit_p, newdata = test, allow_new_levels = TRUE)
    ll_r <- brms::log_lik(fit_r, newdata = test, re_formula = NA, allow_new_levels = TRUE)
    lpd_p <- c(lpd_p, lpd_from_log_lik(ll_p))
    lpd_r <- c(lpd_r, lpd_from_log_lik(ll_r))
  }
  w <- stacking_weight_2model(lpd_p, lpd_r)
  data.frame(
    elpd_group_pooled = sum(lpd_p),
    elpd_group_firmre = sum(lpd_r),
    weight_group_pooled = unname(w["pooled"]),
    weight_group_firmre = unname(w["firmre"])
  )
}

run_one_brms_replication <- function(T, sigma_firm, rep_id, K, n_firms, n_industries, sigma_eps,
                                     chains, iter, warmup, cores, adapt_delta, max_treedepth) {
  seed <- 900000 + as.integer(rep_id) + as.integer(T) * 1000 + round(sigma_firm * 10000)
  df <- simulate_accrual_panel(n_firms, T, sigma_firm, sigma_eps, n_industries, seed)
  fit_p <- fit_brms_model(df, "pooled", chains, iter, warmup, cores, seed + 11, adapt_delta, max_treedepth)
  fit_r <- fit_brms_model(df, "firmre", chains, iter, warmup, cores, seed + 29, adapt_delta, max_treedepth)
  row <- row_loo_score(fit_p, fit_r)
  grp <- group_kfold_score(df, K, chains, iter, warmup, cores, seed + 47, adapt_delta, max_treedepth)
  diag_p <- safe_diag(fit_p)
  diag_r <- safe_diag(fit_r)
  drow <- row$elpd_row_firmre - row$elpd_row_pooled
  dgrp <- grp$elpd_group_firmre - grp$elpd_group_pooled
  data.frame(
    T = T, sigma_firm = sigma_firm, sigma_eps = sigma_eps, rep_id = rep_id,
    n_firms = n_firms, n_obs = nrow(df), K = K, chains = chains, iter = iter, warmup = warmup,
    row, grp,
    delta_row = drow, delta_group = dgrp, elpd_leakage_premium = drow - dgrp,
    weight_leakage_premium = row$weight_row_firmre - grp$weight_group_firmre,
    pooled_max_rhat = diag_p$max_rhat, firmre_max_rhat = diag_r$max_rhat,
    pooled_min_ess_bulk = diag_p$min_ess_bulk, firmre_min_ess_bulk = diag_r$min_ess_bulk,
    pooled_n_divergent = diag_p$n_divergent, firmre_n_divergent = diag_r$n_divergent,
    error = NA_character_, stringsAsFactors = FALSE
  )
}

summarise_brms_leakage <- function(x) {
  ok <- x[is.na(x$error) | x$error == "", , drop = FALSE]
  if (!nrow(ok)) stop("[BLOCKER] No successful Bayesian replication.")
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
      prob_positive_weight_premium = mean(.data$weight_leakage_premium > 0, na.rm = TRUE),
      max_pareto_k_pooled = max(.data$max_pareto_k_pooled, na.rm = TRUE),
      max_pareto_k_firmre = max(.data$max_pareto_k_firmre, na.rm = TRUE),
      max_rhat = max(.data$pooled_max_rhat, .data$firmre_max_rhat, na.rm = TRUE),
      total_divergent = sum(.data$pooled_n_divergent, .data$firmre_n_divergent, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$sigma_firm, .data$T)
}

start_time <- Sys.time()
root <- ensure_brms_sim_dirs()
tables_dir <- file.path(root, "tables")
logs_dir <- file.path(root, "logs")

t_grid <- parse_num_env("ACCRUAL_SIM_BRMS_T_GRID", c(3, 7, 15))
sigma_grid <- parse_num_env("ACCRUAL_SIM_BRMS_SIGMA_FIRM_GRID", c(0, 0.10, 0.30))
R <- parse_int_env("ACCRUAL_SIM_BRMS_REPLICATIONS", 2)
K <- parse_int_env("ACCRUAL_SIM_BRMS_K", 3)
n_firms <- parse_int_env("ACCRUAL_SIM_BRMS_N_FIRMS", 80)
n_industries <- parse_int_env("ACCRUAL_SIM_BRMS_N_INDUSTRIES", 10)
sigma_eps <- parse_num_env("ACCRUAL_SIM_BRMS_SIGMA_EPS", 0.08)[1]
chains <- parse_int_env("ACCRUAL_SIM_BRMS_CHAINS", 2)
iter <- parse_int_env("ACCRUAL_SIM_BRMS_ITER", 1000)
warmup <- parse_int_env("ACCRUAL_SIM_BRMS_WARMUP", 500)
cores <- parse_int_env("ACCRUAL_SIM_BRMS_CORES", min(chains, 2))
adapt_delta <- parse_num_env("ACCRUAL_SIM_BRMS_ADAPT_DELTA", 0.95)[1]
max_treedepth <- parse_int_env("ACCRUAL_SIM_BRMS_MAX_TREEDEPTH", 12)

grid <- expand.grid(T = as.integer(t_grid), sigma_firm = sigma_grid, rep_id = seq_len(R), KEEP.OUT.ATTRS = FALSE)
rep_path <- file.path(tables_dir, "table_brms_leakage_confirmation_rep_results.csv")
sum_path <- file.path(tables_dir, "table_brms_leakage_confirmation_grid_summary.csv")
manifest_path <- file.path(logs_dir, "brms_leakage_confirmation_manifest.csv")

message("Bayesian MCMC leakage confirmation: ", nrow(grid), " replications. This can be slow.")

out <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf("[%d/%d] T=%d sigma=%.2f rep=%d", i, nrow(grid), g$T, g$sigma_firm, g$rep_id))
  out[[i]] <- tryCatch(
    run_one_brms_replication(g$T, g$sigma_firm, g$rep_id, K, n_firms, n_industries, sigma_eps,
                             chains, iter, warmup, cores, adapt_delta, max_treedepth),
    error = function(e) data.frame(T = g$T, sigma_firm = g$sigma_firm, sigma_eps = sigma_eps,
                                   rep_id = g$rep_id, n_firms = n_firms, n_obs = n_firms * g$T,
                                   K = K, chains = chains, iter = iter, warmup = warmup,
                                   error = conditionMessage(e))
  )
  write.csv(dplyr::bind_rows(out[seq_len(i)]), rep_path, row.names = FALSE)
}

results <- dplyr::bind_rows(out)
write.csv(results, rep_path, row.names = FALSE)
summary_df <- summarise_brms_leakage(results)
write.csv(summary_df, sum_path, row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))

manifest <- data.frame(
  script = "scripts/26_sim_brms_leakage_confirmation.R",
  start_time = as.character(start_time), end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(t_grid, collapse = ","), sigma_firm_grid = paste(sigma_grid, collapse = ","),
  replications = R, K = K, n_firms = n_firms, n_industries = n_industries,
  sigma_eps = sigma_eps, chains = chains, iter = iter, warmup = warmup,
  output_root = root,
  successful_replications = sum(is.na(results$error) | results$error == ""),
  failed_replications = sum(!(is.na(results$error) | results$error == ""))
)
write.csv(manifest, manifest_path, row.names = FALSE)

cat("\n[SUCCESS] Bayesian MCMC leakage confirmation completed.\n")
cat("Results:", rep_path, "\n")
cat("Summary:", sum_path, "\n")
