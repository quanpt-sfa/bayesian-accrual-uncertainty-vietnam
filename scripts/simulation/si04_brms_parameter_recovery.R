# -----------------------------------------------------------------------------
# Script: si04_brms_parameter_recovery.R
# Purpose: Auxiliary Bayesian MCMC parameter-recovery simulation for the Firm-RE
#          Student-t accrual specification.
#
# Reviewer-final role:
#   This script is an auxiliary model-behavior diagnostic. It is not the central
#   simulation estimand for leakage or validation-unit sensitivity. The central
#   simulation quantities remain the leakage premium / validation-target premium
#   from the companion simulation scripts. Parameter recovery is reported only to
#   check whether the Bayesian Firm-RE Student-t specification behaves coherently
#   under simulated data with known parameters.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si04", "Simulation: BRMS parameter recovery")
source("scripts/simulation/si00_helpers.R")

check_sim_packages(c("brms", "dplyr", "posterior", "ggplot2"))
suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(posterior)
  library(ggplot2)
})

script_name <- "scripts/simulation/si04_brms_parameter_recovery.R"
script_version <- "2026-06-18-v2-diagnostics-boundary-patch"
script_role <- "auxiliary_parameter_recovery_not_primary_estimand"

recovery_root <- function(root = output_root) file.path(root, "simulation", "brms_parameter_recovery")
ensure_recovery_dirs <- function(root = output_root) {
  r <- recovery_root(root)
  for (d in file.path(r, c("", "tables", "figures", "logs"))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  r
}

simulate_accrual_panel_student_truth <- function(n_firms = 80, T = 7, sigma_firm = 0.10,
                                                 sigma_eps = 0.08, nu = 7,
                                                 n_industries = 10,
                                                 rng_context = "sim_brms_recovery_panel", rng_offset = 0L) {
  set_accrual_seed(rng_context, offset = rng_offset)
  firms <- sprintf("F%04d", seq_len(n_firms))
  years <- seq_len(T)
  inds <- sprintf("IND%02d", seq_len(n_industries))
  ind_map <- sample(rep(inds, length.out = n_firms), n_firms)
  names(ind_map) <- firms
  df <- expand.grid(company = firms, year = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$industry <- ind_map[df$company]

  beta <- c(inv_A_lag = 0.030, dREV_dREC_scaled = 0.060, PPE_scaled = -0.040, ROA_lag = 0.080)
  a_i <- stats::rnorm(n_firms, 0, sigma_firm)
  names(a_i) <- firms
  y_t <- stats::rnorm(T, 0, 0.015)
  names(y_t) <- as.character(years)

  df$inv_A_lag <- stats::rnorm(nrow(df), 0.020, 0.010)
  df$dREV_dREC_scaled <- stats::rnorm(nrow(df), 0.050, 0.100)
  df$PPE_scaled <- stats::rnorm(nrow(df), 0.450, 0.200)
  df$ROA_lag <- stats::rnorm(nrow(df), 0.040, 0.080)

  mu <- beta["inv_A_lag"] * df$inv_A_lag +
    beta["dREV_dREC_scaled"] * df$dREV_dREC_scaled +
    beta["PPE_scaled"] * df$PPE_scaled +
    beta["ROA_lag"] * df$ROA_lag +
    a_i[df$company] + y_t[as.character(df$year)]

  # brms::student() parameterises sigma as the Student-t scale. Therefore the
  # data-generating residual below uses sigma_eps as scale, not unconditional SD.
  df$TA_scaled <- mu + sigma_eps * stats::rt(nrow(df), df = nu)
  df$company <- factor(df$company, levels = firms)
  df$industry <- factor(df$industry, levels = inds)
  df$year <- factor(df$year, levels = as.character(years))

  truth <- data.frame(
    parameter = c("beta_inv_A_lag", "beta_dREV_dREC_scaled", "beta_PPE_scaled", "beta_ROA_lag", "sigma", "nu", "sd_company"),
    draw_var = c("b_inv_A_lag", "b_dREV_dREC_scaled", "b_PPE_scaled", "b_ROA_lag", "sigma", "nu", "sd_company__Intercept"),
    true_value = c(beta["inv_A_lag"], beta["dREV_dREC_scaled"], beta["PPE_scaled"], beta["ROA_lag"], sigma_eps, nu, sigma_firm),
    boundary_parameter = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, sigma_firm == 0),
    stringsAsFactors = FALSE
  )
  attr(df, "truth") <- truth
  df
}

recovery_prior <- function() {
  c(
    brms::set_prior("normal(0, 0.10)", class = "b"),
    brms::set_prior("normal(0, 0.10)", class = "Intercept"),
    brms::set_prior("exponential(10)", class = "sigma"),
    brms::set_prior("exponential(10)", class = "sd"),
    brms::set_prior("gamma(2, 0.1)", class = "nu")
  )
}

fit_recovery_model <- function(df, chains, iter, warmup, cores, rng_context, rng_offset, adapt_delta, max_treedepth) {
  brms::brm(
    formula = brms::bf(TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(year) + (1 | company)),
    data = df,
    family = brms::student(),
    prior = recovery_prior(),
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

diagnostic_status_from_gates <- function(max_rhat, min_ess_bulk, min_ess_tail,
                                         n_divergent, n_treedepth_hit) {
  if (is.na(max_rhat) || is.na(min_ess_bulk) || is.na(min_ess_tail) ||
      is.na(n_divergent) || is.na(n_treedepth_hit)) {
    return("FAIL")
  }
  if (n_divergent > 0 || max_rhat > 1.05 || min_ess_bulk < 100 ||
      min_ess_tail < 100 || n_treedepth_hit > 0) {
    return("FAIL")
  }
  if (max_rhat > 1.01 || min_ess_bulk < 400 || min_ess_tail < 400) {
    return("REVIEW")
  }
  "PASS"
}

diagnostic_action_from_status <- function(status) {
  dplyr::case_when(
    status == "PASS" ~ "parameter_recovery_auxiliary_evidence_allowed",
    status == "REVIEW" ~ "use_only_with_diagnostic_caution",
    status == "FAIL" ~ "do_not_use_as_auxiliary_evidence_without_refit_or_sensitivity",
    TRUE ~ "inspect_diagnostics"
  )
}

extract_recovery <- function(fit, truth, T, sigma_firm, rep_id, n_firms, n_obs,
                             chains, iter, warmup, sd_zero_eps) {
  draws <- posterior::as_draws_df(fit)
  rows <- lapply(seq_len(nrow(truth)), function(i) {
    v <- truth$draw_var[i]
    boundary <- isTRUE(truth$boundary_parameter[i])
    if (!v %in% names(draws)) {
      return(data.frame(
        parameter = truth$parameter[i], draw_var = v, true_value = truth$true_value[i],
        posterior_mean = NA_real_, posterior_sd = NA_real_, q025 = NA_real_, q500 = NA_real_, q975 = NA_real_,
        bias = NA_real_, abs_bias = NA_real_, squared_error = NA_real_, coverage_95 = NA,
        posterior_practical_zero_rate = NA_real_, boundary_coverage_note = ifelse(boundary, "boundary_coverage_not_applicable", NA_character_),
        missing_draw = TRUE, boundary_parameter = boundary
      ))
    }
    x <- as.numeric(draws[[v]])
    qs <- stats::quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)
    est <- mean(x, na.rm = TRUE)
    tv <- truth$true_value[i]
    coverage <- if (boundary) NA else (tv >= unname(qs[1]) && tv <= unname(qs[3]))
    practical_zero_rate <- if (boundary) mean(x <= sd_zero_eps, na.rm = TRUE) else NA_real_
    data.frame(
      parameter = truth$parameter[i], draw_var = v, true_value = tv,
      posterior_mean = est, posterior_sd = stats::sd(x, na.rm = TRUE),
      q025 = unname(qs[1]), q500 = unname(qs[2]), q975 = unname(qs[3]),
      bias = est - tv, abs_bias = abs(est - tv), squared_error = (est - tv)^2,
      coverage_95 = coverage,
      posterior_practical_zero_rate = practical_zero_rate,
      boundary_coverage_note = ifelse(boundary, "coverage_95_not_interpretable_for_zero_boundary_sd", NA_character_),
      missing_draw = FALSE, boundary_parameter = boundary
    )
  })
  out <- dplyr::bind_rows(rows)
  out$T <- T
  out$sigma_firm <- sigma_firm
  out$rep_id <- rep_id
  out$n_firms <- n_firms
  out$n_obs <- n_obs
  out$chains <- chains
  out$iter <- iter
  out$warmup <- warmup
  out$error <- NA_character_
  out[, c(
    "T", "sigma_firm", "rep_id", "n_firms", "n_obs", "chains", "iter", "warmup",
    "parameter", "draw_var", "true_value", "posterior_mean", "posterior_sd", "q025", "q500", "q975",
    "bias", "abs_bias", "squared_error", "coverage_95", "posterior_practical_zero_rate",
    "boundary_coverage_note", "missing_draw", "boundary_parameter", "error"
  )]
}

extract_diagnostics <- function(fit, T, sigma_firm, rep_id, n_firms, n_obs,
                                chains, iter, warmup, max_treedepth) {
  draws <- posterior::as_draws_df(fit)
  s <- posterior::summarise_draws(draws)
  np <- brms::nuts_params(fit)
  treedepths <- np$Value[np$Parameter == "treedepth__"]
  n_transitions <- length(treedepths)
  max_treedepth_observed <- if (n_transitions > 0) suppressWarnings(max(treedepths, na.rm = TRUE)) else NA_real_
  n_treedepth_hit <- if (n_transitions > 0) sum(treedepths >= max_treedepth, na.rm = TRUE) else NA_integer_
  share_treedepth_hit <- if (n_transitions > 0) n_treedepth_hit / n_transitions else NA_real_
  max_rhat <- suppressWarnings(max(s$rhat, na.rm = TRUE))
  min_ess_bulk <- suppressWarnings(min(s$ess_bulk, na.rm = TRUE))
  min_ess_tail <- suppressWarnings(min(s$ess_tail, na.rm = TRUE))
  n_divergent <- sum(np$Parameter == "divergent__" & np$Value == 1, na.rm = TRUE)
  status <- diagnostic_status_from_gates(max_rhat, min_ess_bulk, min_ess_tail, n_divergent, n_treedepth_hit)
  data.frame(
    T = T, sigma_firm = sigma_firm, rep_id = rep_id, n_firms = n_firms, n_obs = n_obs,
    chains = chains, iter = iter, warmup = warmup,
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    n_divergent = n_divergent,
    max_treedepth_config = max_treedepth,
    max_treedepth_observed = max_treedepth_observed,
    n_transitions = n_transitions,
    max_treedepth_hit = n_treedepth_hit,
    share_treedepth_hit = share_treedepth_hit,
    diagnostic_status = status,
    diagnostic_action = diagnostic_action_from_status(status),
    error = NA_character_, stringsAsFactors = FALSE
  )
}

run_one_recovery_replication <- function(T, sigma_firm, rep_id, n_firms, n_industries,
                                         sigma_eps, nu, chains, iter, warmup, cores,
                                         adapt_delta, max_treedepth, sd_zero_eps) {
  replication_offset <- 700000L + as.integer(rep_id) + as.integer(T) * 1000L + as.integer(round(sigma_firm * 10000))
  df <- simulate_accrual_panel_student_truth(
    n_firms, T, sigma_firm, sigma_eps, nu, n_industries,
    rng_context = "sim_brms_recovery_panel", rng_offset = replication_offset
  )
  fit <- fit_recovery_model(
    df, chains, iter, warmup, cores,
    "sim_brms_recovery_fit", replication_offset + 31L, adapt_delta, max_treedepth
  )
  truth <- attr(df, "truth")
  list(
    recovery = extract_recovery(fit, truth, T, sigma_firm, rep_id, n_firms, nrow(df), chains, iter, warmup, sd_zero_eps),
    diagnostics = extract_diagnostics(fit, T, sigma_firm, rep_id, n_firms, nrow(df), chains, iter, warmup, max_treedepth)
  )
}

summarise_recovery <- function(x) {
  ok <- x[(is.na(x$error) | x$error == "") & !isTRUE(x$missing_draw), , drop = FALSE]
  if (!nrow(ok)) stop("[BLOCKER] No successful parameter-recovery rows.")
  dplyr::as_tibble(ok) |>
    dplyr::group_by(.data$T, .data$sigma_firm, .data$parameter) |>
    dplyr::summarise(
      n_rep = dplyr::n(),
      true_value = mean(.data$true_value, na.rm = TRUE),
      mean_posterior_mean = mean(.data$posterior_mean, na.rm = TRUE),
      mean_bias = mean(.data$bias, na.rm = TRUE),
      mean_abs_bias = mean(.data$abs_bias, na.rm = TRUE),
      rmse = sqrt(mean(.data$squared_error, na.rm = TRUE)),
      coverage_95 = if (all(is.na(.data$coverage_95))) NA_real_ else mean(.data$coverage_95, na.rm = TRUE),
      mean_posterior_sd = mean(.data$posterior_sd, na.rm = TRUE),
      mean_posterior_practical_zero_rate = if (all(is.na(.data$posterior_practical_zero_rate))) NA_real_ else mean(.data$posterior_practical_zero_rate, na.rm = TRUE),
      boundary_parameter = any(.data$boundary_parameter, na.rm = TRUE),
      coverage_interpretation = ifelse(any(.data$boundary_parameter, na.rm = TRUE),
                                       "coverage_95_not_interpretable_for_zero_boundary_sd",
                                       "coverage_95_standard"),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$parameter, .data$sigma_firm, .data$T)
}

summarise_diagnostics <- function(x) {
  ok <- x[is.na(x$error) | x$error == "", , drop = FALSE]
  if (!nrow(ok)) return(data.frame())
  dplyr::as_tibble(ok) |>
    dplyr::group_by(.data$T, .data$sigma_firm) |>
    dplyr::summarise(
      n_fit = dplyr::n(),
      n_pass = sum(.data$diagnostic_status == "PASS", na.rm = TRUE),
      n_review = sum(.data$diagnostic_status == "REVIEW", na.rm = TRUE),
      n_fail = sum(.data$diagnostic_status == "FAIL", na.rm = TRUE),
      max_rhat_max = max(.data$max_rhat, na.rm = TRUE),
      min_ess_bulk_min = min(.data$min_ess_bulk, na.rm = TRUE),
      min_ess_tail_min = min(.data$min_ess_tail, na.rm = TRUE),
      total_divergent = sum(.data$n_divergent, na.rm = TRUE),
      total_treedepth_hits = sum(.data$max_treedepth_hit, na.rm = TRUE),
      max_treedepth_observed = max(.data$max_treedepth_observed, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$sigma_firm, .data$T)
}

plot_recovery_summary <- function(summary_df, figures_dir) {
  p1 <- ggplot(summary_df, aes(x = factor(T), y = rmse, group = factor(sigma_firm))) +
    geom_line() + geom_point() +
    facet_wrap(~ parameter, scales = "free_y") +
    labs(x = "Panel length T", y = "RMSE", group = "sigma_firm", title = "Bayesian parameter recovery: RMSE") +
    theme_minimal(base_size = 11)
  ggsave(file.path(figures_dir, "figure_brms_parameter_recovery_rmse.png"), p1, width = 10, height = 7, dpi = 300)

  p2 <- ggplot(summary_df, aes(x = factor(T), y = coverage_95, group = factor(sigma_firm))) +
    geom_hline(yintercept = 0.95, linetype = "dashed") + geom_line() + geom_point() +
    facet_wrap(~ parameter) +
    labs(x = "Panel length T", y = "95% interval coverage", group = "sigma_firm", title = "Bayesian parameter recovery: coverage") +
    theme_minimal(base_size = 11)
  ggsave(file.path(figures_dir, "figure_brms_parameter_recovery_coverage.png"), p2, width = 10, height = 7, dpi = 300)
}

write_reviewer_note <- function(summary_df, diag_summary, logs_dir) {
  note <- c(
    "# BRMS parameter-recovery reviewer note",
    "",
    paste("- Script:", script_name),
    paste("- Version:", script_version),
    paste("- Role:", script_role),
    "",
    "This script is an auxiliary parameter-recovery diagnostic. It does not define the central leakage or validation-unit estimand. The central simulation evidence remains the leakage premium / validation-target premium reported by the simulation scripts designed for that purpose.",
    "",
    "## Diagnostic gates",
    "",
    "A replication is PASS only when max Rhat <= 1.01, min bulk ESS >= 400, min tail ESS >= 400, no divergences, and no transition reaches the configured max_treedepth. REVIEW allows weaker but non-failing diagnostics. FAIL rows should not be used as auxiliary evidence without refit or sensitivity.",
    "",
    "## Boundary parameters",
    "",
    "When sigma_firm = 0, sd_company is a boundary parameter. Standard 95% interval coverage is not interpreted for that parameter. The script instead reports posterior_practical_zero_rate using the configured practical-zero threshold.",
    "",
    "## Outputs",
    "",
    "- table_brms_parameter_recovery_rep_results.csv",
    "- table_brms_parameter_recovery_diagnostics.csv",
    "- table_brms_parameter_recovery_summary.csv",
    "- table_brms_parameter_recovery_diagnostic_summary.csv"
  )
  if (nrow(diag_summary) > 0) {
    note <- c(note, "", "## Diagnostic summary", "", paste(capture.output(print(diag_summary)), collapse = "\n"))
  }
  writeLines(note, file.path(logs_dir, "brms_parameter_recovery_reviewer_note.md"))
}

start_time <- Sys.time()
root <- ensure_recovery_dirs()
tables_dir <- file.path(root, "tables")
figures_dir <- file.path(root, "figures")
logs_dir <- file.path(root, "logs")

sim_cfg <- accrual_simulation_runtime_config("brms_recovery")
t_grid <- sim_cfg$t_grid
sigma_grid <- sim_cfg$sigma_grid
R <- sim_cfg$R
n_firms <- sim_cfg$n_firms
n_industries <- sim_cfg$n_industries
sigma_eps <- sim_cfg$sigma_eps
nu <- sim_cfg$nu
chains <- sim_cfg$chains
iter <- sim_cfg$iter
warmup <- sim_cfg$warmup
cores <- sim_cfg$cores
options(mc.cores = cores)
adapt_delta <- sim_cfg$adapt_delta
max_treedepth <- sim_cfg$max_treedepth
sd_zero_eps <- sim_cfg$sd_zero_eps

grid <- expand.grid(T = as.integer(t_grid), sigma_firm = sigma_grid, rep_id = seq_len(R), KEEP.OUT.ATTRS = FALSE)
rep_path <- file.path(tables_dir, "table_brms_parameter_recovery_rep_results.csv")
diag_path <- file.path(tables_dir, "table_brms_parameter_recovery_diagnostics.csv")
sum_path <- file.path(tables_dir, "table_brms_parameter_recovery_summary.csv")
diag_sum_path <- file.path(tables_dir, "table_brms_parameter_recovery_diagnostic_summary.csv")
manifest_path <- file.path(logs_dir, "brms_parameter_recovery_manifest.csv")

message("Bayesian parameter recovery: ", nrow(grid), " MCMC fits. This can be slow. Role=auxiliary diagnostic.")
message(
  "brms/rstan sampler controls: chains=", chains,
  ", cores=", cores,
  ", iter=", iter,
  ", warmup=", warmup,
  ", adapt_delta=", adapt_delta,
  ", max_treedepth=", max_treedepth
)
rec_out <- list()
diag_out <- list()
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf("[%d/%d] T=%d sigma=%.2f rep=%d", i, nrow(grid), g$T, g$sigma_firm, g$rep_id))
  ans <- tryCatch(
    run_one_recovery_replication(g$T, g$sigma_firm, g$rep_id, n_firms, n_industries, sigma_eps, nu,
                                 chains, iter, warmup, cores, adapt_delta, max_treedepth, sd_zero_eps),
    error = function(e) {
      err <- conditionMessage(e)
      list(
        recovery = data.frame(
          T = g$T, sigma_firm = g$sigma_firm, rep_id = g$rep_id,
          n_firms = n_firms, n_obs = n_firms * g$T, chains = chains, iter = iter, warmup = warmup,
          parameter = NA_character_, draw_var = NA_character_, true_value = NA_real_,
          posterior_mean = NA_real_, posterior_sd = NA_real_, q025 = NA_real_, q500 = NA_real_, q975 = NA_real_,
          bias = NA_real_, abs_bias = NA_real_, squared_error = NA_real_, coverage_95 = NA,
          posterior_practical_zero_rate = NA_real_, boundary_coverage_note = NA_character_,
          missing_draw = NA, boundary_parameter = NA, error = err
        ),
        diagnostics = data.frame(
          T = g$T, sigma_firm = g$sigma_firm, rep_id = g$rep_id,
          n_firms = n_firms, n_obs = n_firms * g$T, chains = chains, iter = iter, warmup = warmup,
          max_rhat = NA_real_, min_ess_bulk = NA_real_, min_ess_tail = NA_real_,
          n_divergent = NA_real_, max_treedepth_config = max_treedepth,
          max_treedepth_observed = NA_real_, n_transitions = NA_integer_,
          max_treedepth_hit = NA_real_, share_treedepth_hit = NA_real_,
          diagnostic_status = "FAIL",
          diagnostic_action = "fit_failed_or_diagnostics_unavailable",
          error = err,
          stringsAsFactors = FALSE
        )
      )
    }
  )
  rec_out[[i]] <- ans$recovery
  diag_out[[i]] <- ans$diagnostics
  write.csv(dplyr::bind_rows(rec_out), rep_path, row.names = FALSE)
  write.csv(dplyr::bind_rows(diag_out), diag_path, row.names = FALSE)
}

rec_results <- dplyr::bind_rows(rec_out)
diag_results <- dplyr::bind_rows(diag_out)
write.csv(rec_results, rep_path, row.names = FALSE)
write.csv(diag_results, diag_path, row.names = FALSE)
summary_df <- summarise_recovery(rec_results)
diag_summary <- summarise_diagnostics(diag_results)
write.csv(summary_df, sum_path, row.names = FALSE)
write.csv(diag_summary, diag_sum_path, row.names = FALSE)
plot_recovery_summary(summary_df, figures_dir)
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))
write_reviewer_note(summary_df, diag_summary, logs_dir)

manifest <- data.frame(
  script = script_name,
  script_version = script_version,
  script_role = script_role,
  start_time = as.character(start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(t_grid, collapse = ","),
  sigma_firm_grid = paste(sigma_grid, collapse = ","),
  replications = R,
  n_firms = n_firms,
  n_industries = n_industries,
  sigma_eps = sigma_eps,
  nu = nu,
  chains = chains,
  cores = cores,
  iter = iter,
  warmup = warmup,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  sd_zero_eps = sd_zero_eps,
  output_root = root,
  successful_fits = sum(is.na(diag_results$error) | diag_results$error == ""),
  failed_fits = sum(!(is.na(diag_results$error) | diag_results$error == "")),
  diagnostic_pass = sum(diag_results$diagnostic_status == "PASS", na.rm = TRUE),
  diagnostic_review = sum(diag_results$diagnostic_status == "REVIEW", na.rm = TRUE),
  diagnostic_fail = sum(diag_results$diagnostic_status == "FAIL", na.rm = TRUE),
  primary_estimand_role = "none_auxiliary_only",
  central_simulation_estimand = "leakage_premium_or_validation_target_premium_from_companion_simulation_scripts",
  stringsAsFactors = FALSE
)
write.csv(manifest, manifest_path, row.names = FALSE)

cat("\n[SUCCESS] Bayesian parameter recovery completed.\n")
cat("Role: auxiliary diagnostic only.\n")
cat("Replication results:", rep_path, "\n")
cat("Diagnostics:", diag_path, "\n")
cat("Summary:", sum_path, "\n")
cat("Diagnostic summary:", diag_sum_path, "\n")
phase_end("si04", "Simulation: BRMS parameter recovery")
