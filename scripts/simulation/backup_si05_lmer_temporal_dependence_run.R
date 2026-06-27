# -----------------------------------------------------------------------------
# Script: si05_lmer_temporal_dependence_run.R
# Purpose: Run LMER row-CV versus grouped-CV simulation with AR(1) and persistent
#          shock accrual mechanisms.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si05", "Simulation: LMER temporal dependence run")
source("scripts/simulation/si00_helpers.R")

check_sim_packages(c("lme4", "dplyr", "ggplot2"))
suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
})

start_time <- Sys.time()
root <- file.path(output_root, "simulation", "lmer_temporal_dependence")
tables_dir <- file.path(root, "tables")
logs_dir <- file.path(root, "logs")
for (d in c(root, tables_dir, logs_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

sim_cfg <- accrual_simulation_runtime_config("lmer_temporal")
t_grid <- sim_cfg$t_grid
sigma_grid <- sim_cfg$sigma_grid
rho_grid <- sim_cfg$rho_grid
shock_duration_grid <- sim_cfg$shock_duration_grid
R <- sim_cfg$R
K <- sim_cfg$K
n_firms <- sim_cfg$n_firms
n_industries <- sim_cfg$n_industries
sigma_eps <- sim_cfg$sigma_eps
shock_size <- sim_cfg$shock_size

safe_seed <- function(context, offset) {
  set_accrual_seed(context, offset = offset)
}

simulate_accrual_panel_ar1 <- function(n_firms = 200, T = 7, sigma_firm = 0.10,
                                       sigma_eps = 0.08, rho = 0.30,
                                       n_industries = 10, rng_offset = 0L) {
  safe_seed("sim_lmer_temporal_ar1", rng_offset)
  firms <- sprintf("F%04d", seq_len(n_firms))
  years <- seq_len(T)
  inds <- sprintf("IND%02d", seq_len(n_industries))
  ind_map <- sample(rep(inds, length.out = n_firms), n_firms)
  names(ind_map) <- firms
  df <- expand.grid(company = firms, year_num = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$industry <- ind_map[df$company]
  a_i <- stats::rnorm(n_firms, 0, sigma_firm); names(a_i) <- firms
  y_t <- stats::rnorm(T, 0, 0.015); names(y_t) <- as.character(years)
  g_j <- stats::rnorm(n_industries, 0, 0.020); names(g_j) <- inds
  df$inv_A_lag <- stats::rnorm(nrow(df), 0.020, 0.010)
  df$dREV_dREC_scaled <- stats::rnorm(nrow(df), 0.050, 0.100)
  df$PPE_scaled <- stats::rnorm(nrow(df), 0.450, 0.200)
  df$ROA_lag <- stats::rnorm(nrow(df), 0.040, 0.080)
  ar <- numeric(nrow(df))
  for (f in firms) {
    idx <- which(df$company == f)
    idx <- idx[order(df$year_num[idx])]
    eps <- stats::rnorm(length(idx), 0, sigma_eps)
    previous <- eps[1] / sqrt(max(1 - rho^2, .Machine$double.eps))
    ar[idx[1]] <- previous
    if (length(idx) > 1) {
      for (pos in seq.int(2L, length(idx))) {
        previous <- rho * previous + eps[pos]
        ar[idx[pos]] <- previous
      }
    }
  }
  mu <- 0.030 * df$inv_A_lag + 0.060 * df$dREV_dREC_scaled -
    0.040 * df$PPE_scaled + 0.080 * df$ROA_lag +
    a_i[df$company] + y_t[as.character(df$year_num)] + g_j[df$industry]
  df$known_abnormal_shock <- 0
  df$TA_scaled <- mu + ar
  df$company <- factor(df$company, levels = firms)
  df$industry <- factor(df$industry, levels = inds)
  df$year <- factor(df$year_num, levels = as.character(years))
  df
}

simulate_accrual_panel_shock_episode <- function(n_firms = 200, T = 7, sigma_firm = 0.10,
                                                 sigma_eps = 0.08, rho = 0.30,
                                                 shock_duration = 2, shock_size = 0.20,
                                                 n_industries = 10, rng_offset = 0L) {
  df <- simulate_accrual_panel_ar1(n_firms, T, sigma_firm, sigma_eps, rho, n_industries, rng_offset)
  safe_seed("sim_lmer_temporal_shock", rng_offset + 17L)
  shock_firms <- sample(levels(df$company), max(1L, floor(0.10 * n_firms)))
  for (f in shock_firms) {
    possible_start <- seq_len(max(1L, T - shock_duration + 1L))
    start <- sample(possible_start, 1)
    yrs <- start:min(T, start + shock_duration - 1L)
    idx <- as.character(df$company) == f & df$year_num %in% yrs
    df$known_abnormal_shock[idx] <- shock_size
  }
  df$TA_scaled <- df$TA_scaled + df$known_abnormal_shock
  df
}

run_temporal_replication <- function(T, sigma_firm, rho, shock_duration, rep_id) {
  offset <- 500000L + as.integer(rep_id) + as.integer(T) * 1000L +
    as.integer(round(sigma_firm * 10000)) + as.integer(round(rho * 1000)) +
    as.integer(shock_duration) * 100L
  df <- simulate_accrual_panel_shock_episode(
    n_firms = n_firms, T = T, sigma_firm = sigma_firm, sigma_eps = sigma_eps,
    rho = rho, shock_duration = shock_duration, shock_size = shock_size,
    n_industries = n_industries, rng_offset = offset
  )
  row <- score_cv(df, "row", K, rng_context = "sim_lmer_temporal_cv", rng_offset = offset + 11L)
  grp <- score_cv(df, "group", K, rng_context = "sim_lmer_temporal_cv", rng_offset = offset + 29L)
  drow <- row$elpd_firmre - row$elpd_pooled
  dgrp <- grp$elpd_firmre - grp$elpd_pooled
  data.frame(
    T = T, sigma_firm = sigma_firm, rho = rho, shock_duration = shock_duration,
    shock_size = shock_size, sigma_eps = sigma_eps, rep_id = rep_id,
    n_firms = n_firms, n_obs = nrow(df), K = K,
    elpd_row_pooled = row$elpd_pooled, elpd_row_firmre = row$elpd_firmre,
    elpd_group_pooled = grp$elpd_pooled, elpd_group_firmre = grp$elpd_firmre,
    delta_row = drow, delta_group = dgrp, elpd_premium = drow - dgrp,
    weight_row_pooled = row$weight_pooled, weight_row_firmre = row$weight_firmre,
    weight_group_pooled = grp$weight_pooled, weight_group_firmre = grp$weight_firmre,
    weight_premium = row$weight_firmre - grp$weight_firmre,
    singular_row_firmre_folds = row$singular_folds,
    singular_group_firmre_folds = grp$singular_folds,
    known_abnormal_shock_rate = mean(df$known_abnormal_shock != 0),
    false_normalization_indicator = as.integer(row$weight_firmre > grp$weight_firmre & mean(df$known_abnormal_shock != 0) > 0),
    error = NA_character_,
    stringsAsFactors = FALSE
  )
}

grid <- expand.grid(
  T = as.integer(t_grid),
  sigma_firm = sigma_grid,
  rho = rho_grid,
  shock_duration = as.integer(shock_duration_grid),
  rep_id = seq_len(R),
  KEEP.OUT.ATTRS = FALSE
)

rep_path <- file.path(tables_dir, "table_lmer_temporal_dependence_rep_results.csv")
sum_path <- file.path(tables_dir, "table_lmer_temporal_dependence_grid_summary.csv")
manifest_path <- file.path(logs_dir, "temporal_dependence_run_manifest.csv")

message("Temporal-dependence LMER simulation: ", nrow(grid), " replications.")
out <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf("[%d/%d] T=%d sigma=%.2f rho=%.2f duration=%d rep=%d",
                  i, nrow(grid), g$T, g$sigma_firm, g$rho, g$shock_duration, g$rep_id))
  out[[i]] <- tryCatch(
    run_temporal_replication(g$T, g$sigma_firm, g$rho, g$shock_duration, g$rep_id),
    error = function(e) data.frame(
      T = g$T, sigma_firm = g$sigma_firm, rho = g$rho, shock_duration = g$shock_duration,
      shock_size = shock_size, sigma_eps = sigma_eps, rep_id = g$rep_id,
      n_firms = n_firms, n_obs = n_firms * g$T, K = K,
      elpd_row_pooled = NA_real_, elpd_row_firmre = NA_real_,
      elpd_group_pooled = NA_real_, elpd_group_firmre = NA_real_,
      delta_row = NA_real_, delta_group = NA_real_, elpd_premium = NA_real_,
      weight_row_pooled = NA_real_, weight_row_firmre = NA_real_,
      weight_group_pooled = NA_real_, weight_group_firmre = NA_real_,
      weight_premium = NA_real_, singular_row_firmre_folds = NA_real_,
      singular_group_firmre_folds = NA_real_, known_abnormal_shock_rate = NA_real_,
      false_normalization_indicator = NA_integer_, error = conditionMessage(e)
    )
  )
  if (i %% 10 == 0 || i == nrow(grid)) write_csv_safely(dplyr::bind_rows(out[seq_len(i)]), rep_path, row.names = FALSE)
}

results <- dplyr::bind_rows(out)
write_csv_safely(results, rep_path, row.names = FALSE)

ok <- results[is.na(results$error) | results$error == "", , drop = FALSE]
if (!nrow(ok)) stop("[BLOCKER] No successful temporal-dependence simulation replications.")
summary_df <- ok %>%
  group_by(.data$T, .data$sigma_firm, .data$rho, .data$shock_duration) %>%
  summarise(
    n_rep = n(),
    mean_weight_row_firmre = mean(.data$weight_row_firmre, na.rm = TRUE),
    mean_weight_group_firmre = mean(.data$weight_group_firmre, na.rm = TRUE),
    mean_weight_premium = mean(.data$weight_premium, na.rm = TRUE),
    median_weight_premium = stats::median(.data$weight_premium, na.rm = TRUE),
    mean_elpd_premium = mean(.data$elpd_premium, na.rm = TRUE),
    prob_positive_weight_premium = mean(.data$weight_premium > 0, na.rm = TRUE),
    mean_singular_row_folds = mean(.data$singular_row_firmre_folds, na.rm = TRUE),
    mean_singular_group_folds = mean(.data$singular_group_firmre_folds, na.rm = TRUE),
    false_normalization_rate = mean(.data$false_normalization_indicator, na.rm = TRUE),
    .groups = "drop"
  )
write_csv_safely(summary_df, sum_path, row.names = FALSE)

manifest <- data.frame(
  script = "scripts/simulation/si05_lmer_temporal_dependence_run.R",
  script_version = "2026-06-22-v1-temporal-dependence-lmer",
  start_time = as.character(start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(t_grid, collapse = ","),
  sigma_firm_grid = paste(sigma_grid, collapse = ","),
  rho_grid = paste(rho_grid, collapse = ","),
  shock_duration_grid = paste(shock_duration_grid, collapse = ","),
  replications = R,
  K = K,
  n_firms = n_firms,
  n_industries = n_industries,
  sigma_eps = sigma_eps,
  shock_size = shock_size,
  successful_replications = nrow(ok),
  failed_replications = sum(!(is.na(results$error) | results$error == "")),
  output_root = root,
  stringsAsFactors = FALSE
)
write_csv_safely(manifest, manifest_path, row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))

cat("[SUCCESS] Temporal-dependence simulation completed.\n")
cat("Results: ", rep_path, "\n", sep = "")
cat("Summary: ", sum_path, "\n", sep = "")
phase_end("si05", "Simulation: LMER temporal dependence run")
