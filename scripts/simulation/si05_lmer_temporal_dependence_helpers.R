# -----------------------------------------------------------------------------
# Script: scripts/simulation/si05_lmer_temporal_dependence_helpers.R
# Purpose: Shared helpers for SI05 split-worker LMER temporal-dependence simulation.
#
# This file is intentionally non-executing: source it from si05a/si05b/si05c
# after sourcing scripts/ma00_setup.R and scripts/simulation/si00_helpers.R.
# -----------------------------------------------------------------------------

si05_temporal_root <- function(root = output_root) {
  file.path(root, "simulation", "lmer_temporal_dependence")
}

si05_temporal_dirs <- function(root = output_root) {
  base <- si05_temporal_root(root)
  list(
    root = base,
    tables = file.path(base, "tables"),
    logs = file.path(base, "logs"),
    task_artifacts = file.path(base, "task_artifacts"),
    task_results = file.path(base, "task_artifacts", "results"),
    task_status = file.path(base, "task_artifacts", "status"),
    task_logs = file.path(base, "task_artifacts", "logs")
  )
}

si05_ensure_temporal_dirs <- function(root = output_root) {
  dirs <- si05_temporal_dirs(root)
  for (d in unlist(dirs, use.names = FALSE)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  dirs
}

si05_slug_num <- function(x) {
  x <- as.character(x)
  x <- gsub("-", "m", x, fixed = TRUE)
  x <- gsub("\\+", "p", x)
  x <- gsub("\\.", "p", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x
}

si05_task_key <- function(T, sigma_firm, rho, shock_duration) {
  paste0(
    "SI05_T", as.integer(T),
    "_sf", si05_slug_num(sprintf("%.4f", as.numeric(sigma_firm))),
    "_rho", si05_slug_num(sprintf("%.4f", as.numeric(rho))),
    "_dur", as.integer(shock_duration)
  )
}

si05_runtime_config <- function() {
  cfg <- accrual_simulation_runtime_config("lmer_temporal")
  cfg$t_grid <- as.integer(cfg$t_grid)
  cfg$sigma_grid <- as.numeric(cfg$sigma_grid)
  cfg$rho_grid <- as.numeric(cfg$rho_grid)
  cfg$shock_duration_grid <- as.integer(cfg$shock_duration_grid)
  cfg$R <- as.integer(cfg$R)
  cfg$K <- as.integer(cfg$K)
  cfg$n_firms <- as.integer(cfg$n_firms)
  cfg$n_industries <- as.integer(cfg$n_industries)
  cfg$sigma_eps <- as.numeric(cfg$sigma_eps)
  cfg$shock_size <- as.numeric(cfg$shock_size)
  cfg
}

si05_safe_seed <- function(context, offset) {
  set_accrual_seed(context, offset = as.integer(offset))
}

si05_simulate_accrual_panel_ar1 <- function(cfg, T, sigma_firm, rho, rng_offset = 0L) {
  si05_safe_seed("sim_lmer_temporal_ar1", rng_offset)
  n_firms <- cfg$n_firms
  n_industries <- cfg$n_industries
  sigma_eps <- cfg$sigma_eps

  firms <- sprintf("F%04d", seq_len(n_firms))
  years <- seq_len(T)
  inds <- sprintf("IND%02d", seq_len(n_industries))

  ind_map <- sample(rep(inds, length.out = n_firms), n_firms)
  names(ind_map) <- firms

  df <- expand.grid(
    company = firms,
    year_num = years,
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

si05_simulate_accrual_panel_shock_episode <- function(cfg, T, sigma_firm, rho,
                                                      shock_duration, rng_offset = 0L) {
  df <- si05_simulate_accrual_panel_ar1(
    cfg = cfg,
    T = T,
    sigma_firm = sigma_firm,
    rho = rho,
    rng_offset = rng_offset
  )

  si05_safe_seed("sim_lmer_temporal_shock", rng_offset + 17L)
  shock_firms <- sample(levels(df$company), max(1L, floor(0.10 * cfg$n_firms)))
  for (f in shock_firms) {
    possible_start <- seq_len(max(1L, T - shock_duration + 1L))
    start <- sample(possible_start, 1)
    yrs <- start:min(T, start + shock_duration - 1L)
    idx <- as.character(df$company) == f & df$year_num %in% yrs
    df$known_abnormal_shock[idx] <- cfg$shock_size
  }
  df$TA_scaled <- df$TA_scaled + df$known_abnormal_shock
  df
}

si05_replication_offset <- function(T, sigma_firm, rho, shock_duration, rep_id) {
  # Mirrors the monolithic SI05 offset formula, with all design-cell dimensions.
  500000L + as.integer(rep_id) + as.integer(T) * 1000L +
    as.integer(round(as.numeric(sigma_firm) * 10000)) +
    as.integer(round(as.numeric(rho) * 1000)) +
    as.integer(shock_duration) * 100L
}

si05_run_temporal_replication <- function(cfg, T, sigma_firm, rho, shock_duration, rep_id) {
  offset <- si05_replication_offset(T, sigma_firm, rho, shock_duration, rep_id)
  df <- si05_simulate_accrual_panel_shock_episode(
    cfg = cfg,
    T = T,
    sigma_firm = sigma_firm,
    rho = rho,
    shock_duration = shock_duration,
    rng_offset = offset
  )

  row <- score_cv(
    df, "row", cfg$K,
    rng_context = "sim_lmer_temporal_cv",
    rng_offset = offset + 11L
  )
  grp <- score_cv(
    df, "group", cfg$K,
    rng_context = "sim_lmer_temporal_cv",
    rng_offset = offset + 29L
  )

  drow <- row$elpd_firmre - row$elpd_pooled
  dgrp <- grp$elpd_firmre - grp$elpd_pooled

  data.frame(
    T = T,
    sigma_firm = sigma_firm,
    rho = rho,
    shock_duration = shock_duration,
    shock_size = cfg$shock_size,
    sigma_eps = cfg$sigma_eps,
    rep_id = rep_id,
    n_firms = cfg$n_firms,
    n_obs = nrow(df),
    K = cfg$K,
    elpd_row_pooled = row$elpd_pooled,
    elpd_row_firmre = row$elpd_firmre,
    elpd_group_pooled = grp$elpd_pooled,
    elpd_group_firmre = grp$elpd_firmre,
    delta_row = drow,
    delta_group = dgrp,
    elpd_premium = drow - dgrp,
    weight_row_pooled = row$weight_pooled,
    weight_row_firmre = row$weight_firmre,
    weight_group_pooled = grp$weight_pooled,
    weight_group_firmre = grp$weight_firmre,
    weight_premium = row$weight_firmre - grp$weight_firmre,
    singular_row_firmre_folds = row$singular_folds,
    singular_group_firmre_folds = grp$singular_folds,
    known_abnormal_shock_rate = mean(df$known_abnormal_shock != 0),
    false_normalization_indicator = as.integer(
      row$weight_firmre > grp$weight_firmre && mean(df$known_abnormal_shock != 0) > 0
    ),
    error = NA_character_,
    stringsAsFactors = FALSE
  )
}

si05_error_replication_row <- function(cfg, T, sigma_firm, rho, shock_duration, rep_id, e) {
  data.frame(
    T = T,
    sigma_firm = sigma_firm,
    rho = rho,
    shock_duration = shock_duration,
    shock_size = cfg$shock_size,
    sigma_eps = cfg$sigma_eps,
    rep_id = rep_id,
    n_firms = cfg$n_firms,
    n_obs = cfg$n_firms * T,
    K = cfg$K,
    elpd_row_pooled = NA_real_,
    elpd_row_firmre = NA_real_,
    elpd_group_pooled = NA_real_,
    elpd_group_firmre = NA_real_,
    delta_row = NA_real_,
    delta_group = NA_real_,
    elpd_premium = NA_real_,
    weight_row_pooled = NA_real_,
    weight_row_firmre = NA_real_,
    weight_group_pooled = NA_real_,
    weight_group_firmre = NA_real_,
    weight_premium = NA_real_,
    singular_row_firmre_folds = NA_real_,
    singular_group_firmre_folds = NA_real_,
    known_abnormal_shock_rate = NA_real_,
    false_normalization_indicator = NA_integer_,
    error = conditionMessage(e),
    stringsAsFactors = FALSE
  )
}

si05_summarise_temporal_results <- function(results) {
  ok <- results[is.na(results$error) | results$error == "", , drop = FALSE]
  if (!nrow(ok)) stop("[BLOCKER] No successful temporal-dependence simulation replications.")

  ok %>%
    dplyr::group_by(.data$T, .data$sigma_firm, .data$rho, .data$shock_duration) %>%
    dplyr::summarise(
      n_rep = dplyr::n(),
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
}
