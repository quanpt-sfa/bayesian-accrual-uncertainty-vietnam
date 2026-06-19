# -----------------------------------------------------------------------------
# Script: 18_sensitivity_construct_DA.R
# Purpose: Recompute uncertainty-adjusted DA separately for each sensitivity scenario.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/00_helpers.R")
ensure_analysis_dirs()
ensure_sensitivity_dirs()
validate_final_analysis_config("sensitivity DA construction", final_mode = TRUE)

dry_run <- env_flag("ACCRUAL_DRY_RUN", "TRUE")
S <- as.integer(env_value("ACCRUAL_STACKING_MIXTURE_DRAWS", as.character(stacking_mixture_draws)))
if (is.na(S) || S <= 0) S <- stacking_mixture_draws

scenarios <- selected_sensitivity_scenarios()
weights_path <- file.path(sensitivity_root(), "tables", "sensitivity_stacking_weights_by_scenario.csv")
ep_sample_path <- file.path(input_winsor_root, "tables", "final_common_ex_post_sample_winsor.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
if (!file.exists(weights_path)) stop("[BLOCKER] Missing sensitivity stacking weights. Run script 17 first.")
if (!file.exists(ep_sample_path) || !file.exists(rt_sample_path)) stop("[BLOCKER] Missing winsor samples.")

weights_df <- read.csv(weights_path, stringsAsFactors = FALSE)
df_ep <- read.csv(ep_sample_path, stringsAsFactors = FALSE)
df_rt <- read.csv(rt_sample_path, stringsAsFactors = FALSE)

compute_stacked_da <- function(df_sample, weights_space, scenario, space_name) {
  N <- nrow(df_sample)
  active <- weights_space %>%
    filter(is.finite(stacking_weight), stacking_weight > 1e-6) %>%
    arrange(desc(stacking_weight))
  if (nrow(active) == 0) stop("[BLOCKER] No finite active sensitivity stacking weights for ", scenario, " / ", space_name)

  set_accrual_seed(
    paste0("sensitivity_construct_da_model_mix_", scenario, "_", space_name),
    offset = match(space_name, c("ex_post", "real_time"), nomatch = 10L)
  )
  sampled_model_indices <- sample(seq_len(nrow(active)), size = S, replace = TRUE, prob = active$stacking_weight)
  stacked_epred <- matrix(NA_real_, nrow = S, ncol = N)
  stacked_predict <- matrix(NA_real_, nrow = S, ncol = N)
  scenario_root <- sensitivity_root(scenario)

  for (m in seq_len(nrow(active))) {
    row <- active[m, ]
    mix_rows <- which(sampled_model_indices == m)
    if (length(mix_rows) == 0) next
    sample_group <- if ("sample_group" %in% names(row) && nzchar(row$sample_group)) row$sample_group else "main_common"
    key <- model_key_sampled(row$model_id, row$target_space, sample_group, row$heterogeneity_variant, paste0("_", scenario, "_winsor"))
    draws_path <- file.path(scenario_root, "draws", paste0("draws_", key, ".rds"))
    if (!file.exists(draws_path)) stop("[BLOCKER] Missing scenario posterior draws: ", draws_path)
    draws <- readRDS(draws_path)
    if (ncol(draws$epred) != N || ncol(draws$predict) != N) {
      stop("[BLOCKER] Draw observation count mismatch for ", draws_path)
    }
    set_accrual_seed(
      paste0("sensitivity_construct_da_draw_rows_", scenario, "_", space_name, "_", row$model_id, "_", row$heterogeneity_variant),
      offset = m
    )
    selected <- sample(seq_len(nrow(draws$epred)), size = length(mix_rows), replace = nrow(draws$epred) < length(mix_rows))
    stacked_epred[mix_rows, ] <- draws$epred[selected, ]
    stacked_predict[mix_rows, ] <- draws$predict[selected, ]
    rm(draws)
    gc()
  }

  if (anyNA(stacked_epred) || anyNA(stacked_predict)) stop("[BLOCKER] Stacked sensitivity draw matrix contains NA.")

  nda_mean <- colMeans(stacked_epred)
  nda_sd_epred <- apply(stacked_epred, 2, sd)
  nda_sd_predict <- apply(stacked_predict, 2, sd)
  q025 <- apply(stacked_predict, 2, quantile, probs = 0.025)
  q975 <- apply(stacked_predict, 2, quantile, probs = 0.975)
  q010 <- apply(stacked_predict, 2, quantile, probs = 0.010)
  q990 <- apply(stacked_predict, 2, quantile, probs = 0.990)
  ppd_percentile <- colMeans(sweep(stacked_predict, 2, df_sample$TA_scaled, FUN = "<="), na.rm = TRUE)
  ppd_tail <- 2 * pmin(ppd_percentile, 1 - ppd_percentile)
  da_raw <- df_sample$TA_scaled - nda_mean
  da_z_est <- da_raw / nda_sd_epred
  da_z_pred <- da_raw / nda_sd_predict

  # Compute DA_ppd_log_score via Kernel Density Estimation (KDE)
  kde_at_point <- function(y_val, x_draws) {
    x_draws <- x_draws[is.finite(x_draws)]
    if (length(x_draws) < 10) return(NA_real_)
    sd_x <- sd(x_draws)
    iqr_x <- IQR(x_draws)
    if (is.na(sd_x) || sd_x <= 0) sd_x <- 1e-4
    if (is.na(iqr_x) || iqr_x <= 0) iqr_x <- sd_x
    h <- 0.9 * min(sd_x, iqr_x / 1.34) * (length(x_draws)^(-0.2))
    if (is.na(h) || h <= 0) h <- 1e-4

    u <- (y_val - x_draws) / h
    mean(dnorm(u)) / h
  }

  N <- nrow(df_sample)
  DA_ppd_log_score_method <- rep("kde_gaussian_kernel_silverman", N)
  DA_ppd_log_score_status <- rep(NA_character_, N)
  DA_ppd_log_score <- numeric(N)
  for (i in seq_len(N)) {
    y_val <- df_sample$TA_scaled[i]
    draws_i <- stacked_predict[, i]
    finite_draws_n <- sum(is.finite(draws_i))
    dens_val <- kde_at_point(y_val, draws_i)
    if (finite_draws_n < 10) {
      dens_val <- 1e-12
      DA_ppd_log_score_status[i] <- "FLOOR_INSUFFICIENT_DRAWS"
    } else if (is.na(dens_val)) {
      dens_val <- 1e-12
      DA_ppd_log_score_status[i] <- "FLOOR_KDE_NA"
    } else if (dens_val <= 0) {
      dens_val <- 1e-12
      DA_ppd_log_score_status[i] <- "FLOOR_KDE_NONPOSITIVE"
    } else {
      dens_val <- max(dens_val, 1e-12)
      DA_ppd_log_score_status[i] <- "OK_KDE"
    }
    DA_ppd_log_score[i] <- log(dens_val)
  }

  data.frame(
    scenario = scenario,
    target_space = space_name,
    company = df_sample$company,
    year = df_sample$year,
    industry = if ("industry" %in% names(df_sample)) df_sample$industry else NA,
    se = if ("se" %in% names(df_sample)) df_sample$se else NA,
    TA_scaled = df_sample$TA_scaled,
    NDA_mean_stacked = nda_mean,
    NDA_sd_epred_stacked = nda_sd_epred,
    NDA_sd_predict_stacked = nda_sd_predict,
    DA_raw = da_raw,
    DA_z_estimation = da_z_est,
    DA_z_predictive = da_z_pred,
    DA_tail_flag_95 = as.integer(df_sample$TA_scaled < q025 | df_sample$TA_scaled > q975),
    DA_tail_flag_98 = as.integer(df_sample$TA_scaled < q010 | df_sample$TA_scaled > q990),
    DA_ppd_tail_prob_two_sided = ppd_tail,
    DA_ppd_percentile = ppd_percentile,
    DA_ppd_tail_surprise_score = -log(pmax(ppd_tail, 1 / S)),
    DA_ppd_log_score = DA_ppd_log_score,
    DA_ppd_log_score_method = DA_ppd_log_score_method,
    DA_ppd_log_score_status = DA_ppd_log_score_status,
    DA_surprise_score_gaussian_approx = -dnorm(df_sample$TA_scaled, mean = nda_mean, sd = nda_sd_predict, log = TRUE),
    Prior_Set_ID = scenarios$Prior_Set_ID[scenarios$Scenario == scenario][1],
    Likelihood_Family = "student",
    Model_Structure = "pooled_random_intercept",
    stringsAsFactors = FALSE
  )
}

da_rows <- list()
plan_rows <- list()

for (sidx in seq_len(nrow(scenarios))) {
  scenario <- scenarios$Scenario[sidx]
  scenario_root <- sensitivity_root(scenario)
  ensure_sensitivity_dirs(scenario)
  sc_weights <- weights_df %>% filter(scenario == !!scenario)
  plan_rows[[length(plan_rows) + 1]] <- data.frame(
    scenario = scenario,
    dry_run = dry_run,
    n_weight_rows = nrow(sc_weights),
    n_finite_weight_rows = sum(is.finite(sc_weights$stacking_weight)),
    output_path = sensitivity_accruals_path(scenario),
    stringsAsFactors = FALSE
  )

  write_run_manifest(
    file.path(scenario_root, "manifests", "DA_manifest.csv"),
    scenario = scenario,
    prior_set_id = scenarios$Prior_Set_ID[scenarios$Scenario == scenario][1],
    family = "student",
    model_structure = "pooled_random_intercept",
    model_list = unique(sc_weights$model_id),
    seed = accrual_seed_for(paste0("sensitivity_construct_da_manifest_", scenario), offset = sidx),
    sampling_config = sprintf("stacking_mixture_draws=%d; dry_run=%s", S, dry_run),
    status = if (dry_run) "DRY_RUN_PLANNED" else "STARTED",
    notes = "DA is recomputed from scenario stacking weights and posterior predictive draws.",
    input_paths = c(weights_path, ep_sample_path, rt_sample_path),
    rng_context = paste0("sensitivity_construct_da_manifest_", scenario),
    rng_offset = sidx
  )

  if (dry_run) next
  ep <- compute_stacked_da(df_ep, sc_weights %>% filter(target_space == "ex_post"), scenario, "ex_post")
  rt <- compute_stacked_da(df_rt, sc_weights %>% filter(target_space == "real_time"), scenario, "real_time")
  scenario_da <- bind_rows(ep, rt)
  write.csv(scenario_da, file.path(scenario_root, "DA", paste0("final_sensitivity_uncertainty_adjusted_accruals_", scenario, ".csv")), row.names = FALSE)
  write.csv(scenario_da, sensitivity_accruals_path(scenario), row.names = FALSE)
  da_rows[[length(da_rows) + 1]] <- scenario_da
}

plan_df <- bind_rows(plan_rows)
write.csv(plan_df, file.path(sensitivity_root(), "tables", "sensitivity_DA_plan.csv"), row.names = FALSE)

all_da <- bind_rows(da_rows)
if (nrow(all_da) > 0) {
  write.csv(all_da, file.path(sensitivity_root(), "tables", "sensitivity_DA_by_scenario_long.csv"), row.names = FALSE)

  stability_rows <- list()
  baseline <- all_da %>% filter(scenario == "baseline")
  for (scenario in setdiff(unique(all_da$scenario), "baseline")) {
    comp <- baseline %>%
      select(target_space, company, year, base_DA_z_predictive = DA_z_predictive, base_tail95 = DA_tail_flag_95, base_tail98 = DA_tail_flag_98) %>%
      inner_join(
        all_da %>% filter(scenario == !!scenario) %>%
          select(target_space, company, year, scenario_DA_z_predictive = DA_z_predictive, scenario_tail95 = DA_tail_flag_95, scenario_tail98 = DA_tail_flag_98),
        by = c("target_space", "company", "year")
      )
    for (space in unique(comp$target_space)) {
      rows <- comp %>% filter(target_space == !!space)
      stability_rows[[length(stability_rows) + 1]] <- data.frame(
        scenario = scenario,
        target_space = space,
        N = nrow(rows),
        pearson_baseline_vs_scenario = cor(rows$base_DA_z_predictive, rows$scenario_DA_z_predictive, use = "complete.obs", method = "pearson"),
        spearman_baseline_vs_scenario = cor(rows$base_DA_z_predictive, rows$scenario_DA_z_predictive, use = "complete.obs", method = "spearman"),
        mean_absolute_difference = mean(abs(rows$base_DA_z_predictive - rows$scenario_DA_z_predictive), na.rm = TRUE),
        tail_flag_agreement_95 = mean(rows$base_tail95 == rows$scenario_tail95, na.rm = TRUE),
        tail_flag_agreement_98 = mean(rows$base_tail98 == rows$scenario_tail98, na.rm = TRUE),
        DA_z_predictive_mean = mean(rows$scenario_DA_z_predictive, na.rm = TRUE),
        DA_z_predictive_sd = sd(rows$scenario_DA_z_predictive, na.rm = TRUE),
        stability_flag = ifelse(
          cor(rows$base_DA_z_predictive, rows$scenario_DA_z_predictive, use = "complete.obs", method = "spearman") < 0.90 ||
            mean(abs(rows$base_DA_z_predictive - rows$scenario_DA_z_predictive), na.rm = TRUE) > 0.25,
          "REVIEW_PRIOR_SENSITIVITY",
          "STABLE"
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  write.csv(bind_rows(stability_rows), file.path(sensitivity_root(), "tables", "sensitivity_DA_stability_summary.csv"), row.names = FALSE)
} else {
  write.csv(data.frame(), file.path(sensitivity_root(), "tables", "sensitivity_DA_by_scenario_long.csv"), row.names = FALSE)
  write.csv(data.frame(), file.path(sensitivity_root(), "tables", "sensitivity_DA_stability_summary.csv"), row.names = FALSE)
}

writeLines(c(
  "Sensitivity DA construction notes",
  sprintf("Dry run: %s", dry_run),
  sprintf("Stacking mixture draws: %d", S),
  "DA_ppd_tail_surprise_score is based on empirical posterior predictive two-sided tail probability.",
  "DA_ppd_log_score is computed via KDE from posterior predictive draws.",
  "DA_ppd_log_score_method and DA_ppd_log_score_status document the KDE method and any fallback-to-floor cases.",
  "Gaussian approximation is retained only as DA_surprise_score_gaussian_approx."
), file.path(sensitivity_root(), "logs", "sensitivity_DA_notes.txt"))

cat("\n[SUCCESS] Sensitivity DA construction completed.\n")
