# -----------------------------------------------------------------------------
# Script: 11_posterior_predictive_checks.R
# Purpose: Evaluate whether the stacked winsorized posterior predictive
#          distribution reproduces the observed TA_scaled distribution.
# -----------------------------------------------------------------------------

library(dplyr)

source("scripts/ma00_setup.R")
phase_begin("ma11", "Posterior predictive checks")
ensure_analysis_dirs()

final_path <- file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_winsor.csv")
ep_weights_path <- file.path(output_root, "tables", "table_stacking_weights_ex_post_winsor_corrected.csv")
rt_weights_path <- file.path(output_root, "tables", "table_stacking_weights_no_lookahead_winsor_corrected.csv")

if (!file.exists(final_path)) stop("[BLOCKER] Missing final winsor uncertainty-adjusted accruals file.")
if (!file.exists(ep_weights_path)) stop("[BLOCKER] Missing ex-post winsor stacking weights.")
if (!file.exists(rt_weights_path)) stop("[BLOCKER] Missing no-look-ahead winsor stacking weights.")

final_df <- read.csv(final_path, stringsAsFactors = FALSE)
w_ep_df <- read.csv(ep_weights_path, stringsAsFactors = FALSE)
w_rt_df <- read.csv(rt_weights_path, stringsAsFactors = FALSE)

summary_path <- file.path(output_root, "tables", "table_posterior_predictive_check_summary.csv")
moments_path <- file.path(output_root, "tables", "table_posterior_predictive_moments.csv")
tail_path <- file.path(output_root, "tables", "table_posterior_predictive_tail_coverage.csv")
notes_path <- file.path(output_root, "logs", "phase5c_posterior_predictive_check_notes.txt")

get_space_df <- function(space) {
  mean_col <- if (space == "ex_post") "NDA_mean_stacked_ep_winsor" else "NDA_mean_stacked_rt_winsor"
  final_df[!is.na(final_df[[mean_col]]), , drop = FALSE]
}

draws_path_for <- function(row, space_name) {
  file.path(
    output_root,
    "draws",
    paste0("draws_", model_key_sampled(row$Model_ID, space_name, row$Sample_Group, row$Heterogeneity_Variant, "_winsor"), ".rds")
  )
}

sample_stacked_predictive <- function(df_sample, weights_df, space_name, n_draws = 4000) {
  active_weights <- weights_df %>%
    filter(Weight > 1e-6, Model_ID != "M08", Model_ID != "M10", Sample_Group == "main_common") %>%
    arrange(desc(Weight))
  if (nrow(active_weights) == 0) {
    stop("[BLOCKER] No active main-stack weights found for posterior predictive checks.")
  }

  n_obs <- nrow(df_sample)
  set_accrual_seed(paste0("baseline_posterior_predictive_model_mix_", space_name))
  sampled_model_idx <- sample(seq_len(nrow(active_weights)), size = n_draws, replace = TRUE, prob = active_weights$Weight)
  stacked_predict <- matrix(NA_real_, nrow = n_draws, ncol = n_obs)

  for (m in seq_len(nrow(active_weights))) {
    row <- active_weights[m, ]
    use_rows <- which(sampled_model_idx == m)
    if (length(use_rows) == 0) next
    draws_path <- draws_path_for(row, space_name)
    if (!file.exists(draws_path)) stop("[BLOCKER] Missing posterior draw file: ", draws_path)
    draws <- readRDS(draws_path)
    if (ncol(draws$predict) != n_obs) {
      stop(sprintf("[BLOCKER] Predictive draw dimension mismatch for %s: expected %d, got %d",
                   draws_path, n_obs, ncol(draws$predict)))
    }
    pool <- seq_len(nrow(draws$predict))
    set_accrual_seed(
      paste0("baseline_posterior_predictive_draw_rows_", space_name, "_", row$Model_ID, "_", row$Heterogeneity_Variant),
      offset = m
    )
    chosen <- sample(pool, size = length(use_rows), replace = length(pool) < length(use_rows))
    stacked_predict[use_rows, ] <- draws$predict[chosen, , drop = FALSE]
    rm(draws)
    gc()
  }

  if (anyNA(stacked_predict)) stop("[BLOCKER] Stacked posterior predictive matrix contains NA values.")
  stacked_predict
}

draw_level_moments <- function(sim_matrix) {
  data.frame(
    Draw_ID = seq_len(nrow(sim_matrix)),
    Mean = rowMeans(sim_matrix),
    SD = apply(sim_matrix, 1, sd),
    P01 = apply(sim_matrix, 1, quantile, probs = 0.01),
    P05 = apply(sim_matrix, 1, quantile, probs = 0.05),
    Median = apply(sim_matrix, 1, quantile, probs = 0.50),
    P95 = apply(sim_matrix, 1, quantile, probs = 0.95),
    P99 = apply(sim_matrix, 1, quantile, probs = 0.99),
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
}

observed_moments <- function(x) {
  qs <- quantile(x, probs = c(0.01, 0.05, 0.50, 0.95, 0.99), na.rm = TRUE, names = FALSE)
  c(
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    P01 = qs[1],
    P05 = qs[2],
    Median = qs[3],
    P95 = qs[4],
    P99 = qs[5]
  )
}

ppc_flag <- function(abs_mean_diff, abs_sd_diff, tail_diff) {
  if (abs_mean_diff > 0.10 || abs_sd_diff > 0.15 || tail_diff > 0.20) return("FAIL")
  if (abs_mean_diff > 0.05 || abs_sd_diff > 0.08 || tail_diff > 0.10) return("REVIEW")
  "PASS"
}

write_density_plot <- function(observed, sim_matrix, path, title_text) {
  png(filename = path, width = 1200, height = 800, res = 140)
  on.exit(dev.off(), add = TRUE)
  plot(density(observed), main = title_text, xlab = "TA_scaled", ylab = "Density", lwd = 3, col = "black")
  overlay_rows <- seq_len(min(20, nrow(sim_matrix)))
  for (idx in overlay_rows) {
    lines(density(sim_matrix[idx, ]), col = rgb(0.75, 0.25, 0.15, 0.16), lwd = 1)
  }
  lines(density(as.vector(sim_matrix[seq_len(min(200, nrow(sim_matrix))), , drop = FALSE])), col = "#C44E2B", lwd = 2, lty = 2)
  legend("topright", legend = c("Observed", "Posterior predictive draws", "Flattened posterior predictive"),
         col = c("black", rgb(0.75, 0.25, 0.15, 0.30), "#C44E2B"), lwd = c(3, 1, 2), lty = c(1, 1, 2), bty = "n")
}

write_ecdf_plot <- function(observed, sim_matrix, path, title_text) {
  png(filename = path, width = 1200, height = 800, res = 140)
  on.exit(dev.off(), add = TRUE)
  plot(stats::ecdf(observed), main = title_text, xlab = "TA_scaled", ylab = "ECDF", lwd = 3, col = "black")
  overlay_rows <- seq_len(min(20, nrow(sim_matrix)))
  for (idx in overlay_rows) {
    lines(stats::ecdf(sim_matrix[idx, ]), col = rgb(0.15, 0.55, 0.30, 0.16), lwd = 1)
  }
  legend("bottomright", legend = c("Observed", "Posterior predictive draws"),
         col = c("black", rgb(0.15, 0.55, 0.30, 0.30)), lwd = c(3, 1), bty = "n")
}

evaluate_space <- function(space_name, weights_df, suffix) {
  df_space <- get_space_df(space_name)
  observed <- df_space$TA_scaled
  sim_matrix <- sample_stacked_predictive(df_space, weights_df, space_name)
  obs_stats <- observed_moments(observed)
  sim_stats <- draw_level_moments(sim_matrix)

  pred_q95_lo <- apply(sim_matrix, 2, quantile, probs = 0.025)
  pred_q95_hi <- apply(sim_matrix, 2, quantile, probs = 0.975)
  pred_q98_lo <- apply(sim_matrix, 2, quantile, probs = 0.010)
  pred_q98_hi <- apply(sim_matrix, 2, quantile, probs = 0.990)

  outside_95 <- mean(observed < pred_q95_lo | observed > pred_q95_hi)
  outside_98 <- mean(observed < pred_q98_lo | observed > pred_q98_hi)
  pred_mean_of_mean <- mean(sim_stats$Mean)
  pred_mean_of_sd <- mean(sim_stats$SD)
  pred_mean_of_p01 <- mean(sim_stats$P01)
  pred_mean_of_p99 <- mean(sim_stats$P99)

  ks_value <- suppressWarnings(as.numeric(stats::ks.test(observed, sim_matrix[1, ])$statistic))
  flag <- ppc_flag(
    abs_mean_diff = abs(obs_stats["Mean"] - pred_mean_of_mean),
    abs_sd_diff = abs(obs_stats["SD"] - pred_mean_of_sd),
    tail_diff = max(abs(obs_stats["P01"] - pred_mean_of_p01), abs(obs_stats["P99"] - pred_mean_of_p99))
  )

  summary_row <- data.frame(
    Target_Space = space_name,
    N_Obs = length(observed),
    Observed_Mean = obs_stats["Mean"],
    Observed_SD = obs_stats["SD"],
    Observed_P01 = obs_stats["P01"],
    Observed_P05 = obs_stats["P05"],
    Observed_Median = obs_stats["Median"],
    Observed_P95 = obs_stats["P95"],
    Observed_P99 = obs_stats["P99"],
    PosteriorPred_Mean_of_Mean = pred_mean_of_mean,
    PosteriorPred_Mean_of_SD = pred_mean_of_sd,
    PosteriorPred_Mean_of_P01 = pred_mean_of_p01,
    PosteriorPred_Mean_of_P05 = mean(sim_stats$P05),
    PosteriorPred_Mean_of_Median = mean(sim_stats$Median),
    PosteriorPred_Mean_of_P95 = mean(sim_stats$P95),
    PosteriorPred_Mean_of_P99 = pred_mean_of_p99,
    Observed_Outside_95PPI = outside_95,
    Observed_Outside_98PPI = outside_98,
    Absolute_Mean_Difference = abs(obs_stats["Mean"] - pred_mean_of_mean),
    Absolute_SD_Difference = abs(obs_stats["SD"] - pred_mean_of_sd),
    Tail_Difference_P01 = abs(obs_stats["P01"] - pred_mean_of_p01),
    Tail_Difference_P99 = abs(obs_stats["P99"] - pred_mean_of_p99),
    KS_Statistic = ks_value,
    PPC_Flag = flag,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )

  moments_rows <- data.frame(
    Target_Space = space_name,
    Statistic = c("Mean", "SD", "P01", "P05", "Median", "P95", "P99"),
    Observed = c(obs_stats["Mean"], obs_stats["SD"], obs_stats["P01"], obs_stats["P05"], obs_stats["Median"], obs_stats["P95"], obs_stats["P99"]),
    PosteriorPred_Mean = c(mean(sim_stats$Mean), mean(sim_stats$SD), mean(sim_stats$P01), mean(sim_stats$P05),
                           mean(sim_stats$Median), mean(sim_stats$P95), mean(sim_stats$P99)),
    PosteriorPred_Q02_5 = c(quantile(sim_stats$Mean, 0.025), quantile(sim_stats$SD, 0.025), quantile(sim_stats$P01, 0.025),
                            quantile(sim_stats$P05, 0.025), quantile(sim_stats$Median, 0.025),
                            quantile(sim_stats$P95, 0.025), quantile(sim_stats$P99, 0.025)),
    PosteriorPred_Q97_5 = c(quantile(sim_stats$Mean, 0.975), quantile(sim_stats$SD, 0.975), quantile(sim_stats$P01, 0.975),
                            quantile(sim_stats$P05, 0.975), quantile(sim_stats$Median, 0.975),
                            quantile(sim_stats$P95, 0.975), quantile(sim_stats$P99, 0.975)),
    stringsAsFactors = FALSE
  )

  tail_row <- data.frame(
    Target_Space = space_name,
    N_Obs = length(observed),
    Outside_95PPI_Count = sum(observed < pred_q95_lo | observed > pred_q95_hi),
    Outside_95PPI_Share = outside_95,
    Outside_98PPI_Count = sum(observed < pred_q98_lo | observed > pred_q98_hi),
    Outside_98PPI_Share = outside_98,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )

  write_density_plot(observed, sim_matrix, file.path(output_root, "figures", paste0("fig_ppc_density_", suffix, ".png")),
                     sprintf("Posterior Predictive Density: %s", space_name))
  write_ecdf_plot(observed, sim_matrix, file.path(output_root, "figures", paste0("fig_ppc_ecdf_", suffix, ".png")),
                  sprintf("Posterior Predictive ECDF: %s", space_name))

  list(summary = summary_row, moments = moments_rows, tail = tail_row)
}

ep_eval <- evaluate_space("ex_post", w_ep_df, "ex_post")
rt_eval <- evaluate_space("real_time", w_rt_df, "no_lookahead")

summary_df <- bind_rows(ep_eval$summary, rt_eval$summary)
moments_df <- bind_rows(ep_eval$moments, rt_eval$moments)
tail_df <- bind_rows(ep_eval$tail, rt_eval$tail)

default_winsor_root <- file.path("out", "interim", "winsor")
old_wide_summary_path <- file.path(default_winsor_root, "tables", "table_posterior_predictive_check_summary.csv")
old_tail_note <- "Default winsor PPC summary not found or current output root is already the default diagnostic root."
if (normalizePath(output_root, winslash = "/", mustWork = FALSE) != normalizePath(default_winsor_root, winslash = "/", mustWork = FALSE) &&
    file.exists(old_wide_summary_path)) {
  old_ppc <- read.csv(old_wide_summary_path, stringsAsFactors = FALSE)
  compare_cols <- intersect(c("Target_Space", "Observed_Outside_95PPI", "Observed_Outside_98PPI", "Tail_Difference_P01", "Tail_Difference_P99"), names(old_ppc))
  if (all(c("Target_Space", "Observed_Outside_95PPI", "Observed_Outside_98PPI") %in% compare_cols)) {
    old_comp <- old_ppc[, compare_cols, drop = FALSE]
    names(old_comp)[names(old_comp) != "Target_Space"] <- paste0(names(old_comp)[names(old_comp) != "Target_Space"], "_Old_Wide_Gaussian")
    summary_df <- summary_df %>%
      left_join(old_comp, by = "Target_Space") %>%
      mutate(
        Delta_Outside_95PPI_vs_Old_Wide_Gaussian = Observed_Outside_95PPI - Observed_Outside_95PPI_Old_Wide_Gaussian,
        Delta_Outside_98PPI_vs_Old_Wide_Gaussian = Observed_Outside_98PPI - Observed_Outside_98PPI_Old_Wide_Gaussian,
        Tail_Improved_vs_Old_Wide_Gaussian = Delta_Outside_95PPI_vs_Old_Wide_Gaussian <= 0 & Delta_Outside_98PPI_vs_Old_Wide_Gaussian <= 0
      )
    old_tail_note <- "Tail coverage compared with the default winsor PPC summary."
  }
}

write.csv(summary_df, summary_path, row.names = FALSE)
write.csv(moments_df, moments_path, row.names = FALSE)
write.csv(tail_df, tail_path, row.names = FALSE)

notes <- c(
  "Phase 5c posterior predictive notes",
  sprintf("Output root: %s", output_root),
  sprintf("Prior set: %s; likelihood family: %s; model structure: %s", prior_set_id, likelihood_family, model_structure),
  "Posterior predictive checks evaluate whether the stacked Bayesian accrual model can reproduce the observed TA_scaled distribution.",
  "PASS means center and tails are broadly matched; REVIEW means center is matched but tails differ; FAIL means center and tails are both poor.",
  old_tail_note,
  "These checks are distributional diagnostics, not outcome validation.",
  paste(
    apply(summary_df, 1, function(x) {
      sprintf("%s: flag=%s, outside95=%.4f, outside98=%.4f, abs_mean_diff=%.4f, abs_sd_diff=%.4f",
              x[["Target_Space"]], x[["PPC_Flag"]], as.numeric(x[["Observed_Outside_95PPI"]]),
              as.numeric(x[["Observed_Outside_98PPI"]]), as.numeric(x[["Absolute_Mean_Difference"]]),
              as.numeric(x[["Absolute_SD_Difference"]]))
    }),
    collapse = "\n"
  )
)
writeLines(notes, notes_path)

if (any(summary_df$PPC_Flag == "FAIL")) {
  warning("[WARNING] Posterior predictive checks failed for at least one target space.")
} else if (any(summary_df$PPC_Flag == "REVIEW")) {
  warning("[WARNING] Posterior predictive checks require review for at least one target space.")
}

cat("\n[SUCCESS] Phase 5c posterior predictive checks completed.\n")
phase_end("ma11", "Posterior predictive checks")
