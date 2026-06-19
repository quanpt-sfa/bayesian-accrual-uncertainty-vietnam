# -----------------------------------------------------------------------------
# Script: 10_construct_uncertainty_adjusted_DA.R
# Purpose: Construct winsorized uncertainty-adjusted abnormal accruals.
# -----------------------------------------------------------------------------

library(dplyr)

source("scripts/00_helpers.R")
ensure_analysis_dirs()
validate_final_analysis_config("Phase 5b baseline uncertainty-adjusted DA", final_mode = TRUE)

script_name <- "scripts/10_construct_uncertainty_adjusted_DA.R"
script_version <- "2026-06-19-v1-secondary-psis-loo-da-manifest"
script_start_time <- Sys.time()

ep_weights_path <- file.path(output_root, "tables", "table_stacking_weights_ex_post_winsor_corrected.csv")
rt_weights_path <- file.path(output_root, "tables", "table_stacking_weights_no_lookahead_winsor_corrected.csv")
ep_sample_path <- file.path(input_winsor_root, "tables", "final_common_ex_post_sample_winsor.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")

file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}
git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}

if (!file.exists(ep_weights_path)) stop("[BLOCKER] Missing winsor ex-post stacking weights. Run Phase 4c first.")
if (!file.exists(rt_weights_path)) stop("[BLOCKER] Missing winsor no-look-ahead stacking weights. Run Phase 4c first.")
if (!file.exists(ep_sample_path)) stop("[BLOCKER] Missing winsor ex-post sample.")
if (!file.exists(rt_sample_path)) stop("[BLOCKER] Missing winsor no-look-ahead sample.")

w_ep_df <- read.csv(ep_weights_path, stringsAsFactors = FALSE)
w_rt_df <- read.csv(rt_weights_path, stringsAsFactors = FALSE)
df_ep <- read.csv(ep_sample_path, stringsAsFactors = FALSE)
df_rt <- read.csv(rt_sample_path, stringsAsFactors = FALSE)

clean_variant_name <- function(heterogeneity_variant, model_name = NULL) {
  variant <- extract_weight_variant(ifelse(is.null(model_name), "", model_name), heterogeneity_variant)
  safe_variant_name(variant)
}

draws_path_for <- function(row, space_name) {
  variant_clean <- clean_variant_name(row$Heterogeneity_Variant, row$Model_Name)
  sample_group <- if ("Sample_Group" %in% names(row)) row$Sample_Group else "main_common"
  file.path(output_root, "draws", paste0("draws_", model_key_sampled(row$Model_ID, space_name, sample_group, row$Heterogeneity_Variant, "_winsor"), ".rds"))
}

compute_stacked_accruals <- function(df_sample, weights_df, space_name, S = stacking_mixture_draws) {
  N <- nrow(df_sample)
  message(sprintf("\n--- Winsor stacking for %s (N = %d, S = %d) ---", space_name, N, S))
  active_weights <- weights_df %>%
    filter(Weight > 1e-6, Model_ID != "M10", Sample_Group == "main_common") %>%
    arrange(desc(Weight))
  if (nrow(active_weights) == 0) stop("[BLOCKER] No active stacking weights for ", space_name)

  set_accrual_seed(paste0("baseline_construct_da_", space_name))
  sampled_model_indices <- sample(seq_len(nrow(active_weights)), size = S, replace = TRUE, prob = active_weights$Weight)
  stacked_epred <- matrix(NA_real_, nrow = S, ncol = N)
  stacked_predict <- matrix(NA_real_, nrow = S, ncol = N)

  for (m in seq_len(nrow(active_weights))) {
    row <- active_weights[m, ]
    mix_rows <- which(sampled_model_indices == m)
    if (length(mix_rows) == 0) next
    draws_path <- draws_path_for(row, space_name)
    if (!file.exists(draws_path)) stop("[BLOCKER] Winsor draws file missing: ", draws_path)
    draws <- readRDS(draws_path)
    if (ncol(draws$epred) != N || ncol(draws$predict) != N) {
      stop(sprintf("[BLOCKER] Draw N mismatch for %s: expected %d, got epred=%d predict=%d",
                   draws_path, N, ncol(draws$epred), ncol(draws$predict)))
    }
    draw_pool <- seq_len(nrow(draws$epred))
    selected_draws <- sample(draw_pool, size = length(mix_rows), replace = length(draw_pool) < length(mix_rows))
    message(sprintf("  %s %s weight=%.4f assigned_draws=%d",
                    row$Model_ID, row$Heterogeneity_Variant, row$Weight, length(mix_rows)))
    stacked_epred[mix_rows, ] <- draws$epred[selected_draws, ]
    stacked_predict[mix_rows, ] <- draws$predict[selected_draws, ]
    rm(draws)
    gc()
  }

  if (anyNA(stacked_epred) || anyNA(stacked_predict)) {
    stop("[BLOCKER] Mixture draw matrix contains NA after stacking.")
  }

  NDA_mean_stacked <- colMeans(stacked_epred)
  NDA_sd_epred_stacked <- apply(stacked_epred, 2, sd)
  NDA_sd_predict_stacked <- apply(stacked_predict, 2, sd)
  NDA_q025_stacked <- apply(stacked_predict, 2, quantile, probs = 0.025)
  NDA_q50_stacked <- apply(stacked_predict, 2, quantile, probs = 0.50)
  NDA_q975_stacked <- apply(stacked_predict, 2, quantile, probs = 0.975)
  NDA_q010_stacked <- apply(stacked_predict, 2, quantile, probs = 0.010)
  NDA_q990_stacked <- apply(stacked_predict, 2, quantile, probs = 0.990)
  DA_ppd_percentile <- colMeans(sweep(stacked_predict, 2, df_sample$TA_scaled, FUN = "<="), na.rm = TRUE)
  DA_ppd_tail_prob_two_sided <- 2 * pmin(DA_ppd_percentile, 1 - DA_ppd_percentile)

  DA_raw_stacked <- df_sample$TA_scaled - NDA_mean_stacked
  DA_z_estimation_stacked <- DA_raw_stacked / NDA_sd_epred_stacked
  DA_z_predictive_stacked <- DA_raw_stacked / NDA_sd_predict_stacked
  DA_ppd_tail_surprise_score <- -log(pmax(DA_ppd_tail_prob_two_sided, 1 / S))
  DA_surprise_score_gaussian_approx <- -dnorm(df_sample$TA_scaled, mean = NDA_mean_stacked, sd = NDA_sd_predict_stacked, log = TRUE)

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
    company = df_sample$company,
    year = df_sample$year,
    NDA_mean_stacked = NDA_mean_stacked,
    NDA_sd_epred_stacked = NDA_sd_epred_stacked,
    NDA_sd_predict_stacked = NDA_sd_predict_stacked,
    NDA_q025_stacked = NDA_q025_stacked,
    NDA_q50_stacked = NDA_q50_stacked,
    NDA_q975_stacked = NDA_q975_stacked,
    DA_raw_stacked = DA_raw_stacked,
    DA_z_estimation_stacked = DA_z_estimation_stacked,
    DA_z_predictive_stacked = DA_z_predictive_stacked,
    Abs_DA_raw_stacked = abs(DA_raw_stacked),
    Abs_DA_z_estimation_stacked = abs(DA_z_estimation_stacked),
    Abs_DA_z_predictive_stacked = abs(DA_z_predictive_stacked),
    DA_tail_flag_95 = as.integer(df_sample$TA_scaled < NDA_q025_stacked | df_sample$TA_scaled > NDA_q975_stacked),
    DA_tail_flag_98 = as.integer(df_sample$TA_scaled < NDA_q010_stacked | df_sample$TA_scaled > NDA_q990_stacked),
    DA_ppd_tail_prob_two_sided = DA_ppd_tail_prob_two_sided,
    DA_ppd_percentile = DA_ppd_percentile,
    DA_ppd_tail_surprise_score = DA_ppd_tail_surprise_score,
    DA_ppd_log_score = DA_ppd_log_score,
    DA_ppd_log_score_method = DA_ppd_log_score_method,
    DA_ppd_log_score_status = DA_ppd_log_score_status,
    DA_surprise_score_gaussian_approx = DA_surprise_score_gaussian_approx,
    NDA_Uncertainty_Type = "epred_estimation_and_predictive_extension",
    Predictive_Tail_Extension = TRUE,
    Near_AccForUncertaintyCode_Output = "NDA_mean_stacked + NDA_sd_epred_stacked",
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
}

stacked_ep <- compute_stacked_accruals(df_ep, w_ep_df, "ex_post")
stacked_rt <- compute_stacked_accruals(df_rt, w_rt_df, "real_time")

message("\nFitting winsorized OLS benchmarks...")
get_ols_data <- function(df) {
  df$industry_f <- factor(df$industry)
  df$year_f <- factor(df$year)
  standardize_predictors(df)
}

df_rt_ols <- get_ols_data(df_rt)
ols_jones <- lm(TA_scaled ~ inv_A_lag_std + dREV_scaled_std + PPE_scaled_std + industry_f + year_f, data = df_rt_ols)
ols_modj <- lm(TA_scaled ~ inv_A_lag_std + dREV_dREC_scaled_std + PPE_scaled_std + industry_f + year_f, data = df_rt_ols)
ols_perf <- lm(TA_scaled ~ inv_A_lag_std + dREV_dREC_scaled_std + PPE_scaled_std + ROA_lag_std + industry_f + year_f, data = df_rt_ols)

df_rt_ols$DA_Jones_OLS_winsor <- residuals(ols_jones)
df_rt_ols$DA_ModJones_OLS_winsor <- residuals(ols_modj)
df_rt_ols$DA_PerfModJones_OLS_winsor <- residuals(ols_perf)

rename_stack <- function(df, suffix) {
  rename_map <- c(
    NDA_mean_stacked = paste0("NDA_mean_stacked_", suffix, "_winsor"),
    NDA_sd_epred_stacked = paste0("NDA_sd_epred_stacked_", suffix, "_winsor"),
    NDA_sd_predict_stacked = paste0("NDA_sd_predict_stacked_", suffix, "_winsor"),
    NDA_q025_stacked = paste0("NDA_q025_stacked_", suffix, "_winsor"),
    NDA_q50_stacked = paste0("NDA_q50_stacked_", suffix, "_winsor"),
    NDA_q975_stacked = paste0("NDA_q975_stacked_", suffix, "_winsor"),
    DA_raw_stacked = paste0("DA_raw_stacked_", suffix, "_winsor"),
    DA_z_estimation_stacked = paste0("DA_z_estimation_stacked_", suffix, "_winsor"),
    DA_z_predictive_stacked = paste0("DA_z_predictive_stacked_", suffix, "_winsor"),
    Abs_DA_raw_stacked = paste0("Abs_DA_raw_stacked_", suffix, "_winsor"),
    Abs_DA_z_estimation_stacked = paste0("Abs_DA_z_estimation_stacked_", suffix, "_winsor"),
    Abs_DA_z_predictive_stacked = paste0("Abs_DA_z_predictive_stacked_", suffix, "_winsor"),
    DA_tail_flag_95 = paste0("DA_tail_flag_95_", suffix, "_winsor"),
    DA_tail_flag_98 = paste0("DA_tail_flag_98_", suffix, "_winsor"),
    DA_ppd_tail_prob_two_sided = paste0("DA_ppd_tail_prob_two_sided_", suffix, "_winsor"),
    DA_ppd_percentile = paste0("DA_ppd_percentile_", suffix, "_winsor"),
    DA_ppd_tail_surprise_score = paste0("DA_ppd_tail_surprise_score_", suffix, "_winsor"),
    DA_ppd_log_score = paste0("DA_ppd_log_score_", suffix, "_winsor"),
    DA_ppd_log_score_method = paste0("DA_ppd_log_score_method_", suffix, "_winsor"),
    DA_ppd_log_score_status = paste0("DA_ppd_log_score_status_", suffix, "_winsor"),
    DA_surprise_score_gaussian_approx = paste0("DA_surprise_score_gaussian_approx_", suffix, "_winsor")
  )
  out <- df
  for (old in names(rename_map)) names(out)[names(out) == old] <- rename_map[[old]]
  out
}

stacked_ep_renamed <- rename_stack(stacked_ep, "ep")
stacked_rt_renamed <- rename_stack(stacked_rt, "rt")

master_df <- df_rt_ols %>%
  select(company, year, industry, se, TA_scaled,
         DA_Jones_OLS_winsor, DA_ModJones_OLS_winsor, DA_PerfModJones_OLS_winsor) %>%
  left_join(stacked_rt_renamed, by = c("company", "year")) %>%
  left_join(stacked_ep_renamed, by = c("company", "year")) %>%
  mutate(
    Sample_Group = "main_common",
    OperatingCycle_In_Main_Stack = FALSE,
    NDA_Uncertainty_Type = "NDA_sd_epred_stacked is estimation uncertainty; NDA_sd_predict_stacked is posterior predictive uncertainty.",
    Predictive_Tail_Extension = TRUE,
    Near_AccForUncertaintyCode_Output = "NDA_mean_stacked + NDA_sd_epred_stacked",
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root
  )

baseline_accruals_path <- baseline_accruals_path()
write.csv(master_df, file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_winsor.csv"), row.names = FALSE)
write.csv(master_df, baseline_accruals_path, row.names = FALSE)

summarise_var <- function(df, v) {
  vals <- df[[v]]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NULL)
  qs <- quantile(vals, probs = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99), names = FALSE)
  data.frame(
    Variable = v,
    N = length(vals),
    Mean = mean(vals),
    SD = sd(vals),
    Min = min(vals),
    P10 = qs[1],
    P25 = qs[2],
    Median = qs[3],
    P75 = qs[4],
    P90 = qs[5],
    P95 = qs[6],
    P99 = qs[7],
    Max = max(vals),
    stringsAsFactors = FALSE
  )
}

da_vars <- c(
  "DA_Jones_OLS_winsor", "DA_ModJones_OLS_winsor", "DA_PerfModJones_OLS_winsor",
  "DA_raw_stacked_ep_winsor", "DA_z_estimation_stacked_ep_winsor", "DA_z_predictive_stacked_ep_winsor",
  "DA_ppd_tail_prob_two_sided_ep_winsor", "DA_ppd_tail_surprise_score_ep_winsor", "DA_ppd_log_score_ep_winsor",
  "DA_raw_stacked_rt_winsor", "DA_z_estimation_stacked_rt_winsor", "DA_z_predictive_stacked_rt_winsor",
  "DA_ppd_tail_prob_two_sided_rt_winsor", "DA_ppd_tail_surprise_score_rt_winsor", "DA_ppd_log_score_rt_winsor"
)
summary_df <- bind_rows(lapply(da_vars, function(v) summarise_var(master_df, v)))
summary_df <- summary_df %>%
  mutate(
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root
  )
write.csv(summary_df, file.path(output_root, "tables", "table_DA_distribution_summary_winsor.csv"), row.names = FALSE)

flag_count <- function(var_name, is_flag = FALSE) {
  vals <- master_df[[var_name]]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(data.frame(Variable = var_name, Total_Obs = 0, Flagged_Count = NA_integer_, Flagged_Rate = NA_real_))
  }
  count <- if (is_flag) {
    sum(vals == 1)
  } else {
    cutoff <- quantile(abs(vals), probs = 0.95, na.rm = TRUE)
    sum(abs(vals) >= cutoff, na.rm = TRUE)
  }
  data.frame(Variable = var_name, Total_Obs = length(vals), Flagged_Count = count, Flagged_Rate = count / length(vals))
}

extreme_vars <- list(
  c("DA_Jones_OLS_winsor", FALSE),
  c("DA_ModJones_OLS_winsor", FALSE),
  c("DA_PerfModJones_OLS_winsor", FALSE),
  c("DA_raw_stacked_ep_winsor", FALSE),
  c("DA_z_estimation_stacked_ep_winsor", FALSE),
  c("DA_z_predictive_stacked_ep_winsor", FALSE),
  c("DA_tail_flag_95_ep_winsor", TRUE),
  c("DA_tail_flag_98_ep_winsor", TRUE),
  c("DA_raw_stacked_rt_winsor", FALSE),
  c("DA_z_estimation_stacked_rt_winsor", FALSE),
  c("DA_z_predictive_stacked_rt_winsor", FALSE),
  c("DA_tail_flag_95_rt_winsor", TRUE),
  c("DA_tail_flag_98_rt_winsor", TRUE)
)
extreme_summary <- bind_rows(lapply(extreme_vars, function(x) flag_count(x[[1]], as.logical(x[[2]]))))
extreme_summary <- extreme_summary %>%
  mutate(
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root
  )
write.csv(extreme_summary, file.path(output_root, "tables", "table_extreme_flag_counts_winsor.csv"), row.names = FALSE)

unc_industry <- master_df %>%
  group_by(industry) %>%
  summarise(
    N = n(),
    mean_sd_predict_ep_winsor = mean(NDA_sd_predict_stacked_ep_winsor, na.rm = TRUE),
    mean_sd_epred_ep_winsor = mean(NDA_sd_epred_stacked_ep_winsor, na.rm = TRUE),
    mean_sd_predict_rt_winsor = mean(NDA_sd_predict_stacked_rt_winsor, na.rm = TRUE),
    mean_sd_epred_rt_winsor = mean(NDA_sd_epred_stacked_rt_winsor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root
  )
write.csv(unc_industry, file.path(output_root, "tables", "table_uncertainty_summary_winsor.csv"), row.names = FALSE)

corr_vars <- da_vars[da_vars %in% colnames(master_df)]
corr_data <- master_df %>% select(all_of(corr_vars)) %>% na.omit()
if (nrow(corr_data) > 1) {
  pearson <- cor(corr_data, method = "pearson")
  spearman <- cor(corr_data, method = "spearman")
  corr_long <- expand.grid(Variable_1 = rownames(pearson), Variable_2 = colnames(pearson), stringsAsFactors = FALSE)
  corr_long$Pearson <- as.vector(pearson)
  corr_long$Spearman <- as.vector(spearman)
  write.csv(corr_long, file.path(output_root, "tables", "table_benchmark_correlations_winsor.csv"), row.names = FALSE)
}

compare_measure <- function(orig_df, win_df, measure, original_col, winsor_col, flag_col_orig = NULL, flag_col_win = NULL) {
  joined <- orig_df %>%
    select(company, year, Original = all_of(original_col), Original_Flag = any_of(flag_col_orig)) %>%
    inner_join(win_df %>% select(company, year, Winsor = all_of(winsor_col), Winsor_Flag = any_of(flag_col_win)),
               by = c("company", "year")) %>%
    filter(!is.na(Original), !is.na(Winsor))
  if (nrow(joined) == 0) return(NULL)
  qo <- quantile(joined$Original, probs = c(0.90, 0.95, 0.99), na.rm = TRUE, names = FALSE)
  qw <- quantile(joined$Winsor, probs = c(0.90, 0.95, 0.99), na.rm = TRUE, names = FALSE)
  data.frame(
    Measure = measure,
    N_Overlap = nrow(joined),
    Correlation_Original_Winsor = cor(joined$Original, joined$Winsor, use = "complete.obs"),
    Mean_Original = mean(joined$Original),
    Mean_Winsor = mean(joined$Winsor),
    SD_Original = sd(joined$Original),
    SD_Winsor = sd(joined$Winsor),
    P90_Original = qo[1],
    P90_Winsor = qw[1],
    P95_Original = qo[2],
    P95_Winsor = qw[2],
    P99_Original = qo[3],
    P99_Winsor = qw[3],
    Extreme_Flag_Count_Original = if (!is.null(flag_col_orig) && "Original_Flag" %in% names(joined)) sum(joined$Original_Flag == 1, na.rm = TRUE) else NA_integer_,
    Extreme_Flag_Count_Winsor = if (!is.null(flag_col_win) && "Winsor_Flag" %in% names(joined)) sum(joined$Winsor_Flag == 1, na.rm = TRUE) else NA_integer_,
    Interpretation = "Compare sign, scale, tail mass, and overlap after 1/99 winsorization.",
    stringsAsFactors = FALSE
  )
}

orig_final_path <- file.path(baseline_root, "tables", "final_uncertainty_adjusted_accruals.csv")
comparison_df <- data.frame()
if (file.exists(orig_final_path)) {
  orig_df <- read.csv(orig_final_path, stringsAsFactors = FALSE)
  comparison_df <- bind_rows(
    compare_measure(orig_df, master_df, "DA_raw_stacked_ep", "DA_raw_stacked_ep", "DA_raw_stacked_ep_winsor", "DA_tail_flag_95_ep", "DA_tail_flag_95_ep_winsor"),
    compare_measure(orig_df, master_df, "DA_z_estimation_stacked_ep", "DA_z_estimation_stacked_ep", "DA_z_estimation_stacked_ep_winsor", "DA_tail_flag_95_ep", "DA_tail_flag_95_ep_winsor"),
    compare_measure(orig_df, master_df, "DA_z_predictive_stacked_ep", "DA_z_predictive_stacked_ep", "DA_z_predictive_stacked_ep_winsor", "DA_tail_flag_95_ep", "DA_tail_flag_95_ep_winsor"),
    compare_measure(orig_df, master_df, "DA_raw_stacked_rt", "DA_raw_stacked_rt", "DA_raw_stacked_rt_winsor", "DA_tail_flag_95_rt", "DA_tail_flag_95_rt_winsor"),
    compare_measure(orig_df, master_df, "DA_z_estimation_stacked_rt", "DA_z_estimation_stacked_rt", "DA_z_estimation_stacked_rt_winsor", "DA_tail_flag_95_rt", "DA_tail_flag_95_rt_winsor"),
    compare_measure(orig_df, master_df, "DA_z_predictive_stacked_rt", "DA_z_predictive_stacked_rt", "DA_z_predictive_stacked_rt_winsor", "DA_tail_flag_95_rt", "DA_tail_flag_95_rt_winsor"),
    compare_measure(orig_df, master_df, "DA_Jones_OLS", "DA_Jones_OLS", "DA_Jones_OLS_winsor"),
    compare_measure(orig_df, master_df, "DA_ModJones_OLS", "DA_ModJones_OLS", "DA_ModJones_OLS_winsor"),
    compare_measure(orig_df, master_df, "DA_PerfModJones_OLS", "DA_PerfModJones_OLS", "DA_PerfModJones_OLS_winsor")
  )
}
write.csv(comparison_df, file.path(output_root, "tables", "table_DA_original_vs_winsor_comparison.csv"), row.names = FALSE)

sd_shrink_path <- file.path(input_winsor_root, "tables", "table_winsor_sd_shrinkage.csv")
stability_path <- file.path(output_root, "tables", "table_weight_stability_original_vs_winsor.csv")
sd_shrink <- if (file.exists(sd_shrink_path)) read.csv(sd_shrink_path, stringsAsFactors = FALSE) else data.frame()
stability <- if (file.exists(stability_path)) read.csv(stability_path, stringsAsFactors = FALSE) else data.frame()

top_ep <- w_ep_df %>% arrange(desc(Weight)) %>% slice(1)
top_rt <- w_rt_df %>% arrange(desc(Weight)) %>% slice(1)
jones_top <- top_ep$Model_ID %in% c("M01", "M02", "M03") || top_rt$Model_ID %in% c("M01", "M02", "M03")
stability_flags <- unique(stability$Headline_Stability_Flag)
headline_decision <- if (jones_top || "Unstable" %in% stability_flags) {
  "DOES_NOT_SURVIVE_WINSORIZATION"
} else if ("Partially Stable" %in% stability_flags || nrow(comparison_df) == 0) {
  "PARTIALLY_SURVIVES_WINSORIZATION"
} else {
  "SURVIVES_WINSORIZATION"
}

key_shrink <- sd_shrink %>%
  filter(Sample %in% c("Core ex-post sample", "Core no-look-ahead sample"),
         Variable %in% c("TA_scaled", "dREV_scaled", "dREC_scaled", "PPE_scaled", "CFO_curr_scaled", "ROA_lag")) %>%
  mutate(Evidence_Text = sprintf("%s/%s: %.2f%%", Sample, Variable, SD_Shrinkage_Pct))

row_count_ok <- nrow(df_ep) == nrow(read.csv(file.path(baseline_root, "tables", "final_common_ex_post_sample.csv"))) &&
  nrow(df_rt) == nrow(read.csv(file.path(baseline_root, "tables", "final_common_realtime_sample.csv")))

tail95_rt <- extreme_summary %>% filter(Variable == "DA_tail_flag_95_rt_winsor")
ols_top5 <- extreme_summary %>% filter(Variable == "DA_Jones_OLS_winsor")

decision_table <- data.frame(
  Criterion = c(
    "Was winsorization applied before z-standardization?",
    "Did row counts remain unchanged?",
    "How much did SD shrink for key variables?",
    "Did stacking top models change in ex-post space?",
    "Did stacking top models change in no-look-ahead space?",
    "Did Jones/Modified Jones become dominant after winsorization?",
    "Did posterior-tail flag rates materially change?",
    "Did DA_z distributions materially change?",
    "Is the original headline stable after winsorization?",
    "Recommended manuscript action."
  ),
  Evidence = c(
    "Phase 1b writes winsorized samples; Phase 3b/4c standardize those samples only after reading them.",
    sprintf("Ex-post N=%d, no-look-ahead N=%d; unchanged=%s.", nrow(df_ep), nrow(df_rt), row_count_ok),
    paste(head(key_shrink$Evidence_Text, 12), collapse = "; "),
    sprintf("Winsor ex-post top model: %s %s (%.4f).", top_ep$Model_ID, top_ep$Model_Name, top_ep$Weight),
    sprintf("Winsor no-look-ahead top model: %s %s (%.4f).", top_rt$Model_ID, top_rt$Model_Name, top_rt$Weight),
    ifelse(jones_top, "Jones/Modified Jones family is top-ranked in at least one stack.", "Jones/Modified Jones family is not top-ranked."),
    sprintf("Winsor RT posterior-tail 95%% count=%s; OLS top-5%% count=%s.",
            ifelse(nrow(tail95_rt) == 1, tail95_rt$Flagged_Count, NA),
            ifelse(nrow(ols_top5) == 1, ols_top5$Flagged_Count, NA)),
    ifelse(nrow(comparison_df) > 0, paste(comparison_df$Measure, round(comparison_df$Correlation_Original_Winsor, 3), collapse = "; "), "Original comparison unavailable."),
    headline_decision,
    "Revise Appendix 1 with corrected units and report winsorized robustness as conservative Reviewer Priority 1 evidence."
  ),
  Decision = c(
    "Yes",
    ifelse(row_count_ok, "Yes", "No"),
    "Document",
    ifelse(any(stability$Target_Space == "ex_post" & stability$Headline_Stability_Flag == "Unstable"), "Changed materially", "Stable or partially stable"),
    ifelse(any(stability$Target_Space == "real_time" & stability$Headline_Stability_Flag == "Unstable"), "Changed materially", "Stable or partially stable"),
    ifelse(jones_top, "Yes", "No"),
    "Compare",
    "Compare",
    headline_decision,
    "Conservative revision"
  ),
  Severity = c("High", "High", "Medium", "High", "High", "High", "Medium", "Medium", "High", "High"),
  Manuscript_Action = c(
    "State explicitly in methods.",
    "Report row-count audit.",
    "Replace Appendix 1 descriptive units with winsor audit values.",
    "Report before/after stacking weights.",
    "Report before/after stacking weights.",
    "If no, note headline is not driven by raw outliers favoring Jones/Modified Jones.",
    "Report posterior-tail rates against OLS top-5% benchmark.",
    "Report original-vs-winsor correlations and scale changes.",
    "Use final decision wording in response letter.",
    "Add robustness subsection and corrected Appendix 1."
  ),
  stringsAsFactors = FALSE
)
decision_table$Prior_Set_ID <- prior_set_id
decision_table$Likelihood_Family <- likelihood_family
decision_table$Model_Structure <- model_structure
decision_table$Output_Root <- output_root
write.csv(decision_table, file.path(output_root, "tables", "table_reviewer_priority1_winsor_decision.csv"), row.names = FALSE)

da_manifest_paths <- c(
  ep_weights_path,
  rt_weights_path,
  ep_sample_path,
  rt_sample_path,
  file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_winsor.csv"),
  baseline_accruals_path,
  file.path(output_root, "tables", "table_DA_distribution_summary_winsor.csv"),
  file.path(output_root, "tables", "table_extreme_flag_counts_winsor.csv"),
  file.path(output_root, "tables", "table_uncertainty_summary_winsor.csv"),
  file.path(output_root, "tables", "table_benchmark_correlations_winsor.csv"),
  file.path(output_root, "tables", "table_DA_original_vs_winsor_comparison.csv"),
  file.path(output_root, "tables", "table_reviewer_priority1_winsor_decision.csv")
)
script_end_time <- Sys.time()
da_io_manifest <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = as.character(script_start_time),
  End_Time = as.character(script_end_time),
  Runtime_Seconds = as.numeric(difftime(script_end_time, script_start_time, units = "secs")),
  Git_Commit = git_commit_or_na(),
  Classification = c(rep("input", 4), rep("output", length(da_manifest_paths) - 4)),
  Path = da_manifest_paths,
  Exists = file.exists(da_manifest_paths),
  Size = vapply(da_manifest_paths, file_size_or_na, numeric(1)),
  MTime = vapply(da_manifest_paths, mtime_or_na, character(1)),
  Hash = vapply(da_manifest_paths, file_hash_or_na, character(1)),
  Primary_Secondary = "secondary_psis_loo_DA",
  Gate_Decision = headline_decision,
  stringsAsFactors = FALSE
)
write.csv(da_io_manifest, file.path(output_root, "tables", "table_secondary_psis_loo_DA_io_manifest.csv"), row.names = FALSE)

notes <- c(
  "Reviewer Priority 1 winsor response notes",
  sprintf("Output root: %s", output_root),
  sprintf("Input winsor root: %s", input_winsor_root),
  sprintf("Prior set: %s; likelihood family: %s; model structure: %s", prior_set_id, likelihood_family, model_structure),
  sprintf("Stacking mixture draws: %d", stacking_mixture_draws),
  "NDA_sd_epred_stacked is estimation uncertainty, closest to the original AccForUncertaintyCode NDA_sd concept.",
  "NDA_sd_predict_stacked and posterior tail flags are posterior-predictive extensions.",
  paste("Variables winsorized when present:", paste(continuous_vars_to_winsor, collapse = ", ")),
  "Winsorization was applied before z-standardization.",
  "Binary variables NEG_CFO and NEG_EARN were not winsorized.",
  "",
  "Before/after descriptive changes for key variables are in table_winsor_before_after_descriptives.csv and table_winsor_sd_shrinkage.csv.",
  paste(head(key_shrink$Evidence_Text, 20), collapse = "\n"),
  "",
  "Appendix 1 issue: compare table_winsor_appendix1_descriptives_corrected.csv with the manuscript appendix. Decimal/unit-formatting errors should be described separately from the substantive winsorization robustness concern.",
  "Substantive winsorization concern: supported enough to warrant a winsorized robustness pipeline because scaled accrual/model variables show tail shrinkage after 1/99 caps.",
  paste("Headline findings after winsorization:", headline_decision),
  "Recommended manuscript changes: correct Appendix 1 units; state 1/99 winsorization-before-standardization robustness; report weight stability; report posterior-tail and DA_z comparison; keep claims conservative."
)
writeLines(notes, file.path(output_root, "logs", "reviewer_priority1_winsor_response_notes.txt"))
writeLines(notes, file.path(output_root, "logs", "phase5b_uncertainty_adjusted_DA_notes_winsor.txt"))

cat("\n===== WINSOR PRIORITY 1 CONSOLE SUMMARY =====\n")
cat("1. Ex-post winsorized sample N: ", nrow(df_ep), "\n", sep = "")
cat("2. No-look-ahead winsorized sample N: ", nrow(df_rt), "\n", sep = "")
cat("3. Top 5 ex-post stacking weights after winsorization:\n")
print(head(w_ep_df %>% arrange(desc(Weight)), 5), row.names = FALSE)
cat("4. Top 5 no-look-ahead stacking weights after winsorization:\n")
print(head(w_rt_df %>% arrange(desc(Weight)), 5), row.names = FALSE)
cat("5. Largest absolute weight changes relative to original:\n")
if (nrow(stability) > 0) print(head(stability %>% arrange(desc(Abs_Weight_Difference)), 5), row.names = FALSE)
cat("6. Before/after SD shrinkage for key variables:\n")
if (nrow(key_shrink) > 0) print(key_shrink %>% select(Sample, Variable, SD_Before, SD_After, SD_Shrinkage_Pct), row.names = FALSE)
cat("7. OLS top-5% flag count vs posterior-tail 95% flag count after winsorization:\n")
print(extreme_summary %>% filter(Variable %in% c("DA_Jones_OLS_winsor", "DA_tail_flag_95_rt_winsor", "DA_tail_flag_95_ep_winsor")), row.names = FALSE)
cat("8. Final Priority 1 decision: ", headline_decision, "\n", sep = "")

cat("\n[SUCCESS] Phase 5b winsor uncertainty-adjusted DA construction completed.\n")
