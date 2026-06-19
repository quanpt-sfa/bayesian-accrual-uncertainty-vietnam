# -----------------------------------------------------------------------------
# Script: 24_sim_lmer_leakage_pilot_run.R
# Purpose: Run lmer pilot grid for row-CV versus firm-CV validation.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si01", "Simulation: LMER leakage pilot run")
source("scripts/simulation/si00_helpers.R")

check_sim_packages(c("lme4", "dplyr", "ggplot2"))
suppressPackageStartupMessages({ library(dplyr); library(lme4) })

start_time <- Sys.time()
root <- ensure_sim_dirs()
tables_dir <- file.path(root, "tables")
logs_dir <- file.path(root, "logs")

t_grid <- parse_num_env("ACCRUAL_SIM_T_GRID", c(3, 7, 15))
sigma_grid <- parse_num_env("ACCRUAL_SIM_SIGMA_FIRM_GRID", c(0, 0.10, 0.30))
R <- parse_int_env("ACCRUAL_SIM_REPLICATIONS", 20)
K <- parse_int_env("ACCRUAL_SIM_K", 5)
n_firms <- parse_int_env("ACCRUAL_SIM_N_FIRMS", 200)
n_industries <- parse_int_env("ACCRUAL_SIM_N_INDUSTRIES", 10)
sigma_eps <- parse_num_env("ACCRUAL_SIM_SIGMA_EPS", 0.08)[1]

grid <- expand.grid(T = as.integer(t_grid), sigma_firm = sigma_grid, rep_id = seq_len(R), KEEP.OUT.ATTRS = FALSE)
rep_path <- file.path(tables_dir, "table_lmer_leakage_pilot_rep_results.csv")
sum_path <- file.path(tables_dir, "table_lmer_leakage_pilot_grid_summary.csv")
manifest_path <- file.path(logs_dir, "lmer_leakage_pilot_run_manifest.csv")

message("Lmer leakage pilot: ", nrow(grid), " replications across grid cells.")

out <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf("[%d/%d] T=%d sigma=%.2f rep=%d", i, nrow(grid), g$T, g$sigma_firm, g$rep_id))
  out[[i]] <- tryCatch(
    run_one_replication(g$T, g$sigma_firm, g$rep_id, K, n_firms, n_industries, sigma_eps),
    error = function(e) data.frame(
      T = g$T, sigma_firm = g$sigma_firm, sigma_eps = sigma_eps, rep_id = g$rep_id,
      n_firms = n_firms, n_obs = n_firms * g$T, K = K,
      elpd_row_pooled = NA_real_, elpd_row_firmre = NA_real_,
      elpd_group_pooled = NA_real_, elpd_group_firmre = NA_real_,
      delta_row = NA_real_, delta_group = NA_real_, elpd_leakage_premium = NA_real_,
      weight_row_pooled = NA_real_, weight_row_firmre = NA_real_,
      weight_group_pooled = NA_real_, weight_group_firmre = NA_real_,
      weight_leakage_premium = NA_real_, singular_row_firmre_folds = NA_real_,
      singular_group_firmre_folds = NA_real_, error = conditionMessage(e)
    )
  )
  if (i %% 10 == 0 || i == nrow(grid)) write.csv(dplyr::bind_rows(out[seq_len(i)]), rep_path, row.names = FALSE)
}

results <- dplyr::bind_rows(out)
write.csv(results, rep_path, row.names = FALSE)
summary_df <- summarise_leakage(results)
write.csv(summary_df, sum_path, row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))

manifest <- data.frame(
  script = "scripts/simulation/si01_lmer_pilot_run.R",
  start_time = as.character(start_time), end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(t_grid, collapse = ","), sigma_firm_grid = paste(sigma_grid, collapse = ","),
  replications = R, K = K, n_firms = n_firms, n_industries = n_industries,
  sigma_eps = sigma_eps, output_root = root,
  successful_replications = sum(is.na(results$error) | results$error == ""),
  failed_replications = sum(!(is.na(results$error) | results$error == ""))
)
write.csv(manifest, manifest_path, row.names = FALSE)

cat("\n[SUCCESS] Simulation pilot completed.\n")
cat("Results:", rep_path, "\n")
cat("Summary:", sum_path, "\n")
phase_end("si01", "Simulation: LMER leakage pilot run")
