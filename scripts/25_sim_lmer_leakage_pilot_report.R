# -----------------------------------------------------------------------------
# Script: 25_sim_lmer_leakage_pilot_report.R
# Purpose: Report lmer pilot results and PASS/STOP decision.
# -----------------------------------------------------------------------------

source("scripts/00_helpers.R")
source("scripts/23_sim_lmer_leakage_pilot_helpers.R")

check_sim_packages(c("dplyr", "ggplot2"))
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

root <- ensure_sim_dirs()
tables_dir <- file.path(root, "tables")
figures_dir <- file.path(root, "figures")
logs_dir <- file.path(root, "logs")

rep_path <- file.path(tables_dir, "table_lmer_leakage_pilot_rep_results.csv")
if (!file.exists(rep_path)) stop("[BLOCKER] Run scripts/24_sim_lmer_leakage_pilot_run.R first.")

results <- read.csv(rep_path, stringsAsFactors = FALSE)
summary_df <- summarise_leakage(results)
write.csv(summary_df, file.path(tables_dir, "table_lmer_leakage_pilot_grid_summary.csv"), row.names = FALSE)

dec <- pilot_decision(summary_df, metric = "mean_weight_premium")

save_heatmap <- function(metric, file_name, title) {
  p <- ggplot(summary_df, aes(x = factor(T), y = factor(sigma_firm), fill = .data[[metric]])) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", .data[[metric]])), size = 3) +
    labs(x = "T", y = "sigma_firm", fill = metric, title = title) +
    theme_minimal(base_size = 11)
  ggsave(file.path(figures_dir, file_name), p, width = 8, height = 5, dpi = 300)
}

save_heatmap("mean_weight_premium", "heatmap_weight_leakage_premium.png", "Weight premium: row CV minus firm CV")
save_heatmap("mean_elpd_premium", "heatmap_elpd_leakage_premium.png", "ELPD premium: row CV minus firm CV")
save_heatmap("prob_positive_weight_premium", "heatmap_prob_positive_weight_premium.png", "Pr(weight premium > 0)")

decision_df <- data.frame(
  decision = dec$decision,
  metric = dec$metric,
  monotone_T_rate = dec$monotone_T_rate,
  monotone_sigma_rate = dec$monotone_sigma_rate,
  high_minus_low = dec$high_minus_low,
  high_prob_positive = dec$high_prob_positive
)
write.csv(decision_df, file.path(tables_dir, "table_lmer_leakage_pilot_decision.csv"), row.names = FALSE)

notes <- c(
  "# Lmer leakage pilot decision", "",
  paste0("Decision: **", dec$decision, "**"), "",
  "The pilot compares row-level CV with grouped-firm CV for pooled and Firm-RE accrual models.",
  "The main diagnostic is the Firm-RE stacking-weight premium under row CV relative to grouped-firm CV.", "",
  "## Decision statistics", "",
  paste0("- metric: `", dec$metric, "`"),
  paste0("- monotone_T_rate: ", sprintf("%.3f", dec$monotone_T_rate)),
  paste0("- monotone_sigma_rate: ", sprintf("%.3f", dec$monotone_sigma_rate)),
  paste0("- high_minus_low: ", sprintf("%.3f", dec$high_minus_low)),
  paste0("- high_prob_positive: ", sprintf("%.3f", dec$high_prob_positive)), "",
  "PASS => run full grid. STOP => do not run full Bayesian simulation for this claim."
)
writeLines(notes, file.path(logs_dir, "lmer_leakage_pilot_decision.md"))

cat("\n[SUCCESS] Report completed.\n")
cat("Decision:", dec$decision, "\n")
