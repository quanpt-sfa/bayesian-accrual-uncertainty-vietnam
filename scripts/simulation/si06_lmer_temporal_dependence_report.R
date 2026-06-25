# -----------------------------------------------------------------------------
# Script: si06_lmer_temporal_dependence_report.R
# Purpose: Summarise the LMER temporal-dependence simulation mechanism.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si06", "Simulation: LMER temporal dependence report")

suppressPackageStartupMessages({
  library(dplyr)
})

root <- file.path(output_root, "simulation", "lmer_temporal_dependence")
tables_dir <- file.path(root, "tables")
figures_dir <- file.path(root, "figures")
notes_dir <- file.path(root, "notes")
for (d in c(tables_dir, figures_dir, notes_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

rep_path <- file.path(tables_dir, "table_lmer_temporal_dependence_rep_results.csv")
grid_summary_path <- file.path(tables_dir, "table_lmer_temporal_dependence_grid_summary.csv")
mechanism_path <- file.path(tables_dir, "table_temporal_dependence_mechanism_summary.csv")
figure_path <- file.path(figures_dir, "figure_temporal_dependence_weight_premium.png")
note_path <- file.path(notes_dir, "temporal_dependence_mechanism_note.md")

if (!file.exists(rep_path)) stop("[BLOCKER] Missing si05 replication results: ", rep_path)
rep <- read.csv(rep_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!nrow(rep)) stop("[BLOCKER] si05 replication results are empty: ", rep_path)

ok <- rep
if ("error" %in% names(ok)) ok <- ok[is.na(ok$error) | ok$error == "", , drop = FALSE]
if (!nrow(ok)) stop("[BLOCKER] No successful si05 replications available for report.")

num <- function(x) suppressWarnings(as.numeric(x))
ok$rho <- num(ok$rho)
ok$T <- as.integer(ok$T)
ok$sigma_firm <- num(ok$sigma_firm)
ok$shock_duration <- as.integer(ok$shock_duration)
ok$weight_premium <- num(ok$weight_premium)
ok$elpd_premium <- num(ok$elpd_premium)

mechanism <- ok %>%
  group_by(.data$T, .data$sigma_firm, .data$rho) %>%
  summarise(
    n_rep = n(),
    mean_weight_premium = mean(.data$weight_premium, na.rm = TRUE),
    median_weight_premium = stats::median(.data$weight_premium, na.rm = TRUE),
    mean_elpd_premium = mean(.data$elpd_premium, na.rm = TRUE),
    prob_positive_weight_premium = mean(.data$weight_premium > 0, na.rm = TRUE),
    mean_row_firmre_weight = mean(.data$weight_row_firmre, na.rm = TRUE),
    mean_grouped_firmre_weight = mean(.data$weight_group_firmre, na.rm = TRUE),
    mean_false_normalization_indicator = mean(.data$false_normalization_indicator, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(.data$T, .data$sigma_firm) %>%
  arrange(.data$rho, .by_group = TRUE) %>%
  mutate(
    rho0_mean_weight_premium = .data$mean_weight_premium[which.min(abs(.data$rho - 0))],
    premium_minus_rho0 = .data$mean_weight_premium - .data$rho0_mean_weight_premium
  ) %>%
  ungroup()

write_csv_safely(mechanism, mechanism_path, row.names = FALSE)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  fig <- ggplot2::ggplot(mechanism, ggplot2::aes(x = rho, y = mean_weight_premium, color = factor(T))) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ sigma_firm, labeller = ggplot2::label_both) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::labs(x = "AR(1) persistence rho", y = "Mean row-minus-grouped Firm-RE weight premium", color = "T") +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(figure_path, fig, width = 8, height = 4.5, dpi = 160)
}

rho_effect <- mechanism %>%
  group_by(.data$T, .data$sigma_firm) %>%
  summarise(max_premium_minus_rho0 = max(.data$premium_minus_rho0, na.rm = TRUE), .groups = "drop")
increases <- mean(rho_effect$max_premium_minus_rho0 > 0, na.rm = TRUE)

note <- c(
  "# Temporal-Dependence Simulation Mechanism Note",
  "",
  "This report summarises the LMER AR(1)/persistent-shock simulation from si05.",
  "",
  "The key estimand is the row-minus-grouped Firm-RE stacking weight premium. Positive values indicate that row-level validation assigns more weight to Firm-RE than grouped-firm validation.",
  "",
  paste0("Successful replications: ", nrow(ok)),
  paste0("Grid cells: ", nrow(mechanism)),
  paste0("Share of T/sigma cells where rho>0 increases the mean premium above rho=0: ", round(increases, 3)),
  "",
  "Interpretation: if the premium rises with rho, persistent within-firm shocks can make row-level validation favor Firm-RE even when grouped out-of-firm validation is more conservative."
)
writeLines(note, note_path, useBytes = TRUE)

cat("[SUCCESS] Temporal-dependence report written under ", root, "\n", sep = "")
phase_end("si06", "Simulation: LMER temporal dependence report")
