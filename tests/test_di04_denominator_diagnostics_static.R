txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

di04_path <- "scripts/diagnostics/di04_denominator_diagnostics.R"
if (!file.exists(di04_path)) stop("Missing denominator diagnostic script: ", di04_path)
di04 <- txt(di04_path)

required_fragments <- c(
  "final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv",
  "final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv",
  "table_denominator_sd_mu_distribution.csv",
  "table_denominator_sd_mu_row_grouped_comparison.csv",
  "table_denominator_capped_jaccard.csv",
  "table_da_z_est_vs_z_pred_comparison.csv",
  "table_denominator_diagnostics_decision.csv",
  "table_denominator_diagnostics_io_manifest.csv",
  "denominator_diagnostics_reviewer_note.md",
  "original_denominator",
  "winsor_p01_p99",
  "winsor_p05_p95",
  "floor_p01",
  "floor_p05",
  "within_target_space_median_denominator"
)

for (fragment in required_fragments) {
  if (!grepl(fragment, di04, fixed = TRUE)) {
    stop("di04 missing denominator diagnostic fragment: ", fragment)
  }
}

forbidden_fragments <- c("brms::brm", "brm(")
for (fragment in forbidden_fragments) {
  if (grepl(fragment, di04, fixed = TRUE)) {
    stop("di04 must remain artifact-only and must not contain model fitting call: ", fragment)
  }
}

cat("test_di04_denominator_diagnostics_static.R passed\n")
