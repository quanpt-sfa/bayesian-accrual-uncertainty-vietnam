txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

di06_path <- "scripts/diagnostics/di06_temporal_dependence_robustness.R"
if (!file.exists(di06_path)) stop("Missing temporal robustness script: ", di06_path)
di06 <- txt(di06_path)

required_fragments <- c(
  "rho",
  "epsilon",
  "row_minus_grouped_firmre_premium",
  "table_temporal_dependence_replications.csv",
  "table_temporal_dependence_firmre_premium.csv",
  "table_temporal_dependence_decision.csv",
  "table_temporal_dependence_io_manifest.csv",
  "temporal_dependence_reviewer_note.md"
)

for (fragment in required_fragments) {
  if (!grepl(fragment, di06, fixed = TRUE)) {
    stop("di06 missing temporal robustness fragment: ", fragment)
  }
}

forbidden_fragments <- c("brms::brm", "brm(", "stan(")
for (fragment in forbidden_fragments) {
  if (grepl(fragment, di06, fixed = TRUE)) {
    stop("di06 must remain non-BRMS/non-Stan and must not contain: ", fragment)
  }
}

cat("test_di06_temporal_robustness_static.R passed\n")
