root <- Sys.getenv("ACCRUAL_PSIS_GATE_CHECK_ROOT", file.path("out", "interim", "winsor", "psis_reliability_gate"))
path <- file.path(root, "tables", "table_psis_reliability_gate.csv")

if (!file.exists(path)) {
  cat("Skipping PSIS reliability gate schema test; no table found:", path, "\n")
  quit(save = "no", status = 0)
}

x <- read.csv(path, nrows = 1, stringsAsFactors = FALSE)
required <- c(
  "source_script", "source_context", "model_id", "model_name", "target_space",
  "sample_group", "heterogeneity_variant", "n_obs", "max_pareto_k",
  "n_k_gt_0_7", "share_k_gt_0_7", "n_k_gt_1_0", "share_k_gt_1_0",
  "moment_match_applied", "corrected_k_above_07", "psis_reliability_status",
  "action", "reviewer_relevance"
)
missing <- setdiff(required, names(x))
if (length(missing) > 0) stop("Missing columns in ", path, ": ", paste(missing, collapse = ", "))

cat("test_psis_reliability_gate_schema.R passed\n")
