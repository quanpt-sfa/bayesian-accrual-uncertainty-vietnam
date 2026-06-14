script_06 <- readLines("scripts/06_prior_predictive_checks.R", warn = FALSE)
script_07 <- readLines("scripts/07_fit_brms_named_models.R", warn = FALSE)

checks <- c(
  any(grepl("prior_predictive_gate_status", script_06, fixed = TRUE)),
  any(grepl("ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL", script_06, fixed = TRUE)),
  any(grepl("prior_predictive_gate_status", script_07, fixed = TRUE)),
  any(grepl("ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL", script_07, fixed = TRUE))
)

if (!all(checks)) {
  stop("Prior predictive gate logic is missing from script 06 or script 07.")
}

cat("test_prior_gate.R passed\n")
