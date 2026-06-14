script_06 <- readLines("scripts/v3/06_v3_prior_predictive_checks_winsor.R", warn = FALSE)
script_07 <- readLines("scripts/v3/07_v3_fit_brms_named_models_winsor.R", warn = FALSE)

checks <- c(
  any(grepl("prior_predictive_gate_status", script_06, fixed = TRUE)),
  any(grepl("V3_ALLOW_PRIOR_PREDICTIVE_FAIL", script_06, fixed = TRUE)),
  any(grepl("prior_predictive_gate_status", script_07, fixed = TRUE)),
  any(grepl("V3_ALLOW_PRIOR_PREDICTIVE_FAIL", script_07, fixed = TRUE))
)

if (!all(checks)) {
  stop("Prior predictive gate logic is missing from script 06 or script 07.")
}

cat("test_prior_gate.R passed\n")
