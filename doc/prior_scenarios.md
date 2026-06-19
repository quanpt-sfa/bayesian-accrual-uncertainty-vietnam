# Prior scenarios

## Baseline prior

The baseline pipeline uses the scale-aware Student-t prior set `scale_aware_student_baseline_v1`. It is the default manuscript candidate after prior predictive checks pass.

## Tight prior

The tight scenario uses `scale_aware_student_tight_v1`. It shrinks slopes and intercepts more aggressively and tightens residual and group-level scales.

## Wide prior

The wide scenario uses `scale_aware_student_wide_v1`. It remains scale-aware but relaxes shrinkage relative to the baseline.

## Prior predictive gate logic

Script `scripts/ma06_prior_predictive_checks.R` computes representative prior predictive draws and assigns `PASS`, `REVIEW`, or `FAIL` using the Chapter 3 gates: prior mass outside `|TA_scaled| > 1` must be no more than 5%, prior mass outside `|TA_scaled| > 2` must be no more than 1%, and the prior predictive 1st-to-99th percentile range must be no more than three times the observed 1st-to-99th percentile range. A `FAIL` blocks downstream fitting unless `ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE` is explicitly set. Script `scripts/ma07_fit_brms_named_models.R` re-checks the gate before fitting.
