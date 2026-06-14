# Prior scenarios

## Baseline prior

The baseline pipeline uses the scale-aware Student-t prior set `scale_aware_student_baseline_v1`. It is the default manuscript candidate after prior predictive checks pass.

## Tight prior

The tight scenario uses `scale_aware_student_tight_v1`. It shrinks slopes and intercepts more aggressively and tightens residual and group-level scales.

## Wide prior

The wide scenario uses `scale_aware_student_wide_v1`. It remains scale-aware but relaxes shrinkage relative to the baseline.

## Prior predictive gate logic

Script `06_prior_predictive_checks.R` computes representative prior predictive draws and assigns `PASS`, `REVIEW`, or `FAIL` based on domain thresholds for extreme `TA_scaled` draws. A `FAIL` blocks downstream fitting unless `ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE` is explicitly set. Script `07_fit_brms_named_models.R` re-checks the gate before fitting.
