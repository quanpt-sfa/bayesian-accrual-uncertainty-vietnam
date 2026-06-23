# Behavioral verification refactor

## Helper extraction

The patch extracted or standardized pure helper functions in `scripts/ma00_setup.R`:

- `get_lag_contiguous()`
- `get_lead_contiguous()`
- `rolling_sd_contiguous_3()`
- `winsorize_vec()`
- `winsorize_with_cutoffs()`
- `optimize_stacking_from_lpd()`
- `assert_training_factor_level_coverage()`

## ma02 refactor-only note

`scripts/ma02_build_common_sample.R` no longer defines local lag/lead helpers or inline rolling-SD loops. It now calls the shared contiguous lag/lead and rolling-SD helpers. The intended behavior is unchanged: lag/lead values are used only across contiguous years, rolling SD requires the complete `(t-2, t-1, t)` window with finite values, zero-as-missing treatment is preserved, 2015 is still excluded, metadata join checks are unchanged, and output columns/schemas are unchanged.

## Behavioral tests added

`tests/test_behavioral_core_helpers.R` uses inline synthetic data and does not read completed pipeline artifacts. It verifies:

- contiguous lag/lead behavior across year gaps;
- rolling SD behavior for contiguous windows, gaps, and missing values;
- winsorization clamping and NA preservation;
- stacking optimizer behavior for one-model, dominant-model, and symmetric cases.

`tests/test_kfold_factor_level_coverage.R` verifies the shared K-fold factor-level coverage guard for passing, missing-industry, missing-year, and optional-column cases.

`tests/test_script_header_filename_consistency.R` verifies that script headers match actual filenames and that `Author: Antigravity` no longer appears in `scripts/**/*.R`.

## K-fold factor-level coverage

`scripts/ma12_grouped_kfold_firm.R` and `scripts/ma13_row_level_exact_kfold.R` now call `assert_training_factor_level_coverage()` before K-fold brms refits. This blocks held-out folds with `industry` or `year` fixed-effect levels that are absent from the training split. The guard complements, rather than replaces, the grouped-firm industry stratification and sparse-industry checks.

## README and provenance cleanup

`README.md` now includes a concise quickstart covering dependency installation/restoration, raw-data placement, dry-run execution, main pipeline execution, and principal output locations. Script `# Script:` headers were reconciled with current filenames. Tool-name authorship was removed from script metadata and replaced with neutral project metadata where applicable. `AI_USE.md` records factual AI-assistance disclosure.

## Design preservation

This patch does not change statistical estimands, formulas, priors, likelihood families, validation target definitions, target-space semantics, RNG logic, or main pipeline ordering. The changes are helper extraction, behavioral verification, pre-fit K-fold robustness checks, and documentation/provenance cleanup.

## Tests run

Validation commands run on 2026-06-23:

```powershell
Rscript run.R --dry-run
Rscript tests/test_behavioral_core_helpers.R
Rscript tests/test_kfold_factor_level_coverage.R
Rscript tests/test_script_header_filename_consistency.R
Rscript tests/test_centralized_runtime_config_static.R
Rscript tests/test_brms_worker_refactor_static.R
Rscript tests/test_no_script_local_env_config_static.R
Rscript tests/test_chapter3_method_alignment_static.R
git diff --check
```

All listed checks passed. A broad parse sweep over `scripts/**/*.R` exposed a pre-existing truncated archive file under `scripts/archive/`; production target scripts modified by this patch parse successfully.
