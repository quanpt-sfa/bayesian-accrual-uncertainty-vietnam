# Chapter 3 Codebase Methodology Alignment Fix Report

Date: 2026-06-19

Authority file: `reports/chapter_3_method_only_reviewer_final_journal_style_transitions.md`

## Final verdict

`PASS_CODE_MATCHES_CHAPTER3_STATICALLY`

This verdict is static/lightweight only. No heavy Stan/brms fitting was run, no raw data were modified, and no empirical posterior results are claimed.

## Files changed

- `README.md`
- `doc/computational_notes.md`
- `doc/pipeline_index.md`
- `doc/prior_scenarios.md`
- `run.R`
- `scripts/ma00_setup.R`
- `scripts/ma06_prior_predictive_checks.R`
- `scripts/ma14_construct_exact_kfold_DA.R`
- `scripts/ma15_audit_DA_finite_outputs.R`
- `scripts/ma16_validate_outcomes.R`
- `scripts/ma17_export_tables_figures.R`
- `scripts/diagnostics/di02_new_firm_predictive_integration_audit.R`
- `scripts/sensitivity/se01_prior_predictive.R`
- `scripts/sensitivity/se07_report.R`
- `tests/test_chapter3_method_alignment_static.R`
- `reports/chapter3_codebase_methodology_alignment_fix_report.md`

## Inconsistencies fixed

### 1. di02 obsolete source paths

What Chapter 3 required:
Firm-RE out-of-firm posterior predictive tail quantities must integrate over a new firm effect in the matched source that generated the quantity. Evidence is source-specific, not global.

What code did before:
`di02_new_firm_predictive_integration_audit.R` inventoried obsolete pre-refactor paths such as `scripts/10_construct_uncertainty_adjusted_DA.R`, `scripts/13_grouped_kfold_firm.R`, and `scripts/29_psis_reliability_gate.R`.

What code does after:
`di02` now audits the active paths:

- `scripts/ma10_construct_psis_loo_DA.R`
- `scripts/robustness/ro01_lofo_stacking.R`
- `scripts/ma12_grouped_kfold_firm.R`
- `scripts/simulation/si03_brms_leakage_confirmation.R`
- `scripts/ma13_row_level_exact_kfold.R`
- `scripts/diagnostics/di01_psis_reliability_gate.R`

The audit remains source-role-specific and fail-safe. A vectorization bug in quantity classification was fixed so the audit completes instead of crashing.

### 2. Prior predictive threshold mismatch

What Chapter 3 required:
The baseline prior is retained only if prior predictive simulations satisfy three design gates:

- mass outside `|TA| > 1` does not exceed 5%;
- mass outside `|TA| > 2` does not exceed 1%;
- the prior predictive 1st-to-99th percentile range does not exceed three times the observed empirical 1st-to-99th percentile range unless justified.

What code did before:
`ma06` used `|TA| > 2 <= 0.005` for PASS and did not implement the Chapter 3 1st-to-99th range gate. `se01` used a separate prior predictive SD rule. `ma17` exported prior predictive acceptance with the old `0.005` threshold and a different range/median rule.

What code does after:
`scripts/ma00_setup.R` defines `chapter3_prior_predictive_thresholds()` and `classify_chapter3_prior_predictive()`. `ma06`, `se01`, and `ma17` now use the Chapter 3 thresholds. `REVIEW` remains a deterministic implementation band derived for reporting; `PASS` exactly follows the Chapter 3 gates.

### 3. Sampler protocol mismatch

What Chapter 3 required:
MCMC through `brms`/Stan uses 4 chains, 3000 iterations per chain, 1000 warmup iterations, fixed seed 42, `adapt_delta = 0.95`, and `max_treedepth = 12`.

What code did before:
Baseline full-sample and sensitivity defaults used 4000 iterations with 1000 warmup, while exact K-fold used 3000/1000. README and docs described the 4000/1000 baseline as intentional.

What code does after:
`accrual_sampler_config("baseline")`, exact grouped K-fold, exact row K-fold, and sensitivity defaults are aligned to Chapter 3. FAST_MODE remains 2 chains, 1000 iterations, 500 warmup and is still excluded from primary inference. Manifests continue to record actual settings.

### 4. Primary versus secondary evidence wording

What Chapter 3 required:
Exact row-level K-fold and exact grouped firm-level K-fold are primary RQ1 evidence. PSIS/LOO is secondary/gated. Exact-KFoldW DA is primary for RQ2. LOFO, sensitivity, and simulation are opt-in branches. New-firm predictive audit is a hard reporting gate.

What code/docs did before:
Several docs and log notes still used pre-refactor script numbers and stale descriptions.

What code/docs do after:
`run.R`, README, computational notes, pipeline index, and relevant script notes now consistently name the active `ma/ro/se/si/di` scripts and preserve the primary/secondary distinction.

## Heavy computation

Heavy Stan/brms fitting was skipped. No baseline, K-fold, sensitivity, or simulation model refits were run.

## Static validation commands run

```powershell
Rscript run.R --dry-run
```

Result: PASS. The dry-run plan lists the main sequence through `ma12`, `ma13`, `ma14`, `ma15`, `ma16`, `di02`, and `ma17`; no scripts were executed.

```powershell
Rscript -e "source('scripts/ma00_setup.R'); print(accrual_sampler_config('baseline')); print(accrual_kfold_config('grouped_firm')); print(accrual_kfold_config('row'))"
```

Result: PASS. Baseline, grouped K-fold, and row K-fold printed 4 chains, 3000 iterations, 1000 warmup, seed 42 for K-fold, `adapt_delta = 0.95`, and `max_treedepth = 12`.

```powershell
Rscript scripts/diagnostics/di02_new_firm_predictive_integration_audit.R
```

Result: PASS as an executable fail-safe audit. The decision was `PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES`, with source-specific verification scope. This is not a pass for empirical tail-flag reporting; it correctly preserves suppression for unverified Firm-RE out-of-firm posterior predictive quantities.

```powershell
Rscript tests/test_chapter3_method_alignment_static.R
Rscript tests/test_prior_gate.R
Rscript -e "files <- c('scripts/ma00_setup.R','scripts/ma06_prior_predictive_checks.R','scripts/sensitivity/se01_prior_predictive.R','scripts/sensitivity/se07_report.R','scripts/diagnostics/di02_new_firm_predictive_integration_audit.R','scripts/ma14_construct_exact_kfold_DA.R','scripts/ma15_audit_DA_finite_outputs.R','scripts/ma16_validate_outcomes.R','scripts/ma17_export_tables_figures.R','run.R'); invisible(lapply(files, parse)); cat('parsed', length(files), 'files\n')"
git diff --check
```

Results: PASS.

## Remaining limitations

- Static alignment does not regenerate posterior fits or update existing empirical artifacts. Existing fitted objects/manifests from older sampler settings remain historical artifacts until an explicit heavy rerun is performed.
- `di02` correctly requires suppression for currently unverified Firm-RE out-of-firm posterior predictive tail quantities. That is a reporting gate outcome, not a newly produced empirical result.
- Existing audit reports under `reports/chapter3_codebase_methodology_alignment_*` may still describe the older mismatch as historical audit findings. This fix report supersedes those findings for static code alignment.

