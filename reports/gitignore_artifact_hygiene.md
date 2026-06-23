# Gitignore artifact hygiene

## Current problem

Local clean runs and ad hoc diagnostics can create generated outputs and scratch files at the repository root or under runtime output directories. Examples reported from local status included:

- `accruals/baseline/final_uncertainty_adjusted_accruals_winsor.csv`
- `audit_mcmc_gate.R`
- `benchmark.R`
- `brms_rstan_benchmark_results/`
- `check_m05_diag.R`
- `si03_sigma0_console_log.txt`

At audit time, `git status --short` was clean on this checkout, while `git status --short --ignored` showed generated/local paths under `data/raw/data.xlsx`, `out/`, `renv/library/`, `reports/`, `scripts/.Rhistory`, and `scripts/archive/`.

## Categories ignored

The `.gitignore` patch adds rules for:

- generated accrual outputs under `/accruals/`;
- clean-run output roots under `/out/runs/`;
- root-level local scratch/debug scripts: `/audit_mcmc_gate.R`, `/benchmark.R`, `/check_m05_diag.R`;
- local benchmark result folders under `/brms_rstan_benchmark_results/`;
- local console/log captures such as `/si03_sigma0_console_log.txt`, `*_console_log.txt`, `*.out`, and `*.err`;
- local temporary diagnostics under `/tmp/`, `/temp/`, and `/scratch/`.

## Reports decision

The repository already tracks selected markdown audit reports under `reports/`, but the folder also contains many generated or local manuscript/report drafts. The ignore policy therefore keeps report contents ignored with `/reports/*` to prevent generated report noise from polluting `git status`. A narrow exception is added for `reports/gitignore_artifact_hygiene.md` so this intentional hygiene report remains trackable.

Tracked report files are not removed from Git. Future intentional report additions can either add a narrow exception or be force-added after review.

## Source files that remain trackable

The hygiene rules do not ignore source trees or project documentation broadly. In particular, `.gitignore` does not contain broad rules for:

- `scripts/`
- `tests/`
- `*.R`
- `*.md`
- `README.md`
- `doc/`

## Tests run

Validation run on 2026-06-23:

```powershell
Rscript tests/test_gitignore_artifact_hygiene_static.R
git status --short
git status --ignored --short
```

The static test passed. Normal `git status --short` showed only `.gitignore`, this report, and the new static test as source-control hygiene changes.
