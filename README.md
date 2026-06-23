# Bayesian Accrual Uncertainty Vietnam

Bayesian pipeline for uncertainty-adjusted discretionary accruals using Vietnamese firm-year data.

This repository organizes the accrual uncertainty pipeline into a reproducible root-level structure. The layout is inspired by reproducible Bayesian accruals research workflows, but it does not claim exact equivalence to any external repository.

## Repository structure

- `scripts/`: active pipeline scripts.
- `data/raw/data.xlsx`: canonical local workbook path.
- `out/`: intermediate tables, fits, diagnostics, logs, and manifests.
- `accruals/`: final DA and NDA output tables.
- `reports/`: final narrative reports and session metadata.
- `doc/`: pipeline, prior, data, and replication documentation.
- `tests/`: lightweight structural and static validation checks.

## Data location

The default data path is `data/raw/data.xlsx`. The file is copied locally and ignored by Git by default. To run against a workbook stored elsewhere, set `ACCRUAL_DATA_PATH`.

The pipeline reads `Sheet1` for firm-year observations and `Sheet2` for metadata. The metadata loader auto-detects a company-code column from common variants such as `company`, `ticker`, `code`, `Ma`, `Mã`, `Mã CK`, and `StockCode`.

## Quick start

1. Install or restore the R dependencies used by the pipeline. If a project lockfile is available, restore from it; otherwise install the required packages used by the scripts, including at least `readxl`, `dplyr`, `brms`, `loo`, `sandwich`, and `lmtest`.
2. Put the raw workbook at `data/raw/data.xlsx`, or set `ACCRUAL_DATA_PATH` to another local path.
3. Inspect the main Chapter 3 plan without running model fits:
   `Rscript run.R --dry-run`
4. Run the main Chapter 3 pipeline. Heavy model-fitting steps require explicit opt-in:
   `ACCRUAL_RUN_HEAVY=TRUE Rscript run.R main`
5. Principal outputs are written under `out/interim/winsor/tables/`, `out/interim/winsor/diagnostics/`, `accruals/`, and `reports/chapter3_methods_tables/`.

## Main Chapter 3 pipeline

`Rscript run.R` selects the `main` target by default. The main sequence is:

1. `ma01` setup and registry
2. `ma02` build common sample
3. `ma03` audit corrected COGS and INV handling
4. `ma04` define named models
5. `ma05` winsorize common samples
6. `ma06` prior predictive checks
7. `ma07` brms fits
8. `ma08` MCMC diagnostics
9. `ma09` PSIS/LOO stacking, reported as secondary to exact K-fold evidence
10. `ma10` PSIS/LOO secondary DA construction
11. `ma11` posterior predictive checks for the secondary PSIS/LOO DA
12. `ma12` grouped exact firm K-fold
13. `ma13` row-level exact K-fold
14. `ma14` primary exact-KFoldW DA construction from completed-run pins
15. `ma15` finite-output gate for exact-KFold DA
16. `ma16` primary validation on exact row-KFold DA
17. `di02` new-firm predictive integration audit as a reporting gate
18. `ma17` manuscript table export

`ma07` fits the winsorized BRMS configurations and can also backfill diagnostics from the winsorized input samples plus existing fitted `.rds` objects when `ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE`. Its diagnostics table records `N_Obs` and `N_Firms` from the input winsorized sample, not from `fit$data`, so pooled models retain correct firm counts. Pareto-k warnings do not fail `ma07`; they are carried forward as `PSIS_REVIEW_REQUIRED`, and `ma09` or exact K-fold should review those models before relying on PSIS-LOO.

Chapter 3 specifies the estimation protocol as 4 chains, 3000 iterations, 1000 warmup iterations, fixed seed 42, `adapt_delta = 0.95`, and `max_treedepth = 12`. Baseline full-sample `brms` fits, exact K-fold refits, and sensitivity refits use those defaults unless explicitly overridden and recorded in manifests. FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference. Heavy steps are skipped only with explicit warnings unless `ACCRUAL_RUN_HEAVY=TRUE`.

Execution configuration is centralized in `scripts/ma00_setup.R`. The pipeline uses one canonical seed, `ACCRUAL_SEED`, with default `42`; `accrual_base_seed()` and `accrual_seed()` return that same seed for baseline, grouped exact K-fold, row exact K-fold, sensitivity, and simulation branches; `accrual_seed_for()` derives deterministic context offsets from that same base seed; and `set_accrual_seed()` is the only helper allowed to call base `set.seed()`. Branch-specific seed env vars such as `ACCRUAL_BASELINE_SEED`, `ACCRUAL_KFOLD_FIRM_SEED`, `ACCRUAL_ROW_KFOLD_SEED`, `ACCRUAL_SENS_SEED`, and `ACCRUAL_SIM_SEED` are deprecated and blocked if they differ from `ACCRUAL_SEED`, to avoid branch-specific tuning or cherry-picking concerns. Sampler settings are read through `accrual_sampler_config()`, exact K-fold K/seed/sampler settings through `accrual_kfold_config()`, and primary model sets through `main_model_ids_for_space()`. The helper registry writes `out/manifests/method_design/execution_config_registry.csv`.

The row-vs-grouped exact K-fold comparison is primary RQ1 evidence. Script `scripts/ma10_construct_psis_loo_DA.R` remains the secondary PSIS/LOO DA constructor, including for secondary validation panels only. Script `scripts/ma14_construct_exact_kfold_DA.R` is the primary exact-KFoldW DA constructor for RQ2 and reads explicit run-root environment variables or `LATEST_COMPLETED_RUN.txt` pins, never moving `LATEST_RUN.txt`, for primary inference. Script `scripts/ma16_validate_outcomes.R` uses the row exact-KFold DA as the primary validation input and labels any PSIS/LOO validation output as secondary. `LATEST_RUN.txt` is operational only and is not valid provenance for primary inference. Script `scripts/ma15_audit_DA_finite_outputs.R` is a hard finite-output gate before RQ2/export. LOFO is a robustness branch and PSIS reliability is a secondary diagnostics branch; neither is required by the default main target. The new-firm predictive integration audit gates Firm-RE out-of-firm posterior predictive tail flags before manuscript export.

## Optional targets

- `Rscript run.R robustness`: grouped PSIS-LOFO robustness only.
- `Rscript run.R sensitivity`: prior sensitivity workflow only.
- `Rscript run.R simulation`: leakage simulation workflow only.
- `Rscript run.R diagnostics`: standalone diagnostics, including PSIS reliability and the new-firm predictive integration audit.
- `Rscript run.R all`: opt-in combined run, ordered as main, diagnostics, robustness, sensitivity, simulation.

## Sensitivity pipeline

The sensitivity sequence is:

1. `se01` prior predictive sensitivity gate
2. `se02` scenario-specific refits
3. `se03` sensitivity diagnostics
4. `se04` scenario stacking
5. `se05` scenario DA construction
6. `se06` scenario validation
7. `se07` sensitivity report

## Simulation / leakage mechanism checks

Scripts `si00`-`si04` support the RQ3 leakage-mechanism audit. `si00` is a helper module; run simulation through the simulation mode:

```powershell
Rscript run.R simulation
```

The BRMS simulation stages `si03` and `si04` are computationally heavy and are skipped unless `ACCRUAL_RUN_HEAVY=TRUE`.

## Exact K-fold and diagnostics

Script `ma12` writes `out/interim/winsor/kfold_firm/LATEST_COMPLETED_RUN.txt` only after a primary-eligible completed grouped exact K-fold refit. Script `ma13` builds an exact row-level K-fold version of the winsorized stack under `out/interim/winsor/row_exact_kfold/` and writes `out/interim/winsor/row_exact_kfold/LATEST_COMPLETED_RUN.txt` only after a full, primary-eligible completed run. Preflight, FAST_MODE, failed, and partial/filtered runs do not update completed-run pins. Scripts `ma12` and `ma13` also write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.

Script `ma14` constructs exact-KFoldW DA outputs from the completed grouped and row exact K-fold run pins, or from explicit `ACCRUAL_GROUPED_KFOLD_RUN_ROOT` and `ACCRUAL_ROW_KFOLD_RUN_ROOT` values. It refuses old or stale run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. Its primary outputs are `final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv`, `final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv`, and provenance/gate tables under `out/interim/winsor/tables/`, including file-size/mtime/hash manifests and `table_model_primary_inclusion_gate.csv`.

Primary model inclusion is explicit. Full-sample MCMC `PASS`/`OK` models may enter primary exact-KFold DA if exact-refit reliability is acceptable. `FAIL` or `LOW_RELIABILITY` models are excluded. `REVIEW`, `CAUTION`, or `PSIS_REVIEW_REQUIRED` models may remain only with the explicit `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS` decision in `table_model_primary_inclusion_gate.csv`; PSIS/LOO weights from script `09` are labelled secondary.

The canonical primary model helpers return M01-M07 for ex-post and M01, M02, M03, M07, M09 for real-time/no-lookahead. M08 and M10 remain secondary/robustness unless a documented secondary flow includes them, and M11/M12 are excluded from active primary helpers.

Script `ma15` audits numeric DA columns and writes `table_DA_finite_gate_decision.csv`. `run.R` requires this gate, the model-inclusion gate, and the new-firm predictive gate before manuscript export. Failed gates are not represented as primary RQ2 evidence. The `all` target de-duplicates `di02` so the new-firm audit is not listed or run twice.

Use preflight first to inspect fold assignment and planned tasks without fitting BRMS models:

```powershell
$env:ACCRUAL_ROW_KFOLD_PREFLIGHT_ONLY = "TRUE"
Rscript scripts/ma13_row_level_exact_kfold.R
Remove-Item Env:\ACCRUAL_ROW_KFOLD_PREFLIGHT_ONLY
```

Script `di01` is light and writes the PSIS reliability gate under `out/interim/winsor/psis_reliability_gate/`:

```powershell
Rscript scripts/diagnostics/di01_psis_reliability_gate.R
```

Script `di02` writes the new-firm predictive integration audit under `out/interim/winsor/new_firm_predictive_audit/`. In the main target, `run.R` reads its decision table before manuscript export. If suppression is required for unverified Firm-RE out-of-firm tail flags, the run stops unless `ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS=TRUE` is set for an explicitly suppressed/non-primary export.

## Outputs

- Intermediate artifacts are written under `out/interim/baseline` and `out/interim/winsor`.
- Final baseline accrual outputs are written under `accruals/baseline`.
- Final sensitivity accrual outputs are written under `accruals/sensitivity/<scenario>`.
- Final reports are written under `reports/`.

Heavy outputs, fitted objects, and local workbook data are not intended for Git commits.

## Reproducibility

This repository uses `renv` for R package reproducibility.

To restore the R environment:

```r
install.packages("renv")
renv::restore()
```
### Windows toolchain requirement

This pipeline uses `brms`/Stan models. On Windows, Stan models require a working C++ toolchain through Rtools.

For the current lockfile, the recorded R version is 4.6.0. Windows users should install Rtools 4.5, not Rtools 4.4.

After installing Rtools, open a new PowerShell session and check:

```powershell
where make
where g++
Rscript -e "Sys.which('make'); Sys.which('g++')"
Rscript -e "pkgbuild::check_build_tools(debug = TRUE)"
```

If `make` is not found, Stan/brms model compilation will fail at the prior predictive or model-fitting stages.

The heavy Bayesian stages are enabled only when:

```powershell
$env:ACCRUAL_DRY_RUN = "FALSE"
$env:ACCRUAL_RUN_HEAVY = "TRUE"
Rscript run.R main
```

## Computational requirements

The light setup, sample-building, and manifest scripts are inexpensive. The `07`, `13`, `15`, `26`, `27`, and `28` stages can be computationally expensive because they trigger Bayesian fitting, simulation fitting, or exact K-fold refits. The repository entrypoint skips those stages with explicit warnings unless `ACCRUAL_RUN_HEAVY=TRUE`. FAST_MODE is for smoke checks only and is not valid for primary RQ1/RQ2 inference.
