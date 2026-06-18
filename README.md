# Bayesian Accrual Uncertainty Vietnam

Bayesian pipeline for uncertainty-adjusted discretionary accruals using Vietnamese firm-year data.

This repository organizes the accrual uncertainty pipeline into a reproducible root-level structure. The layout is inspired by reproducible Bayesian accruals research workflows, but it does not claim exact equivalence to any external repository.

## Repository structure

- `scripts/`: active pipeline scripts.
- `data/raw/data.xlsx`: canonical local workbook path.
- `R/`: lightweight helper modules for orchestration and path access.
- `out/`: intermediate tables, fits, diagnostics, logs, and manifests.
- `accruals/`: final DA and NDA output tables.
- `reports/`: final narrative reports and session metadata.
- `doc/`: pipeline, prior, data, and replication documentation.
- `tests/`: lightweight structural and static validation checks.

## Data location

The default data path is `data/raw/data.xlsx`. The file is copied locally and ignored by Git by default. To run against a workbook stored elsewhere, set `ACCRUAL_DATA_PATH`.

The pipeline reads `Sheet1` for firm-year observations and `Sheet2` for metadata. The metadata loader auto-detects a company-code column from common variants such as `company`, `ticker`, `code`, `Ma`, `Mã`, `Mã CK`, and `StockCode`.

## Quick start

1. Install required R packages used by the pipeline scripts, including at least `readxl`, `dplyr`, `brms`, `loo`, `sandwich`, and `lmtest`.
2. Keep the raw workbook at `data/raw/data.xlsx`, or set `ACCRUAL_DATA_PATH` to another local path.
3. Run a dry-run orchestration:
   `Rscript run.R full`
4. Enable actual heavy computation only when ready:
   `ACCRUAL_DRY_RUN=FALSE ACCRUAL_RUN_HEAVY=TRUE Rscript run.R full`

## Baseline pipeline

The baseline sequence is:

1. `01` setup and registry
2. `02` build common sample
3. `03` audit corrected COGS and INV handling
4. `04` define named models
5. `05` winsorize common samples
6. `06` prior predictive checks
7. `07` brms fits
8. `08` MCMC diagnostics
9. `09` LOO stacking
10. `10` DA construction
11. `11` posterior predictive checks
12. `12` LOFO stacking
13. `13` grouped K-fold
14. `21` validation on baseline DA

Step `07` fits the winsorized BRMS configurations and can also backfill diagnostics from the winsorized input samples plus existing fitted `.rds` objects when `ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE`. Its diagnostics table records `N_Obs` and `N_Firms` from the input winsorized sample, not from `fit$data`, so pooled models retain correct firm counts. Pareto-k warnings do not fail Step `07`; they are carried forward as `PSIS_REVIEW_REQUIRED`, and Step `09` or grouped K-fold should review those models before relying on PSIS-LOO.

## Sensitivity pipeline

The sensitivity sequence is:

1. `14` prior predictive sensitivity gate
2. `15` scenario-specific refits
3. `16` sensitivity diagnostics
4. `17` scenario stacking
5. `18` scenario DA construction
6. `19` scenario validation
7. `20` sensitivity report

## Simulation / leakage mechanism checks

Scripts `23`-`27` support the RQ3 leakage-mechanism audit. Script `23` is a helper module; run scripts `24`-`27` directly or through the simulation mode:

```powershell
Rscript run.R simulation
```

The BRMS simulation stages `26` and `27` are computationally heavy and are skipped unless `ACCRUAL_RUN_HEAVY=TRUE`.

## Reviewer-final method-matching checks

Scripts `28` and `29` are reviewer-final checks for method matching and PSIS reliability.

Script `28` builds an exact row-level K-fold version of the winsorized stack under `out/interim/winsor/row_exact_kfold/`. It does not overwrite Step `13` firm-grouped K-fold outputs.

Use preflight first to inspect fold assignment and planned tasks without fitting BRMS models:

```powershell
$env:ACCRUAL_ROW_KFOLD_PREFLIGHT_ONLY = "TRUE"
Rscript scripts/28_row_level_exact_kfold.R
Remove-Item Env:\ACCRUAL_ROW_KFOLD_PREFLIGHT_ONLY
```

Run the full method-matching branch only when heavy refits are intended:

```powershell
$env:ACCRUAL_DRY_RUN = "FALSE"
$env:ACCRUAL_RUN_HEAVY = "TRUE"
Rscript run.R method_matching
```

Script `29` is light and writes the PSIS reliability gate under `out/interim/winsor/psis_reliability_gate/`:

```powershell
Rscript scripts/29_psis_reliability_gate.R
```

`Rscript run.R full` continues to mean baseline plus sensitivity. Add reviewer-final checks to `full` only when explicitly requested:

```powershell
$env:ACCRUAL_RUN_REVIEWER_FINAL = "TRUE"
$env:ACCRUAL_RUN_HEAVY = "TRUE"
Rscript run.R full
```

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
Rscript run.R full
```

## Computational requirements

The light setup, sample-building, and manifest scripts are inexpensive. The `07`, `13`, `15`, `26`, `27`, and `28` stages can be computationally expensive because they trigger Bayesian fitting, simulation fitting, or exact K-fold refits. The repository entrypoint skips those stages unless `ACCRUAL_RUN_HEAVY=TRUE`.
