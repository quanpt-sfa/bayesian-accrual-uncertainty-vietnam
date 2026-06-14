# Bayesian Accrual Uncertainty Vietnam

Bayesian pipeline for uncertainty-adjusted discretionary accruals using Vietnamese firm-year data.

This repository organizes the active v3 Bayesian accrual workflow into a reproducible root-level structure. The layout is inspired by reproducible Bayesian accruals research workflows, but it does not claim exact equivalence to any external repository.

## Repository structure

- `scripts/v3/`: active pipeline scripts.
- `data/raw/data.xlsx`: canonical local workbook path.
- `R/`: lightweight helper modules for orchestration and path access.
- `out/`: intermediate tables, fits, diagnostics, logs, and manifests.
- `accruals/`: final DA and NDA output tables.
- `reports/`: final narrative reports and session metadata.
- `doc/`: pipeline, prior, data, and replication documentation.
- `tests/`: lightweight structural and static validation checks.

## Data location

The default data path is `data/raw/data.xlsx`. The file is copied locally and ignored by Git by default. To run against a workbook stored elsewhere, set `V3_DATA_PATH`.

## Quick start

1. Install required R packages used by the v3 scripts, including at least `readxl`, `dplyr`, `brms`, `loo`, `sandwich`, and `lmtest`.
2. Keep the raw workbook at `data/raw/data.xlsx`, or set `V3_DATA_PATH` to another local path.
3. Run a dry-run orchestration:
   `Rscript run.R full`
4. Enable actual heavy computation only when ready:
   `V3_DRY_RUN=FALSE V3_RUN_HEAVY=TRUE Rscript run.R full`

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

## Sensitivity pipeline

The sensitivity sequence is:

1. `14` prior predictive sensitivity gate
2. `15` scenario-specific refits
3. `16` sensitivity diagnostics
4. `17` scenario stacking
5. `18` scenario DA construction
6. `19` scenario validation
7. `20` sensitivity report

## Outputs

- Intermediate artifacts are written under `out/interim/baseline` and `out/interim/winsor`.
- Final baseline accrual outputs are written under `accruals/baseline`.
- Final sensitivity accrual outputs are written under `accruals/sensitivity/<scenario>`.
- Final reports are written under `reports/`.

Heavy outputs, fitted objects, and local workbook data are not intended for Git commits.

## Reproducibility

No `renv.lock` was available in the source workspace, so environment setup is documented instead of pinned. If you want package lockfile reproducibility, initialize `renv` after confirming the package set for your machine.

## Computational requirements

The light setup, sample-building, and manifest scripts are inexpensive. The `07`, `13`, and `15` stages can be computationally expensive because they trigger Bayesian fitting or grouped K-fold refits. The repository entrypoint skips those stages unless `V3_RUN_HEAVY=TRUE`.
