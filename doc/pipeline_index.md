# Pipeline index

## Repository structure

- `scripts/`: active pipeline scripts only.
- `data/raw/data.xlsx`: canonical local workbook path.
- `out/interim/baseline`: baseline pre-winsor intermediate outputs.
- `out/interim/winsor`: winsorized intermediate outputs, fits, diagnostics, and validation artifacts.
- `out/manifests`: method-design and run manifests.
- `accruals/baseline`: final baseline DA and NDA tables.
- `accruals/sensitivity/<scenario>`: final sensitivity DA outputs.
- `reports/`: final narrative reports.

## Baseline pipeline order

1. `01_setup_and_registry.R`
2. `02_build_common_sample.R`
3. `03_audit_cogs_inv_operating_cycle.R`
4. `04_define_named_models.R`
5. `05_winsorize_common_samples.R`
6. `06_prior_predictive_checks.R`
7. `07_fit_brms_named_models.R`
8. `08_mcmc_diagnostics.R`
9. `09_loo_stacking.R`
10. `10_construct_uncertainty_adjusted_DA.R`
11. `11_posterior_predictive_checks.R`
12. `12_lofo_stacking.R`
13. `13_grouped_kfold_firm.R`
14. `21_validation_on_scaleaware_student_DA.R`

## Sensitivity pipeline order

1. `14_sensitivity_prior_predictive.R`
2. `15_sensitivity_refit_prior_scenarios.R`
3. `16_sensitivity_mcmc_diagnostics.R`
4. `17_sensitivity_stacking.R`
5. `18_sensitivity_construct_DA.R`
6. `19_sensitivity_validation.R`
7. `20_sensitivity_report.R`

## Dry-run commands

PowerShell:

```powershell
Rscript run.R full
```

POSIX shell:

```bash
Rscript run.R full
```

## Full-run commands

PowerShell:

```powershell
$env:ACCRUAL_DRY_RUN='FALSE'
$env:ACCRUAL_RUN_HEAVY='TRUE'
Rscript run.R full
```

POSIX shell:

```bash
ACCRUAL_DRY_RUN=FALSE ACCRUAL_RUN_HEAVY=TRUE Rscript run.R full
```

## Heavy-step warning

The scripts below are skipped by `run.R` unless `ACCRUAL_RUN_HEAVY=TRUE`:

- `scripts/07_fit_brms_named_models.R`
- `scripts/13_grouped_kfold_firm.R`
- `scripts/15_sensitivity_refit_prior_scenarios.R`

Manual commands:

```bash
Rscript scripts/07_fit_brms_named_models.R
Rscript scripts/13_grouped_kfold_firm.R
Rscript scripts/15_sensitivity_refit_prior_scenarios.R
```

## Environment variables

- `ACCRUAL_DATA_PATH`: override the default workbook path (`data/raw/data.xlsx`).
- `ACCRUAL_DRY_RUN`: keep expensive scripts in planning mode when supported.
- `ACCRUAL_RUN_HEAVY`: allow heavy scripts to run from `run.R`.
- `ACCRUAL_SENS_SCENARIO`: limit sensitivity execution to one or more scenarios.
- `ACCRUAL_VALIDATION_ENGINE`: choose `row_loo`, `firm_lofo`, or `grouped_kfold` in sensitivity stacking.
- `ACCRUAL_FORCE_REFIT`: force refits when cached fits already exist.
- `ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL`: override the prior predictive gate intentionally.
- `ACCRUAL_ALLOW_DIAGNOSTIC_CONFIG`: allow diagnostic-only configurations when a script checks for them.
- `ACCRUAL_ALLOW_PARTIAL_REPORT`: let the sensitivity report render even when some inputs are missing.
