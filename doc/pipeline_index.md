# Pipeline index

## Repository structure

- `scripts/v3/`: active v3 scripts only.
- `data/raw/data.xlsx`: canonical local workbook path.
- `out/interim/baseline`: baseline pre-winsor intermediate outputs.
- `out/interim/winsor`: winsorized intermediate outputs, fits, diagnostics, and validation artifacts.
- `out/manifests`: method-design and run manifests.
- `accruals/baseline`: final baseline DA and NDA tables.
- `accruals/sensitivity/<scenario>`: final sensitivity DA outputs.
- `reports/`: final narrative reports.

## Baseline pipeline order

1. `01_v3_setup_and_registry.R`
2. `02_v3_build_common_sample.R`
3. `03_v3_audit_cogs_inv_operating_cycle_after_fix.R`
4. `04_v3_define_named_models.R`
5. `05_v3_winsorize_common_samples.R`
6. `06_v3_prior_predictive_checks_winsor.R`
7. `07_v3_fit_brms_named_models_winsor.R`
8. `08_v3_mcmc_diagnostics_winsor.R`
9. `09_v3_loo_stacking_winsor.R`
10. `10_v3_construct_uncertainty_adjusted_DA_winsor.R`
11. `11_v3_posterior_predictive_checks_winsor.R`
12. `12_v3_lofo_stacking_winsor.R`
13. `13_v3_grouped_kfold_firm_winsor.R`
14. `21_v3_validation_on_scaleaware_student_DA.R`

## Sensitivity pipeline order

1. `14_v3_sensitivity_prior_predictive_winsor.R`
2. `15_v3_sensitivity_refit_prior_scenarios_winsor.R`
3. `16_v3_sensitivity_mcmc_diagnostics_winsor.R`
4. `17_v3_sensitivity_stacking_winsor.R`
5. `18_v3_sensitivity_construct_DA_winsor.R`
6. `19_v3_sensitivity_validation_winsor.R`
7. `20_v3_sensitivity_report_winsor.R`

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
$env:V3_DRY_RUN='FALSE'
$env:V3_RUN_HEAVY='TRUE'
Rscript run.R full
```

POSIX shell:

```bash
V3_DRY_RUN=FALSE V3_RUN_HEAVY=TRUE Rscript run.R full
```

## Heavy-step warning

The scripts below are skipped by `run.R` unless `V3_RUN_HEAVY=TRUE`:

- `scripts/v3/07_v3_fit_brms_named_models_winsor.R`
- `scripts/v3/13_v3_grouped_kfold_firm_winsor.R`
- `scripts/v3/15_v3_sensitivity_refit_prior_scenarios_winsor.R`

Manual commands:

```bash
Rscript scripts/v3/07_v3_fit_brms_named_models_winsor.R
Rscript scripts/v3/13_v3_grouped_kfold_firm_winsor.R
Rscript scripts/v3/15_v3_sensitivity_refit_prior_scenarios_winsor.R
```

## Environment variables

- `V3_DATA_PATH`: override the default workbook path (`data/raw/data.xlsx`).
- `V3_DRY_RUN`: keep expensive scripts in planning mode when supported.
- `V3_RUN_HEAVY`: allow heavy scripts to run from `run.R`.
- `V3_SENS_SCENARIO`: limit sensitivity execution to one or more scenarios.
- `V3_VALIDATION_ENGINE`: choose `row_loo`, `firm_lofo`, or `grouped_kfold` in sensitivity stacking.
- `V3_FORCE_REFIT`: force refits when cached fits already exist.
- `V3_ALLOW_PRIOR_PREDICTIVE_FAIL`: override the prior predictive gate intentionally.
- `V3_ALLOW_DIAGNOSTIC_CONFIG`: allow diagnostic-only configurations when a script checks for them.
- `V3_ALLOW_PARTIAL_REPORT`: let the sensitivity report render even when some inputs are missing.
