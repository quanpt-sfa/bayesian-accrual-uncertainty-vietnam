# rstan cores configuration patch

This patch makes brms/rstan between-chain parallelization explicit and configurable without changing the statistical design.

## Files changed

- `scripts/ma00_setup.R`: added `cores` to sampler profiles, added `validate_rstan_cores()`, and recorded cores in the execution config registry.
- `scripts/ma06_prior_predictive_checks.R`: passes explicit `cores` to prior-predictive brms fits.
- `scripts/ma07_fit_brms_named_models.R`: passes baseline/remediation `cores` into baseline brms fits.
- `scripts/ma09_loo_stacking.R`: passes baseline `cores` into save-pars LOO refits.
- `scripts/ma12_grouped_kfold_firm.R`: uses grouped K-fold config `cores`.
- `scripts/ma13_row_level_exact_kfold.R`: uses row K-fold config `cores`.
- `scripts/sensitivity/se01_prior_predictive.R`: passes explicit sensitivity prior-predictive `cores`.
- `scripts/sensitivity/se02_refit_prior_scenarios.R`: passes sensitivity config `cores`.
- `scripts/simulation/si03_brms_leakage_confirmation.R`: defaults simulation brms cores to the active chain count through `ACCRUAL_SIM_CORES`.
- `scripts/simulation/si04_brms_parameter_recovery.R`: defaults simulation recovery cores to the active chain count through `ACCRUAL_SIM_CORES`.
- `scripts/diagnostics/di08_mcmc_sampler_calibration.R`: passes explicit calibration `cores`.
- `tests/test_chapter3_method_alignment_static.R`: adds static checks for sampler cores and brm call propagation.

## New environment variables

- `ACCRUAL_BASELINE_CORES`: baseline brms fits, default `ACCRUAL_BASELINE_CHAINS`.
- `ACCRUAL_REMEDIATION_CORES`: baseline remediation fits, default `ACCRUAL_REMEDIATION_CHAINS`.
- `ACCRUAL_KFOLD_FIRM_CORES`: grouped firm K-fold fits, default `ACCRUAL_KFOLD_FIRM_CHAINS`.
- `ACCRUAL_ROW_KFOLD_CORES`: row-level K-fold fits, default `ACCRUAL_ROW_KFOLD_CHAINS`.
- `ACCRUAL_SENS_CORES`: sensitivity fits, default `ACCRUAL_SENS_CHAINS`.
- `ACCRUAL_SIM_CORES`: simulation brms fits, default to each simulation script's active chain count.
- `ACCRUAL_CALIBRATION_CORES`: temporary di08 calibration fits, default to each calibration profile's chain count.

The older simulation-specific overrides `ACCRUAL_SIM_BRMS_CORES` and `ACCRUAL_SIM_RECOVERY_CORES` remain honored when set.

## Why rstan-only

The current pipeline uses brms with the rstan backend. This patch only makes rstan-style between-chain parallelization explicit through `cores = ...` in `brm()` calls. It does not change formulas, priors, likelihoods, seeds, output paths, model inclusion rules, or diagnostics gates.

## Why cores above chains is usually not useful

Under the current rstan backend, brms parallelizes across chains. If `cores > chains`, the extra workers usually have no chain work to do. The helper `validate_rstan_cores()` warns in that case but does not fail, because it is not statistically invalid.

## Why cmdstanr and threading were not added

Migrating to cmdstanr or adding `brms::threading()` changes the computational backend and would require a separate validation pass. This patch intentionally avoids both so the current protocol remains comparable to existing artifacts.

## Recommended default on the current server

```powershell
$env:ACCRUAL_BASELINE_CHAINS = "4"
$env:ACCRUAL_BASELINE_CORES = "4"
```

## Optional diagnostic experiment

```powershell
$env:ACCRUAL_BASELINE_CHAINS = "8"
$env:ACCRUAL_BASELINE_CORES = "8"
```

This increases the number of chains. It is not a pure wall-time speedup experiment because it also changes the amount of MCMC work performed.
