# Accrual Uncertainty Pipeline Index

Script lines use a two-letter prefix for their line, then a sequential number.
Execution order is defined by `run.R`, not by raw file numbers.

- `ma` = main pipeline (RQ1/RQ2), `scripts/` root, `ma00`–`ma17`
- `ro` = robustness, `scripts/robustness/`
- `se` = sensitivity (prior), `scripts/sensitivity/`
- `si` = simulation (RQ3 mechanism), `scripts/simulation/`
- `di` = diagnostics / gates, `scripts/diagnostics/`
- `scripts/archive/` = obsolete, kept on disk but git-ignored

## Main pipeline (`Rscript run.R` / target `main`)

| Order | Script | Role |
|---|---|---|
| ma00 | `ma00_setup.R` | Config, seeds, sampler controls, paths, phase logging (sourced by all lines) |
| ma01 | `ma01_setup_and_registry.R` | Model registry |
| ma02 | `ma02_build_common_sample.R` | Build firm-year samples |
| ma03 | `ma03_audit_data_integrity.R` | COGS/INV/operating-cycle data QA gate |
| ma04 | `ma04_define_named_models.R` | Formulas + space membership |
| ma05 | `ma05_winsorize_common_samples.R` | 1/99 winsorization |
| ma06 | `ma06_prior_predictive_checks.R` | Prior predictive gate |
| ma07 | `ma07_fit_brms_named_models.R` | Baseline brms fits (HEAVY) |
| ma08 | `ma08_mcmc_diagnostics.R` | MCMC diagnostics gate |
| ma09 | `ma09_loo_stacking.R` | PSIS/LOO stacking weights (secondary) |
| ma10 | `ma10_construct_psis_loo_DA.R` | Secondary PSIS/LOO uncertainty-adjusted DA |
| ma11 | `ma11_posterior_predictive_checks.R` | Posterior predictive checks |
| ma12 | `ma12_grouped_kfold_firm.R` | Grouped exact firm K-fold — PRIMARY (HEAVY) |
| ma13 | `ma13_row_level_exact_kfold.R` | Row-level exact K-fold — PRIMARY (HEAVY) |
| ma14 | `ma14_construct_exact_kfold_DA.R` | Exact-KFold primary DA (pinned provenance) |
| ma15 | `ma15_audit_DA_finite_outputs.R` | Finite-output gate |
| ma16 | `ma16_validate_outcomes.R` | Outcome validation (real_time primary) |
| (gate) | `diagnostics/di02_new_firm_predictive_integration_audit.R` | New-firm predictive gate (called by main between ma16 and ma17) |
| ma17 | `ma17_export_tables_figures.R` | Manuscript tables/figures |

## Branches

- Robustness (`run.R robustness`): `ro01_lofo_stacking.R`
- Sensitivity (`run.R sensitivity`): `se01`–`se07`
- Simulation (`run.R simulation`): `si00_helpers.R` (sourced) + `si01`–`si04`
- Diagnostics (`run.R diagnostics`): `di01_psis_reliability_gate.R`, `di02_new_firm_predictive_integration_audit.R`

## Phase logging & timing

Every numbered script calls `phase_begin("<id>", "<label>")` near its top and
`phase_end(...)` at its end (helpers defined in `ma00_setup.R`). Each phase prints
start/end/elapsed and appends a row to `out/logs/phase_runtime_log.csv` so readers
can judge per-phase computational cost.

## Sampler protocol

Baseline fits: 4 chains, 4000 iter, 1000 warmup. Exact K-fold refits: 4 chains,
3000 iter, 1000 warmup. FAST_MODE (2/1000/500) is excluded from primary inference.
One canonical seed (`ACCRUAL_SEED`, default 42) drives all RNG via `ma00_setup.R`.
