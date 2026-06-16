# accrual uncertainty pipeline index

Active scripts use numeric prefixes only. No letter suffixes are used in script numbers.

| Order | Script | Role |
|---|---|---|
| 00 | `00_helpers.R` | Shared helpers and registries |
| 01 | `01_setup_and_registry.R` | Setup and model registry |
| 02 | `02_build_common_sample.R` | Build common samples |
| 03 | `03_audit_cogs_inv_operating_cycle.R` | COGS/INV audit |
| 04 | `04_define_named_models.R` | Define model formulas |
| 05 | `05_winsorize_common_samples.R` | Winsorize common samples |
| 06 | `06_prior_predictive_checks.R` | Baseline prior predictive checks |
| 07 | `07_fit_brms_named_models.R` | Baseline brms fits |
| 08 | `08_mcmc_diagnostics.R` | Baseline MCMC diagnostics |
| 09 | `09_loo_stacking.R` | Baseline LOO stacking |
| 10 | `10_construct_uncertainty_adjusted_DA.R` | Baseline uncertainty-adjusted DA |
| 11 | `11_posterior_predictive_checks.R` | Baseline posterior predictive checks |
| 12 | `12_lofo_stacking.R` | Baseline grouped PSIS-LOFO |
| 13 | `13_grouped_kfold_firm.R` | Baseline exact grouped K-fold |
| 14 | `14_sensitivity_prior_predictive.R` | Sensitivity prior predictive gate |
| 15 | `15_sensitivity_refit_prior_scenarios.R` | Sensitivity full refits by prior scenario |
| 16 | `16_sensitivity_mcmc_diagnostics.R` | Sensitivity MCMC diagnostics gate |
| 17 | `17_sensitivity_stacking.R` | Sensitivity LOO/stacking by scenario |
| 18 | `18_sensitivity_construct_DA.R` | Sensitivity DA reconstruction |
| 19 | `19_sensitivity_validation.R` | Sensitivity validation/outcome tests |
| 20 | `20_sensitivity_report.R` | Sensitivity report |
| 21 | `21_validation_on_scaleaware_student_DA.R` | Baseline validation |
| 22 | `22_reset_and_rerun_after_cogs_inv_fix.R` | Reset/orchestrator |

Sensitivity phases 14-20 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `ACCRUAL_DRY_RUN=FALSE` and the relevant phase is launched intentionally.

The machine-readable pipeline index is written to `out/manifests/method_design/pipeline_index.csv`.
