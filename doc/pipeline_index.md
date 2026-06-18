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
| 23 | `23_sim_lmer_leakage_pilot_helpers.R` | Simulation helper functions for leakage pilot scripts |
| 24 | `24_sim_lmer_leakage_pilot_run.R` | LMER leakage pilot simulation run |
| 25 | `25_sim_lmer_leakage_pilot_report.R` | LMER leakage pilot simulation report |
| 26 | `26_sim_brms_leakage_confirmation.R` | BRMS leakage confirmation simulation |
| 27 | `27_sim_brms_parameter_recovery.R` | BRMS parameter recovery simulation |
| 28 | `28_row_level_exact_kfold.R` | Reviewer-final exact row-level K-fold method-matching check |
| 29 | `29_psis_reliability_gate.R` | Reviewer-final PSIS reliability gate |

Sensitivity phases 14-20 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `ACCRUAL_DRY_RUN=FALSE` and the relevant phase is launched intentionally.

Sampler protocol: full-sample baseline `brms` fits use 4 chains, 4000 iterations, and 1000 warmup iterations; exact K-fold refits use 4 chains, 3000 iterations, and 1000 warmup iterations because they are repeated across validation folds and are used for method-matched validation comparisons; FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference. The baseline 4000/1000 setting is intentional, while 3000/1000 is the primary validation-refit protocol. Manifests should record actual sampler settings.

Optional artifact-level sanity checks live under `tests/`. Use `tests/test_kfold_weights_sanity.R` to inspect Step 13 exact grouped K-fold weights for a completed run. Override the default artifact root with `ACCRUAL_KFOLD_CHECK_ROOT` when needed.

Scripts 23-27 support the RQ3 leakage-mechanism checks; script 23 is a helper and is not run directly by `run.R`. Scripts 28-29 address reviewer method-matching and reliability concerns: script 28 compares exact row-level K-fold weights against Step 13 firm-grouped K-fold weights, while script 29 centralizes Pareto-k reliability evidence for PSIS-LOO and optional simulation diagnostics.

The machine-readable pipeline index is written to `out/manifests/method_design/pipeline_index.csv`.
