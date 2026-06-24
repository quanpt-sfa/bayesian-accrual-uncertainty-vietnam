# BRMS Worker Refactor Plan

The shared worker architecture is centralized in `scripts/ma00_setup.R`:

- `accrual_fit_worker_config()`
- `accrual_run_task_pool()`
- `accrual_task_status_blocker()`
- `accrual_sampler_config()`
- `accrual_kfold_config()`
- `accrual_simulation_runtime_config()`
- `accrual_run_profile_config("full_clean_production_5w4c")`
- `accrual_heavy_fit_stage_registry()`

## Split Stages

| original mixed stage | original script | plan stage | worker fit stage | collector stage | evidence role |
|---|---|---|---|---|---|
| `ma09` | `scripts/ma09_loo_stacking.R` | `ma09a_plan_loo_savepars_refits.R` | `ma09b_fit_loo_savepars_refits.R` | `ma09c_collect_loo_stacking.R` | PSIS/LOO secondary evidence |
| `ma12` | `scripts/ma12_grouped_kfold_firm.R` | `ma12a_plan_grouped_kfold_firm.R` | `ma12b_fit_grouped_kfold_firm_workers.R` | `ma12c_collect_grouped_kfold_firm_scores.R` | primary grouped-firm exact K-fold |
| `ma13` | `scripts/ma13_row_level_exact_kfold.R` | `ma13a_plan_row_level_exact_kfold.R` | `ma13b_fit_row_level_exact_kfold_workers.R` | `ma13c_collect_row_level_exact_kfold_scores.R` | primary row-level exact K-fold |
| `se01` | `scripts/sensitivity/se01_prior_predictive.R` | existing se01 parent plan | existing se01 prior predictive workers | existing se01 parent collect | sensitivity prior predictive gate |
| `se02` | `scripts/sensitivity/se02_refit_prior_scenarios.R` | `se02a_plan_prior_scenario_refits.R` | `se02b_fit_prior_scenario_workers.R` | `se02c_collect_prior_scenario_outputs.R` | sensitivity refits |
| `si03` | `scripts/simulation/si03_brms_leakage_confirmation.R` | `si03a_plan_brms_leakage_confirmation.R` | `si03b_fit_brms_leakage_confirmation_workers.R` | `si03c_collect_brms_leakage_confirmation.R` | simulation only |
| `si04` | `scripts/simulation/si04_brms_parameter_recovery.R` | `si04a_plan_brms_parameter_recovery.R` | `si04b_fit_brms_parameter_recovery_workers.R` | `si04c_collect_brms_parameter_recovery.R` | simulation only |
| `di08` | `scripts/diagnostics/di08_mcmc_sampler_calibration.R` | `di08a_plan_mcmc_sampler_calibration.R` | `di08b_fit_mcmc_sampler_calibration_workers.R` | `di08c_collect_mcmc_sampler_calibration.R` | diagnostic-only calibration |

## Write Ownership

Worker scripts may write only task-local fit, prediction, metadata, result, and log artifacts. Collectors own shared tables, validation scores, stacking weights, completed-run pins, global manifests, reports, and manuscript-facing outputs.

The production worker policy is defined once in `ma00_setup.R`. The production profile uses 5 model-level workers with 4 rstan cores per fit under a total active core budget of 20, but tests read those values from `accrual_run_profile_config()` rather than duplicating them.

No validation-target semantics changed. PSIS/LOO remains secondary evidence. Exact grouped firm K-fold and exact row-level K-fold remain the primary RQ1 validation evidence.
