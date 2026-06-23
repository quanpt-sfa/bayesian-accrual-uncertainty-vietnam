# brms worker refactor plan

## Summary

The repository now has a common worker-pool convention for independent brms fit tasks:

1. Parent code builds a deterministic task manifest.
2. Worker code fits one independent task at a time.
3. Workers write only task-level logs or task-specific artifacts.
4. Parent or collector code writes shared CSVs, manifests, gates, stacking weights, pins, and manuscript tables.
5. Seeds are task-specific and independent of worker scheduling order.

## Workerized now

`scripts/ma06_prior_predictive_checks.R` now builds `table_ma06_prior_predictive_task_manifest.csv`, runs prior predictive fit tasks through `accrual_run_task_pool()`, writes `table_ma06_prior_predictive_task_status.csv`, and then writes the existing shared prior predictive outputs in the parent process.

`scripts/sensitivity/se01_prior_predictive.R` now uses the same worker pool for sensitivity scenario x eligible formula prior predictive tasks. Workers write only task-level fit/log artifacts; parent code writes scenario prior predictive tables, aggregate gate tables, notes, and manifests.

`scripts/ma07a_fit_brms_named_models.R` was already split from `ma07b`; it now uses the shared worker helper rather than its own inline PSOCK scheduling block.

## Deferred split candidates

The following scripts contain independent brms fit workloads but also own collector behavior that should not be changed in the same patch:

| script | planned split | reason deferred |
|---|---|---|
| `scripts/ma09_loo_stacking.R` | `ma09a_refit_savepars`, `ma09b_compute_loo_stacking` | LOO and stacking outputs are shared collector products and remain secondary evidence |
| `scripts/ma12_grouped_kfold_firm.R` | `ma12a_grouped_kfold_plan`, `ma12b_grouped_kfold_fit`, `ma12c_grouped_kfold_score_collect` | exact K-fold manifests, scores, weights, and completed-run pins must stay parent/collector-only |
| `scripts/ma13_row_level_exact_kfold.R` | `ma13a_row_kfold_plan`, `ma13b_row_kfold_fit`, `ma13c_row_kfold_score_collect` | row-level validation target semantics and cache provenance are primary-method sensitive |
| `scripts/sensitivity/se02_refit_prior_scenarios.R` | `se02a_plan_prior_scenario_refits`, `se02b_fit_prior_scenario_models`, `se02c_collect_prior_scenario_outputs` | sensitivity refits, diagnostics, summaries, and stacking inputs are currently interleaved |

Simulation and diagnostic brms scripts are deferred as isolated non-production branches.

## Shared helpers

`scripts/ma00_setup.R` now provides:

- `accrual_fit_worker_config(kind, cores_per_fit, context)`
- `accrual_run_task_pool(tasks, worker_fun, parallel_cfg, export_names, packages, context)`
- `accrual_task_status_blocker(status_df, required_col, context)`

These helpers preserve the existing model-level CPU budget rule:

`total_active_cores = workers x cores_per_fit`

`validate_model_parallel_budget()` still blocks requests that exceed `ACCRUAL_TOTAL_CORE_BUDGET`.

## Windows/rstan nested parallel policy

On Windows, enabling PSOCK model-level workers while each brms/rstan fit also uses more than one core can be unstable. The pipeline now blocks that combination unless the user explicitly sets:

```powershell
$env:ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"
```

For a safer smoke run, use one rstan core per worker:

```powershell
$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "2"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "2"
$env:ACCRUAL_PRIOR_PRED_CHAINS = "2"
$env:ACCRUAL_PRIOR_PRED_CORES = "1"
$env:ACCRUAL_PRIOR_PRED_ITER = "200"
$env:ACCRUAL_PRIOR_PRED_WARMUP = "100"
Rscript scripts/ma06_prior_predictive_checks.R
```

## Recommended static checks

```powershell
Rscript run.R --dry-run
Rscript tests/test_ma07_fit_collect_refactor_static.R
Rscript tests/test_centralized_runtime_config_static.R
Rscript tests/test_chapter3_method_alignment_static.R
Rscript tests/test_brms_worker_refactor_static.R
```

No heavy brms fits were run for this refactor patch.
