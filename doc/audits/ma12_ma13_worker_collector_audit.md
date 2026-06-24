# ma12/ma13 Exact K-fold Worker/Collector Audit

## Scope

This audit covers the exact K-fold split stages:

- `scripts/ma12a_plan_grouped_kfold_firm.R`
- `scripts/ma12b_fit_grouped_kfold_firm_workers.R`
- `scripts/ma12c_collect_grouped_kfold_firm_scores.R`
- `scripts/ma13a_plan_row_level_exact_kfold.R`
- `scripts/ma13b_fit_row_level_exact_kfold_workers.R`
- `scripts/ma13c_collect_row_level_exact_kfold_scores.R`
- shared worker/config helpers in `scripts/ma00_setup.R`
- `run.R` stage ordering

## Findings

| Question | Finding |
| --- | --- |
| Does `ma12b` dispatch task-level work through the shared worker pool? | Yes. `ma12b` uses `accrual_fit_worker_config()` and `accrual_run_task_pool()` with one task per manifest row. |
| Does `ma13b` dispatch task-level work through the shared worker pool? | Yes. `ma13b` uses the same shared worker-pool helpers with one task per manifest row. |
| Do workers use `cores_per_fit` from the manifest/config authority? | Yes. Both workers set `cores_per_fit` from `max(as.integer(tasks$cores), na.rm = TRUE)`, where `tasks$cores` is written by the planner from `ma00_setup.R`. |
| Is worker configuration visible at runtime? | Yes. `accrual_run_task_pool()` prints a `[WORKER POOL]` line with `workers`, `cores_per_fit`, `total_core_budget`, `backend`, `fit_kind`, and `context` before task dispatch. |
| Can enabled model-parallel mode silently run as a single worker? | No. `accrual_model_parallel_config()` blocks when `ACCRUAL_ENABLE_MODEL_PARALLEL=TRUE` but effective workers are one, unless `ACCRUAL_ALLOW_SINGLE_WORKER_MODEL_PARALLEL=TRUE` is explicitly set. |
| Are `ma12c` and `ma13c` true collectors? | Yes. They read task-local result artifacts, bind diagnostics and observation scores, compute deterministic summary/weight tables, and write shared outputs serially. |
| Was heavy collector work moved to workers? | No move was needed for ma12/ma13. Heavy `brms::brm()` and `brms::log_lik()` work already occurs in `ma12b`/`ma13b`; the collectors do not call `brm()`, `log_lik()`, `posterior_epred()`, `posterior_predict()`, or `loo()`. |

## Final Architecture

### Grouped-firm exact K-fold

1. `ma12a` plans fixed grouped-firm fold assignments and writes the task manifest.
2. `ma12b` fits each fold/model task through the shared worker pool. Workers write only task-local fit, result, metadata, and log artifacts.
3. `ma12c` serially validates task statuses, reads task-local results, and writes shared diagnostics, scores, and stacking-weight tables.

### Row-level exact K-fold

1. `ma13a` plans fixed row-level fold assignments and writes the task manifest.
2. `ma13b` fits each fold/model task through the shared worker pool. Workers write only task-local fit, result, metadata, and log artifacts.
3. `ma13c` serially validates task statuses, reads task-local results, and writes shared diagnostics, scores, and stacking-weight tables.

## Reuse and Provenance Guards

The worker metadata now records task-local reuse provenance including `Task_Key`, sampler controls, backend, effective seed, runtime seconds, `fit_path`, and `result_path`. Existing fits are reused only when metadata matches the current manifest; otherwise the stage blocks unless `ACCRUAL_FORCE_REFIT=TRUE`.

`ma12b` and `ma13b` also block stale manifests whose sampler settings no longer match current `ma00_setup.R` K-fold configuration, unless `ACCRUAL_ALLOW_STALE_KFOLD_MANIFEST=TRUE` is explicitly set.

## Remaining Risks

- Actual parallel speedup depends on `ACCRUAL_ENABLE_MODEL_PARALLEL=TRUE`, `ACCRUAL_MODEL_PARALLEL_WORKERS > 1`, and a valid `workers * cores_per_fit <= ACCRUAL_TOTAL_CORE_BUDGET` budget.
- On Windows, nested PSOCK workers with rstan chain parallelism require explicit `ACCRUAL_ALLOW_NESTED_RSTAN_CORES=TRUE`.
- This audit is static/lightweight; it does not run heavy brms refits.

## Validation

Static checks were added in `tests/test_ma12_ma13_worker_collector_static.R` to enforce the worker-pool contract, collector serial ownership, runtime logging, metadata provenance fragments, and `run.R` split-stage ordering.
