# ma07 fit/collect parallel refactor

## What was split

`ma07` is now split into two automated stages:

- `scripts/ma07a_fit_brms_named_models.R`: fit-stage worker script for baseline/remediation brms fits.
- `scripts/ma07b_collect_brms_fit_outputs.R`: collector script for diagnostics, coefficient summaries, audit tables, draw artifacts, and baseline manifests.

The compatibility wrapper `scripts/ma07_fit_brms_named_models.R` still runs both stages in order.

## Why split fit and collect stages

The fit stage is embarrassingly parallel across independent model tasks. Shared CSVs and manifests are not safe to append from multiple workers, so workers write only task-specific artifacts:

- `models/fit_<model_key>.rds`
- `models/meta_<model_key>.csv`
- `logs/fit_<model_key>.log`

The collector is the only stage that writes shared outputs such as diagnostics tables, coefficient summaries, artifact audits, draw files, and baseline manifests.

## Worker-level parallelism versus rstan cores

There are two levels of CPU use:

- Model-level workers run independent model tasks.
- rstan `cores` parallelize chains within a single brms fit.

The total CPU budget is:

```text
workers x cores_per_fit
```

For example, `5 workers x 4 cores per fit = 20 cores`.

## Recommended server setting

```powershell
$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "5"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "24"
$env:ACCRUAL_BASELINE_CHAINS = "4"
$env:ACCRUAL_BASELINE_CORES = "4"
$env:ACCRUAL_RUN_HEAVY = "TRUE"
Rscript run.R main
```

## Why workers never write shared CSVs

Concurrent worker writes to shared CSVs can corrupt files or create nondeterministic row order. `ma07a` therefore writes only task-specific fit, metadata, and log files. The parent writes task manifest/status tables after worker results return. `ma07b` then reads the deterministic task manifest/status and fit artifacts and writes shared outputs in a fixed order.

## Resume behavior

Each fit task has a metadata CSV. If `fit_<model_key>.rds` and matching metadata exist, `ma07a` marks the task `SKIPPED_EXISTING_MATCHED_FIT`. If metadata mismatches and `ACCRUAL_FORCE_REFIT` is not true, the task is blocked. If `ACCRUAL_FORCE_REFIT=TRUE`, the task intentionally refits.

Seeds remain deterministic by task identity and original row index. Worker IDs are not used in RNG contexts or offsets.

## Phase 2 plan

The same pattern can later be applied to:

- `ma12_grouped_kfold_firm.R` -> `ma12a_plan`, `ma12b_fit`, `ma12c_score_collect`
- `ma13_row_level_exact_kfold.R` -> `ma13a_plan`, `ma13b_fit`, `ma13c_score_collect`
- `ma09_loo_stacking.R` -> `ma09a_refit_savepars`, `ma09b_compute_loo_stacking`
- `sensitivity/se02_refit_prior_scenarios.R` -> `se02a_plan`, `se02b_fit`, `se02c_collect`

Those are intentionally not implemented in this Phase 1 patch.
