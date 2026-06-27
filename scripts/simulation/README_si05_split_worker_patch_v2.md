# SI05 split-worker patch v2: one-screen MA07-style worker pool

This patch replaces the earlier `si05b` design that required opening multiple PowerShell windows.  The new `si05b_run_lmer_temporal_dependence_workers.R` follows the same runtime pattern used by MA07: it calls `accrual_run_task_pool()` and launches PSOCK workers from one R session when model parallelism is enabled.

## Files

Copy these files into the repository:

```text
scripts/simulation/si05_lmer_temporal_dependence_helpers.R
scripts/simulation/si05a_plan_lmer_temporal_dependence_tasks.R
scripts/simulation/si05b_run_lmer_temporal_dependence_workers.R
scripts/simulation/si05c_collect_lmer_temporal_dependence_results.R
```

Optional PowerShell runner:

```text
run_si05_one_screen_example.ps1
```

## Run from one PowerShell screen

```powershell
$env:ACCRUAL_SIM_TEMPORAL_RHO_GRID = "-0.10,-0.05,0,0.05,0.10"
$env:ACCRUAL_SIM_TEMPORAL_T_GRID = "3,7,15"
$env:ACCRUAL_SIM_TEMPORAL_SIGMA_FIRM_GRID = "0,0.1,0.3"
$env:ACCRUAL_SIM_TEMPORAL_SHOCK_DURATION_GRID = "1"
$env:ACCRUAL_SIM_TEMPORAL_REPLICATIONS = "20"

$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "10"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "10"
$env:ACCRUAL_PARALLEL_BACKEND = "base_parallel"

Rscript scripts\simulation\si05a_plan_lmer_temporal_dependence_tasks.R
Rscript scripts\simulation\si05b_run_lmer_temporal_dependence_workers.R
Rscript scripts\simulation\si05c_collect_lmer_temporal_dependence_results.R
```

## Smoke test

```powershell
$env:ACCRUAL_SIM_TEMPORAL_RHO_GRID = "-0.10,0,0.10"
$env:ACCRUAL_SIM_TEMPORAL_T_GRID = "3"
$env:ACCRUAL_SIM_TEMPORAL_SIGMA_FIRM_GRID = "0,0.3"
$env:ACCRUAL_SIM_TEMPORAL_SHOCK_DURATION_GRID = "1"
$env:ACCRUAL_SIM_TEMPORAL_REPLICATIONS = "2"

$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "2"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "2"

Rscript scripts\simulation\si05a_plan_lmer_temporal_dependence_tasks.R
Rscript scripts\simulation\si05b_run_lmer_temporal_dependence_workers.R
Rscript scripts\simulation\si05c_collect_lmer_temporal_dependence_results.R
```

## Outputs

Final outputs are still compatible with the original SI05 naming convention:

```text
OUT/simulation/lmer_temporal_dependence/tables/table_lmer_temporal_dependence_rep_results.csv
OUT/simulation/lmer_temporal_dependence/tables/table_lmer_temporal_dependence_grid_summary.csv
OUT/simulation/lmer_temporal_dependence/tables/table_si05_lmer_temporal_decision.csv
OUT/simulation/lmer_temporal_dependence/tables/table_si05_lmer_temporal_status_combined.csv
```

Workers never write to the final combined CSVs.  Each task writes to `task_artifacts/results/` and `task_artifacts/status/`; `si05c` is the only collector.

## Why v2 is safer

- One command for `si05b`, no manually managed worker windows.
- Uses the repository's existing `accrual_run_task_pool()` helper.
- Uses PSOCK workers on Windows through `parallel::makeCluster()`.
- Uses `cores_per_fit = 1`, because SI05 uses `lmer()` and should parallelize across design-cell tasks rather than nested fit-level threads.
