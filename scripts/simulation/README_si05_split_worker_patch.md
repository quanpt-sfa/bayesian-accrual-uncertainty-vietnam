# SI05 split-worker patch

This patch splits the monolithic `si05_lmer_temporal_dependence_run.R` into:

1. `scripts/simulation/si05_lmer_temporal_dependence_helpers.R`
2. `scripts/simulation/si05a_plan_lmer_temporal_dependence_tasks.R`
3. `scripts/simulation/si05b_run_lmer_temporal_dependence_workers.R`
4. `scripts/simulation/si05c_collect_lmer_temporal_dependence_results.R`
5. `run_si05_split_workers_example.ps1` (optional runner)

## Design

Task granularity is one design cell:

```text
(T, sigma_firm, rho, shock_duration)
```

Each task runs all replications `rep_id = 1:R` for that cell and writes its own result CSV under:

```text
OUT/simulation/lmer_temporal_dependence/task_artifacts/results/
```

The worker never writes the final shared output table. Only `si05c` writes:

```text
OUT/simulation/lmer_temporal_dependence/tables/table_lmer_temporal_dependence_rep_results.csv
OUT/simulation/lmer_temporal_dependence/tables/table_lmer_temporal_dependence_grid_summary.csv
OUT/simulation/lmer_temporal_dependence/tables/table_si05_lmer_temporal_decision.csv
OUT/simulation/lmer_temporal_dependence/tables/table_si05_lmer_temporal_status_combined.csv
```

This avoids multiple workers overwriting the same CSV.

## Usage

Copy the four R files into `scripts/simulation/`.

From the repository root:

```powershell
$env:ACCRUAL_SIM_TEMPORAL_RHO_GRID = "-0.10,-0.05,0,0.05,0.10"
$env:ACCRUAL_SIM_TEMPORAL_T_GRID = "3,7,15"
$env:ACCRUAL_SIM_TEMPORAL_SIGMA_FIRM_GRID = "0,0.1,0.3"
$env:ACCRUAL_SIM_TEMPORAL_SHOCK_DURATION_GRID = "1"
$env:ACCRUAL_SIM_TEMPORAL_REPLICATIONS = "20"

Rscript scripts\simulation\si05a_plan_lmer_temporal_dependence_tasks.R

1..10 | ForEach-Object {
  Start-Process powershell -ArgumentList "-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", "`$env:ACCRUAL_SI05_WORKER_ID='$_'; `$env:ACCRUAL_SI05_N_WORKERS='10'; `$env:ACCRUAL_SIM_TEMPORAL_RHO_GRID='-0.10,-0.05,0,0.05,0.10'; `$env:ACCRUAL_SIM_TEMPORAL_T_GRID='3,7,15'; `$env:ACCRUAL_SIM_TEMPORAL_SIGMA_FIRM_GRID='0,0.1,0.3'; `$env:ACCRUAL_SIM_TEMPORAL_SHOCK_DURATION_GRID='1'; `$env:ACCRUAL_SIM_TEMPORAL_REPLICATIONS='20'; Rscript scripts\simulation\si05b_run_lmer_temporal_dependence_workers.R"
}

# after all worker windows finish:
Rscript scripts\simulation\si05c_collect_lmer_temporal_dependence_results.R
```

Or use the optional runner:

```powershell
powershell -ExecutionPolicy Bypass -File run_si05_split_workers_example.ps1 -Workers 10 -Replications 20
```

## Rerun behavior

Workers skip an existing task result CSV by default. To force rerun:

```powershell
$env:ACCRUAL_SI05_FORCE_RERUN = "TRUE"
```

or:

```powershell
$env:ACCRUAL_FORCE_REFIT = "TRUE"
```

## Notes

The helper retains the monolithic SI05 DGP and scoring logic:
- AR(1) residual process
- persistent shock episode
- row-level CV versus grouped-firm CV
- pooled `lm()` and Firm-RE `lmer()` scoring through `score_cv()` from `si00_helpers.R`

Seed offsets still depend on `T`, `sigma_firm`, `rho`, `shock_duration`, and `rep_id`.
