# Run SI05 split-worker pipeline in one PowerShell screen.
# Copy scripts/simulation/*.R into the repo first, then run from repo root.

$ErrorActionPreference = "Stop"

# ---- Simulation grid ----
$env:ACCRUAL_SIM_TEMPORAL_RHO_GRID = "-0.10,-0.05,0,0.05,0.10"
$env:ACCRUAL_SIM_TEMPORAL_T_GRID = "3,7,15"
$env:ACCRUAL_SIM_TEMPORAL_SIGMA_FIRM_GRID = "0,0.1,0.3"
$env:ACCRUAL_SIM_TEMPORAL_SHOCK_DURATION_GRID = "1"
$env:ACCRUAL_SIM_TEMPORAL_REPLICATIONS = "20"

# ---- One-screen worker pool, MA07-style ----
$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "10"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "10"
$env:ACCRUAL_PARALLEL_BACKEND = "base_parallel"

# Optional: rerun even if task result CSV already exists.
# $env:ACCRUAL_SI05_FORCE_RERUN = "TRUE"

Rscript scripts\simulation\si05a_plan_lmer_temporal_dependence_tasks.R
Rscript scripts\simulation\si05b_run_lmer_temporal_dependence_workers.R
Rscript scripts\simulation\si05c_collect_lmer_temporal_dependence_results.R
