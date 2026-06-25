# run_profiles/run_full_clean_production_5w4c.ps1
# Production full clean run profile.
# Purpose: auditable full pipeline run using a fresh output root.
# Parallel policy: 5 model-level workers x 4 rstan cores = 20 active cores.
# Sampler policy: conservative final-analysis MCMC settings.

$ErrorActionPreference = "Stop"

function Invoke-RscriptChecked {
    param(
        [Parameter(Mandatory = $true)]
        [string[]]$Arguments,
        [Parameter(Mandatory = $true)]
        [string]$Context
    )

    & Rscript @Arguments
    $rscriptExitCode = $LASTEXITCODE
    if ($rscriptExitCode -ne 0) {
        throw "$Context failed with exit code $rscriptExitCode."
    }
}

# ---------------------------------------------------------------------
# 0. Root checks
# ---------------------------------------------------------------------

if (!(Test-Path "scripts/ma00_setup.R")) {
    throw "Run this script from the repository root. Missing scripts/ma00_setup.R"
}

if (!(Test-Path "data/raw")) {
    throw "Missing data/raw directory."
}

# ---------------------------------------------------------------------
# 1. Run identity
# ---------------------------------------------------------------------

$run_id = Get-Date -Format "yyyyMMdd_HHmmss"
$git_sha = git rev-parse HEAD
$git_branch = git rev-parse --abbrev-ref HEAD
$run_root = "out/runs/full_clean_${run_id}_$($git_sha.Substring(0, 7))"

New-Item -ItemType Directory -Force $run_root | Out-Null
New-Item -ItemType Directory -Force "$run_root/logs" | Out-Null
New-Item -ItemType Directory -Force "$run_root/manifests" | Out-Null

$transcript_path = "$run_root/logs/powershell_transcript_$run_id.txt"
$transcript_started = $false

try {
    Start-Transcript -Path $transcript_path -Force
    $transcript_started = $true

    Write-Host "RUN_ID      = $run_id"
    Write-Host "GIT_BRANCH  = $git_branch"
    Write-Host "GIT_SHA     = $git_sha"
    Write-Host "RUN_ROOT    = $run_root"

    # ---------------------------------------------------------------------
    # 2. Clean-root output policy
    # ---------------------------------------------------------------------

    $env:ACCRUAL_BASELINE_ROOT = "$run_root/interim/baseline"
    $env:ACCRUAL_OUTPUT_ROOT = "$run_root/interim/winsor"
    $env:ACCRUAL_INPUT_WINSOR_ROOT = "$run_root/interim/winsor"
    $env:ACCRUAL_ACCRUALS_ROOT = "$run_root/accruals"
    $env:ACCRUAL_REPORTS_ROOT = "$run_root/reports"
    $env:ACCRUAL_LOG_ROOT = "$run_root/logs"
    $env:ACCRUAL_METHOD_DESIGN_ROOT = "$run_root/manifests/method_design"

    Remove-Item Env:\ACCRUAL_GROUPED_KFOLD_RUN_ROOT -ErrorAction SilentlyContinue
    Remove-Item Env:\ACCRUAL_ROW_KFOLD_RUN_ROOT -ErrorAction SilentlyContinue
    Remove-Item Env:\ACCRUAL_KFOLD_CHECK_ROOT -ErrorAction SilentlyContinue

    # ---------------------------------------------------------------------
    # 3. Final-analysis identity
    # ---------------------------------------------------------------------

    $env:ACCRUAL_SEED = "42"
    $env:ACCRUAL_PRIOR_SET_ID = "scale_aware_student_baseline_v1"
    $env:ACCRUAL_FAMILY = "student"
    $env:ACCRUAL_MODEL_STRUCTURE = "pooled_random_intercept"

    $env:ACCRUAL_RUN_HEAVY = "TRUE"
    $env:ACCRUAL_DRY_RUN = "FALSE"
    $env:ACCRUAL_FORCE_REFIT = "TRUE"

    Remove-Item Env:\ACCRUAL_ALLOW_DIAGNOSTIC_CONFIG -ErrorAction SilentlyContinue
    Remove-Item Env:\ACCRUAL_ADOPT_LEGACY_MA07_FITS -ErrorAction SilentlyContinue
    Remove-Item Env:\ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY -ErrorAction SilentlyContinue

    # ---------------------------------------------------------------------
    # 4. Parallel policy
    # ---------------------------------------------------------------------

    $env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
    $env:ACCRUAL_MODEL_PARALLEL_WORKERS = "5"
    $env:ACCRUAL_TOTAL_CORE_BUDGET = "20"

    # Required on Windows when using model-level PSOCK workers + rstan cores_per_fit > 1.
    $env:ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"

    # ---------------------------------------------------------------------
    # 5. Prior predictive
    # ---------------------------------------------------------------------

    $env:ACCRUAL_PRIOR_PRED_CHAINS = "4"
    $env:ACCRUAL_PRIOR_PRED_CORES = "4"
    $env:ACCRUAL_PRIOR_PRED_ITER = "1000"
    $env:ACCRUAL_PRIOR_PRED_WARMUP = "500"
    $env:ACCRUAL_PRIOR_PRED_REFRESH = "0"

    # ---------------------------------------------------------------------
    # 6. Baseline full-sample brms fits
    # ---------------------------------------------------------------------

    $env:ACCRUAL_BASELINE_CHAINS = "4"
    $env:ACCRUAL_BASELINE_CORES = "4"
    $env:ACCRUAL_BASELINE_ITER = "12000"
    $env:ACCRUAL_BASELINE_WARMUP = "4000"
    $env:ACCRUAL_BASELINE_ADAPT_DELTA = "0.99"
    $env:ACCRUAL_BASELINE_MAX_TREEDEPTH = "15"
    $env:ACCRUAL_BASELINE_REFRESH = "500"

    # Remediation profile.
    $env:ACCRUAL_REMEDIATION_CHAINS = "4"
    $env:ACCRUAL_REMEDIATION_CORES = "4"
    $env:ACCRUAL_REMEDIATION_ITER = "16000"
    $env:ACCRUAL_REMEDIATION_WARMUP = "6000"
    $env:ACCRUAL_REMEDIATION_ADAPT_DELTA = "0.99"
    $env:ACCRUAL_REMEDIATION_MAX_TREEDEPTH = "15"
    $env:ACCRUAL_REMEDIATION_REFRESH = "500"

    # ---------------------------------------------------------------------
    # 7. Exact grouped firm K-fold
    # ---------------------------------------------------------------------

    $env:ACCRUAL_KFOLD_FIRM_MODE = "FULL_MODE"
    $env:ACCRUAL_KFOLD_FIRM_K = "5"
    $env:ACCRUAL_KFOLD_FIRM_CHAINS = "4"
    $env:ACCRUAL_KFOLD_FIRM_CORES = "4"
    $env:ACCRUAL_KFOLD_FIRM_ITER = "12000"
    $env:ACCRUAL_KFOLD_FIRM_WARMUP = "4000"
    $env:ACCRUAL_KFOLD_FIRM_ADAPT_DELTA = "0.99"
    $env:ACCRUAL_KFOLD_FIRM_MAX_TREEDEPTH = "15"
    $env:ACCRUAL_KFOLD_FIRM_REFRESH = "500"
    $env:ACCRUAL_KFOLD_FIRM_OVERWRITE = "TRUE"

    # ---------------------------------------------------------------------
    # 8. Exact row-level K-fold
    # ---------------------------------------------------------------------

    $env:ACCRUAL_ROW_KFOLD_MODE = "FULL_MODE"
    $env:ACCRUAL_ROW_KFOLD_K = "5"
    $env:ACCRUAL_ROW_KFOLD_CHAINS = "4"
    $env:ACCRUAL_ROW_KFOLD_CORES = "4"
    $env:ACCRUAL_ROW_KFOLD_ITER = "12000"
    $env:ACCRUAL_ROW_KFOLD_WARMUP = "4000"
    $env:ACCRUAL_ROW_KFOLD_ADAPT_DELTA = "0.99"
    $env:ACCRUAL_ROW_KFOLD_MAX_TREEDEPTH = "15"
    $env:ACCRUAL_ROW_KFOLD_REFRESH = "500"
    $env:ACCRUAL_ROW_KFOLD_OVERWRITE = "TRUE"

    # ---------------------------------------------------------------------
    # 9. Sensitivity refits
    # ---------------------------------------------------------------------

    $env:ACCRUAL_SENS_CHAINS = "4"
    $env:ACCRUAL_SENS_CORES = "4"
    $env:ACCRUAL_SENS_ITER = "12000"
    $env:ACCRUAL_SENS_WARMUP = "4000"
    $env:ACCRUAL_SENS_ADAPT_DELTA = "0.99"
    $env:ACCRUAL_SENS_MAX_TREEDEPTH = "15"
    $env:ACCRUAL_SENS_REFRESH = "500"

    # ---------------------------------------------------------------------
    # 10. Simulation / Monte Carlo
    # ---------------------------------------------------------------------

    $env:ACCRUAL_SIM_REPLICATIONS = "500"
    $env:ACCRUAL_SIM_TEMPORAL_REPLICATIONS = "500"

    $env:ACCRUAL_SIM_BRMS_REPLICATIONS = "30"
    $env:ACCRUAL_SIM_BRMS_CHAINS = "4"
    $env:ACCRUAL_SIM_BRMS_CORES = "4"
    $env:ACCRUAL_SIM_BRMS_ITER = "4000"
    $env:ACCRUAL_SIM_BRMS_WARMUP = "1500"
    $env:ACCRUAL_SIM_BRMS_ADAPT_DELTA = "0.99"
    $env:ACCRUAL_SIM_BRMS_MAX_TREEDEPTH = "15"

    $env:ACCRUAL_SIM_RECOVERY_REPLICATIONS = "30"
    $env:ACCRUAL_SIM_RECOVERY_CHAINS = "4"
    $env:ACCRUAL_SIM_RECOVERY_CORES = "4"
    $env:ACCRUAL_SIM_RECOVERY_ITER = "4000"
    $env:ACCRUAL_SIM_RECOVERY_WARMUP = "1500"
    $env:ACCRUAL_SIM_RECOVERY_ADAPT_DELTA = "0.99"
    $env:ACCRUAL_SIM_RECOVERY_MAX_TREEDEPTH = "15"

    # ---------------------------------------------------------------------
    # 11. Write shell-level audit manifest
    # ---------------------------------------------------------------------

    $env_manifest = Get-ChildItem Env:ACCRUAL_* |
        Sort-Object Name |
        Select-Object Name, Value

    $env_manifest_path = "$run_root/manifests/powershell_env_manifest_$run_id.csv"
    $env_manifest | Export-Csv -NoTypeInformation -Encoding UTF8 $env_manifest_path

    $run_identity = [PSCustomObject]@{
        run_id = $run_id
        git_branch = $git_branch
        git_sha = $git_sha
        run_root = $run_root
        started_at = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss zzz")
        profile = "run_full_clean_production_5w4c.ps1"
        parallel_policy = "5 model-level workers x 4 rstan cores = 20 active cores"
        baseline_sampler = "4 chains, 12000 iter, 4000 warmup, adapt_delta=0.99, max_treedepth=15"
        kfold_sampler = "4 chains, 12000 iter, 4000 warmup, adapt_delta=0.99, max_treedepth=15"
        sensitivity_sampler = "4 chains, 12000 iter, 4000 warmup, adapt_delta=0.99, max_treedepth=15"
    }

    $run_identity_path = "$run_root/manifests/run_identity_$run_id.csv"
    $run_identity | Export-Csv -NoTypeInformation -Encoding UTF8 $run_identity_path

    Write-Host "Environment manifest written to: $env_manifest_path"
    Write-Host "Run identity written to: $run_identity_path"

    # ---------------------------------------------------------------------
    # 12. Preflight tests
    # ---------------------------------------------------------------------

    Write-Host "Running dry-run and static/behavioral tests..."

    Invoke-RscriptChecked -Context "Dry-run pipeline plan" -Arguments @("run.R", "all", "--dry-run")

    Invoke-RscriptChecked -Context "test_behavioral_core_helpers.R" -Arguments @("tests/test_behavioral_core_helpers.R")
    Invoke-RscriptChecked -Context "test_kfold_factor_level_coverage.R" -Arguments @("tests/test_kfold_factor_level_coverage.R")
    Invoke-RscriptChecked -Context "test_script_header_filename_consistency.R" -Arguments @("tests/test_script_header_filename_consistency.R")
    Invoke-RscriptChecked -Context "test_no_script_local_env_config_static.R" -Arguments @("tests/test_no_script_local_env_config_static.R")
    Invoke-RscriptChecked -Context "test_centralized_runtime_config_static.R" -Arguments @("tests/test_centralized_runtime_config_static.R")
    Invoke-RscriptChecked -Context "test_brms_worker_refactor_static.R" -Arguments @("tests/test_brms_worker_refactor_static.R")
    Invoke-RscriptChecked -Context "test_chapter3_method_alignment_static.R" -Arguments @("tests/test_chapter3_method_alignment_static.R")
    Invoke-RscriptChecked -Context "test_run_profile_production_5w4c_static.R" -Arguments @("tests/test_run_profile_production_5w4c_static.R")
    Invoke-RscriptChecked -Context "test_test_config_hygiene_static.R" -Arguments @("tests/test_test_config_hygiene_static.R")
    Invoke-RscriptChecked -Context "test_heavy_stage_worker_coverage_static.R" -Arguments @("tests/test_heavy_stage_worker_coverage_static.R")
    Invoke-RscriptChecked -Context "test_split_fit_collect_contract_static.R" -Arguments @("tests/test_split_fit_collect_contract_static.R")
    Invoke-RscriptChecked -Context "test_run_dry_plan_split_stages_static.R" -Arguments @("tests/test_run_dry_plan_split_stages_static.R")
    Invoke-RscriptChecked -Context "test_ma10_safe_csv_and_profile_failfast_static.R" -Arguments @("tests/test_ma10_safe_csv_and_profile_failfast_static.R")
    Invoke-RscriptChecked -Context "test_repo_wide_csv_writer_static.R" -Arguments @("tests/test_repo_wide_csv_writer_static.R")
    Invoke-RscriptChecked -Context "test_baseline_completion_gate_static.R" -Arguments @("tests/test_baseline_completion_gate_static.R")

    if (Test-Path "tests/test_gitignore_artifact_hygiene_static.R") {
        Invoke-RscriptChecked -Context "test_gitignore_artifact_hygiene_static.R" -Arguments @("tests/test_gitignore_artifact_hygiene_static.R")
    }

    Write-Host "Preflight tests passed."

    # ---------------------------------------------------------------------
    # 13. Run pipeline by stages
    # ---------------------------------------------------------------------

    Write-Host "Running main pipeline..."
    Invoke-RscriptChecked -Context "Main pipeline" -Arguments @("run.R", "main")
    $baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT "BASELINE_MA17_COMPLETE.txt"
    if (!(Test-Path $baselineMarker)) {
        throw "Main pipeline completed without BASELINE_MA17_COMPLETE marker: $baselineMarker"
    }

    Write-Host "Running sensitivity pipeline..."
    Invoke-RscriptChecked -Context "Sensitivity pipeline" -Arguments @("run.R", "sensitivity")

    Write-Host "Running simulation pipeline..."
    Invoke-RscriptChecked -Context "Simulation pipeline" -Arguments @("run.R", "simulation")

    Write-Host "Running reviewer package..."
    Invoke-RscriptChecked -Context "Reviewer package" -Arguments @("run.R", "reviewer")

    # ---------------------------------------------------------------------
    # 14. End manifest
    # ---------------------------------------------------------------------

    $run_end = [PSCustomObject]@{
        run_id = $run_id
        git_branch = $git_branch
        git_sha = $git_sha
        run_root = $run_root
        ended_at = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss zzz")
        status = "COMPLETED_SCRIPT_EXIT_ZERO"
    }

    $run_end_path = "$run_root/manifests/run_completion_$run_id.csv"
    $run_end | Export-Csv -NoTypeInformation -Encoding UTF8 $run_end_path

    Write-Host "Run completed."
    Write-Host "RUN_ROOT = $run_root"
    Write-Host "Completion manifest written to: $run_end_path"
}
finally {
    if ($transcript_started) {
        Stop-Transcript
    }
}
