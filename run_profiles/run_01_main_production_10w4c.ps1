# run_profiles/run_01_main_production_10w4c.ps1
# Main-only production profile.
# Purpose: run only Rscript run.R main through ma17, then write/check BASELINE_MA17_COMPLETE marker.
# Parallel policy: 10 model-level workers x 4 rstan cores = 40 active cores.

param(
    [string]$RepoRoot = (Resolve-Path ".").Path,
    [string]$RscriptPath = "Rscript",
    [switch]$SkipPreflight
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Invoke-RscriptChecked {
    param(
        [Parameter(Mandatory = $true)] [string[]]$Arguments,
        [Parameter(Mandatory = $true)] [string]$Context
    )
    & $RscriptPath @Arguments
    $rscriptExitCode = $LASTEXITCODE
    if ($rscriptExitCode -ne 0) {
        throw "$Context failed with exit code $rscriptExitCode."
    }
}

function Invoke-TestIfExists {
    param([Parameter(Mandatory = $true)] [string]$Path)
    if (Test-Path $Path) {
        Invoke-RscriptChecked -Context $Path -Arguments @($Path)
    } else {
        Write-Warning "Skipping missing optional test: $Path"
    }
}

if (!(Test-Path $RepoRoot)) { throw "RepoRoot does not exist: $RepoRoot" }
Set-Location $RepoRoot
if (!(Test-Path "run.R")) { throw "run.R not found. Run from repo root or pass -RepoRoot." }
if (!(Test-Path "scripts/ma00_setup.R")) { throw "Missing scripts/ma00_setup.R" }
if (!(Test-Path "data/raw")) { throw "Missing data/raw directory." }

if ($RscriptPath -eq "Rscript") {
    $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
    if ($null -ne $cmd) { $RscriptPath = $cmd.Source }
}

$run_id = Get-Date -Format "yyyyMMdd_HHmmss"
$git_sha = (git rev-parse HEAD).Trim()
$git_branch = (git rev-parse --abbrev-ref HEAD).Trim()
$short_sha = $git_sha.Substring(0, 7)
$run_root = "out/runs/main_10w4c_${run_id}_$short_sha"

New-Item -ItemType Directory -Force $run_root | Out-Null
New-Item -ItemType Directory -Force "$run_root/logs" | Out-Null
New-Item -ItemType Directory -Force "$run_root/manifests" | Out-Null
New-Item -ItemType Directory -Force "out/runs" | Out-Null

$transcript_path = "$run_root/logs/powershell_transcript_main_10w4c_$run_id.txt"
$transcript_started = $false

try {
    Start-Transcript -Path $transcript_path -Force
    $transcript_started = $true

    Write-Host "RUN_ID      = $run_id"
    Write-Host "GIT_BRANCH  = $git_branch"
    Write-Host "GIT_SHA     = $git_sha"
    Write-Host "RUN_ROOT    = $run_root"
    Write-Host "Rscript     = $RscriptPath"

    # Output policy for this run.
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

    # Final-analysis identity.
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

    # Parallel policy.
    $env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
    $env:ACCRUAL_MODEL_PARALLEL_WORKERS = "10"
    $env:ACCRUAL_TOTAL_CORE_BUDGET = "40"
    $env:ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"

    # Prior predictive.
    $env:ACCRUAL_PRIOR_PRED_CHAINS = "4"
    $env:ACCRUAL_PRIOR_PRED_CORES = "4"
    $env:ACCRUAL_PRIOR_PRED_ITER = "1000"
    $env:ACCRUAL_PRIOR_PRED_WARMUP = "500"
    $env:ACCRUAL_PRIOR_PRED_REFRESH = "0"

    # Baseline full-sample brms fits.
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

    # Exact grouped firm K-fold.
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

    # Exact row-level K-fold.
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

    # Shell-level audit manifests.
    $env_manifest = Get-ChildItem Env:ACCRUAL_* | Sort-Object Name | Select-Object Name, Value
    $env_manifest_path = "$run_root/manifests/powershell_env_manifest_main_10w4c_$run_id.csv"
    $env_manifest | Export-Csv -NoTypeInformation -Encoding UTF8 $env_manifest_path

    $run_identity = [PSCustomObject]@{
        run_id = $run_id
        git_branch = $git_branch
        git_sha = $git_sha
        run_root = $run_root
        started_at = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss zzz")
        profile = "run_01_main_production_10w4c.ps1"
        branch_scope = "main only"
        parallel_policy = "10 model-level workers x 4 rstan cores = 40 active cores"
        baseline_sampler = "4 chains, 12000 iter, 4000 warmup, adapt_delta=0.99, max_treedepth=15"
        kfold_sampler = "4 chains, 12000 iter, 4000 warmup, adapt_delta=0.99, max_treedepth=15"
    }
    $run_identity_path = "$run_root/manifests/run_identity_main_10w4c_$run_id.csv"
    $run_identity | Export-Csv -NoTypeInformation -Encoding UTF8 $run_identity_path

    if (-not $SkipPreflight) {
        Write-Host "Running main-only dry-run and static tests..."
        Invoke-RscriptChecked -Context "Dry-run main pipeline plan" -Arguments @("run.R", "main", "--dry-run")
        Invoke-TestIfExists "tests/test_behavioral_core_helpers.R"
        Invoke-TestIfExists "tests/test_centralized_runtime_config_static.R"
        Invoke-TestIfExists "tests/test_repo_wide_csv_writer_static.R"
        Invoke-TestIfExists "tests/test_baseline_completion_gate_static.R"
        Invoke-TestIfExists "tests/test_ma10_safe_csv_and_profile_failfast_static.R"
        Invoke-TestIfExists "tests/test_test_config_hygiene_static.R"
        Invoke-TestIfExists "tests/test_run_profile_registry_static.R"
        Invoke-TestIfExists "tests/test_run_profile_simulation_after_main_static.R"
        Write-Host "Preflight tests passed."
    }

    Write-Host "Running MAIN pipeline only..."
    Invoke-RscriptChecked -Context "Main pipeline" -Arguments @("run.R", "main")

    $baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT "BASELINE_MA17_COMPLETE.txt"
    if (!(Test-Path $baselineMarker)) {
        throw "Main pipeline completed without BASELINE_MA17_COMPLETE marker: $baselineMarker"
    }

    $latestPointer = "out/runs/LATEST_MAIN_10W4C_RUN_ROOT.txt"
    Set-Content -Path $latestPointer -Value $run_root -Encoding UTF8

    $run_end = [PSCustomObject]@{
        run_id = $run_id
        git_branch = $git_branch
        git_sha = $git_sha
        run_root = $run_root
        ended_at = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss zzz")
        status = "MAIN_COMPLETED_THROUGH_MA17"
        baseline_marker = $baselineMarker
        latest_pointer = $latestPointer
    }
    $run_end_path = "$run_root/manifests/run_completion_main_10w4c_$run_id.csv"
    $run_end | Export-Csv -NoTypeInformation -Encoding UTF8 $run_end_path

    Write-Host "Main pipeline completed through ma17."
    Write-Host "RUN_ROOT = $run_root"
    Write-Host "Baseline marker = $baselineMarker"
    Write-Host "Latest pointer = $latestPointer"
}
finally {
    if ($transcript_started) { Stop-Transcript }
}
