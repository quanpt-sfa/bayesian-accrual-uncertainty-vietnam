# run_profiles/run_03_diagnostics_after_main_10w4c.ps1
# Standalone diagnostics profile. Requires a completed main run marker.

param(
    [string]$RepoRoot = (Resolve-Path ".").Path,
    [string]$RunRoot = "",
    [string]$RscriptPath = "Rscript",
    [switch]$DryRun,
    [switch]$Resume
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

if (!(Test-Path $RepoRoot)) { throw "RepoRoot does not exist: $RepoRoot" }
Set-Location $RepoRoot
if (!(Test-Path "run.R")) { throw "run.R not found. Run from repo root or pass -RepoRoot." }
if (!(Test-Path "scripts/ma00_setup.R")) { throw "Missing scripts/ma00_setup.R" }
if (!(Test-Path "data/raw")) { throw "Missing data/raw directory." }

if ($RscriptPath -eq "Rscript") {
    $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
    if ($null -ne $cmd) { $RscriptPath = $cmd.Source }
}

if ([string]::IsNullOrWhiteSpace($RunRoot)) {
    $pointer = "out/runs/LATEST_MAIN_10W4C_RUN_ROOT.txt"
    if (!(Test-Path $pointer)) {
        throw "RunRoot was not supplied and latest main pointer was not found: $pointer. Run run_01_main_production_10w4c.ps1 first or pass -RunRoot."
    }
    $RunRoot = (Get-Content $pointer -Raw).Trim()
}

if (!(Test-Path $RunRoot)) { throw "RunRoot does not exist: $RunRoot" }

$env:ACCRUAL_BASELINE_ROOT = "$RunRoot/interim/baseline"
$env:ACCRUAL_OUTPUT_ROOT = "$RunRoot/interim/winsor"
$env:ACCRUAL_INPUT_WINSOR_ROOT = "$RunRoot/interim/winsor"
$env:ACCRUAL_ACCRUALS_ROOT = "$RunRoot/accruals"
$env:ACCRUAL_REPORTS_ROOT = "$RunRoot/reports"
$env:ACCRUAL_LOG_ROOT = "$RunRoot/logs"
$env:ACCRUAL_METHOD_DESIGN_ROOT = "$RunRoot/manifests/method_design"

$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT "BASELINE_MA17_COMPLETE.txt"
if (-not $DryRun -and !(Test-Path $baselineMarker)) {
    throw "diagnostics pipeline requires successful main pipeline completion through ma17. Missing marker: $baselineMarker"
}

$env:ACCRUAL_SEED = "42"
$env:ACCRUAL_PRIOR_SET_ID = "scale_aware_student_baseline_v1"
$env:ACCRUAL_FAMILY = "student"
$env:ACCRUAL_MODEL_STRUCTURE = "pooled_random_intercept"
$env:ACCRUAL_RUN_HEAVY = "TRUE"
$env:ACCRUAL_DRY_RUN = if ($DryRun) { "TRUE" } else { "FALSE" }
$env:ACCRUAL_FORCE_REFIT = if ($Resume) { "FALSE" } else { "TRUE" }
$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "10"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "40"
$env:ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"

New-Item -ItemType Directory -Force "$RunRoot/logs" | Out-Null
New-Item -ItemType Directory -Force "$RunRoot/manifests" | Out-Null
$timestamp = Get-Date -Format "yyyyMMdd_HHmmss"
$consoleLog = "$RunRoot/logs/diagnostics_after_main_10w4c_$timestamp.log"

Write-Host "Branch      : diagnostics"
Write-Host "RunRoot     : $RunRoot"
Write-Host "Output root : $env:ACCRUAL_OUTPUT_ROOT"
Write-Host "Marker      : $baselineMarker"
Write-Host "Rscript     : $RscriptPath"
Write-Host "Dry run     : $DryRun"
Write-Host "Resume      : $Resume"


# Diagnostics standalone may include heavy sampler-calibration workers.
$env:ACCRUAL_DIAG_CHAINS = "4"
$env:ACCRUAL_DIAG_CORES = "4"
$env:ACCRUAL_DIAG_ITER = "4000"
$env:ACCRUAL_DIAG_WARMUP = "1500"
$env:ACCRUAL_DIAG_ADAPT_DELTA = "0.99"
$env:ACCRUAL_DIAG_MAX_TREEDEPTH = "15"
$env:ACCRUAL_DIAG_REFRESH = "0"

$argsForRun = @("run.R", "diagnostics")
if ($DryRun) { $argsForRun += "--dry-run" }

Write-Host "Running diagnostics pipeline..."
& $RscriptPath @argsForRun 2>&1 | Tee-Object -FilePath $consoleLog
$rscriptExitCode = $LASTEXITCODE
if ($rscriptExitCode -ne 0) {
    throw "diagnostics pipeline failed with exit code $rscriptExitCode. See log: $consoleLog"
}

Write-Host "diagnostics pipeline completed successfully."
Write-Host "Log: $consoleLog"
