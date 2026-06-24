# run_profiles/run_simulation_16w4c.ps1
# Simulation-only runner for the split si01-si04c target.
# Parallel policy: 15 model-level workers x 4 rstan cores = 60 active cores.

param(
    [string]$RepoRoot = (Resolve-Path ".").Path,
    [string]$RscriptPath = "Rscript",
    [switch]$DryRun,
    [switch]$Resume,
    [switch]$AllowOversubscription
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Write-Host "=== Bayesian Accrual Simulation 15w4c Runner ==="

if (!(Test-Path $RepoRoot)) {
    throw "RepoRoot does not exist: $RepoRoot"
}

Set-Location $RepoRoot

if (!(Test-Path "run.R")) {
    throw "run.R not found. Run this script from the project root or pass -RepoRoot."
}

if (!(Test-Path "scripts\ma00_setup.R")) {
    throw "scripts\ma00_setup.R not found. This is not the expected project root."
}

if (!(Test-Path "data\raw")) {
    throw "data\raw not found. run.R requires data/raw to exist."
}

if ($RscriptPath -eq "Rscript") {
    $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
    if ($null -ne $cmd) {
        $RscriptPath = $cmd.Source
    } else {
        $candidates = Get-ChildItem "C:\Program Files\R" -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue |
            Sort-Object FullName -Descending

        if ($candidates.Count -eq 0) {
            throw "Rscript.exe not found in PATH or C:\Program Files\R. Pass -RscriptPath explicitly."
        }

        $RscriptPath = $candidates[0].FullName
    }
}

Write-Host "Rscript path     : $RscriptPath"

$cpu = Get-CimInstance Win32_Processor
$logicalCores = ($cpu | Measure-Object NumberOfLogicalProcessors -Sum).Sum
$physicalCores = ($cpu | Measure-Object NumberOfCores -Sum).Sum

Write-Host "Physical cores detected: $physicalCores"
Write-Host "Logical cores detected : $logicalCores"

$workers = 15
$coresPerFit = 4
$totalBudget = $workers * $coresPerFit

if ($logicalCores -lt $totalBudget -and -not $AllowOversubscription) {
    throw "15 workers x 4 rstan cores = 60 active cores, but this machine has only $logicalCores logical cores. Use a 60-logical-core machine, or rerun with -AllowOversubscription if you accept slower/unstable performance."
}

if ($logicalCores -lt $totalBudget -and $AllowOversubscription) {
    Write-Warning "Oversubscription allowed: requested $totalBudget active cores on $logicalCores logical cores."
}

# Heavy simulation execution.
$env:ACCRUAL_RUN_HEAVY = "TRUE"
$env:ACCRUAL_DRY_RUN = if ($DryRun) { "TRUE" } else { "FALSE" }
$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "$workers"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "$totalBudget"
$env:ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"
$env:ACCRUAL_TASK_RETRY_FAILED = "FALSE"

# Backend policy: repo currently uses brms/rstan and base PSOCK workers.
$env:ACCRUAL_BRMS_BACKEND = "rstan"
$env:ACCRUAL_PARALLEL_BACKEND = "base_parallel"

# Fresh run by default. Use -Resume after an interrupted run.
if ($Resume) {
    $env:ACCRUAL_FORCE_REFIT = "FALSE"
} else {
    $env:ACCRUAL_FORCE_REFIT = "TRUE"
}

# Production-style simulation replication settings.
$env:ACCRUAL_SIM_REPLICATIONS = "500"
$env:ACCRUAL_SIM_TEMPORAL_REPLICATIONS = "500"

# BRMS leakage simulation settings.
$env:ACCRUAL_SIM_BRMS_REPLICATIONS = "30"
$env:ACCRUAL_SIM_BRMS_CHAINS = "4"
$env:ACCRUAL_SIM_BRMS_CORES = "$coresPerFit"
$env:ACCRUAL_SIM_BRMS_ITER = "4000"
$env:ACCRUAL_SIM_BRMS_WARMUP = "1500"
$env:ACCRUAL_SIM_BRMS_ADAPT_DELTA = "0.99"
$env:ACCRUAL_SIM_BRMS_MAX_TREEDEPTH = "15"

# BRMS parameter-recovery simulation settings.
$env:ACCRUAL_SIM_RECOVERY_REPLICATIONS = "30"
$env:ACCRUAL_SIM_RECOVERY_CHAINS = "4"
$env:ACCRUAL_SIM_RECOVERY_CORES = "$coresPerFit"
$env:ACCRUAL_SIM_RECOVERY_ITER = "4000"
$env:ACCRUAL_SIM_RECOVERY_WARMUP = "1500"
$env:ACCRUAL_SIM_RECOVERY_ADAPT_DELTA = "0.99"
$env:ACCRUAL_SIM_RECOVERY_MAX_TREEDEPTH = "15"

# Keep refresh quiet for worker logs.
$env:ACCRUAL_SIM_REFRESH = "0"

New-Item -ItemType Directory -Force -Path "logs" | Out-Null
$timestamp = Get-Date -Format "yyyyMMdd_HHmmss"
$consoleLog = "logs\simulation_15w4c_$timestamp.log"

Write-Host ""
Write-Host "Configuration:"
Write-Host " Workers          : $env:ACCRUAL_MODEL_PARALLEL_WORKERS"
Write-Host " Cores per fit    : $coresPerFit"
Write-Host " Total core budget: $env:ACCRUAL_TOTAL_CORE_BUDGET"
Write-Host " Force refit      : $env:ACCRUAL_FORCE_REFIT"
Write-Host " BRMS backend     : $env:ACCRUAL_BRMS_BACKEND"
Write-Host " Parallel backend : $env:ACCRUAL_PARALLEL_BACKEND"
Write-Host " Dry run          : $DryRun"
Write-Host " Console log      : $consoleLog"
Write-Host ""

# run.R receives the target as the first positional argument. --dry-run is a separate flag.
$argsForRun = @("run.R", "simulation")
if ($DryRun) {
    $argsForRun += "--dry-run"
}

$cmdLine = '"' + $RscriptPath + '" ' + ($argsForRun -join " ") + " 2>&1"
& cmd.exe /d /s /c $cmdLine | Tee-Object -FilePath $consoleLog
$rscriptExitCode = $LASTEXITCODE

if ($rscriptExitCode -ne 0) {
    throw "Simulation pipeline failed with exit code $rscriptExitCode. See log: $consoleLog"
}

Write-Host ""
Write-Host "Simulation pipeline completed successfully."
Write-Host "Log: $consoleLog"

<#
Usage:

cd E:\Quan\bayesian-accrual-uncertainty-vietnam

.\run_profiles\run_simulation_16w4c.ps1 -DryRun
.\run_profiles\run_simulation_16w4c.ps1

If the machine has fewer than 60 logical cores but you still want to force
15 workers x 4 rstan cores:

.\run_profiles\run_simulation_16w4c.ps1 -AllowOversubscription

Oversubscription is not recommended on smaller machines. The active core request
is workers x cores_per_fit, and ma00 enforces this against ACCRUAL_TOTAL_CORE_BUDGET.
#>
