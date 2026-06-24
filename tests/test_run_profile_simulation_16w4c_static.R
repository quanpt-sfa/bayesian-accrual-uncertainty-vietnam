profile_path <- file.path("run_profiles", "run_simulation_16w4c.ps1")
if (!file.exists(profile_path)) stop("Missing simulation run profile: ", profile_path)

txt <- paste(readLines(profile_path, warn = FALSE), collapse = "\n")

required_fragments <- c(
  "[string]$RepoRoot = (Resolve-Path \".\").Path",
  "[string]$RscriptPath = \"Rscript\"",
  "Get-Command Rscript -ErrorAction SilentlyContinue",
  "Get-ChildItem \"C:\\Program Files\\R\" -Recurse -Filter Rscript.exe",
  "throw \"Rscript.exe not found in PATH or C:\\Program Files\\R. Pass -RscriptPath explicitly.\"",
  "Write-Host \"Rscript path     : $RscriptPath\"",
  "$workers = 15",
  "$coresPerFit = 4",
  "$totalBudget = $workers * $coresPerFit",
  "$env:ACCRUAL_MODEL_PARALLEL_WORKERS = \"$workers\"",
  "$env:ACCRUAL_TOTAL_CORE_BUDGET = \"$totalBudget\"",
  "$env:ACCRUAL_ALLOW_NESTED_RSTAN_CORES = \"TRUE\"",
  "$env:ACCRUAL_BRMS_BACKEND = \"rstan\"",
  "$env:ACCRUAL_PARALLEL_BACKEND = \"base_parallel\"",
  "$env:ACCRUAL_SIM_BRMS_REPLICATIONS = \"30\"",
  "$env:ACCRUAL_SIM_BRMS_CORES = \"$coresPerFit\"",
  "$env:ACCRUAL_SIM_RECOVERY_REPLICATIONS = \"30\"",
  "$env:ACCRUAL_SIM_RECOVERY_CORES = \"$coresPerFit\"",
  "$argsForRun = @(\"run.R\", \"simulation\")",
  "$previousErrorActionPreference = $ErrorActionPreference",
  "$ErrorActionPreference = \"Continue\"",
  "& $RscriptPath @argsForRun 2>&1 | Tee-Object -FilePath $consoleLog",
  "$rscriptExitCode = $LASTEXITCODE",
  "$ErrorActionPreference = $previousErrorActionPreference",
  "if ($rscriptExitCode -ne 0)"
)

for (fragment in required_fragments) {
  if (!grepl(fragment, txt, fixed = TRUE)) {
    stop("Simulation run profile missing required fragment: ", fragment)
  }
}

forbidden_fragments <- c("--target simulation", "\"--target\"", "'--target'")
hits <- forbidden_fragments[vapply(forbidden_fragments, grepl, logical(1), x = txt, fixed = TRUE)]
if (length(hits)) {
  stop("Simulation run profile must not use run.R --target syntax: ", paste(hits, collapse = ", "))
}

cat("test_run_profile_simulation_16w4c_static.R passed\n")
