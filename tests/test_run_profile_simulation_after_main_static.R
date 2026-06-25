source("scripts/ma00_setup.R")

txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

registry <- accrual_run_profile_registry()
simulation_ids <- names(registry)[vapply(registry, function(entry) identical(entry$target, "simulation"), logical(1))]
if (!length(simulation_ids)) stop("Run-profile registry must define a current simulation profile.")
if (length(simulation_ids) != 1L) stop("Run-profile registry must define exactly one current simulation profile.")

entry <- registry[[simulation_ids]]
profile_path <- entry$profile_path
if (!file.exists(profile_path)) stop("Missing registered simulation profile: ", profile_path)

body <- txt(profile_path)

for (fragment in c(
  "[string]$RepoRoot = (Resolve-Path \".\").Path",
  "[string]$RscriptPath = \"Rscript\"",
  "Get-Command Rscript -ErrorAction SilentlyContinue",
  paste0("$pointer = \"", entry$latest_main_pointer, "\""),
  "latest main pointer was not found",
  paste0("$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT \"", entry$baseline_marker_file, "\""),
  "simulation pipeline requires successful main pipeline completion through ma17",
  paste0("$argsForRun = @(\"run.R\", \"", entry$target, "\")"),
  "& $RscriptPath @argsForRun 2>&1 | Tee-Object -FilePath $consoleLog",
  "$rscriptExitCode = $LASTEXITCODE",
  "if ($rscriptExitCode -ne 0)"
)) {
  if (!grepl(fragment, body, fixed = TRUE)) {
    stop("Simulation-after-main profile missing required fragment from current contract: ", fragment)
  }
}

for (name in names(entry$env)) {
  assignment <- paste0("$env:", name, " = \"", entry$env[[name]], "\"")
  if (!grepl(assignment, body, fixed = TRUE)) {
    stop("Simulation-after-main profile assignment differs from registry for ", name)
  }
}

forbidden_fragments <- c("--target simulation", "\"--target\"", "'--target'")
hits <- forbidden_fragments[vapply(forbidden_fragments, grepl, logical(1), x = body, fixed = TRUE)]
if (length(hits)) {
  stop("Simulation-after-main profile must not use run.R --target syntax: ",
       paste(hits, collapse = ", "))
}

cat("test_run_profile_simulation_after_main_static.R passed\n")
