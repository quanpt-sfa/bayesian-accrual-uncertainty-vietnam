source("scripts/ma00_setup.R")

txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

registry <- accrual_run_profile_registry()
if (!length(registry)) stop("accrual_run_profile_registry() must define current production profiles.")

profile_paths <- vapply(registry, `[[`, character(1), "profile_path")
if (anyDuplicated(profile_paths)) stop("Run profile registry must not contain duplicate profile paths.")

check_static_env_assignment <- function(body, name, value, path) {
  assignment <- paste0("$env:", name, " = \"", value, "\"")
  if (!grepl(assignment, body, fixed = TRUE)) {
    stop(path, " assignment differs from ma00 run-profile registry for ", name,
         ". Expected: ", assignment)
  }
}

for (profile_id in names(registry)) {
  entry <- registry[[profile_id]]
  path <- entry$profile_path
  if (!file.exists(path)) stop("Registered run profile is missing: ", path)
  body <- txt(path)

  for (fragment in c(
    "$ErrorActionPreference = \"Stop\"",
    "$LASTEXITCODE",
    "if ($rscriptExitCode -ne 0)"
  )) {
    if (!grepl(fragment, body, fixed = TRUE)) {
      stop(path, " missing fail-fast fragment: ", fragment)
    }
  }

  direct_rscript_lines <- grep("^\\s*Rscript\\s+", strsplit(body, "\n", fixed = TRUE)[[1]],
                               value = TRUE, perl = TRUE)
  if (length(direct_rscript_lines)) {
    stop(path, " must not call Rscript directly without checking LASTEXITCODE: ",
         paste(direct_rscript_lines, collapse = " | "))
  }

  target_fragment <- if (identical(entry$target, "main")) {
    "Invoke-RscriptChecked -Context \"Main pipeline\" -Arguments @(\"run.R\", \"main\")"
  } else {
    paste0("$argsForRun = @(\"run.R\", \"", entry$target, "\")")
  }
  if (!grepl(target_fragment, body, fixed = TRUE)) {
    stop(path, " does not run registered target branch: ", entry$target)
  }

  if (!identical(entry$workers, as.integer(entry$env[["ACCRUAL_MODEL_PARALLEL_WORKERS"]]))) {
    stop(profile_id, " registry worker field must match ACCRUAL_MODEL_PARALLEL_WORKERS.")
  }
  if (!identical(entry$total_core_budget, as.integer(entry$env[["ACCRUAL_TOTAL_CORE_BUDGET"]]))) {
    stop(profile_id, " registry total_core_budget field must match ACCRUAL_TOTAL_CORE_BUDGET.")
  }
  if (entry$workers * entry$rstan_cores_per_fit != entry$total_core_budget) {
    stop(profile_id, " registry workers * rstan_cores_per_fit must equal total_core_budget.")
  }

  for (name in names(entry$env)) {
    check_static_env_assignment(body, name, entry$env[[name]], path)
  }

  marker_fragment <- paste0("\"", entry$baseline_marker_file, "\"")
  if (isTRUE(entry$requires_baseline_marker) || isTRUE(entry$writes_baseline_marker)) {
    if (!grepl(marker_fragment, body, fixed = TRUE)) {
      stop(path, " must reference registry baseline marker file: ", entry$baseline_marker_file)
    }
  }
  if (isTRUE(entry$requires_latest_main_pointer) || isTRUE(entry$writes_latest_main_pointer)) {
    if (!grepl(entry$latest_main_pointer, body, fixed = TRUE)) {
      stop(path, " must reference registry latest-main pointer: ", entry$latest_main_pointer)
    }
  }

  if (identical(entry$target, "main")) {
    main_pos <- regexpr(target_fragment, body, fixed = TRUE)[1]
    marker_pos <- regexpr("$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT", body, fixed = TRUE)[1]
    pointer_pos <- regexpr(paste0("$latestPointer = \"", entry$latest_main_pointer, "\""), body, fixed = TRUE)[1]
    completion_pos <- regexpr("run_completion_main_10w4c", body, fixed = TRUE)[1]
    if (!(main_pos < marker_pos && marker_pos < pointer_pos && marker_pos < completion_pos)) {
      stop(path, " must check the baseline marker after main and before pointer/completion writes.")
    }
  } else {
    pointer_pos <- regexpr(paste0("$pointer = \"", entry$latest_main_pointer, "\""), body, fixed = TRUE)[1]
    read_pointer_pos <- regexpr("$RunRoot = (Get-Content $pointer -Raw).Trim()", body, fixed = TRUE)[1]
    marker_pos <- regexpr("$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT", body, fixed = TRUE)[1]
    target_pos <- regexpr(target_fragment, body, fixed = TRUE)[1]
    if (!(pointer_pos < read_pointer_pos && read_pointer_pos < marker_pos && marker_pos < target_pos)) {
      stop(path, " must resolve pointer, check baseline marker, then run downstream target.")
    }
  }
}

cat("test_run_profile_registry_static.R passed\n")
