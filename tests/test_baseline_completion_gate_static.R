txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

ma00 <- txt("scripts/ma00_setup.R")
run_body <- txt("run.R")
profile_paths <- list.files("run_profiles", pattern = "\\.ps1$", full.names = TRUE)
profile_paths <- gsub("\\\\", "/", profile_paths)

for (fragment in c(
  "baseline_ma17_marker_path <- function(root = output_root)",
  "write_baseline_ma17_complete_marker <- function(root = output_root, context = \"main pipeline\")",
  "assert_baseline_ma17_complete <- function(root = output_root, context = \"downstream branch\")",
  "\"BASELINE_MA17_COMPLETE\"",
  "\"[BASELINE COMPLETION BLOCKER] \"",
  "git_commit_or_na <- function()"
)) {
  if (!grepl(fragment, ma00, fixed = TRUE)) {
    stop("ma00 missing baseline completion marker helper fragment: ", fragment)
  }
}

for (fragment in c(
  "downstream_targets <- c(\"robustness\", \"sensitivity\", \"simulation\", \"diagnostics\", \"reviewer\")",
  "if (!dry_run && target %in% downstream_targets)",
  "assert_baseline_ma17_complete(output_root, context = paste0(\"run.R target \", target))",
  "if (identical(s$id, \"ma17\"))",
  "write_baseline_ma17_complete_marker(output_root, context = paste0(\"run.R \", target, \"/ma17\"))",
  "if (target %in% c(\"main\", \"all\"))",
  "unlink(baseline_ma17_marker_path(output_root), force = TRUE)",
  "Cleared stale marker before",
  "invisible(lapply(main_steps, run_step))",
  "assert_baseline_ma17_complete(output_root, context = \"run.R target all\")",
  "downstream_steps_for_all <- c(diagnostics_steps_for_all, robustness_steps, sensitivity_steps, simulation_steps, reviewer_steps)"
)) {
  if (!grepl(fragment, run_body, fixed = TRUE)) {
    stop("run.R missing baseline completion gate fragment: ", fragment)
  }
}

guard_pos <- regexpr("if (!dry_run && target %in% downstream_targets)", run_body, fixed = TRUE)[1]
registry_pos <- regexpr("write_config_registry_if_available()", run_body, fixed = TRUE)[1]
if (guard_pos < 0 || registry_pos < 0 || guard_pos > registry_pos) {
  stop("run.R standalone downstream marker guard must run before config registry writes.")
}

dry_run_pos <- regexpr("if (dry_run)", run_body, fixed = TRUE)[1]
dry_run_quit_pos <- regexpr("quit(save = \"no\", status = 0)", run_body, fixed = TRUE)[1]
cleanup_target_pos <- regexpr("if (target %in% c(\"main\", \"all\"))", run_body, fixed = TRUE)[1]
cleanup_unlink_pos <- regexpr("unlink(baseline_ma17_marker_path(output_root), force = TRUE)", run_body, fixed = TRUE)[1]
all_main_pos <- regexpr("invisible(lapply(main_steps, run_step))", run_body, fixed = TRUE)[1]
all_assert_pos <- regexpr("assert_baseline_ma17_complete(output_root, context = \"run.R target all\")", run_body, fixed = TRUE)[1]
all_downstream_pos <- regexpr("downstream_steps_for_all <- c(diagnostics_steps_for_all, robustness_steps, sensitivity_steps, simulation_steps, reviewer_steps)", run_body, fixed = TRUE)[1]
if (dry_run_pos < 0 || all_main_pos < 0 || all_assert_pos < 0 || all_downstream_pos < 0 ||
    !(dry_run_pos < all_main_pos && all_main_pos < all_assert_pos && all_assert_pos < all_downstream_pos)) {
  stop("run.R target all must dry-run first, then run main, assert marker, then define downstream steps.")
}

if (dry_run_quit_pos < 0 || cleanup_target_pos < 0 || cleanup_unlink_pos < 0 ||
    !(dry_run_quit_pos < cleanup_target_pos && cleanup_target_pos < cleanup_unlink_pos && cleanup_unlink_pos < all_main_pos)) {
  stop("run.R must clear stale baseline marker after dry-run handling and before running main_steps for target all.")
}

if (grepl("assert_baseline_ma17_complete(output_root, context = paste0(\"run.R target \", target))\\s*[\\s\\S]*?if \\(dry_run\\)", run_body, perl = TRUE)) {
  stop("run.R must not require the baseline marker before dry-run can print a downstream plan.")
}

if (!length(profile_paths)) stop("No PowerShell profiles found.")
for (path in profile_paths) {
  body <- txt(path)
  if (!grepl("$ErrorActionPreference = \"Stop\"", body, fixed = TRUE)) {
    stop(path, " must set ErrorActionPreference to Stop.")
  }
  direct_rscript_lines <- grep("^\\s*Rscript\\s+", strsplit(body, "\n", fixed = TRUE)[[1]], value = TRUE, perl = TRUE)
  if (length(direct_rscript_lines)) {
    stop(path, " must not call Rscript directly without checking LASTEXITCODE: ",
         paste(direct_rscript_lines, collapse = " | "))
  }
  if (!grepl("$LASTEXITCODE", body, fixed = TRUE)) {
    stop(path, " must check LASTEXITCODE after Rscript execution.")
  }
}

main_profile_paths <- profile_paths[vapply(profile_paths, function(path) {
  body <- txt(path)
  grepl("Invoke-RscriptChecked -Context \"Main pipeline\" -Arguments @(\"run.R\", \"main\")", body, fixed = TRUE) &&
    grepl("BASELINE_MA17_COMPLETE.txt", body, fixed = TRUE)
}, logical(1))]
if (!length(main_profile_paths)) {
  stop("No current main production profile runs `run.R main` and checks BASELINE_MA17_COMPLETE.txt.")
}
if (!"run_profiles/run_01_main_production_10w4c.ps1" %in% main_profile_paths) {
  stop("Documented main production profile must be covered: run_profiles/run_01_main_production_10w4c.ps1")
}

main_profile <- txt("run_profiles/run_01_main_production_10w4c.ps1")
for (fragment in c(
  "Invoke-RscriptChecked -Context \"Main pipeline\" -Arguments @(\"run.R\", \"main\")",
  "$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT \"BASELINE_MA17_COMPLETE.txt\"",
  "Main pipeline completed without BASELINE_MA17_COMPLETE marker",
  "$latestPointer = \"out/runs/LATEST_MAIN_10W4C_RUN_ROOT.txt\"",
  "$run_end_path = \"$run_root/manifests/run_completion_main_10w4c_$run_id.csv\""
)) {
  if (!grepl(fragment, main_profile, fixed = TRUE)) {
    stop("main production profile missing baseline marker/pointer fragment: ", fragment)
  }
}
main_run_pos <- regexpr("Invoke-RscriptChecked -Context \"Main pipeline\" -Arguments @(\"run.R\", \"main\")", main_profile, fixed = TRUE)[1]
main_marker_pos <- regexpr("$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT \"BASELINE_MA17_COMPLETE.txt\"", main_profile, fixed = TRUE)[1]
main_pointer_pos <- regexpr("$latestPointer = \"out/runs/LATEST_MAIN_10W4C_RUN_ROOT.txt\"", main_profile, fixed = TRUE)[1]
main_completion_pos <- regexpr("$run_end_path = \"$run_root/manifests/run_completion_main_10w4c_$run_id.csv\"", main_profile, fixed = TRUE)[1]
if (!(main_run_pos < main_marker_pos && main_marker_pos < main_pointer_pos && main_marker_pos < main_completion_pos)) {
  stop("main production profile must check BASELINE_MA17_COMPLETE after main and before latest pointer/completion manifest writes.")
}

downstream_profile_paths <- profile_paths[vapply(profile_paths, function(path) {
  body <- txt(path)
  grepl("$argsForRun = @(\"run.R\", \"", body, fixed = TRUE) &&
    !grepl("$argsForRun = @(\"run.R\", \"main\")", body, fixed = TRUE) &&
    grepl("BASELINE_MA17_COMPLETE.txt", body, fixed = TRUE)
}, logical(1))]
expected_downstream_profiles <- paste0(
  "run_profiles/",
  c(
    "run_02_sensitivity_after_main_10w4c.ps1",
    "run_03_diagnostics_after_main_10w4c.ps1",
    "run_04_simulation_after_main_10w4c.ps1"
  )
)
missing_downstream_profiles <- setdiff(expected_downstream_profiles, downstream_profile_paths)
if (length(missing_downstream_profiles)) {
  stop("Documented downstream production profile(s) missing from marker-gate coverage: ",
       paste(missing_downstream_profiles, collapse = ", "))
}

for (path in expected_downstream_profiles) {
  body <- txt(path)
  for (fragment in c(
    "$pointer = \"out/runs/LATEST_MAIN_10W4C_RUN_ROOT.txt\"",
    "latest main pointer was not found",
    "$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT \"BASELINE_MA17_COMPLETE.txt\"",
    "requires successful main pipeline completion through ma17",
    "Missing marker: $baselineMarker"
  )) {
    if (!grepl(fragment, body, fixed = TRUE)) {
      stop(path, " missing downstream baseline marker guard fragment: ", fragment)
    }
  }
  pointer_pos <- regexpr("$pointer = \"out/runs/LATEST_MAIN_10W4C_RUN_ROOT.txt\"", body, fixed = TRUE)[1]
  read_pointer_pos <- regexpr("$RunRoot = (Get-Content $pointer -Raw).Trim()", body, fixed = TRUE)[1]
  marker_pos <- regexpr("$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT \"BASELINE_MA17_COMPLETE.txt\"", body, fixed = TRUE)[1]
  downstream_run_pos <- regexpr("$argsForRun = @(\"run.R\", \"", body, fixed = TRUE)[1]
  if (!(pointer_pos < read_pointer_pos && read_pointer_pos < marker_pos && marker_pos < downstream_run_pos)) {
    stop(path, " must resolve the latest main pointer, check BASELINE_MA17_COMPLETE, then run its downstream branch.")
  }
}

source("scripts/ma00_setup.R")
tmp_root <- file.path(tempdir(), paste0("baseline_marker_test_", Sys.getpid()))
missing_error <- tryCatch(
  {
    assert_baseline_ma17_complete(tmp_root, context = "test downstream")
    NA_character_
  },
  error = conditionMessage
)
if (is.na(missing_error) || !grepl("[BASELINE COMPLETION BLOCKER]", missing_error, fixed = TRUE)) {
  stop("assert_baseline_ma17_complete must fail with [BASELINE COMPLETION BLOCKER] when marker is missing.")
}
marker <- write_baseline_ma17_complete_marker(tmp_root, context = "test main")
if (!file.exists(marker)) stop("write_baseline_ma17_complete_marker did not create marker: ", marker)
assert_baseline_ma17_complete(tmp_root, context = "test downstream")

sub_root <- file.path(tempdir(), paste0("runr_missing_marker_", Sys.getpid()))
dir.create(sub_root, recursive = TRUE, showWarnings = FALSE)
old_env <- Sys.getenv(c("ACCRUAL_OUTPUT_ROOT", "ACCRUAL_METHOD_DESIGN_ROOT"), unset = NA_character_)
names(old_env) <- c("ACCRUAL_OUTPUT_ROOT", "ACCRUAL_METHOD_DESIGN_ROOT")
on.exit({
  for (nm in names(old_env)) {
    if (is.na(old_env[[nm]])) {
      Sys.unsetenv(nm)
    } else {
      do.call(Sys.setenv, as.list(stats::setNames(old_env[[nm]], nm)))
    }
  }
}, add = TRUE)
Sys.setenv(
  ACCRUAL_OUTPUT_ROOT = normalizePath(sub_root, winslash = "/", mustWork = FALSE),
  ACCRUAL_METHOD_DESIGN_ROOT = normalizePath(file.path(sub_root, "method_design"), winslash = "/", mustWork = FALSE)
)
cmd_out <- system2(
  "Rscript",
  c("run.R", "sensitivity"),
  stdout = TRUE,
  stderr = TRUE
)
exit_status <- attr(cmd_out, "status")
if (is.null(exit_status) || exit_status == 0L) {
  stop("Rscript run.R sensitivity must fail when BASELINE_MA17_COMPLETE marker is missing.")
}
if (!any(grepl("[BASELINE COMPLETION BLOCKER]", cmd_out, fixed = TRUE))) {
  stop("Rscript run.R sensitivity missing-marker failure did not include [BASELINE COMPLETION BLOCKER]. Output: ",
       paste(cmd_out, collapse = "\n"))
}

cat("test_baseline_completion_gate_static.R passed\n")
