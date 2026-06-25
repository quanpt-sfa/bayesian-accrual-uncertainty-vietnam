txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

source("scripts/ma00_setup.R")

ma00 <- txt("scripts/ma00_setup.R")
ma10 <- txt("scripts/ma10_construct_psis_loo_DA.R")
safe_csv_scripts <- c(
  "scripts/ma10_construct_psis_loo_DA.R",
  "scripts/ma11_posterior_predictive_checks.R",
  "scripts/ma14_construct_exact_kfold_DA.R",
  "scripts/ma15_audit_DA_finite_outputs.R",
  "scripts/ma16_validate_outcomes.R",
  "scripts/ma17_export_tables_figures.R",
  "scripts/sensitivity/se03_mcmc_diagnostics.R",
  "scripts/sensitivity/se04_stacking.R",
  "scripts/sensitivity/se05_construct_DA.R",
  "scripts/sensitivity/se06_validation.R",
  "scripts/sensitivity/se07_report.R"
)

for (fragment in c(
  "write_csv_safely <- function(x, file, row.names = FALSE, ...)",
  "dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)",
  "write.csv(x, file = file, row.names = row.names, ...)",
  "invisible(file)"
)) {
  if (!grepl(fragment, ma00, fixed = TRUE)) {
    stop("ma00 missing safe CSV writer fragment: ", fragment)
  }
}

for (path in safe_csv_scripts) {
  body <- txt(path)
  if (grepl("write.csv(", body, fixed = TRUE)) {
    stop(path, " must not use direct write.csv(); use write_csv_safely() or explicit dir.create(dirname(path)).")
  }
  if (!grepl("write_csv_safely(", body, fixed = TRUE)) {
    stop(path, " must use write_csv_safely() for dynamically constructed CSV output paths.")
  }
}

for (fragment in c(
  "baseline_accruals_path <- baseline_accruals_path()",
  "write_csv_safely(master_df, file.path(output_root, \"tables\", \"final_uncertainty_adjusted_accruals_winsor.csv\")",
  "write_csv_safely(master_df, baseline_accruals_path",
  "write_csv_safely(summary_df, file.path(output_root, \"tables\", \"table_DA_distribution_summary_winsor.csv\")",
  "write_csv_safely(da_io_manifest, file.path(output_root, \"tables\", \"table_secondary_psis_loo_DA_io_manifest.csv\")"
)) {
  if (!grepl(fragment, ma10, fixed = TRUE)) {
    stop("ma10 missing safe output write fragment: ", fragment)
  }
}

for (fragment in c(
  "write_csv_safely(summary_df, summary_path",
  "write_csv_safely(moments_df, moments_path",
  "write_csv_safely(tail_df, tail_path"
)) {
  if (!grepl(fragment, txt("scripts/ma11_posterior_predictive_checks.R"), fixed = TRUE)) {
    stop("ma11 missing safe posterior-predictive CSV write fragment: ", fragment)
  }
}

for (fragment in c(
  "write_csv_safely(grouped_out, grouped_out_path",
  "write_csv_safely(row_out, row_out_path",
  "write_csv_safely(io_manifest, file.path(tables_dir, \"table_DA_exact_kfold_io_manifest.csv\")"
)) {
  if (!grepl(fragment, txt("scripts/ma14_construct_exact_kfold_DA.R"), fixed = TRUE)) {
    stop("ma14 missing safe exact-Kfold DA CSV write fragment: ", fragment)
  }
}

for (fragment in c(
  "write_csv_safely(column_audit, file.path(tables_dir, \"table_DA_nonfinite_column_audit.csv\")",
  "write_csv_safely(decision, file.path(tables_dir, \"table_DA_finite_gate_decision.csv\")"
)) {
  if (!grepl(fragment, txt("scripts/ma15_audit_DA_finite_outputs.R"), fixed = TRUE)) {
    stop("ma15 missing safe finite-audit CSV write fragment: ", fragment)
  }
}

for (fragment in c(
  "write_csv_safely(unweighted_df, file.path(validation_root, \"table_unweighted_validation_scaleaware_student.csv\")",
  "write_csv_safely(validation_io_manifest, file.path(validation_root, \"table_validation_io_manifest.csv\")"
)) {
  if (!grepl(fragment, txt("scripts/ma16_validate_outcomes.R"), fixed = TRUE)) {
    stop("ma16 missing safe validation CSV write fragment: ", fragment)
  }
}

for (fragment in c(
  "write_csv_safely(df, csv_path",
  "write_csv_safely(qc, qc_path"
)) {
  if (!grepl(fragment, txt("scripts/ma17_export_tables_figures.R"), fixed = TRUE)) {
    stop("ma17 missing safe export CSV write fragment: ", fragment)
  }
}

for (path in c(
  "scripts/sensitivity/se03_mcmc_diagnostics.R",
  "scripts/sensitivity/se04_stacking.R",
  "scripts/sensitivity/se05_construct_DA.R",
  "scripts/sensitivity/se06_validation.R",
  "scripts/sensitivity/se07_report.R"
)) {
  body <- txt(path)
  if (!grepl("write_csv_safely(", body, fixed = TRUE)) {
    stop(path, " must use write_csv_safely() for sensitivity CSV outputs.")
  }
}

baseline_line <- grep("baseline_accruals_path", strsplit(ma10, "\n", fixed = TRUE)[[1]], value = TRUE)
if (!any(grepl("write_csv_safely\\(master_df, baseline_accruals_path", baseline_line))) {
  stop("ma10 must write baseline_accruals_path through write_csv_safely().")
}

profile_registry <- accrual_run_profile_registry()
main_profiles <- Filter(function(entry) identical(entry$target, "main"), profile_registry)
downstream_profiles <- Filter(function(entry) !identical(entry$target, "main"), profile_registry)
if (!length(main_profiles)) stop("Run-profile registry must include a main production profile.")
if (!length(downstream_profiles)) stop("Run-profile registry must include downstream profiles.")

for (entry in profile_registry) {
  body <- txt(entry$profile_path)
  for (fragment in c(
    "$ErrorActionPreference = \"Stop\"",
    "$rscriptExitCode = $LASTEXITCODE",
    "if ($rscriptExitCode -ne 0)"
  )) {
    if (!grepl(fragment, body, fixed = TRUE)) {
      stop(entry$profile_path, " missing fail-fast fragment: ", fragment)
    }
  }
  direct_rscript_lines <- grep("^\\s*Rscript\\s+", strsplit(body, "\n", fixed = TRUE)[[1]], value = TRUE, perl = TRUE)
  if (length(direct_rscript_lines)) {
    stop(entry$profile_path, " must not use direct Rscript calls without checking LASTEXITCODE: ",
         paste(direct_rscript_lines, collapse = " | "))
  }
  target_fragment <- if (identical(entry$target, "main")) {
    "Invoke-RscriptChecked -Context \"Main pipeline\" -Arguments @(\"run.R\", \"main\")"
  } else {
    paste0("$argsForRun = @(\"run.R\", \"", entry$target, "\")")
  }
  if (!grepl(target_fragment, body, fixed = TRUE)) {
    stop(entry$profile_path, " does not run registry target: ", entry$target)
  }
  if (!identical(entry$target, "main")) {
    marker_pos <- regexpr("$baselineMarker = Join-Path $env:ACCRUAL_OUTPUT_ROOT", body, fixed = TRUE)[1]
    target_pos <- regexpr(target_fragment, body, fixed = TRUE)[1]
    if (!(marker_pos > 0 && marker_pos < target_pos)) {
      stop(entry$profile_path, " must check baseline marker before downstream execution.")
    }
  }
}

cat("test_ma10_safe_csv_and_profile_failfast_static.R passed\n")
