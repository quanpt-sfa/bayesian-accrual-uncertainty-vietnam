# -----------------------------------------------------------------------------
# Script: 32_audit_DA_finite_outputs.R
# Purpose: Gate DA outputs for finite/numeric validity before RQ2/export.
# -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args || "-h" %in% args) {
  cat("Usage: Rscript scripts/ma15_audit_DA_finite_outputs.R\n")
  cat("Audits exact-KFold DA outputs as primary and PSIS/LOO DA as secondary when present.\n")
  cat("Set ACCRUAL_DA_FINITE_GATE_STRICT=TRUE to exit non-zero on failed primary gate.\n")
  quit(save = "no", status = 0)
}

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma15", "Audit DA finite outputs")
ensure_analysis_dirs()

script_name <- "scripts/ma15_audit_DA_finite_outputs.R"
script_version <- "2026-06-18-v1-finite-da-gate"
script_start_time <- Sys.time()
strict_gate <- toupper(Sys.getenv("ACCRUAL_DA_FINITE_GATE_STRICT", "FALSE")) %in% c("TRUE", "1", "YES", "Y")

tables_dir <- file.path(output_root, "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}
git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}

primary_columns <- c(
  "NDA_mean_stacked",
  "NDA_sd_epred_stacked",
  "NDA_sd_predict_stacked",
  "DA_raw_stacked",
  "DA_z_estimation_stacked",
  "DA_z_predictive_stacked",
  "DA_tail_flag_95",
  "DA_tail_flag_98",
  "DA_ppd_tail_prob_two_sided"
)

input_specs <- data.frame(
  output_file = c(
    file.path(tables_dir, "final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv"),
    file.path(tables_dir, "final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv"),
    file.path(tables_dir, "final_uncertainty_adjusted_accruals_winsor.csv")
  ),
  DA_Source = c("exact_grouped_kfold", "exact_row_kfold", "secondary_psis_loo"),
  Primary_For_RQ2 = c(TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

audit_one_file <- function(path, source, primary_for_rq2) {
  if (!file.exists(path)) {
    if (primary_for_rq2) stop("[BLOCKER] Missing primary DA output for finite gate: ", path)
    return(list(column = data.frame(), row = data.frame()))
  }
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"DA_Source" %in% names(df)) df$DA_Source <- source
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  cols_to_audit <- union(numeric_cols, primary_columns)

  column_audit <- bind_rows(lapply(cols_to_audit, function(col) {
    present <- col %in% names(df)
    x <- if (present) df[[col]] else rep(NA_real_, nrow(df))
    is_nan <- if (present) is.nan(x) else rep(FALSE, nrow(df))
    is_na <- if (present) is.na(x) & !is_nan else rep(TRUE, nrow(df))
    is_pos_inf <- if (present) is.infinite(x) & x > 0 else rep(FALSE, nrow(df))
    is_neg_inf <- if (present) is.infinite(x) & x < 0 else rep(FALSE, nrow(df))
    n_nonfinite <- if (present) sum(!is.finite(x)) else nrow(df)
    data.frame(
      output_file = path,
      DA_Source = source,
      column = col,
      column_present = present,
      structural_missing = !present,
      n_rows = nrow(df),
      n_na = sum(is_na),
      n_nan = sum(is_nan),
      n_pos_inf = sum(is_pos_inf),
      n_neg_inf = sum(is_neg_inf),
      n_nonfinite = n_nonfinite,
      share_nonfinite = if (nrow(df) > 0) n_nonfinite / nrow(df) else NA_real_,
      primary_column = col %in% primary_columns,
      Primary_For_RQ2 = primary_for_rq2,
      stringsAsFactors = FALSE
    )
  }))

  row_flags <- rep(FALSE, nrow(df))
  present_primary <- intersect(primary_columns, names(df))
  if (length(present_primary) > 0) {
    row_flags <- apply(df[, present_primary, drop = FALSE], 1, function(z) any(!is.finite(as.numeric(z))))
  }
  row_audit <- data.frame(
    output_file = path,
    DA_Source = source,
    row_index = which(row_flags),
    n_nonfinite_primary_columns = if (length(present_primary) > 0) {
      rowSums(!is.finite(as.matrix(df[row_flags, present_primary, drop = FALSE])))
    } else integer(0),
    Primary_For_RQ2 = primary_for_rq2,
    stringsAsFactors = FALSE
  )
  list(column = column_audit, row = row_audit)
}

audits <- lapply(seq_len(nrow(input_specs)), function(i) {
  audit_one_file(input_specs$output_file[i], input_specs$DA_Source[i], input_specs$Primary_For_RQ2[i])
})
column_audit <- bind_rows(lapply(audits, `[[`, "column"))
row_audit <- bind_rows(lapply(audits, `[[`, "row"))

primary_audit <- column_audit %>% filter(Primary_For_RQ2, primary_column)
primary_nonfinite <- primary_audit %>%
  filter(!structural_missing, n_nonfinite > 0)
primary_structural <- primary_audit %>% filter(structural_missing)
secondary_nonfinite <- column_audit %>%
  filter(!Primary_For_RQ2, primary_column, !structural_missing, n_nonfinite > 0)

gate_decision <- if (nrow(primary_nonfinite) > 0) {
  "FAIL_NONFINITE_PRIMARY_COLUMNS"
} else if (nrow(primary_structural) > 0) {
  "PASS_WITH_STRUCTURAL_NA_ONLY"
} else if (nrow(secondary_nonfinite) > 0) {
  "WARN_SECONDARY_NONFINITE_ONLY"
} else {
  "PASS"
}

decision <- data.frame(
  gate = "DA_finite_output_gate",
  gate_decision = gate_decision,
  n_primary_nonfinite_columns = nrow(primary_nonfinite),
  n_primary_structural_missing_columns = nrow(primary_structural),
  n_secondary_nonfinite_columns = nrow(secondary_nonfinite),
  strict_gate = strict_gate,
  Script_Name = script_name,
  Script_Version = script_version,
  stringsAsFactors = FALSE
)

write.csv(column_audit, file.path(tables_dir, "table_DA_nonfinite_column_audit.csv"), row.names = FALSE)
write.csv(row_audit, file.path(tables_dir, "table_DA_nonfinite_row_audit.csv"), row.names = FALSE)
write.csv(decision, file.path(tables_dir, "table_DA_finite_gate_decision.csv"), row.names = FALSE)

manifest_paths <- c(
  input_specs$output_file,
  file.path(tables_dir, "table_DA_nonfinite_column_audit.csv"),
  file.path(tables_dir, "table_DA_nonfinite_row_audit.csv"),
  file.path(tables_dir, "table_DA_finite_gate_decision.csv")
)
script_end_time <- Sys.time()
io_manifest <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = as.character(script_start_time),
  End_Time = as.character(script_end_time),
  Runtime_Seconds = as.numeric(difftime(script_end_time, script_start_time, units = "secs")),
  Git_Commit = git_commit_or_na(),
  Classification = c(rep("input", nrow(input_specs)), rep("output", 3)),
  Path = manifest_paths,
  Exists = file.exists(manifest_paths),
  Size = vapply(manifest_paths, file_size_or_na, numeric(1)),
  MTime = vapply(manifest_paths, mtime_or_na, character(1)),
  Hash = vapply(manifest_paths, file_hash_or_na, character(1)),
  Gate_Decision = gate_decision,
  Primary_Secondary = ifelse(seq_along(manifest_paths) <= nrow(input_specs) &
                               input_specs$Primary_For_RQ2[seq_along(manifest_paths)] %in% TRUE,
                             "primary_exact_kfold", "secondary_or_gate_output"),
  stringsAsFactors = FALSE
)
write.csv(io_manifest, file.path(tables_dir, "table_DA_finite_io_manifest.csv"), row.names = FALSE)

note_path <- file.path(output_root, "logs", "DA_finite_gate_reviewer_note.md")
dir.create(dirname(note_path), recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  "# DA finite-output gate",
  "",
  paste("- Script:", script_name),
  paste("- Version:", script_version),
  paste("- Decision:", gate_decision),
  "",
  "Primary RQ2 DA files are the exact grouped K-fold and exact row K-fold outputs from ma14.",
  "The PSIS/LOO DA file from ma10 is audited as secondary when present.",
  "Primary DA columns must be finite unless a missing column is explicitly classified as structural."
), note_path)

cat("\n[SUCCESS] DA finite-output audit completed.\n")
cat("Decision:", gate_decision, "\n")

if (strict_gate && identical(gate_decision, "FAIL_NONFINITE_PRIMARY_COLUMNS")) {
  stop("[GATE BLOCKER] DA finite gate failed for primary exact-KFold columns.")
}
phase_end("ma15", "Audit DA finite outputs")
