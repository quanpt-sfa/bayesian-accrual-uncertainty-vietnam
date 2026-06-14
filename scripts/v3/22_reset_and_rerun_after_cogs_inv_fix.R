# -----------------------------------------------------------------------------
# Script: 22_reset_and_rerun_after_cogs_inv_fix.R
# Purpose: Quarantine invalid v3 outputs after corrected COGS/INV data, then
#          optionally rerun the corrected v3 pipeline.
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE)

script_start <- Sys.time()
script_path <- "scripts/v3/22_reset_and_rerun_after_cogs_inv_fix.R"

env_true <- function(name, default = "FALSE") {
  identical(toupper(Sys.getenv(name, unset = default)), "TRUE")
}

env_value <- function(name, default) {
  val <- Sys.getenv(name, unset = default)
  if (!nzchar(val)) default else val
}

dry_run <- env_true("V3_RESET_DRY_RUN", "TRUE")
reset_confirm <- env_true("V3_RESET_CONFIRM", "FALSE")
rerun_after_reset <- env_true("V3_RERUN_AFTER_RESET", "FALSE")
run_kfold <- env_true("V3_RUN_KFOLD", "FALSE")
continue_after_audit_warning <- env_true("V3_CONTINUE_AFTER_AUDIT_WARNING", "FALSE")
run_scaleaware_student_only <- env_true("V3_RUN_SCALEAWARE_STUDENT_ONLY", "FALSE")
run_prior_predictive <- env_true("V3_RUN_PRIOR_PREDICTIVE", "FALSE")
run_mcmc_diagnostics <- env_true("V3_RUN_MCMC_DIAGNOSTICS", "FALSE")
run_posterior_ppc <- env_true("V3_RUN_POSTERIOR_PPC", "FALSE")
run_sensitivity <- env_true("V3_RUN_SENSITIVITY", "FALSE")
run_varying_slopes <- env_true("V3_RUN_VARYING_SLOPES", "FALSE")
run_validation <- env_true("V3_RUN_VALIDATION", "FALSE")
prior_predictive_mode <- env_value("V3_PRIOR_PREDICTIVE_MODE", "REPRESENTATIVE")
sensitivity_mode <- env_value("V3_SENSITIVITY_MODE", "SUMMARY_ONLY")
sensitivity_dry_run <- env_value("V3_DRY_RUN", "TRUE")
kfold_target_mode <- env_value("V3_KFOLD_TARGET_MODE", "PARETO_PROBLEM_ONLY")
kfold_mode <- env_value("V3_KFOLD_MODE", env_value("V3_KFOLD_FIRM_MODE", "FULL_MODE"))
kfold_k <- env_value("V3_KFOLD_K", env_value("V3_KFOLD_FIRM_K", "5"))

if (!kfold_mode %in% c("FULL_MODE", "FAST_MODE")) {
  stop("[BLOCKER] V3_KFOLD_MODE must be FULL_MODE or FAST_MODE.")
}

dir.create("out", recursive = TRUE, showWarnings = FALSE)
if (file.exists("scripts/v3/00_v3_winsor_helpers.R")) {
  source("scripts/v3/00_v3_winsor_helpers.R")
  ensure_v3_baseline_dirs()
  ensure_v3_winsor_dirs()
  ensure_v3_sensitivity_dirs()
  write_method_design_files_v3()
  write_prior_registry_v3()
}

baseline_root <- if (exists("v3_original_root", inherits = FALSE)) v3_original_root else file.path("out", "interim", "baseline")
winsor_input_root <- if (exists("v3_input_winsor_root", inherits = FALSE)) v3_input_winsor_root else file.path("out", "interim", "winsor")
scaleaware_root <- env_value("V3_OUTPUT_ROOT", file.path("out", "interim", "winsor_scaleaware_student"))
method_design_root <- if (exists("v3_method_design_root", inherits = FALSE)) v3_method_design_root else file.path("out", "manifests", "method_design")
reset_manifest_root <- file.path("out", "manifests")
reset_log_root <- file.path("out", "logs")
dir.create(reset_manifest_root, recursive = TRUE, showWarnings = FALSE)
dir.create(reset_log_root, recursive = TRUE, showWarnings = FALSE)

baseline_table_path <- function(file_name) file.path(baseline_root, "tables", file_name)
baseline_log_path <- function(file_name) file.path(baseline_root, "logs", file_name)
winsor_table_path <- function(file_name) file.path(winsor_input_root, "tables", file_name)
scaleaware_table_path <- function(file_name) file.path(scaleaware_root, "tables", file_name)
scaleaware_log_path <- function(file_name) file.path(scaleaware_root, "logs", file_name)
scaleaware_lofo_table_path <- function(file_name) file.path(scaleaware_root, "lofo", "tables", file_name)
scaleaware_lofo_log_path <- function(file_name) file.path(scaleaware_root, "lofo", "logs", file_name)
scaleaware_sensitivity_table_path <- function(file_name) file.path(scaleaware_root, "sensitivity", "tables", file_name)
scaleaware_sensitivity_log_path <- function(file_name) file.path(scaleaware_root, "sensitivity", "logs", file_name)
scaleaware_sensitivity_report_path <- function(file_name) file.path("reports", "sensitivity", file_name)
scaleaware_validation_path <- function(file_name) file.path(scaleaware_root, "validation", file_name)
scaleaware_varyslopes_table_path <- function(file_name) file.path(scaleaware_root, "varyslopes", "tables", file_name)
scaleaware_varyslopes_log_path <- function(file_name) file.path(scaleaware_root, "varyslopes", "logs", file_name)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
quarantine_root <- file.path("out", paste0("INVALID_COGS_INV_SWAP_", timestamp))
corrected_data_manifest_path <- file.path(reset_manifest_root, "corrected_data_manifest.csv")
corrected_rerun_manifest_path <- file.path(reset_manifest_root, "corrected_rerun_manifest.csv")
corrected_rerun_summary_path <- file.path(reset_log_root, "corrected_rerun_summary.txt")
latest_quarantine_path <- file.path(reset_log_root, "latest_invalid_cogs_inv_quarantine.txt")

normalize_path <- function(path) {
  gsub("\\\\", "/", path)
}

file_info_row <- function(path) {
  if (!file.exists(path)) {
    return(list(size = NA_real_, mtime = NA_character_))
  }
  info <- file.info(path)
  list(size = as.numeric(info$size[1]), mtime = format(info$mtime[1], "%Y-%m-%d %H:%M:%S %z"))
}

data_path <- if (exists("v3_data_path", inherits = FALSE)) v3_data_path else file.path("data", "raw", "data.xlsx")
if (!file.exists(data_path)) {
  stop("[BLOCKER] Corrected raw data workbook not found at: ", data_path)
}
data_info <- file_info_row(data_path)
sha <- NA_character_
hash_note <- "digest package unavailable; recorded file size and modified time only."
if (requireNamespace("digest", quietly = TRUE)) {
  sha <- digest::digest(data_path, algo = "sha256", file = TRUE)
  hash_note <- "SHA256 computed with digest::digest(file = TRUE)."
}

write.csv(
  data.frame(
    Data_File = data_path,
    Exists = file.exists(data_path),
    File_Size_Bytes = data_info$size,
    Modified_Time = data_info$mtime,
    SHA256_If_Available = sha,
    Reset_Timestamp = timestamp,
    Notes = hash_note,
    stringsAsFactors = FALSE
  ),
  corrected_data_manifest_path,
  row.names = FALSE
)

safe_recreate_dirs <- unique(c(
  "out",
  reset_manifest_root,
  reset_log_root,
  file.path(baseline_root, c("", "tables", "models", "draws", "logs", "figures", "validation")),
  file.path(winsor_input_root, c("", "tables", "models", "draws", "logs", "figures", "validation", "lofo", "kfold_firm", "sensitivity", "sensitivity/tables", "sensitivity/logs")),
  file.path(scaleaware_root, c("", "tables", "models", "draws", "logs", "figures", "validation", "lofo", "lofo/tables", "lofo/logs", "kfold_firm", "sensitivity", "sensitivity/tables", "sensitivity/logs", "varyslopes", "varyslopes/tables", "varyslopes/logs")),
  file.path("reports", "sensitivity")
))

relative_quarantine_path <- function(path) {
  file.path(quarantine_root, normalize_path(path))
}

list_existing_files <- function(path) {
  if (!file.exists(path)) return(character())
  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    files[file.exists(files) & !dir.exists(files)]
  } else {
    path
  }
}

target_specs <- data.frame(
  Path = c(baseline_root, winsor_input_root, scaleaware_root, file.path("out", "INVALID_OLD_RUNS")),
  Reason = c("old_v3_output", "old_v3_winsor_output", "old_scaleaware_output", "old_lofo_or_kfold_output"),
  stringsAsFactors = FALSE
)

manifest_rows <- list()
move_items <- data.frame(Original = character(), Quarantine = character(), Reason = character(), stringsAsFactors = FALSE)

add_manifest_for_target <- function(path, reason) {
  files <- list_existing_files(path)
  if (length(files) == 0 && file.exists(path)) files <- path
  if (length(files) == 0) {
    fi <- file_info_row(path)
    manifest_rows[[length(manifest_rows) + 1]] <<- data.frame(
      Original_Path = normalize_path(path),
      Quarantine_Path = normalize_path(relative_quarantine_path(path)),
      Exists_Before = file.exists(path),
      Moved = FALSE,
      File_Size_Bytes = fi$size,
      Modified_Time = fi$mtime,
      Reason = reason,
      Notes = if (file.exists(path)) "Directory exists but no files found." else "Target did not exist.",
      stringsAsFactors = FALSE
    )
    return(invisible(NULL))
  }
  for (f in files) {
    fi <- file_info_row(f)
    manifest_rows[[length(manifest_rows) + 1]] <<- data.frame(
      Original_Path = normalize_path(f),
      Quarantine_Path = normalize_path(relative_quarantine_path(f)),
      Exists_Before = TRUE,
      Moved = FALSE,
      File_Size_Bytes = fi$size,
      Modified_Time = fi$mtime,
      Reason = reason,
      Notes = "Planned quarantine because output is invalid after COGS/INV correction.",
      stringsAsFactors = FALSE
    )
  }
  move_items <<- unique(rbind(
    move_items,
    data.frame(
      Original = path,
      Quarantine = relative_quarantine_path(path),
      Reason = reason,
      stringsAsFactors = FALSE
    )
  ))
}

for (i in seq_len(nrow(target_specs))) {
  add_manifest_for_target(target_specs$Path[i], target_specs$Reason[i])
}

standalone_patterns <- "\\.(csv|rds|RData)$"
standalone_files <- if (dir.exists("outputs")) {
  list.files("outputs", pattern = standalone_patterns, recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
} else {
  character()
}
standalone_exclude <- c(
  corrected_data_manifest_path,
  corrected_rerun_manifest_path,
  corrected_rerun_summary_path,
  latest_quarantine_path
)
standalone_files <- setdiff(normalize_path(standalone_files), normalize_path(standalone_exclude))
standalone_files <- standalone_files[!grepl("^outputs/dry_run_invalid_output_move_manifest_.*\\.csv$", standalone_files)]
for (f in standalone_files) {
  fi <- file_info_row(f)
  manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
    Original_Path = normalize_path(f),
    Quarantine_Path = normalize_path(relative_quarantine_path(f)),
    Exists_Before = TRUE,
    Moved = FALSE,
    File_Size_Bytes = fi$size,
    Modified_Time = fi$mtime,
    Reason = "invalid_due_to_cogs_inv_swap",
    Notes = "Standalone generated output under outputs/.",
    stringsAsFactors = FALSE
  )
  move_items <- unique(rbind(
    move_items,
    data.frame(Original = f, Quarantine = relative_quarantine_path(f), Reason = "invalid_due_to_cogs_inv_swap")
  ))
}

if (length(manifest_rows) == 0) {
  move_manifest <- data.frame(
    Original_Path = character(),
    Quarantine_Path = character(),
    Exists_Before = logical(),
    Moved = logical(),
    File_Size_Bytes = numeric(),
    Modified_Time = character(),
    Reason = character(),
    Notes = character(),
    stringsAsFactors = FALSE
  )
} else {
  move_manifest <- do.call(rbind, manifest_rows)
}

actual_reset <- FALSE
if (!dry_run && reset_confirm) {
  dir.create(quarantine_root, recursive = TRUE, showWarnings = FALSE)
  writeLines(normalize_path(quarantine_root), latest_quarantine_path)
  for (i in seq_len(nrow(move_items))) {
    src <- move_items$Original[i]
    dst <- move_items$Quarantine[i]
    if (!file.exists(src)) next
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    ok <- file.rename(src, dst)
    if (!ok) {
      stop("[BLOCKER] Failed to quarantine: ", src, " -> ", dst)
    }
  }
  if (nrow(move_manifest) > 0) {
    existed <- move_manifest$Exists_Before
    move_manifest$Moved <- existed & file.exists(move_manifest$Quarantine_Path) & !file.exists(move_manifest$Original_Path)
  }
  actual_reset <- TRUE
} else {
  if (!dry_run && !reset_confirm && !rerun_after_reset) {
    stop("[BLOCKER] Refusing to move outputs because V3_RESET_CONFIRM is not TRUE.")
  }
}

if (dry_run) {
  dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
  dry_manifest_path <- file.path("outputs", paste0("dry_run_invalid_output_move_manifest_", timestamp, ".csv"))
  write.csv(move_manifest, dry_manifest_path, row.names = FALSE)
  cat("\n===== DRY RUN: INVALID OUTPUTS THAT WOULD BE QUARANTINED =====\n")
  planned <- unique(move_manifest$Original_Path[move_manifest$Exists_Before])
  if (length(planned) == 0) {
    cat("No existing invalid generated outputs found.\n")
  } else {
    cat(paste(planned, collapse = "\n"), "\n")
  }
  cat("Dry-run manifest:", dry_manifest_path, "\n")
} else {
  dir.create(quarantine_root, recursive = TRUE, showWarnings = FALSE)
  write.csv(move_manifest, file.path(quarantine_root, "invalid_output_move_manifest.csv"), row.names = FALSE)
}

if (actual_reset || (!dry_run && reset_confirm)) {
  for (d in safe_recreate_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  write.csv(
    data.frame(
      Data_File = data_path,
      Exists = file.exists(data_path),
      File_Size_Bytes = data_info$size,
      Modified_Time = data_info$mtime,
      SHA256_If_Available = sha,
      Reset_Timestamp = timestamp,
      Notes = hash_note,
      stringsAsFactors = FALSE
    ),
    corrected_data_manifest_path,
    row.names = FALSE
  )
}

step_manifest <- data.frame(
  Step_ID = character(),
  Step_Name = character(),
  Script_Path = character(),
  Started_At = character(),
  Ended_At = character(),
  Runtime_Seconds = numeric(),
  Status = character(),
  Exit_Code = integer(),
  Key_Output_1 = character(),
  Key_Output_2 = character(),
  Notes = character(),
  stringsAsFactors = FALSE
)

append_step <- function(step_id, step_name, path, started, ended, status, exit_code, key1 = "", key2 = "", notes = "") {
  step_manifest <<- rbind(
    step_manifest,
    data.frame(
      Step_ID = step_id,
      Step_Name = step_name,
      Script_Path = path,
      Started_At = if (is.na(started)) "" else format(started, "%Y-%m-%d %H:%M:%S %z"),
      Ended_At = if (is.na(ended)) "" else format(ended, "%Y-%m-%d %H:%M:%S %z"),
      Runtime_Seconds = if (is.na(started) || is.na(ended)) NA_real_ else as.numeric(difftime(ended, started, units = "secs")),
      Status = status,
      Exit_Code = exit_code,
      Key_Output_1 = key1,
      Key_Output_2 = key2,
      Notes = notes,
      stringsAsFactors = FALSE
    )
  )
  write.csv(step_manifest, corrected_rerun_manifest_path, row.names = FALSE)
}

rscript_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

run_r_script <- function(step_id, step_name, path, key1 = "", key2 = "", extra_env = character()) {
  if (!file.exists(path)) {
    append_step(step_id, step_name, path, NA, NA, "SKIPPED", NA_integer_, key1, key2, "Script not found.")
    return("SKIPPED")
  }
  started <- Sys.time()
  append_step(step_id, step_name, path, started, NA, "RUNNING", NA_integer_, key1, key2, "")
  old_env <- character()
  if (length(extra_env) > 0) {
    old_env <- Sys.getenv(names(extra_env), unset = NA_character_)
    names(old_env) <- names(extra_env)
    do.call(Sys.setenv, as.list(extra_env))
  }
  exit_code <- system2(rscript_bin, args = path)
  if (length(extra_env) > 0) {
    for (nm in names(extra_env)) {
      if (is.na(old_env[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, stats::setNames(as.list(old_env[[nm]]), nm))
      }
    }
  }
  ended <- Sys.time()
  status <- if (exit_code == 0) "SUCCESS" else "FAILED"
  step_manifest <<- step_manifest[!(step_manifest$Step_ID == step_id & step_manifest$Status == "RUNNING"), , drop = FALSE]
  append_step(step_id, step_name, path, started, ended, status, as.integer(exit_code), key1, key2, "")
  if (exit_code != 0) {
    stop("[BLOCKER] Pipeline phase failed: ", step_name, " (", path, "), exit code ", exit_code)
  }
  status
}

mark_skipped_step <- function(step_id, step_name, path, key1 = "", key2 = "", notes = "") {
  append_step(step_id, step_name, path, NA, NA, "SKIPPED", NA_integer_, key1, key2, notes)
  "SKIPPED"
}

sample_n <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  nrow(read.csv(path, stringsAsFactors = FALSE))
}

read_first_line <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  x <- readLines(path, warn = FALSE)
  if (length(x) == 0) return(NA_character_)
  trimws(x[1])
}

compare_old_invalid_vs_corrected <- function() {
  quarantine <- if (file.exists(latest_quarantine_path)) trimws(readLines(latest_quarantine_path, warn = FALSE)[1]) else ""
  out_path <- baseline_table_path("table_v3_old_invalid_vs_corrected_sample_comparison.csv")
  if (!nzchar(quarantine)) return(FALSE)
  baseline_summary_rel <- normalizePath(baseline_table_path("table_v3_common_sample_summary.csv"), winslash = "/", mustWork = FALSE)
  winsor_summary_rel <- normalizePath(winsor_table_path("table_v3_common_sample_summary_winsor.csv"), winslash = "/", mustWork = FALSE)
  comparisons <- list(
    list(
      old = file.path(quarantine, baseline_summary_rel),
      new = baseline_table_path("table_v3_common_sample_summary.csv"),
      note = "old_invalid non-winsorized"
    ),
    list(
      old = file.path(quarantine, winsor_summary_rel),
      new = winsor_table_path("table_v3_common_sample_summary_winsor.csv"),
      note = "old_invalid winsorized"
    )
  )
  rows <- list()
  for (cmp in comparisons) {
    if (!file.exists(cmp$old) || !file.exists(cmp$new)) next
    old_df <- read.csv(cmp$old, stringsAsFactors = FALSE)
    new_df <- read.csv(cmp$new, stringsAsFactors = FALSE)
    sample_col_old <- intersect(c("Sample", "Sample_Name"), names(old_df))[1]
    sample_col_new <- intersect(c("Sample", "Sample_Name"), names(new_df))[1]
    n_col_old <- intersect(c("N_Obs", "N_Rows", "N"), names(old_df))[1]
    n_col_new <- intersect(c("N_Obs", "N_Rows", "N"), names(new_df))[1]
    if (any(is.na(c(sample_col_old, sample_col_new, n_col_old, n_col_new)))) next
    merged <- merge(
      data.frame(Sample_Name = old_df[[sample_col_old]], N_Old_Invalid = old_df[[n_col_old]]),
      data.frame(Sample_Name = new_df[[sample_col_new]], N_Corrected = new_df[[n_col_new]]),
      by = "Sample_Name",
      all = TRUE
    )
    merged$Difference <- merged$N_Corrected - merged$N_Old_Invalid
    merged$Difference_Pct <- ifelse(is.na(merged$N_Old_Invalid) | merged$N_Old_Invalid == 0,
                                    NA_real_, 100 * merged$Difference / merged$N_Old_Invalid)
    merged$Notes <- cmp$note
    rows[[length(rows) + 1]] <- merged
  }
  if (length(rows) == 0) return(FALSE)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(do.call(rbind, rows), out_path, row.names = FALSE)
  TRUE
}

final_status <- "DRY_RUN_ONLY"
audit_status <- "NOT_RUN"
main_sample_audit_status <- "NOT_RUN"
secondary_operating_cycle_audit_status <- "NOT_RUN"

if (!dry_run && reset_confirm && !rerun_after_reset) {
  final_status <- "RESET_ONLY_NO_RERUN"
}

if (dry_run && rerun_after_reset) {
  stop("[BLOCKER] Refusing rerun in dry-run mode. Set V3_RESET_DRY_RUN='FALSE' and V3_RESET_CONFIRM='TRUE'.")
}

if (!rerun_after_reset) {
  append_step("RESET", "Quarantine invalid old outputs", script_path, script_start, Sys.time(),
              if (dry_run) "SKIPPED" else "SUCCESS", 0L,
              quarantine_root, corrected_data_manifest_path,
              if (dry_run) "Dry run only; nothing moved." else "Invalid outputs moved to quarantine when present.")
} else {
  append_step("RESET", "Quarantine invalid old outputs", script_path, script_start, Sys.time(),
              if (actual_reset) "SUCCESS" else "SKIPPED", 0L, quarantine_root, corrected_data_manifest_path,
              if (actual_reset) "Invalid outputs quarantined." else "Reset skipped; rerun requested without moving outputs.")

  append_step("POSITIONING", "Method positioning and adaptation-not-replication note", "scripts/v3/00_v3_winsor_helpers.R",
              script_start, Sys.time(), "SUCCESS", 0L,
              file.path(method_design_root, "differences_from_AccForUncertaintyCode.csv"),
              file.path(method_design_root, "method_note_adaptation_not_replication.txt"),
              "Created/updated method design documentation; no model phase run.")

  run_r_script("P0", "Phase 0 setup and registry", "scripts/v3/01_v3_setup_and_registry.R",
               baseline_table_path("table_v3_model_registry.csv"), baseline_log_path("v3_phase0_registry_notes.txt"))
  run_r_script("P1", "Phase 1 build common sample", "scripts/v3/02_v3_build_common_sample.R",
               baseline_table_path("table_v3_common_sample_summary.csv"), baseline_table_path("final_v3_common_ex_post_sample.csv"))

  audit_started <- Sys.time()
  audit_exit <- system2(rscript_bin, args = "scripts/v3/03_v3_audit_cogs_inv_operating_cycle_after_fix.R",
                        env = paste0("V3_COGS_INV_QUARANTINE_PATH=", normalizePath(quarantine_root, mustWork = FALSE)))
  audit_ended <- Sys.time()
  audit_table <- baseline_table_path("table_v3_cogs_inv_operating_cycle_audit_corrected.csv")
  audit_status_file <- baseline_log_path("cogs_inv_correction_audit_status.txt")
  audit_status_from_file <- read_first_line(audit_status_file)
  audit_status_value <- if (!is.na(audit_status_from_file) && nzchar(audit_status_from_file)) {
    audit_status_from_file
  } else if (file.exists(audit_table)) {
    tmp_audit <- read.csv(audit_table, stringsAsFactors = FALSE)
    if ("Audit_Status" %in% names(tmp_audit)) tmp_audit$Audit_Status[1] else NA_character_
  } else {
    NA_character_
  }
  if (file.exists(audit_table)) {
    tmp_audit <- read.csv(audit_table, stringsAsFactors = FALSE)
    main_sample_audit_status <- if ("Main_Sample_Status" %in% names(tmp_audit)) tmp_audit$Main_Sample_Status[1] else audit_status_value
    secondary_operating_cycle_audit_status <- if ("Secondary_OperatingCycle_Status" %in% names(tmp_audit)) {
      tmp_audit$Secondary_OperatingCycle_Status[1]
    } else if ("Secondary_Operating_Cycle_Status" %in% names(tmp_audit)) {
      tmp_audit$Secondary_Operating_Cycle_Status[1]
    } else {
      audit_status_value
    }
  }
  allowed_audit_statuses <- c("PASS", "PASS_FOR_MAIN_SAMPLE_SECONDARY_OC_REVIEW")
  audit_status <- if (audit_status_value %in% allowed_audit_statuses) {
    audit_status_value
  } else if (identical(audit_status_value, "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT")) {
    "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT"
  } else if (audit_exit != 0) {
    "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT"
  } else {
    audit_status_value
  }
  append_step("P1C", "COGS/INV operating-cycle audit", "scripts/v3/03_v3_audit_cogs_inv_operating_cycle_after_fix.R",
              audit_started, audit_ended, ifelse(audit_status %in% allowed_audit_statuses, "SUCCESS", "REVIEW_REQUIRED"), as.integer(audit_exit),
              baseline_table_path("table_v3_cogs_inv_operating_cycle_audit_corrected.csv"),
              baseline_log_path("cogs_inv_correction_audit_notes.txt"),
              paste("Audit status:", audit_status))
  if (audit_status == "REVIEW_REQUIRED_COGS_INV_STILL_SUSPECT" && !continue_after_audit_warning) {
    final_status <- "REVIEW_REQUIRED_AFTER_COGS_INV_AUDIT"
  } else {
    run_r_script("P2", "Phase 2 define named-model spaces", "scripts/v3/04_v3_define_named_models.R",
                 baseline_table_path("table_v3_named_model_formulas.csv"),
                 baseline_log_path("v3_phase2_model_space_notes.txt"))

    run_r_script("P1B", "Phase 1b winsorize common samples", "scripts/v3/05_v3_winsorize_common_samples.R",
                 winsor_table_path("final_v3_common_ex_post_sample_winsor.csv"),
                 winsor_table_path("final_v3_common_realtime_sample_winsor.csv"))
    if (run_prior_predictive) {
      run_r_script("P3A_PRIOR_PREDICTIVE", "Phase 3a prior predictive checks", "scripts/v3/06_v3_prior_predictive_checks_winsor.R",
                   scaleaware_table_path("table_v3_prior_predictive_summary.csv"),
                   scaleaware_log_path("v3_phase3a_prior_predictive_notes.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_PRIOR_PREDICTIVE_MODE = prior_predictive_mode
                   ))
    } else {
      mark_skipped_step("P3A_PRIOR_PREDICTIVE", "Phase 3a prior predictive checks", "scripts/v3/06_v3_prior_predictive_checks_winsor.R",
                        "", "", "V3_RUN_PRIOR_PREDICTIVE is FALSE.")
    }
    if (run_scaleaware_student_only) {
      run_r_script("P3B_SCALEAWARE_STUDENT", "Scale-aware Student-t brms fits", "scripts/v3/07_v3_fit_brms_named_models_winsor.R",
                   scaleaware_table_path("table_v3_brms_diagnostics_winsor.csv"),
                   scaleaware_log_path("v3_phase3b_fit_notes_winsor.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept")
                   ))
    } else {
      mark_skipped_step("P3B_SCALEAWARE_STUDENT", "Scale-aware Student-t brms fits", "scripts/v3/07_v3_fit_brms_named_models_winsor.R",
                        scaleaware_table_path("table_v3_brms_diagnostics_winsor.csv"),
                        "", "V3_RUN_SCALEAWARE_STUDENT_ONLY is FALSE.")
    }
    if (run_mcmc_diagnostics) {
      run_r_script("P3C_MCMC_DIAGNOSTICS", "Phase 3c MCMC diagnostics", "scripts/v3/08_v3_mcmc_diagnostics_winsor.R",
                   scaleaware_table_path("table_v3_mcmc_diagnostics_model_summary.csv"),
                   scaleaware_log_path("v3_phase3c_mcmc_diagnostics_notes.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept")
                   ))
    } else {
      mark_skipped_step("P3C_MCMC_DIAGNOSTICS", "Phase 3c MCMC diagnostics", "scripts/v3/08_v3_mcmc_diagnostics_winsor.R",
                        "", "", "V3_RUN_MCMC_DIAGNOSTICS is FALSE.")
    }
    if (run_scaleaware_student_only) {
      run_r_script("P4C_SCALEAWARE_STUDENT", "Scale-aware Student-t row-level LOO stacking", "scripts/v3/09_v3_loo_stacking_winsor.R",
                   scaleaware_table_path("table_v3_stacking_weights_ex_post_winsor_corrected.csv"),
                   scaleaware_table_path("table_v3_stacking_weights_no_lookahead_winsor_corrected.csv"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept")
                   ))
      run_r_script("P5B_SCALEAWARE_STUDENT", "Scale-aware Student-t uncertainty-adjusted DA", "scripts/v3/10_v3_construct_uncertainty_adjusted_DA_winsor.R",
                   file.path("accruals", "baseline", "final_v3_uncertainty_adjusted_accruals_winsor.csv"),
                   scaleaware_log_path("v3_phase5b_uncertainty_adjusted_DA_notes_winsor.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept")
                   ))
    } else {
      mark_skipped_step("P4C_SCALEAWARE_STUDENT", "Scale-aware Student-t row-level LOO stacking", "scripts/v3/09_v3_loo_stacking_winsor.R",
                        scaleaware_table_path("table_v3_stacking_weights_ex_post_winsor_corrected.csv"),
                        "", "V3_RUN_SCALEAWARE_STUDENT_ONLY is FALSE.")
      mark_skipped_step("P5B_SCALEAWARE_STUDENT", "Scale-aware Student-t uncertainty-adjusted DA", "scripts/v3/10_v3_construct_uncertainty_adjusted_DA_winsor.R",
                        file.path("accruals", "baseline", "final_v3_uncertainty_adjusted_accruals_winsor.csv"),
                        "", "V3_RUN_SCALEAWARE_STUDENT_ONLY is FALSE.")
    }
    if (run_posterior_ppc) {
      run_r_script("P5C_POSTERIOR_PPC", "Phase 5c posterior predictive checks", "scripts/v3/11_v3_posterior_predictive_checks_winsor.R",
                   scaleaware_table_path("table_v3_posterior_predictive_check_summary.csv"),
                   scaleaware_log_path("v3_phase5c_posterior_predictive_check_notes.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept")
                   ))
    } else {
      mark_skipped_step("P5C_POSTERIOR_PPC", "Phase 5c posterior predictive checks", "scripts/v3/11_v3_posterior_predictive_checks_winsor.R",
                        "", "", "V3_RUN_POSTERIOR_PPC is FALSE.")
    }
    if (run_scaleaware_student_only) {
      run_r_script("P4D_SCALEAWARE_STUDENT", "Scale-aware Student-t grouped PSIS-LOFO", "scripts/v3/12_v3_lofo_stacking_winsor.R",
                   scaleaware_lofo_table_path("table_reviewer_priority2_lofo_decision.csv"),
                   scaleaware_lofo_log_path("v3_phase4d_lofo_stacking_winsor_notes.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept")
                   ))
    } else {
      mark_skipped_step("P4D_SCALEAWARE_STUDENT", "Scale-aware Student-t grouped PSIS-LOFO", "scripts/v3/12_v3_lofo_stacking_winsor.R",
                        scaleaware_lofo_table_path("table_reviewer_priority2_lofo_decision.csv"),
                        "", "V3_RUN_SCALEAWARE_STUDENT_ONLY is FALSE.")
    }

    if (run_kfold) {
      run_r_script("P4E_PRE", "Priority 2b exact grouped K-fold preflight", "scripts/v3/13_v3_grouped_kfold_firm_winsor.R",
                   file.path(scaleaware_root, "kfold_firm", "LATEST_RUN.txt"), "",
                   c(
                     V3_KFOLD_FIRM_PREFLIGHT_ONLY = "TRUE",
                     V3_KFOLD_FIRM_MODE = kfold_mode,
                     V3_KFOLD_FIRM_K = kfold_k,
                     V3_KFOLD_TARGET_MODE = kfold_target_mode,
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_KFOLD_FIRM_RUN_ID = "corrected_data_main_preflight"
                   ))
      run_r_script("P4E", "Priority 2b exact grouped K-fold", "scripts/v3/13_v3_grouped_kfold_firm_winsor.R",
                   file.path(scaleaware_root, "kfold_firm", "LATEST_RUN.txt"), "",
                   c(
                     V3_KFOLD_FIRM_PREFLIGHT_ONLY = "FALSE",
                     V3_KFOLD_FIRM_MODE = kfold_mode,
                     V3_KFOLD_FIRM_K = kfold_k,
                     V3_KFOLD_TARGET_MODE = kfold_target_mode,
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_KFOLD_FIRM_RUN_ID = "corrected_data_main_kfold"
                   ))
    } else {
      append_step("P4E", "Priority 2b exact grouped K-fold", "scripts/v3/13_v3_grouped_kfold_firm_winsor.R",
                  NA, NA, "SKIPPED", NA_integer_, "", "", "V3_RUN_KFOLD is FALSE.")
    }
    if (run_sensitivity) {
      sens_env <- c(
        V3_OUTPUT_ROOT = scaleaware_root,
        V3_INPUT_WINSOR_ROOT = winsor_input_root,
        V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
        V3_FAMILY = env_value("V3_FAMILY", "student"),
        V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept"),
        V3_SENSITIVITY_MODE = sensitivity_mode,
        V3_DRY_RUN = sensitivity_dry_run
      )
      run_r_script("P7A_SENS_PRIOR_PREDICTIVE", "Sensitivity prior predictive gate", "scripts/v3/14_v3_sensitivity_prior_predictive_winsor.R",
                   scaleaware_sensitivity_table_path("sensitivity_prior_predictive_summary.csv"),
                   scaleaware_sensitivity_table_path("sensitivity_prior_predictive_gate.csv"),
                   sens_env)
      run_r_script("P7B_SENS_REFIT", "Sensitivity full refits by prior scenario", "scripts/v3/15_v3_sensitivity_refit_prior_scenarios_winsor.R",
                   scaleaware_sensitivity_table_path("sensitivity_refit_plan.csv"),
                   scaleaware_sensitivity_table_path("sensitivity_refit_fit_status.csv"),
                   sens_env)
      run_r_script("P7C_SENS_MCMC_DIAGNOSTICS", "Sensitivity MCMC diagnostics gate", "scripts/v3/16_v3_sensitivity_mcmc_diagnostics_winsor.R",
                   scaleaware_sensitivity_table_path("sensitivity_mcmc_diagnostics_summary.csv"),
                   scaleaware_sensitivity_table_path("sensitivity_stacking_eligibility_counts.csv"),
                   sens_env)
      run_r_script("P7D_SENS_STACKING", "Sensitivity stacking weights", "scripts/v3/17_v3_sensitivity_stacking_winsor.R",
                   scaleaware_sensitivity_table_path("sensitivity_stacking_weights_by_scenario.csv"),
                   scaleaware_sensitivity_table_path("sensitivity_top_models_comparison.csv"),
                   sens_env)
      run_r_script("P7E_SENS_DA", "Sensitivity uncertainty-adjusted DA", "scripts/v3/18_v3_sensitivity_construct_DA_winsor.R",
                   scaleaware_sensitivity_table_path("sensitivity_DA_by_scenario_long.csv"),
                   scaleaware_sensitivity_table_path("sensitivity_DA_stability_summary.csv"),
                   sens_env)
      run_r_script("P7F_SENS_VALIDATION", "Sensitivity validation/outcome tests", "scripts/v3/19_v3_sensitivity_validation_winsor.R",
                   scaleaware_sensitivity_table_path("sensitivity_validation_summary.csv"),
                   scaleaware_sensitivity_log_path("v3_sensitivity_validation_notes.txt"),
                   sens_env)
      run_r_script("P7G_SENS_REPORT", "Sensitivity report", "scripts/v3/20_v3_sensitivity_report_winsor.R",
                   scaleaware_sensitivity_report_path("sensitivity_report_v3.md"),
                   scaleaware_sensitivity_table_path("sensitivity_reproducibility_info.csv"),
                   sens_env)
    } else {
      mark_skipped_step("P7_SENSITIVITY", "Phase 7 full-refit sensitivity workflow", "scripts/v3/14_v3_sensitivity_prior_predictive_winsor.R",
                        scaleaware_sensitivity_table_path("sensitivity_prior_predictive_summary.csv"),
                        "", "V3_RUN_SENSITIVITY is FALSE.")
    }
    if (run_validation) {
      run_r_script("P6B_VALIDATION", "Phase 6b validation on scale-aware Student-t DA", "scripts/v3/21_v3_validation_on_scaleaware_student_DA.R",
                   scaleaware_validation_path("table_v3_validation_comparison_summary_scaleaware_student.csv"),
                   scaleaware_validation_path("v3_phase6b_validation_scaleaware_student_notes.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = env_value("V3_MODEL_STRUCTURE", "pooled_random_intercept")
                   ))
    } else {
      mark_skipped_step("P6B_VALIDATION", "Phase 6b validation on scale-aware Student-t DA", "scripts/v3/21_v3_validation_on_scaleaware_student_DA.R",
                        scaleaware_validation_path("table_v3_validation_comparison_summary_scaleaware_student.csv"),
                        "", "V3_RUN_VALIDATION is FALSE.")
    }
    if (run_varying_slopes) {
      run_r_script("P3B_VARYSLOPES", "Breuer-like varying-slope robustness fits", "scripts/v3/07_v3_fit_brms_named_models_winsor.R",
                   scaleaware_varyslopes_table_path("table_v3_varyslopes_diagnostics.csv"),
                   scaleaware_varyslopes_log_path("v3_varyslopes_notes.txt"),
                   c(
                     V3_OUTPUT_ROOT = scaleaware_root,
                     V3_INPUT_WINSOR_ROOT = winsor_input_root,
                     V3_PRIOR_SET_ID = env_value("V3_PRIOR_SET_ID", "scale_aware_student_baseline_v1"),
                     V3_FAMILY = env_value("V3_FAMILY", "student"),
                     V3_MODEL_STRUCTURE = "breuer_varying_slopes",
                     V3_RUN_VARYING_SLOPES = "TRUE",
                     V3_VARYSLOPE_SCOPE = env_value("V3_VARYSLOPE_SCOPE", "LEADING_ONLY"),
                     V3_VARYSLOPE_GROUP = env_value("V3_VARYSLOPE_GROUP", "industry_year")
                   ))
    } else {
      mark_skipped_step("P3B_VARYSLOPES", "Breuer-like varying-slope robustness fits", "scripts/v3/07_v3_fit_brms_named_models_winsor.R",
                        scaleaware_varyslopes_table_path("table_v3_varyslopes_diagnostics.csv"),
                        "", "V3_RUN_VARYING_SLOPES is FALSE.")
    }
    compare_old_invalid_vs_corrected()
    final_status <- "CORRECTED_RERUN_SUCCESS"
  }
}

phase_status <- function(pattern) {
  hits <- step_manifest[grepl(pattern, step_manifest$Step_ID), , drop = FALSE]
  if (nrow(hits) == 0) return("NOT_RUN")
  paste(unique(hits$Status), collapse = "/")
}

corrected_counts <- c(
  ex_post = sample_n(v3_baseline_table_path("final_v3_common_ex_post_sample.csv")),
  no_lookahead = sample_n(v3_baseline_table_path("final_v3_common_realtime_sample.csv")),
  winsor_ex_post = sample_n(file.path(v3_input_winsor_root, "tables", "final_v3_common_ex_post_sample_winsor.csv")),
  winsor_no_lookahead = sample_n(file.path(v3_input_winsor_root, "tables", "final_v3_common_realtime_sample_winsor.csv"))
)

summary_lines <- c(
  "===== COGS/INV CORRECTED DATA RESET + RERUN SUMMARY =====",
  paste("1. Raw workbook hash:", ifelse(is.na(sha), "SHA256 unavailable", sha)),
  paste("2. Invalid outputs quarantine folder:", normalize_path(quarantine_root)),
  paste("3. Dry run or actual reset:", ifelse(dry_run, "DRY_RUN", ifelse(actual_reset, "ACTUAL_RESET", "NO_OUTPUTS_TO_MOVE_OR_ALREADY_RESET"))),
  paste("4. COGS/INV audit status:", audit_status),
  paste("4a. Main sample audit status:", main_sample_audit_status),
  paste("4b. Secondary operating-cycle audit status:", secondary_operating_cycle_audit_status),
  paste("5. Phase 1 status:", phase_status("^P1$")),
  paste("6. Winsorization status:", phase_status("^P1B$")),
  paste("6a. positioning/difference note status:", phase_status("^POSITIONING$")),
  paste("7. prior predictive status:", phase_status("^P3A_PRIOR_PREDICTIVE$")),
  paste("8. scale-aware Student-t brms fit status:", phase_status("^P3B_SCALEAWARE_STUDENT$")),
  paste("9. MCMC diagnostics status:", phase_status("^P3C_MCMC_DIAGNOSTICS$")),
  paste("10. row-level LOO status:", phase_status("^P4C_SCALEAWARE_STUDENT$")),
  paste("11. uncertainty-adjusted DA status:", phase_status("^P5B_SCALEAWARE_STUDENT$")),
  paste("12. posterior predictive check status:", phase_status("^P5C_POSTERIOR_PPC$")),
  paste("13. grouped PSIS-LOFO status:", phase_status("^P4D_SCALEAWARE_STUDENT$")),
  paste("14. exact grouped K-fold status:", phase_status("^P4E")),
  paste("15. sensitivity prior predictive status:", phase_status("^P7A_SENS_PRIOR_PREDICTIVE$")),
  paste("16. sensitivity refit status:", phase_status("^P7B_SENS_REFIT$")),
  paste("17. sensitivity diagnostics status:", phase_status("^P7C_SENS_MCMC_DIAGNOSTICS$")),
  paste("18. sensitivity stacking status:", phase_status("^P7D_SENS_STACKING$")),
  paste("19. sensitivity DA status:", phase_status("^P7E_SENS_DA$")),
  paste("20. sensitivity validation status:", phase_status("^P7F_SENS_VALIDATION$")),
  paste("21. sensitivity report status:", phase_status("^P7G_SENS_REPORT$")),
  paste("22. validation rerun status:", phase_status("^P6B_VALIDATION$")),
  paste("23. varying-slope robustness status:", phase_status("^P3B_VARYSLOPES$")),
  paste("24. Final corrected rerun status:", final_status),
  "",
  paste("Corrected data manifest:", normalize_path(corrected_data_manifest_path)),
  paste("Corrected rerun manifest:", normalize_path(corrected_rerun_manifest_path)),
  paste("Move manifest:", ifelse(dry_run, "outputs/dry_run_invalid_output_move_manifest_<timestamp>.csv", file.path(quarantine_root, "invalid_output_move_manifest.csv"))),
  paste("Corrected common sample N ex-post:", corrected_counts[["ex_post"]]),
  paste("Corrected common sample N no-look-ahead:", corrected_counts[["no_lookahead"]]),
  paste("Corrected winsor sample N ex-post:", corrected_counts[["winsor_ex_post"]]),
  paste("Corrected winsor sample N no-look-ahead:", corrected_counts[["winsor_no_lookahead"]]),
  paste("Prior predictive mode:", prior_predictive_mode),
  paste("Sensitivity mode:", sensitivity_mode),
  paste("Sensitivity dry run:", sensitivity_dry_run),
  paste("K-fold:", ifelse(run_kfold, paste("RUN", kfold_mode, "K=", kfold_k, "target_mode=", kfold_target_mode), "SKIPPED")),
  "Next actions:",
  if (dry_run) {
    "Set V3_RESET_DRY_RUN='FALSE' and V3_RESET_CONFIRM='TRUE' to quarantine invalid outputs."
  } else if (!rerun_after_reset) {
    "Set V3_RERUN_AFTER_RESET='TRUE' after reviewing quarantine manifest to run the corrected pipeline."
  } else if (final_status == "REVIEW_REQUIRED_AFTER_COGS_INV_AUDIT") {
    paste("Review", normalize_path(baseline_log_path("cogs_inv_correction_audit_notes.txt")), "then rerun with V3_CONTINUE_AFTER_AUDIT_WARNING='TRUE' only if acceptable.")
  } else {
    "Review corrected manifests and run exact grouped K-fold separately if it was skipped."
  }
)

writeLines(summary_lines, corrected_rerun_summary_path)
cat(paste(summary_lines[1:15], collapse = "\n"), "\n")
