# Script: se02c_collect_prior_scenario_outputs.R
# Purpose: Collect task-local SE02 prior-scenario outputs into canonical scenario paths.

source("scripts/ma00_setup.R")
phase_begin("se02c", "Collect prior-scenario outputs")

ensure_analysis_dirs()
ensure_sensitivity_dirs()

tables_dir <- file.path(output_root, "sensitivity", "tables")
manifest_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_status.csv")

if (!file.exists(manifest_path) || !file.exists(status_path)) {
  stop("[BLOCKER] se02c requires se02a manifest and se02b task status.")
}

manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE, check.names = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "se02c sensitivity collect")

required_status <- status[status$Required %in% c(TRUE, "TRUE", "true", "1", 1L), , drop = FALSE]
bad_status <- required_status[!required_status$status %in% c("SUCCESS", "WARNING"), , drop = FALSE]
if (nrow(bad_status)) {
  stop("[BLOCKER] se02c requires every required SE02B task to have SUCCESS or WARNING status before collecting: ",
       paste(bad_status$Task_Key, collapse = ", "))
}

status_small <- status[, intersect(c("Task_Key", "status", "reason", "fit_path", "draw_path", "metadata_path"), names(status)), drop = FALSE]
manifest_status <- merge(manifest, status_small, by = "Task_Key", all.x = TRUE, suffixes = c("", "_status"), sort = FALSE)

read_task_metadata <- function(path) {
  if (!file.exists(path)) stop("[BLOCKER] se02c missing task metadata: ", path)
  meta <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(meta)) stop("[BLOCKER] se02c empty task metadata: ", path)
  meta
}

metadata_matches_manifest <- function(meta, task) {
  cols <- c("Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure")
  missing <- setdiff(cols, names(meta))
  if (length(missing)) return(paste("metadata missing columns:", paste(missing, collapse = ", ")))
  mismatches <- character()
  for (col in cols) {
    if (!identical(as.character(meta[[col]][1]), as.character(task[[col]][1]))) {
      mismatches <- c(mismatches, paste0(col, " metadata=", meta[[col]][1], " manifest=", task[[col]][1]))
    }
  }
  if (length(mismatches)) paste(mismatches, collapse = "; ") else NA_character_
}

copy_checked <- function(from, to, label) {
  if (!file.exists(from)) stop("[BLOCKER] se02c missing task-local ", label, ": ", from)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(from, to, overwrite = TRUE)
  if (!isTRUE(ok)) stop("[BLOCKER] se02c failed to copy ", label, " from ", from, " to ", to)
  invisible(to)
}

plan_rows <- list()
fit_status_rows <- list()
audit_rows <- list()

for (i in seq_len(nrow(manifest_status))) {
  task <- manifest_status[i, , drop = FALSE]
  task_metadata_path <- task$metadata_path
  task_fit_path <- task$fit_path
  task_draw_path <- task$draw_path
  meta <- read_task_metadata(task_metadata_path)
  mismatch <- metadata_matches_manifest(meta, task)
  if (!is.na(mismatch)) {
    stop("[BLOCKER] se02c task metadata scenario identifiers do not match manifest for ",
         task$Task_Key, ": ", mismatch)
  }
  for (path in c(task_fit_path, task_draw_path, task_metadata_path)) {
    if (!file.exists(path)) stop("[BLOCKER] se02c missing required task-local artifact for ", task$Task_Key, ": ", path)
  }

  scenario <- as.character(task$Scenario)
  scenario_root <- sensitivity_root(scenario)
  model_key <- model_key_sampled(
    task$Model_ID,
    task$Target_Space,
    task$Sample_Group,
    task$Heterogeneity_Variant,
    paste0("_", scenario, "_winsor")
  )
  canonical_fit_path <- file.path(scenario_root, "fits", paste0("fit_", model_key, ".rds"))
  canonical_draw_path <- file.path(scenario_root, "draws", paste0("draws_", model_key, ".rds"))
  canonical_metadata_path <- file.path(scenario_root, "fits", paste0("fit_", model_key, "_metadata.csv"))

  copy_checked(task_fit_path, canonical_fit_path, "fit")
  copy_checked(task_draw_path, canonical_draw_path, "draw")
  copy_checked(task_metadata_path, canonical_metadata_path, "metadata")

  plan_rows[[length(plan_rows) + 1L]] <- data.frame(
    scenario = scenario,
    prior_set_id = task$Prior_Set_ID,
    family = task$Likelihood_Family,
    model_structure = task$Model_Structure,
    model_id = task$Model_ID,
    model_name = task$Model_Name,
    target_space = task$Target_Space,
    sample_group = task$Sample_Group,
    heterogeneity_variant = task$Heterogeneity_Variant,
    target_sample = task$Target_Sample,
    brms_formula = task$brms_Formula,
    chains = as.integer(task$chains),
    cores = as.integer(task$cores),
    iter = as.integer(task$iter),
    warmup = as.integer(task$warmup),
    adapt_delta = as.numeric(task$adapt_delta),
    max_treedepth = as.integer(task$max_treedepth),
    refresh = as.integer(task$refresh),
    backend = task$backend,
    RNG_Context = task$RNG_Context,
    RNG_Offset = as.integer(task$RNG_Offset),
    Canonical_Seed = as.integer(task$Canonical_Seed),
    Effective_Seed = as.integer(task$Effective_Seed),
    RNG_Source = task$RNG_Source,
    fit_path = canonical_fit_path,
    draw_path = canonical_draw_path,
    metadata_path = canonical_metadata_path,
    task_fit_path = task_fit_path,
    task_draw_path = task_draw_path,
    task_metadata_path = task_metadata_path,
    status = task$status,
    stringsAsFactors = FALSE
  )

  fit_status_rows[[length(fit_status_rows) + 1L]] <- cbind(
    meta[1, , drop = FALSE],
    data.frame(
      canonical_fit_path = canonical_fit_path,
      canonical_draw_path = canonical_draw_path,
      canonical_metadata_path = canonical_metadata_path,
      task_fit_path = task_fit_path,
      task_draw_path = task_draw_path,
      task_metadata_path = task_metadata_path,
      stringsAsFactors = FALSE
    )
  )

  audit_rows[[length(audit_rows) + 1L]] <- data.frame(
    scenario = scenario,
    prior_set_id = task$Prior_Set_ID,
    family = task$Likelihood_Family,
    model_structure = task$Model_Structure,
    model_id = task$Model_ID,
    target_space = task$Target_Space,
    sample_group = task$Sample_Group,
    heterogeneity_variant = task$Heterogeneity_Variant,
    status = task$status,
    warning_count = if ("warning_count" %in% names(meta)) meta$warning_count[1] else NA_integer_,
    error_message = if ("reason" %in% names(meta)) meta$reason[1] else NA_character_,
    elapsed_seconds = if ("runtime_seconds" %in% names(meta)) meta$runtime_seconds[1] else NA_real_,
    fit_path = canonical_fit_path,
    draw_path = canonical_draw_path,
    metadata_path = canonical_metadata_path,
    task_fit_path = task_fit_path,
    task_draw_path = task_draw_path,
    task_metadata_path = task_metadata_path,
    stringsAsFactors = FALSE
  )
}

plan <- do.call(rbind, plan_rows)
fit_status <- do.call(rbind, fit_status_rows)
audit_summary <- do.call(rbind, audit_rows)

write_csv_safely(plan, file.path(tables_dir, "sensitivity_refit_plan.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(fit_status, file.path(tables_dir, "sensitivity_refit_fit_status.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(audit_summary, file.path(tables_dir, "sensitivity_refit_audit_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

message("se02c collected sensitivity task metadata and wrote shared outputs.")
phase_end("se02c", "Collect prior-scenario outputs")
