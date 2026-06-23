# -----------------------------------------------------------------------------
# Script: ma07a_fit_brms_named_models.R
# Purpose: Fit-stage worker for baseline/remediation brms models.
#          Workers write only task-specific fit/meta/log artifacts.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(brms)
})

source("scripts/ma00_setup.R")
phase_begin("ma07a", "Fit baseline brms models")
ensure_analysis_dirs()
write_method_design_files()
write_prior_registry()
write_execution_config_registry()
validate_final_analysis_config("ma07a baseline brms fit", final_mode = TRUE)

backfill_diagnostics_only <- env_flag("ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY", "FALSE")
remediation_targets_raw <- trimws(env_value("ACCRUAL_MCMC_REMEDIATION_TARGETS", ""))
remediation_mode <- nzchar(remediation_targets_raw)
adopt_legacy_ma07_fits <- env_flag("ACCRUAL_ADOPT_LEGACY_MA07_FITS", "FALSE")
if (backfill_diagnostics_only && force_refit) {
  stop("[BLOCKER] ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE cannot be combined with ACCRUAL_FORCE_REFIT=TRUE.")
}
if (remediation_mode && force_refit) {
  stop("[BLOCKER] ACCRUAL_MCMC_REMEDIATION_TARGETS cannot be combined with ACCRUAL_FORCE_REFIT=TRUE.")
}
if (remediation_mode && backfill_diagnostics_only) {
  stop("[BLOCKER] ACCRUAL_MCMC_REMEDIATION_TARGETS cannot be combined with ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE.")
}

gate_csv_path <- file.path(output_root, "prior_predictive_gate_status.csv")
if (!file.exists(gate_csv_path)) {
  stop("[BLOCKER] Prior predictive gate status file does not exist. Please run ma06 first.")
}
gate_df <- read.csv(gate_csv_path, stringsAsFactors = FALSE)
if (any(gate_df$status == "FAIL") && !env_flag("ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL", "FALSE")) {
  stop("[BLOCKER] Prior predictive check gate contains FAIL. Fitting blocked.")
}

diagnostic_key_for_row <- function(row) {
  paste(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, sep = "|")
}

parse_remediation_targets <- function(raw_value) {
  if (!nzchar(raw_value)) return(character())
  entries <- trimws(unlist(strsplit(raw_value, ";", fixed = TRUE)))
  entries <- entries[nzchar(entries)]
  invalid <- entries[vapply(entries, function(entry) {
    parts <- trimws(unlist(strsplit(entry, "|", fixed = TRUE)))
    length(parts) != 4 || any(!nzchar(parts))
  }, logical(1))]
  if (length(invalid)) {
    stop("[BLOCKER] Invalid ACCRUAL_MCMC_REMEDIATION_TARGETS key(s): ", paste(invalid, collapse = "; "))
  }
  unique(entries)
}

metadata_matches_file <- function(path, expected) {
  if (!file.exists(path)) return(FALSE)
  old <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  if (nrow(old) == 0) return(FALSE)
  for (nm in names(expected)) {
    if (!nm %in% names(old)) return(FALSE)
    if (!identical(as.character(old[[nm]][1]), as.character(expected[[nm]][1]))) return(FALSE)
  }
  TRUE
}

metadata_state_file <- function(path, expected) {
  if (!file.exists(path)) return("legacy_metadata_missing")
  if (metadata_matches_file(path, expected)) return("metadata_matched")
  "metadata_mismatch"
}

write_metadata_file <- function(path, expected) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(as.data.frame(expected, stringsAsFactors = FALSE), path, row.names = FALSE)
}

run_varying_slope_models <- identical(model_structure, "breuer_varying_slopes")
if (run_varying_slope_models && !run_varying_slopes) {
  stop("[BLOCKER] ACCRUAL_MODEL_STRUCTURE='breuer_varying_slopes' requires ACCRUAL_RUN_VARYING_SLOPES='TRUE'.")
}

phase_root <- if (run_varying_slope_models) varyslopes_root else output_root
for (d in file.path(phase_root, c("", "tables", "models", "draws", "logs", "figures", "manifests"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) {
  stop("[BLOCKER] Winsorized formula table not found. Run ma05 first.")
}
formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
if (run_varying_slope_models) {
  formulas_df <- formulas_df %>%
    filter(Main_Stack_Inclusion == TRUE) %>%
    filter(mapply(varying_slope_candidate, Model_ID, Target_Space)) %>%
    group_by(Model_ID, Target_Space, Sample_Group) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      Heterogeneity_Variant = paste0("Breuer-like varying slopes (", varyslope_group, ")"),
      brms_Formula = vapply(Base_Formula, varying_slope_formula, character(1), group = varyslope_group),
      Model_Structure = model_structure,
      VarySlope_Group = varyslope_group,
      VarySlope_Scope = varyslope_scope
    )
}

sampler_cfg <- accrual_sampler_config("baseline", varying_slopes = run_varying_slope_models)
baseline_sampler_controls <- sampler_cfg[c("chains", "cores", "iter", "warmup", "adapt_delta", "max_treedepth")]
remediation_cfg <- accrual_sampler_config("baseline_remediation")
remediation_sampler_controls <- remediation_cfg[c("chains", "cores", "iter", "warmup", "adapt_delta", "max_treedepth")]
parallel_cfg <- accrual_model_parallel_config(baseline_sampler_controls$cores, "ma07a baseline brms fit")

formulas_df <- formulas_df %>%
  mutate(
    order_key = case_when(
      Model_ID %in% c("M01", "M02", "M03", "M04", "M05", "M06") ~ 1,
      Model_ID %in% c("M07", "M09", "M10") ~ 2,
      Model_ID == "M08" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  arrange(order_key, Model_ID, Target_Space, Heterogeneity_Variant)

remediation_targets <- parse_remediation_targets(remediation_targets_raw)
if (length(remediation_targets)) {
  formula_keys <- vapply(seq_len(nrow(formulas_df)), function(i) diagnostic_key_for_row(formulas_df[i, ]), character(1))
  unmatched <- setdiff(remediation_targets, formula_keys)
  if (length(unmatched)) {
    stop("[BLOCKER] ACCRUAL_MCMC_REMEDIATION_TARGETS contains key(s) that do not match any Step 7 formula row: ",
         paste(unmatched, collapse = "; "))
  }
}

build_task_manifest <- function(formulas) {
  rows <- lapply(seq_len(nrow(formulas)), function(i) {
    row <- formulas[i, ]
    key <- diagnostic_key_for_row(row)
    is_remediation <- key %in% remediation_targets
    controls <- if (is_remediation) remediation_sampler_controls else baseline_sampler_controls
    model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
    rng_context <- if (is_remediation) {
      paste0("baseline_fit_brms_named_models_remediation_", key)
    } else {
      paste0("baseline_fit_brms_named_models_", key)
    }
    rng <- accrual_rng_metadata_list(rng_context, offset = i)
    data.frame(
      task_index = i,
      task_key = key,
      model_key = model_key,
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Target_Sample = row$Target_Sample,
      brms_Formula = row$brms_Formula,
      Main_Stack_Inclusion = row$Main_Stack_Inclusion,
      Secondary_Robustness = row$Secondary_Robustness,
      Reason = if ("Reason" %in% names(row)) row$Reason else NA_character_,
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      chains = controls$chains,
      cores = controls$cores,
      iter = controls$iter,
      warmup = controls$warmup,
      adapt_delta = controls$adapt_delta,
      max_treedepth = controls$max_treedepth,
      backend = "rstan",
      RNG_Context = rng$RNG_Context,
      RNG_Offset = rng$RNG_Offset,
      Canonical_Seed = rng$Canonical_Seed,
      Effective_Seed = rng$Effective_Seed,
      RNG_Source = rng$RNG_Source,
      row_is_remediation_target = is_remediation,
      fit_path = file.path(phase_root, "models", paste0("fit_", model_key, ".rds")),
      draw_path = file.path(phase_root, "draws", paste0("draws_", model_key, ".rds")),
      metadata_path = file.path(phase_root, "models", paste0("meta_", model_key, ".csv")),
      log_path = file.path(phase_root, "logs", paste0("fit_", model_key, ".log")),
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
}

task_manifest <- build_task_manifest(formulas_df)
manifest_path <- file.path(phase_root, "tables", "table_ma07_fit_task_manifest.csv")
status_path <- file.path(phase_root, "tables", "table_ma07_fit_task_status.csv")
write.csv(task_manifest, manifest_path, row.names = FALSE)

fit_ma07a_task_worker <- function(task) {
  suppressPackageStartupMessages({
    library(brms)
    library(dplyr)
  })
  source("scripts/ma00_setup.R")
  status <- "FAILED"
  error_message <- ""
  metadata_status <- NA_character_
  started_at <- as.character(Sys.time())
  fit_exists_before <- file.exists(task$fit_path)
  metadata_matches <- FALSE
  dir.create(dirname(task$fit_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$log_path), recursive = TRUE, showWarnings = FALSE)

  expected_meta <- task[c(
    "task_key", "model_key", "Model_ID", "Model_Name", "Target_Space", "Sample_Group",
    "Heterogeneity_Variant", "Target_Sample", "brms_Formula", "Prior_Set_ID",
    "Likelihood_Family", "Model_Structure", "chains", "cores", "iter", "warmup",
    "adapt_delta", "max_treedepth", "backend", "RNG_Context", "RNG_Offset",
    "Canonical_Seed", "Effective_Seed", "RNG_Source"
  )]

  log_lines <- c(
    paste0("Task: ", task$task_key),
    paste0("Started: ", started_at),
    paste0("Fit path: ", task$fit_path),
    paste0("Metadata path: ", task$metadata_path)
  )

  result <- tryCatch({
    metadata_status <- metadata_state_file(task$metadata_path, expected_meta)
    metadata_matches <- identical(metadata_status, "metadata_matched")
    if (file.exists(task$fit_path) && metadata_matches && !force_refit && !isTRUE(task$row_is_remediation_target)) {
      status <- "SKIPPED_EXISTING_MATCHED_FIT"
    } else if (file.exists(task$fit_path) &&
               identical(metadata_status, "legacy_metadata_missing") &&
               backfill_diagnostics_only &&
               !force_refit) {
      status <- "SKIPPED_BACKFILL_EXISTING_FIT"
    } else if (file.exists(task$fit_path) &&
               identical(metadata_status, "legacy_metadata_missing") &&
               adopt_legacy_ma07_fits &&
               !force_refit) {
      write_metadata_file(task$metadata_path, expected_meta)
      metadata_status <- "legacy_metadata_adopted"
      status <- "SKIPPED_ADOPTED_LEGACY_FIT"
    } else if (file.exists(task$fit_path) &&
               identical(metadata_status, "legacy_metadata_missing") &&
               !force_refit) {
      status <- "BLOCKED_METADATA_MISSING"
      stop("[BLOCKER] Existing ma07 fit is missing metadata. Set ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE for diagnostics-only backfill or ACCRUAL_ADOPT_LEGACY_MA07_FITS=TRUE to adopt the legacy fit without refitting: ", task$fit_path)
    } else if (file.exists(task$fit_path) && !metadata_matches && !force_refit && !isTRUE(task$row_is_remediation_target)) {
      status <- "BLOCKED_METADATA_MISMATCH"
      stop("[BLOCKER] Existing ma07 fit metadata does not match requested configuration: ", task$fit_path)
    } else if (remediation_mode && !isTRUE(task$row_is_remediation_target)) {
      if (!file.exists(task$fit_path)) {
        status <- "BLOCKED_MISSING_NON_TARGET_FIT"
        stop("[BLOCKER] Non-target remediation row is missing required fit .rds file: ", task$task_key)
      }
      status <- "SKIPPED_REMEDIATION_NON_TARGET"
    } else if (backfill_diagnostics_only) {
      if (!file.exists(task$fit_path)) {
        status <- "BLOCKED_BACKFILL_MISSING_FIT"
        stop("[BLOCKER] ACCRUAL_STEP7_BACKFILL_DIAGNOSTICS_ONLY=TRUE requires existing fit: ", task$fit_path)
      }
      status <- "SKIPPED_BACKFILL_EXISTING_FIT"
    } else {
      df_scaled <- read_winsor_sample(task$Target_Sample)
      if (identical(model_structure, "breuer_varying_slopes")) {
        df_scaled <- prepare_varying_slope_data(df_scaled)
      }
      formula_str <- fix_formula(task$brms_Formula)
      prior_list <- default_prior_list(task$Heterogeneity_Variant, model_structure = model_structure)
      options(mc.cores = task$cores)
      fit <- brms::brm(
        formula = brms::bf(stats::as.formula(formula_str)),
        data = df_scaled,
        family = brms_family(),
        prior = prior_list,
        chains = task$chains,
        cores = task$cores,
        iter = task$iter,
        warmup = task$warmup,
        control = list(adapt_delta = task$adapt_delta, max_treedepth = task$max_treedepth),
        seed = task$Effective_Seed,
        save_pars = brms::save_pars(all = TRUE),
        refresh = 500
      )
      saveRDS(fit, task$fit_path)
      write_metadata_file(task$metadata_path, expected_meta)
      metadata_status <- "metadata_written"
      status <- "SUCCESS"
    }
    NULL
  }, error = function(e) {
    error_message <<- conditionMessage(e)
    NULL
  })
  invisible(result)

  ended_at <- as.character(Sys.time())
  log_lines <- c(log_lines, paste0("Ended: ", ended_at), paste0("Status: ", status),
                 paste0("Metadata status: ", metadata_status), paste0("Error: ", error_message))
  writeLines(log_lines, task$log_path, useBytes = TRUE)
  data.frame(
    task_index = task$task_index,
    task_key = task$task_key,
    model_key = task$model_key,
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    Main_Stack_Inclusion = task$Main_Stack_Inclusion,
    Secondary_Robustness = task$Secondary_Robustness,
    status = status,
    error_message = error_message,
    fit_path = task$fit_path,
    draw_path = task$draw_path,
    metadata_path = task$metadata_path,
    log_path = task$log_path,
    fit_exists_before = fit_exists_before,
    fit_exists_after = file.exists(task$fit_path),
    metadata_matches_before = metadata_matches,
    metadata_status = metadata_status,
    started_at = started_at,
    ended_at = ended_at,
    stringsAsFactors = FALSE
  )
}

task_list <- lapply(seq_len(nrow(task_manifest)), function(i) as.list(task_manifest[i, ]))
if (parallel_cfg$enabled && parallel_cfg$workers > 1L) {
  message("[MA07A] Running model-level worker pool with workers=", parallel_cfg$workers,
          ", cores_per_fit=", parallel_cfg$cores_per_fit,
          ", total_core_budget=", parallel_cfg$total_core_budget)
  cl <- parallel::makeCluster(parallel_cfg$workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(
    cl,
    varlist = c("fit_ma07a_task_worker", "metadata_matches_file", "metadata_state_file",
                "write_metadata_file", "force_refit", "remediation_mode",
                "backfill_diagnostics_only", "adopt_legacy_ma07_fits"),
    envir = environment()
  )
  statuses <- parallel::parLapplyLB(cl, task_list, fit_ma07a_task_worker)
} else {
  message("[MA07A] Running sequential fit stage.")
  statuses <- lapply(task_list, fit_ma07a_task_worker)
}

status_df <- bind_rows(statuses) %>% arrange(task_index)
write.csv(status_df, status_path, row.names = FALSE)

blocking <- status_df %>%
  filter(.data$status %in% c("FAILED", "BLOCKED_METADATA_MISSING", "BLOCKED_METADATA_MISMATCH", "BLOCKED_MISSING_NON_TARGET_FIT", "BLOCKED_BACKFILL_MISSING_FIT"))
if (nrow(blocking) && any(blocking$Main_Stack_Inclusion %in% TRUE)) {
  stop("[BLOCKER] Required main-stack ma07a fit task failed or was blocked: ",
       paste(blocking$task_key[blocking$Main_Stack_Inclusion %in% TRUE], collapse = "; "))
}

cat("[SUCCESS] ma07a fit-stage completed. Task status: ", status_path, "\n", sep = "")
phase_end("ma07a", "Fit baseline brms models")
