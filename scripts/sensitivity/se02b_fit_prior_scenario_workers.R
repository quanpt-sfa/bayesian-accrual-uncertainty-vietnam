# Script: se02b_fit_prior_scenario_workers.R
# Purpose: Fit scenario-specific sensitivity prior refit tasks through worker pool.

source("scripts/ma00_setup.R")
phase_begin("se02b", "Fit prior-scenario workers")

tables_dir <- file.path(output_root, "sensitivity", "tables")
manifest_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se02_prior_scenario_refit_task_status.csv")

if (!file.exists(manifest_path)) stop("[BLOCKER] Missing se02a task manifest: ", manifest_path)
tasks <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c(
  "Task_Key", "Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure",
  "Model_ID", "Model_Name", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
  "Target_Sample", "brms_Formula", "fit_path", "draw_path", "metadata_path",
  "task_log_path", "chains", "cores", "iter", "warmup", "adapt_delta",
  "max_treedepth", "refresh", "backend", "RNG_Context", "RNG_Offset",
  "Canonical_Seed", "Effective_Seed", "RNG_Source", "Required"
)
missing_cols <- setdiff(required_cols, names(tasks))
if (length(missing_cols)) {
  stop("[BLOCKER] se02b task manifest missing required columns: ", paste(missing_cols, collapse = ", "))
}

se02b_expected_metadata <- function(task, status = "PLANNED", reason = NA_character_,
                                    warning_count = 0L, runtime_seconds = NA_real_) {
  data.frame(
    Scenario = task$Scenario,
    Prior_Set_ID = task$Prior_Set_ID,
    Likelihood_Family = task$Likelihood_Family,
    Model_Structure = task$Model_Structure,
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    Target_Sample = task$Target_Sample,
    brms_Formula = task$brms_Formula,
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
    status = status,
    reason = reason,
    warning_count = as.integer(warning_count),
    runtime_seconds = as.numeric(runtime_seconds),
    fit_path = task$fit_path,
    draw_path = task$draw_path,
    metadata_path = task$metadata_path,
    stringsAsFactors = FALSE
  )
}

se02b_metadata_matches_task <- function(task) {
  if (!file.exists(task$metadata_path)) return(list(matches = FALSE, reason = "metadata missing"))
  meta <- tryCatch(read.csv(task$metadata_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(meta) || !nrow(meta)) return(list(matches = FALSE, reason = "metadata unreadable or empty"))
  expected <- se02b_expected_metadata(task, status = if ("status" %in% names(meta)) meta$status[1] else "SUCCESS")
  compare_cols <- c(
    "Scenario", "Prior_Set_ID", "Likelihood_Family", "Model_Structure",
    "Model_ID", "Model_Name", "Target_Space", "Sample_Group", "Heterogeneity_Variant",
    "Target_Sample", "brms_Formula", "chains", "cores", "iter", "warmup",
    "adapt_delta", "max_treedepth", "refresh", "backend", "RNG_Context",
    "RNG_Offset", "Canonical_Seed", "Effective_Seed", "RNG_Source",
    "fit_path", "draw_path", "metadata_path"
  )
  missing <- setdiff(compare_cols, names(meta))
  if (length(missing)) return(list(matches = FALSE, reason = paste("metadata missing columns:", paste(missing, collapse = ", "))))
  mismatches <- character()
  for (col in compare_cols) {
    actual <- meta[[col]][1]
    wanted <- expected[[col]][1]
    equal <- if (col %in% c("chains", "cores", "iter", "warmup", "max_treedepth", "refresh", "RNG_Offset", "Canonical_Seed", "Effective_Seed")) {
      identical(as.integer(actual), as.integer(wanted))
    } else if (identical(col, "adapt_delta")) {
      isTRUE(all.equal(as.numeric(actual), as.numeric(wanted), tolerance = 1e-12))
    } else {
      identical(as.character(actual), as.character(wanted))
    }
    if (!equal) mismatches <- c(mismatches, paste0(col, " metadata=", actual, " manifest=", wanted))
  }
  if (length(mismatches)) return(list(matches = FALSE, reason = paste(mismatches, collapse = "; ")))
  list(matches = TRUE, reason = NA_character_)
}

se02b_task_artifacts_exist <- function(task) {
  file.exists(task$fit_path) && file.exists(task$draw_path) && file.exists(task$metadata_path)
}

se02b_write_metadata <- function(task, status, reason, warning_count, runtime_seconds) {
  write_csv_safely(
    se02b_expected_metadata(task, status = status, reason = reason,
                            warning_count = warning_count, runtime_seconds = runtime_seconds),
    task$metadata_path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

fit_se02b_task_worker <- function(task) {
  task <- as.list(task)
  for (path in c(task$fit_path, task$draw_path, task$metadata_path, task$task_log_path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  started <- Sys.time()
  status <- "FAILED"
  reason <- NA_character_
  warning_count <- 0L

  writeLines(c(
    "se02b task log",
    paste("Task_Key:", task$Task_Key),
    paste("Scenario:", task$Scenario),
    paste("Prior_Set_ID:", task$Prior_Set_ID),
    paste("Likelihood_Family:", task$Likelihood_Family),
    paste("Model_Structure:", task$Model_Structure),
    paste("Effective_Seed:", task$Effective_Seed)
  ), task$task_log_path, useBytes = TRUE)

  if (se02b_task_artifacts_exist(task) && !isTRUE(force_refit)) {
    match_state <- se02b_metadata_matches_task(task)
    if (isTRUE(match_state$matches)) {
      runtime <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      se02b_write_metadata(task, "SUCCESS", "reused_existing_task_local_artifacts", 0L, runtime)
      return(data.frame(
        Task_Key = task$Task_Key,
        status = "SUCCESS",
        reason = "reused_existing_task_local_artifacts",
        Required = task$Required,
        fit_path = task$fit_path,
        draw_path = task$draw_path,
        metadata_path = task$metadata_path,
        stringsAsFactors = FALSE
      ))
    }
    stop(
      "[BLOCKER] Existing SE02B task-local artifacts do not match requested scenario metadata for ",
      task$Task_Key, ". Reason: ", match_state$reason,
      ". Set ACCRUAL_FORCE_REFIT=TRUE only if overwrite is intentional."
    )
  }

  fit_result <- tryCatch({
    df_scaled <- read_winsor_sample(task$Target_Sample)
    formula_str <- fix_formula(task$brms_Formula)
    prior_list <- default_prior_list(
      task$Heterogeneity_Variant,
      model_structure = task$Model_Structure,
      prior_set_id = task$Prior_Set_ID,
      family = task$Likelihood_Family
    )
    warning_log <- new.env(parent = emptyenv())
    warning_log$messages <- character()
    fit <- withCallingHandlers(
      brms::brm(
        formula = brms::bf(stats::as.formula(formula_str)),
        data = df_scaled,
        family = brms_family(task$Likelihood_Family),
        prior = prior_list,
        chains = as.integer(task$chains),
        cores = as.integer(task$cores),
        iter = as.integer(task$iter),
        warmup = as.integer(task$warmup),
        control = list(
          adapt_delta = as.numeric(task$adapt_delta),
          max_treedepth = as.integer(task$max_treedepth)
        ),
        seed = as.integer(task$Effective_Seed),
        save_pars = brms::save_pars(all = TRUE),
        refresh = as.integer(task$refresh)
      ),
      warning = function(w) {
        warning_log$messages <- c(warning_log$messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    draws <- list(
      epred = brms::posterior_epred(fit),
      predict = brms::posterior_predict(fit)
    )
    list(fit = fit, draws = draws, reason = NA_character_, warning_count = length(warning_log$messages))
  }, error = function(e) {
    list(fit = NULL, draws = NULL, reason = conditionMessage(e), warning_count = 0L)
  })

  reason <- fit_result$reason
  warning_count <- fit_result$warning_count
  if (!is.null(fit_result$fit) && !is.null(fit_result$draws)) {
    saveRDS(fit_result$fit, task$fit_path)
    saveRDS(fit_result$draws, task$draw_path)
    status <- if (warning_count > 0L) "WARNING" else "SUCCESS"
  }

  runtime <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  se02b_write_metadata(task, status, reason, warning_count, runtime)
  data.frame(
    Task_Key = task$Task_Key,
    status = status,
    reason = reason,
    Required = task$Required,
    fit_path = task$fit_path,
    draw_path = task$draw_path,
    metadata_path = task$metadata_path,
    stringsAsFactors = FALSE
  )
}

parallel_cfg <- accrual_fit_worker_config("sensitivity", max(as.integer(tasks$cores), na.rm = TRUE), "se02b prior-scenario workers")
results <- accrual_run_task_pool(
  split(tasks, seq_len(nrow(tasks))),
  fit_se02b_task_worker,
  parallel_cfg,
  export_names = c(
    "fit_se02b_task_worker",
    "se02b_expected_metadata",
    "se02b_metadata_matches_task",
    "se02b_task_artifacts_exist",
    "se02b_write_metadata"
  ),
  packages = "brms",
  context = "se02b prior-scenario workers"
)
status <- do.call(rbind, results)
write_task_status(status_path, status)
accrual_task_status_blocker(status, required_col = "Required", context = "se02b prior-scenario workers")
phase_end("se02b", "Fit prior-scenario workers")
