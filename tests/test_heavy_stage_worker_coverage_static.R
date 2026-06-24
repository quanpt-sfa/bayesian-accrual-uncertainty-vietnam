source("scripts/ma00_setup.R")

txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

registry <- accrual_heavy_fit_stage_registry()
required_cols <- c("stage_id", "fit_script", "collect_script", "original_script", "fit_kind",
                   "config_helper", "worker_required", "shared_outputs_parent_only", "notes")
missing_cols <- setdiff(required_cols, names(registry))
if (length(missing_cols)) stop("Heavy fit stage registry missing columns: ", paste(missing_cols, collapse = ", "))

required_stage_ids <- c("ma09", "ma12", "ma13", "se02", "si03", "si04", "di08")
missing_stage_ids <- setdiff(required_stage_ids, registry$stage_id)
if (length(missing_stage_ids)) stop("Heavy fit stage registry missing stage_id(s): ", paste(missing_stage_ids, collapse = ", "))

shared_output_fragments <- c(
  "table_stacking_weights",
  "final_uncertainty_adjusted",
  "LATEST_COMPLETED_RUN",
  "manuscript",
  "table_winsor_kfold_weights",
  "table_winsor_row_exact_kfold_weights",
  "table_loo_comparison_winsor_corrected.csv"
)

for (i in seq_len(nrow(registry))) {
  row <- registry[i, ]
  fit_script <- row$fit_script
  collect_script <- row$collect_script
  if (!file.exists(fit_script)) stop("Missing worker fit script: ", fit_script)
  if (!file.exists(collect_script)) stop("Missing collect script: ", collect_script)

  fit_text <- txt(fit_script)
  if (isTRUE(row$worker_required)) {
    if (grepl("BLOCKED_PENDING_SPLIT_IMPLEMENTATION", fit_text, fixed = TRUE)) {
      stop(fit_script, " still contains the forbidden split-stage placeholder status.")
    }
    if (!grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", fit_text, perl = TRUE)) {
      stop(fit_script, " must contain the migrated task-specific brms fit body.")
    }
    for (fragment in c("accrual_run_task_pool(", "accrual_fit_worker_config(", "write_task_status(")) {
      if (!grepl(fragment, fit_text, fixed = TRUE)) stop(fit_script, " missing worker fragment: ", fragment)
    }
    if (!grepl("Effective_Seed", fit_text, fixed = TRUE) || !grepl("RNG_Context", fit_text, fixed = TRUE)) {
      stop(fit_script, " must use deterministic ma00 RNG metadata from the task manifest.")
    }
    if (grepl("worker_id|cluster_id|Sys\\.getpid|process ID|seq_along\\(results\\)", fit_text, perl = TRUE)) {
      stop(fit_script, " must not derive seeds from worker identity or scheduling order.")
    }
    if (row$stage_id %in% c("ma12", "ma13")) {
      if (!grepl("Fold_Assignment_Path", fit_text, fixed = TRUE)) {
        stop(fit_script, " must read planned K-fold assignments from the task manifest.")
      }
      if (grepl("\\bsample\\s*\\(", fit_text, perl = TRUE) || grepl("\\bset\\.seed\\s*\\(", fit_text, perl = TRUE)) {
        stop(fit_script, " must not sample K-fold partitions inside the worker.")
      }
    }
    worker_shared_hits <- shared_output_fragments[vapply(shared_output_fragments, grepl, logical(1), x = fit_text, fixed = TRUE)]
    if (length(worker_shared_hits)) {
      stop(fit_script, " worker stage names collector-owned shared output(s): ", paste(worker_shared_hits, collapse = ", "))
    }
  }

  collect_text <- txt(collect_script)
  if (grepl("BLOCKED_PENDING_SPLIT_IMPLEMENTATION", collect_text, fixed = TRUE)) {
    stop(collect_script, " still contains the forbidden split-stage placeholder status.")
  }
  if (grepl("collect_contract|contract is in place", collect_text, perl = TRUE)) {
    stop(collect_script, " still contains collector-contract placeholder output.")
  }
  for (fragment in c("read.csv(manifest_path", "read.csv(status_path", "accrual_task_status_blocker(", "write.csv(")) {
    if (!grepl(fragment, collect_text, fixed = TRUE)) stop(collect_script, " missing collector fragment: ", fragment)
  }
  if (grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", collect_text, perl = TRUE)) {
    stop(collect_script, " must not fit brms models.")
  }
}

cat("test_heavy_stage_worker_coverage_static.R passed\n")
