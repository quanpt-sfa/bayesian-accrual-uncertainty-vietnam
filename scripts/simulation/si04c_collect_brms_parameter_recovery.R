# Script: si04c_collect_brms_parameter_recovery.R
# Purpose: Collect BRMS parameter recovery simulation outputs.

source("scripts/ma00_setup.R")
phase_begin("si04c", "Collect BRMS parameter recovery")
root <- file.path(output_root, "simulation", "brms_parameter_recovery")
manifest_path <- file.path(root, "tables", "table_si04_brms_recovery_task_manifest.csv")
status_path <- file.path(root, "tables", "table_si04_brms_recovery_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) stop("[BLOCKER] si04c requires si04a manifest and si04b status.")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
dgp_cfg <- accrual_simulation_dgp_config("brms_recovery")

si04c_recovery_targets <- function(dgp_cfg) {
  data.frame(
    parameter = c("dREV_scaled_std", "PPE_scaled_std", "ROA_lag_std"),
    true_value = c(dgp_cfg$beta_drev, dgp_cfg$beta_ppe, dgp_cfg$beta_roa),
    recovery_role = c("primary_accrual_driver", "primary_accrual_driver", "performance_control"),
    stringsAsFactors = FALSE
  )
}

enrich_si04c_recovery_result <- function(task_row, result_df, dgp_cfg) {
  task_row <- as.list(task_row)
  if (!is.data.frame(result_df)) {
    stop("[BLOCKER] si04c task result is not a data frame: ", task_row$result_path)
  }
  required <- si04c_recovery_targets(dgp_cfg)
  if (all(required$parameter %in% result_df$parameter)) return(result_df)

  if (!"Task_Key" %in% names(result_df)) result_df$Task_Key <- task_row$Task_Key
  result_df$recovery_role <- ifelse(
    result_df$parameter %in% c("dREV_scaled_std", "PPE_scaled_std"),
    "primary_accrual_driver",
    ifelse(result_df$parameter == "ROA_lag_std", "performance_control", NA_character_)
  )
  result_df$backfilled_from_fit <- FALSE

  missing_targets <- setdiff(required$parameter, result_df$parameter)
  if (!identical(missing_targets, "ROA_lag_std")) {
    stop("[BLOCKER] si04c task result has unsupported missing recovery target(s): ",
         paste(missing_targets, collapse = ", "), " for ", task_row$Task_Key)
  }
  if (is.null(task_row$fit_path) || is.na(task_row$fit_path) || !nzchar(task_row$fit_path) ||
      !file.exists(task_row$fit_path)) {
    stop("[BLOCKER] si04c cannot backfill ROA_lag_std because fit_path is missing: ", task_row$Task_Key)
  }
  fit <- tryCatch(
    readRDS(task_row$fit_path),
    error = function(e) stop("[BLOCKER] si04c cannot read fit_path for ROA_lag_std backfill: ",
                             task_row$fit_path, " (", conditionMessage(e), ")")
  )
  if (!inherits(fit, "brmsfit")) {
    stop("[BLOCKER] si04c fit_path is not a brmsfit for ROA_lag_std backfill: ", task_row$fit_path)
  }
  fx <- tryCatch(
    brms::fixef(fit),
    error = function(e) stop("[BLOCKER] si04c cannot extract brms::fixef for ROA_lag_std backfill: ",
                             task_row$fit_path, " (", conditionMessage(e), ")")
  )
  if (!"ROA_lag_std" %in% rownames(fx) || !"Estimate" %in% colnames(fx)) {
    stop("[BLOCKER] si04c fit_path lacks ROA_lag_std fixed-effect estimate: ", task_row$fit_path)
  }
  backfill <- result_df[1, , drop = FALSE]
  backfill$Task_Key <- task_row$Task_Key
  backfill$parameter <- "ROA_lag_std"
  backfill$true_value <- dgp_cfg$beta_roa
  backfill$estimate <- fx["ROA_lag_std", "Estimate"]
  backfill$recovery_role <- "performance_control"
  backfill$backfilled_from_fit <- TRUE
  rbind(result_df, backfill)
}

assert_si04c_recovery_targets <- function(replications, required_targets) {
  dup_key <- duplicated(replications[, c("T", "sigma_firm", "Replication", "parameter"), drop = FALSE])
  if (any(dup_key)) {
    dup_rows <- replications[dup_key, c("T", "sigma_firm", "Replication", "parameter"), drop = FALSE]
    stop("[BLOCKER] si04c duplicate recovery parameter rows detected: ",
         paste(apply(dup_rows, 1, paste, collapse = "|"), collapse = ", "))
  }
  groups <- split(replications$parameter, paste(replications$Task_Key, replications$Replication, sep = "|"))
  expected <- paste(sort(required_targets), collapse = "|")
  bad <- names(groups)[vapply(groups, function(x) {
    !identical(paste(sort(unique(as.character(x))), collapse = "|"), expected) ||
      length(x) != length(required_targets)
  }, logical(1))]
  if (length(bad)) {
    stop("[BLOCKER] si04c each Task_Key/Replication must contain exactly recovery targets ",
         paste(required_targets, collapse = ", "), ". Bad groups: ", paste(bad, collapse = ", "))
  }
  invisible(TRUE)
}

accrual_task_status_blocker(status, required_col = "Required", context = "si04c brms recovery collect")
required_failed <- merge(
  manifest[, c("Task_Key", "Required"), drop = FALSE],
  status[, intersect(c("Task_Key", "status", "reason"), names(status)), drop = FALSE],
  by = "Task_Key",
  all.x = TRUE
)
required_failed <- required_failed[
  required_failed$Required %in% c(TRUE, "TRUE", "true", "1", 1L) &
    (!(required_failed$status %in% c("SUCCESS")) | is.na(required_failed$status)),
  ,
  drop = FALSE
]
if (nrow(required_failed)) {
  stop("[BLOCKER] si04c required task failures remain: ", paste(required_failed$Task_Key, collapse = ", "))
}
replications <- do.call(rbind, lapply(seq_len(nrow(manifest)), function(i) {
  task_row <- manifest[i, , drop = FALSE]
  path <- task_row$result_path[[1]]
  if (!file.exists(path)) stop("[BLOCKER] si04c missing task result: ", path)
  enrich_si04c_recovery_result(task_row, readRDS(path), dgp_cfg)
}))
assert_si04c_recovery_targets(replications, si04c_recovery_targets(dgp_cfg)$parameter)
replications$error <- replications$estimate - replications$true_value
summary <- aggregate(error ~ T + sigma_firm + parameter, replications, function(x) c(mean = mean(x), rmse = sqrt(mean(x^2))))
summary <- data.frame(
  T = summary$T,
  sigma_firm = summary$sigma_firm,
  parameter = summary$parameter,
  mean_error = summary$error[, "mean"],
  rmse = summary$error[, "rmse"],
  stringsAsFactors = FALSE
)
task_diag <- unique(replications[, c(
  "T", "sigma_firm", "Replication", "max_rhat", "min_ess_bulk", "min_ess_tail",
  "total_divergent", "max_treedepth_hits"
), drop = FALSE])
diagnostic_summary <- do.call(rbind, lapply(split(task_diag, paste(task_diag$T, task_diag$sigma_firm, sep = "|")), function(x) {
  data.frame(
    T = x$T[1],
    sigma_firm = x$sigma_firm[1],
    n_replications = length(unique(x$Replication)),
    max_rhat_max = suppressWarnings(max(x$max_rhat, na.rm = TRUE)),
    min_ess_bulk_min = suppressWarnings(min(x$min_ess_bulk, na.rm = TRUE)),
    min_ess_tail_min = suppressWarnings(min(x$min_ess_tail, na.rm = TRUE)),
    total_divergent = sum(x$total_divergent, na.rm = TRUE),
    max_treedepth_hits = sum(x$max_treedepth_hits, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
diagnostic_summary$max_rhat_max[!is.finite(diagnostic_summary$max_rhat_max)] <- NA_real_
diagnostic_summary$min_ess_bulk_min[!is.finite(diagnostic_summary$min_ess_bulk_min)] <- NA_real_
diagnostic_summary$min_ess_tail_min[!is.finite(diagnostic_summary$min_ess_tail_min)] <- NA_real_
has_key_cell <- any(diagnostic_summary$T == 15L & abs(diagnostic_summary$sigma_firm - 0.3) < 1e-8)
if (!has_key_cell) {
  stop("[BLOCKER] si04c diagnostic summary lacks required cell T=15, sigma_firm=0.3.")
}
write_csv_safely(replications, file.path(root, "tables", "table_brms_parameter_recovery_replications.csv"), row.names = FALSE)
write_csv_safely(summary, file.path(root, "tables", "table_brms_parameter_recovery_summary.csv"), row.names = FALSE)
write_csv_safely(diagnostic_summary, file.path(root, "tables", "table_brms_parameter_recovery_diagnostic_summary.csv"), row.names = FALSE)
phase_end("si04c", "Collect BRMS parameter recovery")
