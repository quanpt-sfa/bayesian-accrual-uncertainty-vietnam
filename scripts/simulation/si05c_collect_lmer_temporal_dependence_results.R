# -----------------------------------------------------------------------------
# Script: scripts/simulation/si05c_collect_lmer_temporal_dependence_results.R
# Purpose: Collect SI05 split-worker task outputs into final tables.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si05c", "Collect LMER temporal-dependence split-worker results")
source("scripts/simulation/si00_helpers.R")
source("scripts/simulation/si05_lmer_temporal_dependence_helpers.R")

check_sim_packages(c("lme4", "dplyr", "ggplot2"))
suppressPackageStartupMessages({
  library(dplyr)
})

start_time <- Sys.time()
dirs <- si05_ensure_temporal_dirs()
cfg <- si05_runtime_config()

manifest_path <- file.path(dirs$tables, "table_si05_lmer_temporal_task_manifest.csv")
if (!file.exists(manifest_path)) {
  stop("[BLOCKER] Missing SI05 task manifest. Run si05a first: ", manifest_path)
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!nrow(manifest)) stop("[BLOCKER] SI05 task manifest has zero rows.")

rep_path <- file.path(dirs$tables, "table_lmer_temporal_dependence_rep_results.csv")
sum_path <- file.path(dirs$tables, "table_lmer_temporal_dependence_grid_summary.csv")
status_combined_path <- file.path(dirs$tables, "table_si05_lmer_temporal_status_combined.csv")
decision_path <- file.path(dirs$tables, "table_si05_lmer_temporal_decision.csv")
collect_manifest_path <- file.path(dirs$logs, "temporal_dependence_run_manifest.csv")

read_csv_or_empty <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
}

status_files <- as.character(manifest$status_path)
status_combined <- bind_rows(lapply(status_files, read_csv_or_empty))
if (!nrow(status_combined)) {
  status_combined <- manifest %>%
    transmute(
      Task_ID,
      Task_Key,
      T,
      sigma_firm,
      rho,
      shock_duration,
      Replications,
      status = "MISSING_STATUS",
      worker_id = NA_integer_,
      start_time = NA_character_,
      end_time = NA_character_,
      runtime_seconds = NA_real_,
      n_rows = NA_integer_,
      n_successful_replications = NA_integer_,
      n_failed_replications = NA_integer_,
      result_path,
      status_path,
      error = "Missing per-task status file."
    )
}
write_csv_safely(status_combined, status_combined_path, row.names = FALSE, fileEncoding = "UTF-8")

missing_results <- manifest$result_path[!file.exists(manifest$result_path)]
required_missing <- manifest$Required %in% c(TRUE, "TRUE", 1L) & !file.exists(manifest$result_path)
if (any(required_missing)) {
  missing_example <- paste(head(manifest$result_path[required_missing], 5), collapse = "\n")
  stop("[BLOCKER] Missing SI05 required task result file(s). Examples:\n", missing_example,
       "\nRun unfinished workers or inspect: ", status_combined_path)
}

results <- bind_rows(lapply(as.character(manifest$result_path), function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  x$Source_Result_Path <- path
  x
}))

if (!nrow(results)) stop("[BLOCKER] SI05 collect found zero result rows.")
write_csv_safely(results, rep_path, row.names = FALSE, fileEncoding = "UTF-8")

cell_coverage <- bind_rows(lapply(seq_len(nrow(manifest)), function(i) {
  task <- manifest[i, , drop = FALSE]
  x <- results[results$Source_Result_Path == as.character(task$result_path), , drop = FALSE]
  expected_reps <- seq.int(as.integer(task$Rep_Start), as.integer(task$Rep_End))
  actual_reps <- if (nrow(x) && "rep_id" %in% names(x)) sort(unique(as.integer(x$rep_id))) else integer()
  missing_reps <- setdiff(expected_reps, actual_reps)
  extra_reps <- setdiff(actual_reps, expected_reps)
  duplicate_rep_count <- if (nrow(x) && "rep_id" %in% names(x)) sum(duplicated(as.integer(x$rep_id))) else NA_integer_
  successful <- if (nrow(x) && "error" %in% names(x)) sum(is.na(x$error) | x$error == "") else 0L
  failed <- if (nrow(x) && "error" %in% names(x)) sum(!(is.na(x$error) | x$error == "")) else 0L
  design_match <- nrow(x) > 0 &&
    all(as.integer(x$T) == as.integer(task$T), na.rm = TRUE) &&
    all(abs(as.numeric(x$sigma_firm) - as.numeric(task$sigma_firm)) < 1e-12, na.rm = TRUE) &&
    all(abs(as.numeric(x$rho) - as.numeric(task$rho)) < 1e-12, na.rm = TRUE) &&
    all(as.integer(x$shock_duration) == as.integer(task$shock_duration), na.rm = TRUE)
  complete <- nrow(x) == length(expected_reps) &&
    length(missing_reps) == 0L && length(extra_reps) == 0L &&
    identical(as.integer(duplicate_rep_count), 0L) && isTRUE(design_match)
  data.frame(
    Task_ID = as.integer(task$Task_ID),
    Task_Key = as.character(task$Task_Key),
    T = as.integer(task$T),
    sigma_firm = as.numeric(task$sigma_firm),
    rho = as.numeric(task$rho),
    shock_duration = as.integer(task$shock_duration),
    expected_replications = length(expected_reps),
    observed_rows = nrow(x),
    unique_replications = length(actual_reps),
    successful_replications = successful,
    failed_replications = failed,
    missing_replication_count = length(missing_reps),
    extra_replication_count = length(extra_reps),
    duplicate_replication_count = duplicate_rep_count,
    design_match = design_match,
    complete = complete,
    missing_replications_preview = paste(head(missing_reps, 20), collapse = ","),
    result_path = as.character(task$result_path),
    stringsAsFactors = FALSE
  )
}))
coverage_path <- file.path(dirs$tables, "table_si05_lmer_temporal_cell_coverage.csv")
write_csv_safely(cell_coverage, coverage_path, row.names = FALSE, fileEncoding = "UTF-8")

incomplete_cells <- cell_coverage[!isTRUE(cell_coverage$complete) & !cell_coverage$complete, , drop = FALSE]

summary_df <- si05_summarise_temporal_results(results)
write_csv_safely(summary_df, sum_path, row.names = FALSE, fileEncoding = "UTF-8")

ok <- results[is.na(results$error) | results$error == "", , drop = FALSE]
failed_n <- sum(!(is.na(results$error) | results$error == ""))

decision_value <- dplyr::case_when(
  nrow(ok) == 0 ~ "FAIL_NO_SUCCESSFUL_REPLICATIONS",
  nrow(incomplete_cells) > 0 ~ "FAIL_INCOMPLETE_CELL_COVERAGE",
  failed_n > 0 ~ "PASS_WITH_FAILED_REPLICATIONS_REVIEW",
  TRUE ~ "PASS"
)

decision <- data.frame(
  si05_decision = decision_value,
  total_tasks = nrow(manifest),
  completed_task_status_rows = nrow(status_combined),
  expected_replications = sum(as.integer(manifest$Replications)),
  observed_replication_rows = nrow(results),
  successful_replications = nrow(ok),
  failed_replications = failed_n,
  incomplete_cells = nrow(incomplete_cells),
  coverage_path = coverage_path,
  T_grid = paste(cfg$t_grid, collapse = ","),
  sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
  rho_grid = paste(cfg$rho_grid, collapse = ","),
  shock_duration_grid = paste(cfg$shock_duration_grid, collapse = ","),
  replications_per_cell = cfg$R,
  K = cfg$K,
  n_firms = cfg$n_firms,
  n_industries = cfg$n_industries,
  sigma_eps = cfg$sigma_eps,
  shock_size = cfg$shock_size,
  interpretation = dplyr::case_when(
    identical(decision_value, "PASS") ~ "All SI05 split-worker cells reached the manifest replication target without replication-level errors.",
    identical(decision_value, "FAIL_INCOMPLETE_CELL_COVERAGE") ~ "At least one SI05 design cell did not reach its manifest replication target. Inspect the coverage table and rerun stale/incomplete tasks.",
    TRUE ~ "SI05 outputs were collected, but failed/missing replications or tasks require review."
  ),
  stringsAsFactors = FALSE
)
write_csv_safely(decision, decision_path, row.names = FALSE, fileEncoding = "UTF-8")

manifest_row <- data.frame(
  script = "scripts/simulation/si05c_collect_lmer_temporal_dependence_results.R",
  script_version = "2026-06-27-v1-split-worker-collect",
  start_time = as.character(start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(cfg$t_grid, collapse = ","),
  sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
  rho_grid = paste(cfg$rho_grid, collapse = ","),
  shock_duration_grid = paste(cfg$shock_duration_grid, collapse = ","),
  replications = cfg$R,
  K = cfg$K,
  n_firms = cfg$n_firms,
  n_industries = cfg$n_industries,
  sigma_eps = cfg$sigma_eps,
  shock_size = cfg$shock_size,
  successful_replications = nrow(ok),
  failed_replications = failed_n,
  output_root = dirs$root,
  task_manifest_path = manifest_path,
  task_status_combined_path = status_combined_path,
  replications_path = rep_path,
  summary_path = sum_path,
  decision_path = decision_path,
  coverage_path = coverage_path,
  incomplete_cells = nrow(incomplete_cells),
  stringsAsFactors = FALSE
)
write_csv_safely(manifest_row, collect_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(dirs$logs, "sessionInfo_si05c.txt"))

cat("[SUCCESS] SI05 split-worker collect completed.\n")
cat("Decision: ", decision_value, "\n", sep = "")
cat("Results: ", rep_path, "\n", sep = "")
cat("Summary: ", sum_path, "\n", sep = "")
cat("Status combined: ", status_combined_path, "\n", sep = "")
cat("Decision table: ", decision_path, "\n", sep = "")

phase_end("si05c", "Collect LMER temporal-dependence split-worker results")
