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

summary_df <- si05_summarise_temporal_results(results)
write_csv_safely(summary_df, sum_path, row.names = FALSE, fileEncoding = "UTF-8")

ok <- results[is.na(results$error) | results$error == "", , drop = FALSE]
failed_n <- sum(!(is.na(results$error) | results$error == ""))

decision_value <- dplyr::case_when(
  nrow(ok) == 0 ~ "FAIL_NO_SUCCESSFUL_REPLICATIONS",
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
  interpretation = ifelse(
    identical(decision_value, "PASS"),
    "All SI05 split-worker task result rows were collected without replication-level errors.",
    "SI05 outputs were collected, but failed/missing replications or tasks require review."
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
