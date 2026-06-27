# -----------------------------------------------------------------------------
# Script: scripts/diagnostics/di09a_plan_temporal_dependence_robustness_tasks.R
# Purpose: Plan split-worker tasks for DI09 temporal-dependence robustness.
#
# Task granularity:
#   one task = one design cell (T, rho, sigma_firm), running Replication=1:R.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("di09a", "Plan temporal-dependence robustness split-worker tasks")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()
source("scripts/diagnostics/di09_temporal_dependence_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
})

start_time <- Sys.time()
dirs <- di09_temporal_dirs()
cfg <- di09_runtime_config()

manifest_path <- file.path(dirs$tables, "table_di09_temporal_dependence_task_manifest.csv")
status_path <- file.path(dirs$tables, "table_di09_temporal_dependence_task_status.csv")
plan_manifest_path <- file.path(dirs$logs, "di09_temporal_dependence_plan_manifest.csv")

cell_grid <- expand.grid(
  T = as.integer(cfg$t_grid),
  rho = as.numeric(cfg$rho_grid),
  sigma_firm = as.numeric(cfg$sigma_grid),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

if (!nrow(cell_grid)) stop("[BLOCKER] DI09 design grid is empty.")
if (any(!is.finite(cell_grid$rho) | abs(cell_grid$rho) >= 1)) {
  stop("[BLOCKER] DI09 rho values must be finite and satisfy abs(rho) < 1.")
}
if (cfg$R < 1L) stop("[BLOCKER] DI09 replications per cell must be >= 1.")
if (cfg$K < 2L) stop("[BLOCKER] DI09 K must be >= 2.")

cell_grid$Task_ID <- seq_len(nrow(cell_grid))
cell_grid$Task_Key <- mapply(
  di09_task_key,
  T = cell_grid$T,
  rho = cell_grid$rho,
  sigma_firm = cell_grid$sigma_firm,
  USE.NAMES = FALSE
)

if (anyDuplicated(cell_grid$Task_Key)) {
  dup <- unique(cell_grid$Task_Key[duplicated(cell_grid$Task_Key)])
  stop("[BLOCKER] Duplicate DI09 task keys: ", paste(head(dup, 5), collapse = ", "))
}

manifest <- cell_grid %>%
  mutate(
    Rep_Start = 1L,
    Rep_End = as.integer(cfg$R),
    Replications = as.integer(cfg$R),
    K = as.integer(cfg$K),
    n_firms = as.integer(cfg$n_firms),
    n_industries = as.integer(cfg$n_industries),
    sigma_eps = as.numeric(cfg$sigma_eps),
    base_seed = as.integer(cfg$seed),
    result_path = file.path(dirs$task_results, paste0(.data$Task_Key, "_results.csv")),
    status_path = file.path(dirs$task_status, paste0(.data$Task_Key, "_status.csv")),
    task_log_path = file.path(dirs$task_logs, paste0(.data$Task_Key, ".log")),
    Required = TRUE,
    stringsAsFactors = FALSE
  )

status <- manifest %>%
  transmute(
    Task_ID,
    Task_Key,
    T,
    rho,
    sigma_firm,
    Replications,
    status = "PENDING",
    worker_pid = NA_integer_,
    start_time = NA_character_,
    end_time = NA_character_,
    runtime_seconds = NA_real_,
    n_rows = NA_integer_,
    n_successful_replication_pairs = NA_integer_,
    n_failed_rows = NA_integer_,
    result_path,
    status_path,
    error = NA_character_
  )

write_csv_safely(manifest, manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(status, status_path, row.names = FALSE, fileEncoding = "UTF-8")

plan_manifest <- data.frame(
  script = "scripts/diagnostics/di09a_plan_temporal_dependence_robustness_tasks.R",
  script_version = di09_script_version(),
  start_time = as.character(start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(cfg$t_grid, collapse = ","),
  rho_grid = paste(cfg$rho_grid, collapse = ","),
  sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
  replications_per_cell = cfg$R,
  n_tasks = nrow(manifest),
  total_replications = nrow(manifest) * cfg$R,
  expected_output_rows = nrow(manifest) * cfg$R * 2L,
  K = cfg$K,
  n_firms = cfg$n_firms,
  n_industries = cfg$n_industries,
  sigma_eps = cfg$sigma_eps,
  seed = cfg$seed,
  output_root = dirs$root,
  manifest_path = manifest_path,
  status_path = status_path,
  stringsAsFactors = FALSE
)
write_csv_safely(plan_manifest, plan_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(dirs$logs, "sessionInfo_di09a.txt"))

cat("[SUCCESS] DI09 task planning completed.\n")
cat("Tasks: ", nrow(manifest), "\n", sep = "")
cat("Total replications: ", nrow(manifest) * cfg$R, "\n", sep = "")
cat("Expected output rows: ", nrow(manifest) * cfg$R * 2L, "\n", sep = "")
cat("Manifest: ", manifest_path, "\n", sep = "")
cat("Status: ", status_path, "\n", sep = "")

phase_end("di09a", "Plan temporal-dependence robustness split-worker tasks")
