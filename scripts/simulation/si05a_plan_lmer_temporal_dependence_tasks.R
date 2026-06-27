# -----------------------------------------------------------------------------
# Script: scripts/simulation/si05a_plan_lmer_temporal_dependence_tasks.R
# Purpose: Plan split-worker tasks for SI05 LMER temporal-dependence simulation.
#
# Task granularity:
#   one task = one design cell (T, sigma_firm, rho, shock_duration),
#   and that task runs rep_id = 1:R.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("si05a", "Plan LMER temporal-dependence split-worker tasks")
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
status_path <- file.path(dirs$tables, "table_si05_lmer_temporal_task_status.csv")
run_manifest_path <- file.path(dirs$logs, "si05_lmer_temporal_plan_manifest.csv")

cell_grid <- expand.grid(
  T = as.integer(cfg$t_grid),
  sigma_firm = cfg$sigma_grid,
  rho = cfg$rho_grid,
  shock_duration = as.integer(cfg$shock_duration_grid),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

if (!nrow(cell_grid)) stop("[BLOCKER] SI05 design grid is empty.")
if (any(!is.finite(cell_grid$rho) | abs(cell_grid$rho) >= 1)) {
  stop("[BLOCKER] SI05 rho values must be finite and satisfy abs(rho) < 1.")
}

cell_grid$Task_ID <- seq_len(nrow(cell_grid))
cell_grid$Task_Key <- mapply(
  si05_task_key,
  T = cell_grid$T,
  sigma_firm = cell_grid$sigma_firm,
  rho = cell_grid$rho,
  shock_duration = cell_grid$shock_duration,
  USE.NAMES = FALSE
)

if (anyDuplicated(cell_grid$Task_Key)) {
  dup <- unique(cell_grid$Task_Key[duplicated(cell_grid$Task_Key)])
  stop("[BLOCKER] Duplicate SI05 task keys: ", paste(head(dup, 5), collapse = ", "))
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
    shock_size = as.numeric(cfg$shock_size),
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
    sigma_firm,
    rho,
    shock_duration,
    Replications,
    status = "PENDING",
    worker_id = NA_integer_,
    start_time = NA_character_,
    end_time = NA_character_,
    runtime_seconds = NA_real_,
    n_rows = NA_integer_,
    n_successful_replications = NA_integer_,
    n_failed_replications = NA_integer_,
    result_path,
    status_path,
    error = NA_character_
  )

write_task_manifest(manifest_path, manifest)
write_csv_safely(status, status_path, row.names = FALSE, fileEncoding = "UTF-8")

run_manifest <- data.frame(
  script = "scripts/simulation/si05a_plan_lmer_temporal_dependence_tasks.R",
  script_version = "2026-06-27-v1-split-worker-plan",
  start_time = as.character(start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
  T_grid = paste(cfg$t_grid, collapse = ","),
  sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
  rho_grid = paste(cfg$rho_grid, collapse = ","),
  shock_duration_grid = paste(cfg$shock_duration_grid, collapse = ","),
  replications_per_cell = cfg$R,
  n_tasks = nrow(manifest),
  total_replications = nrow(manifest) * cfg$R,
  K = cfg$K,
  n_firms = cfg$n_firms,
  n_industries = cfg$n_industries,
  sigma_eps = cfg$sigma_eps,
  shock_size = cfg$shock_size,
  output_root = dirs$root,
  manifest_path = manifest_path,
  status_path = status_path,
  stringsAsFactors = FALSE
)
write_csv_safely(run_manifest, run_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(dirs$logs, "sessionInfo_si05a.txt"))

cat("[SUCCESS] SI05 task planning completed.\n")
cat("Tasks: ", nrow(manifest), "\n", sep = "")
cat("Total replications: ", nrow(manifest) * cfg$R, "\n", sep = "")
cat("Manifest: ", manifest_path, "\n", sep = "")
cat("Status: ", status_path, "\n", sep = "")

phase_end("si05a", "Plan LMER temporal-dependence split-worker tasks")
