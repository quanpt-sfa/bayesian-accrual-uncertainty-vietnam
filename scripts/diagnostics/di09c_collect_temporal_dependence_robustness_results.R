# -----------------------------------------------------------------------------
# Script: scripts/diagnostics/di09c_collect_temporal_dependence_robustness_results.R
# Purpose: Collect DI09 split-worker task outputs into final robustness tables.
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")
phase_begin("di09c", "Collect temporal-dependence robustness split-worker results")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()
source("scripts/diagnostics/di09_temporal_dependence_helpers.R")

suppressPackageStartupMessages({
  library(dplyr)
})

script_start_time <- Sys.time()
script_name <- "scripts/diagnostics/di09c_collect_temporal_dependence_robustness_results.R"
script_version <- di09_script_version()
dirs <- di09_temporal_dirs()
cfg <- di09_runtime_config()

manifest_path <- file.path(dirs$tables, "table_di09_temporal_dependence_task_manifest.csv")
if (!file.exists(manifest_path)) {
  stop("[BLOCKER] Missing DI09 task manifest. Run di09a first: ", manifest_path)
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!nrow(manifest)) stop("[BLOCKER] DI09 task manifest has zero rows.")

replications_path <- file.path(dirs$tables, "table_temporal_dependence_replications.csv")
premium_path <- file.path(dirs$tables, "table_temporal_dependence_firmre_premium.csv")
decision_path <- file.path(dirs$tables, "table_temporal_dependence_decision.csv")
io_manifest_path <- file.path(dirs$tables, "table_temporal_dependence_io_manifest.csv")
note_path <- file.path(dirs$logs, "temporal_dependence_reviewer_note.md")
status_combined_path <- file.path(dirs$tables, "table_di09_temporal_dependence_status_combined.csv")
coverage_path <- file.path(dirs$tables, "table_di09_temporal_dependence_cell_coverage.csv")
collect_manifest_path <- file.path(dirs$logs, "di09_temporal_dependence_collect_manifest.csv")

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
      rho,
      sigma_firm,
      Replications,
      status = "MISSING_STATUS",
      worker_pid = NA_integer_,
      start_time = NA_character_,
      end_time = NA_character_,
      runtime_seconds = NA_real_,
      n_rows = NA_integer_,
      n_successful_replication_pairs = NA_integer_,
      n_failed_rows = NA_integer_,
      result_path,
      status_path,
      error = "Missing per-task status file."
    )
}
write_csv_safely(status_combined, status_combined_path, row.names = FALSE, fileEncoding = "UTF-8")

required_missing <- manifest$Required %in% c(TRUE, "TRUE", 1L) & !file.exists(manifest$result_path)
if (any(required_missing)) {
  missing_example <- paste(head(manifest$result_path[required_missing], 5), collapse = "\n")
  stop("[BLOCKER] Missing DI09 required task result file(s). Examples:\n", missing_example,
       "\nRun unfinished workers or inspect: ", status_combined_path)
}

results <- bind_rows(lapply(as.character(manifest$result_path), function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  x$Source_Result_Path <- path
  x
}))
if (!nrow(results)) stop("[BLOCKER] DI09 collect found zero result rows.")
write_csv_safely(results, replications_path, row.names = FALSE, fileEncoding = "UTF-8")

cell_coverage <- bind_rows(lapply(seq_len(nrow(manifest)), function(i) {
  task <- manifest[i, , drop = FALSE]
  x <- results[results$Source_Result_Path == as.character(task$result_path), , drop = FALSE]
  expected_reps <- seq.int(as.integer(task$Rep_Start), as.integer(task$Rep_End))
  actual_reps <- if (nrow(x) && "Replication" %in% names(x)) sort(unique(as.integer(x$Replication))) else integer()
  missing_reps <- setdiff(expected_reps, actual_reps)
  extra_reps <- setdiff(actual_reps, expected_reps)
  duplicate_pair_count <- if (nrow(x) && all(c("Replication", "validation_target") %in% names(x))) {
    sum(duplicated(x[, c("Replication", "validation_target"), drop = FALSE]))
  } else {
    NA_integer_
  }
  target_pair_ok <- di09_expected_target_pair_ok(x)
  successful_pairs <- di09_successful_replication_pairs(x)
  failed_rows <- if (nrow(x) && "fit_status" %in% names(x)) sum(x$fit_status != "SUCCESS", na.rm = TRUE) else 0L
  insufficient_rows <- if (nrow(x) && "fit_status" %in% names(x)) sum(x$fit_status == "INSUFFICIENT_DEPENDENCY", na.rm = TRUE) else 0L
  design_match <- nrow(x) > 0 &&
    all(as.integer(x$T) == as.integer(task$T), na.rm = TRUE) &&
    all(abs(as.numeric(x$rho) - as.numeric(task$rho)) < 1e-12, na.rm = TRUE) &&
    all(abs(as.numeric(x$sigma_firm) - as.numeric(task$sigma_firm)) < 1e-12, na.rm = TRUE)
  complete <- nrow(x) == length(expected_reps) * 2L &&
    length(missing_reps) == 0L && length(extra_reps) == 0L &&
    identical(as.integer(duplicate_pair_count), 0L) && isTRUE(design_match) &&
    isTRUE(target_pair_ok)
  success_complete <- isTRUE(complete) && successful_pairs == length(expected_reps) && failed_rows == 0L
  data.frame(
    Task_ID = as.integer(task$Task_ID),
    Task_Key = as.character(task$Task_Key),
    T = as.integer(task$T),
    rho = as.numeric(task$rho),
    sigma_firm = as.numeric(task$sigma_firm),
    expected_replications = length(expected_reps),
    expected_rows = length(expected_reps) * 2L,
    observed_rows = nrow(x),
    unique_replications = length(actual_reps),
    successful_replication_pairs = successful_pairs,
    failed_rows = failed_rows,
    insufficient_dependency_rows = insufficient_rows,
    missing_replication_count = length(missing_reps),
    extra_replication_count = length(extra_reps),
    duplicate_replication_target_pair_count = duplicate_pair_count,
    target_pair_ok = target_pair_ok,
    design_match = design_match,
    complete = complete,
    success_complete = success_complete,
    missing_replications_preview = paste(head(missing_reps, 20), collapse = ","),
    result_path = as.character(task$result_path),
    stringsAsFactors = FALSE
  )
}))
write_csv_safely(cell_coverage, coverage_path, row.names = FALSE, fileEncoding = "UTF-8")

incomplete_cells <- cell_coverage[!cell_coverage$complete, , drop = FALSE]
failed_or_insufficient_cells <- cell_coverage[!cell_coverage$success_complete, , drop = FALSE]

lme4_available <- requireNamespace("lme4", quietly = TRUE)
usable_pairs <- results %>%
  filter(.data$fit_status == "SUCCESS") %>%
  distinct(.data$Replication, .data$n_firms, .data$T, .data$rho, .data$sigma_firm, .data$K,
           .data$row_minus_grouped_firmre_premium) %>%
  group_by(.data$n_firms, .data$T, .data$rho, .data$sigma_firm, .data$K) %>%
  summarise(
    R = n(),
    mean_row_minus_grouped_firmre_premium = mean(.data$row_minus_grouped_firmre_premium, na.rm = TRUE),
    median_row_minus_grouped_firmre_premium = stats::median(.data$row_minus_grouped_firmre_premium, na.rm = TRUE),
    sd_row_minus_grouped_firmre_premium = stats::sd(.data$row_minus_grouped_firmre_premium, na.rm = TRUE),
    p05_row_minus_grouped_firmre_premium = as.numeric(stats::quantile(.data$row_minus_grouped_firmre_premium, 0.05, na.rm = TRUE, names = FALSE)),
    p95_row_minus_grouped_firmre_premium = as.numeric(stats::quantile(.data$row_minus_grouped_firmre_premium, 0.95, na.rm = TRUE, names = FALSE)),
    share_row_minus_grouped_positive = mean(.data$row_minus_grouped_firmre_premium > 0, na.rm = TRUE),
    .groups = "drop"
  )

if (nrow(usable_pairs) > 0) {
  target_long <- results %>%
    filter(.data$fit_status == "SUCCESS") %>%
    group_by(.data$n_firms, .data$T, .data$rho, .data$sigma_firm, .data$validation_target) %>%
    summarise(mean_firmre_premium = mean(.data$firmre_premium, na.rm = TRUE), .groups = "drop")

  if (requireNamespace("tidyr", quietly = TRUE)) {
    target_summary <- target_long %>%
      tidyr::pivot_wider(names_from = "validation_target", values_from = "mean_firmre_premium")
  } else {
    row_summary <- target_long[target_long$validation_target == "row_level_kfold", ]
    grouped_summary <- target_long[target_long$validation_target == "grouped_firm_kfold", ]
    target_summary <- merge(
      row_summary[, c("n_firms", "T", "rho", "sigma_firm", "mean_firmre_premium")],
      grouped_summary[, c("n_firms", "T", "rho", "sigma_firm", "mean_firmre_premium")],
      by = c("n_firms", "T", "rho", "sigma_firm"),
      all = TRUE,
      suffixes = c("_row_level_kfold", "_grouped_firm_kfold")
    )
    names(target_summary)[names(target_summary) == "mean_firmre_premium_row_level_kfold"] <- "row_level_kfold"
    names(target_summary)[names(target_summary) == "mean_firmre_premium_grouped_firm_kfold"] <- "grouped_firm_kfold"
  }

  premium_summary <- usable_pairs %>%
    left_join(target_summary, by = c("n_firms", "T", "rho", "sigma_firm")) %>%
    transmute(
      n_firms = .data$n_firms,
      T = .data$T,
      rho = .data$rho,
      sigma_firm = .data$sigma_firm,
      R = .data$R,
      mean_row_firmre_premium = .data$row_level_kfold,
      mean_grouped_firmre_premium = .data$grouped_firm_kfold,
      mean_row_minus_grouped_firmre_premium = .data$mean_row_minus_grouped_firmre_premium,
      median_row_minus_grouped_firmre_premium = .data$median_row_minus_grouped_firmre_premium,
      sd_row_minus_grouped_firmre_premium = .data$sd_row_minus_grouped_firmre_premium,
      p05_row_minus_grouped_firmre_premium = .data$p05_row_minus_grouped_firmre_premium,
      p95_row_minus_grouped_firmre_premium = .data$p95_row_minus_grouped_firmre_premium,
      share_row_minus_grouped_positive = .data$share_row_minus_grouped_positive,
      interpretation = ifelse(
        .data$mean_row_minus_grouped_firmre_premium > 0,
        "Row-level validation gives a larger Firm-RE premium; interpret as within-firm interpolation under persistent same-firm shocks.",
        "Grouped validation premium is not below row-level premium in this scenario."
      )
    )
} else {
  premium_summary <- data.frame(
    n_firms = integer(0),
    T = integer(0),
    rho = numeric(0),
    sigma_firm = numeric(0),
    R = integer(0),
    mean_row_firmre_premium = numeric(0),
    mean_grouped_firmre_premium = numeric(0),
    mean_row_minus_grouped_firmre_premium = numeric(0),
    median_row_minus_grouped_firmre_premium = numeric(0),
    sd_row_minus_grouped_firmre_premium = numeric(0),
    p05_row_minus_grouped_firmre_premium = numeric(0),
    p95_row_minus_grouped_firmre_premium = numeric(0),
    share_row_minus_grouped_positive = numeric(0),
    interpretation = character(0),
    stringsAsFactors = FALSE
  )
}
write_csv_safely(premium_summary, premium_path, row.names = FALSE, fileEncoding = "UTF-8")

usable_n <- nrow(usable_pairs)
failed_rows_n <- sum(results$fit_status == "FAILED", na.rm = TRUE)
insufficient_rows_n <- sum(results$fit_status == "INSUFFICIENT_DEPENDENCY", na.rm = TRUE)
decision_value <- "FAIL_TEMPORAL_ROBUSTNESS_UNAVAILABLE"
decision_reason <- "No usable Firm-RE replications were available."

if (!lme4_available || insufficient_rows_n > 0L) {
  decision_value <- "INSUFFICIENT_INPUTS"
  decision_reason <- "Package lme4 is unavailable or at least one task produced insufficient-dependency rows, so Firm-RE scoring was not fully computed."
} else if (nrow(incomplete_cells) > 0L) {
  decision_value <- "FAIL_INCOMPLETE_CELL_COVERAGE"
  decision_reason <- "At least one DI09 design cell did not reach its manifest replication target. Inspect the coverage table and rerun stale/incomplete tasks."
} else if (failed_rows_n > 0L) {
  decision_value <- "FAIL_REPLICATION_ERRORS"
  decision_reason <- "At least one DI09 replication row failed. Inspect task status and logs."
} else if (usable_n > 0) {
  rho_trend <- premium_summary %>%
    group_by(.data$T, .data$sigma_firm) %>%
    summarise(
      rho_slope = if (length(unique(.data$rho)) >= 2) stats::coef(stats::lm(mean_row_minus_grouped_firmre_premium ~ rho, data = dplyr::cur_data()))[[2]] else NA_real_,
      .groups = "drop"
    )
  material_increase <- any(rho_trend$rho_slope > 0.05, na.rm = TRUE)
  mixed <- any(rho_trend$rho_slope > 0.02, na.rm = TRUE) && any(rho_trend$rho_slope < -0.02, na.rm = TRUE)
  decision_value <- dplyr::case_when(
    material_increase ~ "WARN_ROW_PREMIUM_INCREASES_WITH_TEMPORAL_DEPENDENCE",
    mixed ~ "WARN_TEMPORAL_RESULTS_MIXED",
    TRUE ~ "PASS_TEMPORAL_ROBUSTNESS_AVAILABLE"
  )
  decision_reason <- dplyr::case_when(
    material_increase ~ "The row-minus-grouped Firm-RE premium increases materially with rho in at least one scenario.",
    mixed ~ "The rho pattern is mixed across T and sigma_firm scenarios.",
    TRUE ~ "Temporal persistence does not materially increase the row-minus-grouped Firm-RE premium."
  )
}

decision <- data.frame(
  temporal_decision = decision_value,
  usable_replication_pairs = usable_n,
  requested_replications_per_cell = cfg$R,
  expected_tasks = nrow(manifest),
  incomplete_cells = nrow(incomplete_cells),
  failed_or_insufficient_cells = nrow(failed_or_insufficient_cells),
  failed_rows = failed_rows_n,
  insufficient_dependency_rows = insufficient_rows_n,
  coverage_path = coverage_path,
  task_status_combined_path = status_combined_path,
  rho_grid = paste(cfg$rho_grid, collapse = ","),
  sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
  T_grid = paste(cfg$t_grid, collapse = ","),
  n_firms = cfg$n_firms,
  K = cfg$K,
  lme4_available = lme4_available,
  interpretation = decision_reason,
  row_validation_interpretation = "Row-level K-fold is within-firm interpolation when other years of the same firm remain in training.",
  grouped_validation_interpretation = "Grouped firm K-fold is out-of-firm prediction because held-out firms have no training observations.",
  stringsAsFactors = FALSE
)
write_csv_safely(decision, decision_path, row.names = FALSE, fileEncoding = "UTF-8")

note <- c(
  "# Temporal Dependence Robustness Note",
  "",
  "This lightweight AR(1) mechanism simulation tests whether the row-minus-grouped Firm-RE premium changes as same-firm residual shocks become temporally persistent.",
  "",
  "The simulated panel follows `TA_it = X_it beta + industry_year_effect + u_i + epsilon_it`, with `epsilon_it = rho * epsilon_i,t-1 + nu_it`.",
  "",
  "Row-level K-fold allows other years of the same firm to remain in training, so it should be interpreted as within-firm interpolation when persistent same-firm shocks are present.",
  "",
  "Grouped firm-level K-fold holds out entire firms and is therefore out-of-firm prediction.",
  "",
  paste0("Decision: `", decision_value, "`."),
  decision_reason,
  "",
  paste0("Coverage table: `", coverage_path, "`."),
  "",
  "A warning is not a failure of the paper. It means row-level validation is capturing within-firm temporal information and should not be interpreted as out-of-time or out-of-firm validity.",
  "",
  "Temporal persistence does not by itself prove leakage, earnings management, or managerial intent."
)
writeLines(note, note_path, useBytes = TRUE)

output_paths <- c(replications_path, premium_path, decision_path, coverage_path, status_combined_path, note_path)
io_manifest <- rbind(
  data.frame(
    script_name = script_name,
    script_version = script_version,
    start_time = as.character(script_start_time),
    end_time = as.character(Sys.time()),
    runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
    git_commit = di09_git_commit_or_na(),
    output_root = output_root,
    io_class = "output",
    path = output_paths,
    exists = file.exists(output_paths),
    file_size_bytes = vapply(output_paths, di09_file_size_or_na, numeric(1)),
    modified_time = vapply(output_paths, di09_mtime_or_na, character(1)),
    md5 = vapply(output_paths, di09_file_hash_or_na, character(1)),
    rho_grid = paste(cfg$rho_grid, collapse = ","),
    sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
    T_grid = paste(cfg$t_grid, collapse = ","),
    replications = cfg$R,
    n_firms = cfg$n_firms,
    K = cfg$K,
    seed = cfg$seed,
    stringsAsFactors = FALSE
  ),
  data.frame(
    script_name = script_name,
    script_version = script_version,
    start_time = as.character(script_start_time),
    end_time = as.character(Sys.time()),
    runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
    git_commit = di09_git_commit_or_na(),
    output_root = output_root,
    io_class = "output",
    path = io_manifest_path,
    exists = TRUE,
    file_size_bytes = NA_real_,
    modified_time = NA_character_,
    md5 = "self_referential_manifest",
    rho_grid = paste(cfg$rho_grid, collapse = ","),
    sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
    T_grid = paste(cfg$t_grid, collapse = ","),
    replications = cfg$R,
    n_firms = cfg$n_firms,
    K = cfg$K,
    seed = cfg$seed,
    stringsAsFactors = FALSE
  )
)
write_csv_safely(io_manifest, io_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")

collect_manifest <- data.frame(
  script = script_name,
  script_version = script_version,
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  T_grid = paste(cfg$t_grid, collapse = ","),
  rho_grid = paste(cfg$rho_grid, collapse = ","),
  sigma_firm_grid = paste(cfg$sigma_grid, collapse = ","),
  replications = cfg$R,
  K = cfg$K,
  n_firms = cfg$n_firms,
  n_industries = cfg$n_industries,
  sigma_eps = cfg$sigma_eps,
  seed = cfg$seed,
  output_root = dirs$root,
  task_manifest_path = manifest_path,
  task_status_combined_path = status_combined_path,
  replications_path = replications_path,
  premium_path = premium_path,
  decision_path = decision_path,
  coverage_path = coverage_path,
  incomplete_cells = nrow(incomplete_cells),
  failed_or_insufficient_cells = nrow(failed_or_insufficient_cells),
  decision = decision_value,
  stringsAsFactors = FALSE
)
write_csv_safely(collect_manifest, collect_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(dirs$logs, "sessionInfo_di09c.txt"))

cat("[SUCCESS] DI09 split-worker collect completed.\n")
cat("Decision: ", decision_value, "\n", sep = "")
cat("Replications: ", replications_path, "\n", sep = "")
cat("Premium summary: ", premium_path, "\n", sep = "")
cat("Coverage: ", coverage_path, "\n", sep = "")
cat("Decision table: ", decision_path, "\n", sep = "")

phase_end("di09c", "Collect temporal-dependence robustness split-worker results")
