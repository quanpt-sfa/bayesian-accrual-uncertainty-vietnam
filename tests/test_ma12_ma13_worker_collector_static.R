txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

ma00 <- txt("scripts/ma00_setup.R")
for (fragment in c(
  "[WORKER POOL]",
  "workers=",
  "cores_per_fit=",
  "total_core_budget=",
  "backend=",
  "fit_kind=",
  "ACCRUAL_ALLOW_SINGLE_WORKER_MODEL_PARALLEL"
)) {
  if (!grepl(fragment, ma00, fixed = TRUE)) {
    stop("ma00 worker-pool helper must expose runtime worker configuration and single-worker opt-in: ", fragment)
  }
}

worker_scripts <- c(
  ma12b = "scripts/ma12b_fit_grouped_kfold_firm_workers.R",
  ma13b = "scripts/ma13b_fit_row_level_exact_kfold_workers.R"
)

for (nm in names(worker_scripts)) {
  path <- worker_scripts[[nm]]
  body <- txt(path)
  for (fragment in c(
    "accrual_fit_worker_config(",
    "accrual_run_task_pool(",
    "split(tasks, seq_len(nrow(tasks)))",
    "max(as.integer(tasks$cores), na.rm = TRUE)",
    "brms::brm(",
    "brms::log_lik(",
    "saveRDS(out, task$result_path)",
    "write_csv_safely(data.frame",
    "fit_path = task$fit_path",
    "result_path = task$result_path",
    "accrual_assert_reusable_fit_metadata",
    "accrual_assert_kfold_manifest_matches_config"
  )) {
    if (!grepl(fragment, body, fixed = TRUE)) {
      stop(path, " missing required worker-pool or task-local metadata fragment: ", fragment)
    }
  }
  if (grepl("\\bsample\\s*\\(", body, perl = TRUE) || grepl("\\bset\\.seed\\s*\\(", body, perl = TRUE)) {
    stop(path, " must not create randomized K-fold assignments inside the worker stage.")
  }
  if (grepl("table_winsor_kfold_weights|table_winsor_row_exact_kfold_weights|final_uncertainty_adjusted", body, perl = TRUE)) {
    stop(path, " worker stage must not name collector-owned shared K-fold outputs.")
  }
}

collector_scripts <- c(
  ma12c = "scripts/ma12c_collect_grouped_kfold_firm_scores.R",
  ma13c = "scripts/ma13c_collect_row_level_exact_kfold_scores.R"
)

heavy_fragments <- c(
  "brms::brm(",
  "brm(",
  "brms::log_lik(",
  "log_lik(",
  "posterior_epred(",
  "posterior_predict(",
  "loo::loo(",
  "accrual_run_task_pool(",
  "parallel::makeCluster(",
  "parallel::parLapply"
)

for (path in collector_scripts) {
  body <- txt(path)
  for (fragment in heavy_fragments) {
    if (grepl(fragment, body, fixed = TRUE)) {
      stop(path, " must remain a serial/shared-output collector; found heavy or parallel fragment: ", fragment)
    }
  }
  for (fragment in c("accrual_task_status_blocker(", "readRDS(path)", "write_csv_safely")) {
    if (!grepl(fragment, body, fixed = TRUE)) {
      stop(path, " missing collector validation/bind/write fragment: ", fragment)
    }
  }
}

run_body <- txt("run.R")
ordered_fragments <- c(
  'step("ma12a"',
  'step("ma12b"',
  'step("ma12c"',
  'step("ma13a"',
  'step("ma13b"',
  'step("ma13c"'
)
positions <- vapply(ordered_fragments, function(fragment) regexpr(fragment, run_body, fixed = TRUE)[[1]], numeric(1))
if (any(positions < 0)) {
  stop("run.R missing K-fold split stage id(s): ", paste(ordered_fragments[positions < 0], collapse = ", "))
}
if (!all(diff(positions[1:3]) > 0) || !all(diff(positions[4:6]) > 0)) {
  stop("run.R must preserve ma12a -> ma12b -> ma12c and ma13a -> ma13b -> ma13c ordering.")
}

cat("test_ma12_ma13_worker_collector_static.R passed\n")
