txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

si04b_path <- "scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R"
si04c_path <- "scripts/simulation/si04c_collect_brms_parameter_recovery.R"
si04b <- txt(si04b_path)
si04c <- txt(si04c_path)

for (fragment in c(
  "si04b_recovery_targets <- function(dgp_cfg)",
  "parameter = c(\"dREV_scaled_std\", \"PPE_scaled_std\", \"ROA_lag_std\")",
  "true_value = c(dgp_cfg$beta_drev, dgp_cfg$beta_ppe, dgp_cfg$beta_roa)",
  "recovery_role = c(\"primary_accrual_driver\", \"primary_accrual_driver\", \"performance_control\")",
  "estimate = fx[recovery_targets$parameter, \"Estimate\"]",
  "backfilled_from_fit = FALSE"
)) {
  if (!grepl(fragment, si04b, fixed = TRUE)) {
    stop("si04b missing explicit three-target recovery output fragment: ", fragment)
  }
}

for (fragment in c(
  "enrich_si04c_recovery_result <- function(task_row, result_df, dgp_cfg)",
  "if (all(required$parameter %in% result_df$parameter)) return(result_df)",
  "result_df$backfilled_from_fit <- FALSE",
  "readRDS(task_row$fit_path)",
  "inherits(fit, \"brmsfit\")",
  "brms::fixef(fit)",
  "fx[\"ROA_lag_std\", \"Estimate\"]",
  "backfill$parameter <- \"ROA_lag_std\"",
  "backfill$true_value <- dgp_cfg$beta_roa",
  "backfill$recovery_role <- \"performance_control\"",
  "backfill$backfilled_from_fit <- TRUE",
  "assert_si04c_recovery_targets(replications, si04c_recovery_targets(dgp_cfg)$parameter)",
  "duplicated(replications[, c(\"T\", \"sigma_firm\", \"Replication\", \"parameter\"), drop = FALSE])",
  "table_brms_parameter_recovery_replications.csv",
  "table_brms_parameter_recovery_summary.csv",
  "table_brms_parameter_recovery_diagnostic_summary.csv"
)) {
  if (!grepl(fragment, si04c, fixed = TRUE)) {
    stop("si04c missing recovery enrichment/backfill contract fragment: ", fragment)
  }
}

if (grepl("saveRDS", si04c, fixed = TRUE)) {
  stop("si04c must not overwrite task-local result_path RDS artifacts.")
}

for (fragment in c("recovery_role", "backfilled_from_fit")) {
  if (!grepl(fragment, si04c, fixed = TRUE)) {
    stop("si04c collector output must carry ", fragment)
  }
}

extract_assignment <- function(path, name, envir) {
  exprs <- parse(path)
  for (expr in exprs) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) && identical(as.character(expr[[2]]), name)) {
      eval(expr, envir = envir)
      return(invisible(TRUE))
    }
  }
  stop("Missing function assignment in ", path, ": ", name)
}

if (requireNamespace("brms", quietly = TRUE)) {
  behavior_env <- new.env(parent = globalenv())
  extract_assignment(si04c_path, "si04c_recovery_targets", behavior_env)
  extract_assignment(si04c_path, "enrich_si04c_recovery_result", behavior_env)

  fixef.brmsfit <- function(object, ...) {
    matrix(
      c(0.04, -0.03, 0.021),
      ncol = 1L,
      dimnames = list(c("dREV_scaled_std", "PPE_scaled_std", "ROA_lag_std"), "Estimate")
    )
  }

  tmp_root <- file.path(tempdir(), paste0("si04c_roa_backfill_test_", Sys.getpid()))
  dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
  fit_path <- file.path(tmp_root, "fit.rds")
  result_path <- file.path(tmp_root, "old_result_should_not_be_written.rds")
  fake_fit <- list(note = "fake brmsfit for S3 fixef dispatch only")
  class(fake_fit) <- "brmsfit"
  saveRDS(fake_fit, fit_path)

  task_row <- data.frame(
    Task_Key = "si04_mock_T15_sigma0p3_rep001",
    fit_path = fit_path,
    result_path = result_path,
    stringsAsFactors = FALSE
  )
  old_result <- data.frame(
    T = 15L,
    sigma_firm = 0.3,
    Replication = 1L,
    parameter = c("dREV_scaled_std", "PPE_scaled_std"),
    true_value = c(0.04, -0.03),
    estimate = c(0.039, -0.031),
    max_rhat = 1.01,
    min_ess_bulk = 1000,
    min_ess_tail = 900,
    total_divergent = 0L,
    max_treedepth_hits = 0L,
    status = "SUCCESS",
    stringsAsFactors = FALSE
  )
  dgp_cfg <- list(beta_drev = 0.04, beta_ppe = -0.03, beta_roa = 0.02)
  enriched <- behavior_env$enrich_si04c_recovery_result(task_row, old_result, dgp_cfg)

  if (!identical(sort(enriched$parameter), sort(c("dREV_scaled_std", "PPE_scaled_std", "ROA_lag_std")))) {
    stop("Behavioral si04c enrichment did not return exactly the three recovery targets.")
  }
  roa <- enriched[enriched$parameter == "ROA_lag_std", , drop = FALSE]
  if (nrow(roa) != 1L ||
      !identical(roa$Task_Key[[1]], task_row$Task_Key[[1]]) ||
      !isTRUE(all.equal(roa$estimate[[1]], 0.021)) ||
      !isTRUE(all.equal(roa$true_value[[1]], dgp_cfg$beta_roa)) ||
      !identical(roa$recovery_role[[1]], "performance_control") ||
      !isTRUE(roa$backfilled_from_fit[[1]])) {
    stop("Behavioral si04c enrichment did not append the expected ROA_lag_std backfill row.")
  }
  existing <- enriched[enriched$parameter %in% c("dREV_scaled_std", "PPE_scaled_std"), , drop = FALSE]
  if (!all(existing$recovery_role == "primary_accrual_driver") ||
      !all(existing$backfilled_from_fit == FALSE)) {
    stop("Behavioral si04c enrichment did not mark existing recovery rows correctly.")
  }
  if (file.exists(result_path)) {
    stop("Behavioral si04c enrichment wrote or overwrote the task-local result_path artifact.")
  }
} else {
  message("Skipping behavioral si04c ROA_lag_std backfill test because brms is unavailable.")
}

cat("test_si04_recovery_targets_static.R passed\n")
