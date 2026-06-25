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

cat("test_si04_recovery_targets_static.R passed\n")
