# Static contract for grouped exact K-fold compatibility artifacts produced by ma12c.

ma12c_path <- "scripts/ma12c_collect_grouped_kfold_firm_scores.R"
ma17_path <- "scripts/ma17_export_tables_figures.R"

if (!file.exists(ma12c_path)) stop("Missing ma12c collector: ", ma12c_path)
if (!file.exists(ma17_path)) stop("Missing ma17 exporter: ", ma17_path)

ma12c <- paste(readLines(ma12c_path, warn = FALSE), collapse = "\n")
ma17 <- paste(readLines(ma17_path, warn = FALSE), collapse = "\n")

required_ma12c_fragments <- c(
  "resolve_fold_assignment_path <- function",
  "table_ma12_grouped_kfold_fold_assignment.csv",
  "Fold_Assignment_Path",
  "[BLOCKER] ma12c cannot produce grouped K-fold fold-balance diagnostics",
  "normalize_fold_assignment <- function",
  "table_winsor_firm_fold_assignment.csv",
  "table_winsor_kfold_balance.csv",
  "table_winsor_kfold_industry_fold_coverage.csv",
  "reconstruct_grouped_kfold_balance <- function",
  "reconstruct_grouped_kfold_industry_coverage <- function",
  "Year_Distribution",
  "Industry_Distribution",
  "Stratified_Grouped_KFold",
  "Repeated_Grouped_KFold_Repeats",
  "Present_In_All_Folds",
  "Present_In_All_Folds = n_distinct(.data$Fold_ID) >= .env$K",
  "table_ma12_grouped_kfold_compatibility_manifest.csv",
  "compat_file_row <- function",
  "tools::md5sum",
  "write_dual_csv(compatibility_manifest",
  "read_target_sample_for_balance"
)
for (fragment in required_ma12c_fragments) {
  if (!grepl(fragment, ma12c, fixed = TRUE)) {
    stop("ma12c compatibility contract missing fragment: ", fragment)
  }
}

if (grepl("table_winsor_kfold_train_standardization_audit.csv", ma12c, fixed = TRUE)) {
  stop("ma12c must not fabricate table_winsor_kfold_train_standardization_audit.csv.")
}

if (grepl("brms::brm\\s*\\(|\\bbrm\\s*\\(", ma12c, perl = TRUE)) {
  stop("ma12c collector must not refit BRMS models.")
}

required_ma17_fragments <- c(
  "split_grouped_kfold_fold_balance_artifacts_missing",
  "safe_min(.data$number_of_firms_in_fold)",
  "safe_max(.data$number_of_firms_in_fold)",
  "safe_min(.data$number_of_firm_year_observations_in_fold)",
  "safe_max(.data$number_of_firm_year_observations_in_fold)"
)
for (fragment in required_ma17_fragments) {
  if (!grepl(fragment, ma17, fixed = TRUE)) {
    stop("ma17 grouped K-fold defensive diagnostic missing fragment: ", fragment)
  }
}

cat("test_ma12c_grouped_kfold_compatibility_static.R passed\n")
