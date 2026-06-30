# Static contract for SE10 pooled-only sub-stacking sensitivity.

txt <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

path <- "scripts/sensitivity/se10_pooled_only_substacking.R"
se10 <- txt(path)

for (fragment in c(
  "ACCRUAL_SE10_SOURCE_GROUPED_KFOLD_RUN_ROOT",
  "ACCRUAL_SE10_SOURCE_ROW_KFOLD_RUN_ROOT",
  "kfold_firm",
  "row_exact_kfold",
  "LATEST_COMPLETED_RUN.txt",
  "table_winsor_kfold_observation_scores.csv",
  "table_winsor_kfold_model_scores.csv",
  "table_winsor_row_exact_kfold_observation_scores.csv",
  "table_winsor_row_exact_kfold_model_scores.csv"
)) {
  if (!grepl(fragment, se10, fixed = TRUE)) {
    stop("SE10 missing source-discovery fragment: ", fragment)
  }
}

for (fragment in c(
  "grepl(\"Pooled\"",
  "Firm RE",
  "Random Intercept",
  "firm random effect",
  "Pooled_Only_Candidate",
  "Included_In_Stack",
  "reliability_flag",
  "OK",
  "CAUTION"
)) {
  if (!grepl(fragment, se10, fixed = TRUE)) {
    stop("SE10 missing pooled/gate fragment: ", fragment)
  }
}

if (!grepl("optimize_stacking_from_lpd\\s*\\(", se10, perl = TRUE) &&
    !grepl("optimize_stacking_from_lpd_fast\\s*\\(", se10, perl = TRUE)) {
  stop("SE10 must use an existing stacking optimizer.")
}

for (fragment in c(
  "table_se10_pooled_only_weights_grouped_ex_post.csv",
  "table_se10_pooled_only_weights_grouped_no_lookahead.csv",
  "table_se10_pooled_only_weights_row_ex_post.csv",
  "table_se10_pooled_only_weights_row_no_lookahead.csv",
  "table_se10_pooled_only_row_vs_grouped_family_shift.csv",
  "table_se10_pooled_only_decision.csv",
  "run_config_manifest.csv",
  "LATEST_COMPLETED_RUN.txt"
)) {
  if (!grepl(fragment, se10, fixed = TRUE)) {
    stop("SE10 missing output fragment: ", fragment)
  }
}

for (fragment in c(
  "BLOCKED_MISSING_SOURCE_SCORES",
  "SE10 missing required row/grouped K-fold score tables",
  "BLOCKED_NO_POOLED_CANDIDATES",
  "fewer than two source-gated pooled candidates"
)) {
  if (!grepl(fragment, se10, fixed = TRUE)) {
    stop("SE10 missing blocker fragment: ", fragment)
  }
}

for (bad in c("brms::brm\\s*\\(", "\\bbrm\\s*\\(", "\\bsampling\\s*\\(", "cmdstan_model")) {
  if (grepl(bad, se10, perl = TRUE)) {
    stop("SE10 must not refit or sample Bayesian models; found pattern: ", bad)
  }
}

if (!grepl("if \\(isTRUE\\(completed\\)\\) writeLines\\(se10_root", se10, perl = TRUE)) {
  stop("SE10 must write LATEST_COMPLETED_RUN.txt only after successful completion.")
}

invisible(parse(path))
cat("test_se10_pooled_only_substacking_static.R passed\n")
