txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

parse_targets <- c(
  "run.R",
  "scripts/diagnostics/di04_denominator_diagnostics.R",
  "scripts/diagnostics/di05_economic_validity_top_tail.R",
  "scripts/diagnostics/di09_temporal_dependence_robustness.R",
  "scripts/diagnostics/di07_section4_7_reviewer_package.R",
  "scripts/ma17_export_tables_figures.R"
)

for (path in parse_targets) {
  if (!file.exists(path)) stop("Missing parse target: ", path)
  tryCatch(parse(path), error = function(e) stop("Parse failed for ", path, ": ", conditionMessage(e)))
}

run_text <- txt("run.R")
main_start <- regexpr("main_steps <- list", run_text, fixed = TRUE)[1]
robustness_start <- regexpr("robustness_steps <- list", run_text, fixed = TRUE)[1]
reviewer_start <- regexpr("reviewer_steps <- list", run_text, fixed = TRUE)[1]
diagnostics_start <- regexpr("diagnostics_steps <- list", run_text, fixed = TRUE)[1]
if (any(c(main_start, robustness_start, reviewer_start, diagnostics_start) < 0)) {
  stop("Could not isolate run.R step blocks.")
}
main_text <- substr(run_text, main_start, robustness_start - 1L)
reviewer_text <- substr(run_text, reviewer_start, diagnostics_start - 1L)

if (grepl("scripts/diagnostics/di09_temporal_dependence_robustness.R", main_text, fixed = TRUE)) {
  stop("Temporal robustness must not be a default main step.")
}
if (!grepl("scripts/diagnostics/di05_economic_validity_top_tail.R", main_text, fixed = TRUE)) {
  stop("Main pipeline must include canonical di05 economic-validity diagnostic.")
}
if (!grepl('step("rv09"', reviewer_text, fixed = TRUE)) {
  stop("Reviewer branch must use a unique step id for temporal robustness.")
}

main_review_text <- paste(main_text, reviewer_text, sep = "\n")
main_review_ids <- regmatches(main_review_text, gregexpr('step\\("[^"]+"', main_review_text, perl = TRUE))[[1]]
main_review_ids <- sub('step\\("', "", main_review_ids)
main_review_ids <- sub('"$', "", main_review_ids)
dupes <- unique(main_review_ids[duplicated(main_review_ids)])
if (length(dupes)) {
  stop("run.R main/reviewer step ids must be unique; duplicate id(s): ", paste(dupes, collapse = ", "))
}

di05 <- txt("scripts/diagnostics/di05_economic_validity_top_tail.R")
if (grepl("scripts/diagnostics/di06_temporal_dependence_robustness.R", run_text, fixed = TRUE)) {
  stop("run.R must not reference stale di06 temporal script path.")
}

for (fragment in c(
  "table_top_tail_set_membership_exact_kfold.csv",
  "table_top_tail_set_counts_exact_kfold.csv",
  "table_top_tail_group_outcome_means.csv",
  "table_top_tail_group_economic_validity.csv",
  "table_top_tail_group_economic_validity_decision.csv",
  "table_top_tail_group_economic_validity_io_manifest.csv",
  "economic_validity_top_tail_reviewer_note.md",
  "get_lead_contiguous",
  "Primary exact-KFold magnitude evidence"
)) {
  if (!grepl(fragment, di05, fixed = TRUE)) {
    stop("di05 economic-validity script missing required fragment: ", fragment)
  }
}

di07 <- txt("scripts/diagnostics/di07_section4_7_reviewer_package.R")
for (fragment in c(
  "table_denominator_sd_mu_distribution.csv",
  "table_denominator_capped_jaccard.csv",
  "table_da_z_est_vs_z_pred_comparison.csv",
  "table_denominator_diagnostics_decision.csv",
  "table_top_tail_group_economic_validity.csv",
  "table_top_tail_group_economic_validity_decision.csv",
  "table_top_tail_group_outcome_means.csv",
  "table_temporal_dependence_firmre_premium.csv",
  "table_temporal_dependence_decision.csv"
)) {
  if (!grepl(fragment, di07, fixed = TRUE)) {
    stop("di07 reviewer package missing canonical artifact fragment: ", fragment)
  }
}

cat("test_diagnostics_integration_static.R passed\n")
