chapter3_path <- "reports/chapter_3_method_only_reviewer_final_journal_style_transitions.md"
if (!file.exists(chapter3_path)) stop("Missing Chapter 3 authority file: ", chapter3_path)

source("scripts/ma00_setup.R")

chapter3 <- readLines(chapter3_path, warn = FALSE, encoding = "UTF-8")
chapter3_text <- paste(chapter3, collapse = "\n")
if (!grepl("mass outside \\(|TA|>2\\) should not exceed 1%", chapter3_text, fixed = TRUE)) {
  stop("Chapter 3 authority text no longer contains the expected |TA|>2 <= 1% rule.")
}

thr <- chapter3_prior_predictive_thresholds()
if (!identical(thr$abs_gt_1_pass, 0.05)) stop("Chapter 3 |TA|>1 PASS threshold must be 0.05.")
if (!identical(thr$abs_gt_2_pass, 0.01)) stop("Chapter 3 |TA|>2 PASS threshold must be 0.01.")
if (!identical(thr$range_ratio_pass, 3.00)) stop("Chapter 3 prior predictive range ratio PASS threshold must be 3.00.")

baseline_cfg <- accrual_sampler_config("baseline")
if (!identical(baseline_cfg$chains, 4L) ||
    !identical(baseline_cfg$iter, 3000L) ||
    !identical(baseline_cfg$warmup, 1000L) ||
    !isTRUE(all.equal(baseline_cfg$adapt_delta, 0.95)) ||
    !identical(baseline_cfg$max_treedepth, 12L)) {
  stop("Baseline sampler defaults do not match Chapter 3 4/3000/1000/adapt_delta=.95/max_treedepth=12.")
}

grouped_cfg <- accrual_kfold_config("grouped_firm")
row_cfg <- accrual_kfold_config("row")
for (cfg_name in c("grouped_cfg", "row_cfg")) {
  cfg <- get(cfg_name)
  if (!identical(cfg$K, 5L) ||
      !identical(cfg$seed, 42L) ||
      !identical(cfg$chains, 4L) ||
      !identical(cfg$iter, 3000L) ||
      !identical(cfg$warmup, 1000L)) {
    stop(cfg_name, " does not match Chapter 3 exact K-fold defaults.")
  }
}

fast_cfg <- accrual_sampler_config("row_kfold", run_mode = "FAST_MODE")
if (!identical(fast_cfg$chains, 2L) || !identical(fast_cfg$iter, 1000L) || !identical(fast_cfg$warmup, 500L)) {
  stop("FAST_MODE defaults must remain 2 chains, 1000 iter, 500 warmup.")
}

di02 <- readLines("scripts/diagnostics/di02_new_firm_predictive_integration_audit.R", warn = FALSE)
di02_text <- paste(di02, collapse = "\n")
obsolete_di02_paths <- c(
  "scripts/10_construct_uncertainty_adjusted_DA.R",
  "scripts/12_lofo_stacking.R",
  "scripts/13_grouped_kfold_firm.R",
  "scripts/26_sim_brms_leakage_confirmation.R",
  "scripts/28_row_level_exact_kfold.R",
  "scripts/29_psis_reliability_gate.R"
)
hits <- obsolete_di02_paths[vapply(obsolete_di02_paths, grepl, logical(1), x = di02_text, fixed = TRUE)]
if (length(hits)) stop("di02 still references obsolete script path(s): ", paste(hits, collapse = ", "))

required_di02_paths <- c(
  "ma10_construct_psis_loo_DA.R",
  "robustness\", \"ro01_lofo_stacking.R",
  "ma12_grouped_kfold_firm.R",
  "simulation\", \"si03_brms_leakage_confirmation.R",
  "ma13_row_level_exact_kfold.R",
  "diagnostics\", \"di01_psis_reliability_gate.R"
)
missing <- required_di02_paths[vapply(required_di02_paths, function(x) !grepl(x, di02_text, fixed = TRUE), logical(1))]
if (length(missing)) stop("di02 is missing active source path fragment(s): ", paste(missing, collapse = ", "))

if (!grepl("source_role_specific_not_global", di02_text, fixed = TRUE) ||
    !grepl("does not verify posterior predictive tail draws", di02_text, fixed = TRUE)) {
  stop("di02 must preserve source-specific verification and reject global evidence transfer.")
}

ma14 <- paste(readLines("scripts/ma14_construct_exact_kfold_DA.R", warn = FALSE), collapse = "\n")
if (!grepl("Completed_Run_Pin_Eligible", ma14, fixed = TRUE) ||
    !grepl("LATEST_COMPLETED_RUN.txt", ma14, fixed = TRUE)) {
  stop("ma14 must retain completed-pin eligibility checks and use LATEST_COMPLETED_RUN pins.")
}
if (grepl('file.path\\([^\\n]*"LATEST_RUN.txt"', ma14)) {
  stop("ma14 must not use moving LATEST_RUN.txt as primary provenance.")
}

dry <- system2("Rscript", c("run.R", "--dry-run"), stdout = TRUE, stderr = TRUE)
dry_text <- paste(dry, collapse = "\n")
required_dry_paths <- c(
  "scripts/ma12_grouped_kfold_firm.R",
  "scripts/ma13_row_level_exact_kfold.R",
  "scripts/ma14_construct_exact_kfold_DA.R",
  "scripts/ma15_audit_DA_finite_outputs.R",
  "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R"
)
missing_dry <- required_dry_paths[vapply(required_dry_paths, function(x) !grepl(x, dry_text, fixed = TRUE), logical(1))]
if (length(missing_dry)) stop("run.R --dry-run missing required primary/gate path(s): ", paste(missing_dry, collapse = ", "))

cat("test_chapter3_method_alignment_static.R passed\n")
