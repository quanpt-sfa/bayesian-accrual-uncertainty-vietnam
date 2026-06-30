# Static contract for reproducible base-R SE08C collection.

se08c_path <- "scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R"
if (!file.exists(se08c_path)) stop("Missing SE08C collector: ", se08c_path)

lines <- readLines(se08c_path, warn = FALSE, encoding = "UTF-8")
txt <- paste(lines, collapse = "\n")

non_comment <- trimws(lines[!grepl("^\\s*(#|$)", lines)])
if (!length(non_comment) || !identical(non_comment[1], "se08c_top_lock <- local({")) {
  stop("SE08C first non-comment executable statement must be the deterministic lock guard.")
}
source_idx <- grep("source\\(\"scripts/ma00_setup.R\"\\)", non_comment)
lock_idx <- grep("se08c_top_lock <- local\\(\\{", non_comment)
if (!length(source_idx) || !length(lock_idx) || min(lock_idx) >= min(source_idx)) {
  stop("SE08C must acquire the deterministic lock before sourcing scripts/ma00_setup.R.")
}

forbidden <- c(
  "library(dplyr)",
  "library(tidyr)",
  "dplyr::",
  "tidyr::",
  "%>%",
  "pivot_wider",
  ".data$",
  "system(",
  "system2(",
  "shell(",
  "cmd.exe",
  "Rscript",
  "callr",
  "processx",
  ".rs.restartR",
  "rstudioapi"
)
for (fragment in forbidden) {
  if (grepl(fragment, txt, fixed = TRUE)) {
    stop("SE08C must not contain tidyverse dependency/syntax fragment: ", fragment)
  }
}

required_fragments <- c(
  "bind_rows_base",
  "aggregate_by_base",
  "left_join_base",
  "se08c_top_lock",
  "se08c_collect.lock",
  "logs_dir_lock_path",
  "PID=",
  "commandArgs=",
  "working_directory=",
  "duplicate_count",
  "[BLOCKER] duplicate se08c process detected",
  "[BLOCKER] se08c lock exists; refusing to start another collector",
  "ACCRUAL_SE08C_CLEAR_STALE_LOCK",
  "se08c_checkpoint",
  "grouped ex_post stacking begin",
  "grouped real_time stacking begin",
  "row ex_post stacking begin",
  "row real_time stacking begin",
  "optimize_stacking_guarded",
  "ACCRUAL_SE08C_STACKING_METHOD",
  "fast_exact",
  "singleton",
  "pseudo_bma",
  "exact_legacy",
  "optimize_stacking_from_lpd_fast",
  "stacking_singleton_fallback",
  "falling back to best singleton ELPD model",
  "Stacking_Method_Fold_Local",
  "Stacking_Fallback_Used",
  "Stacking_Convergence_Code",
  "Stacking_Objective",
  "Singleton_Objective",
  "Stacking_Context",
  "read_single_line_no_bom",
  "reliability_label",
  "FAILED",
  "LOW_RELIABILITY",
  "OK",
  "CAUTION",
  "optimize_stacking_from_lpd",
  "_se08_grouped_fold_local",
  "_se08_row_fold_local",
  "table_se08_fold_local_preprocessing_audit.csv",
  "table_se08_fold_local_cutoff_summary.csv",
  "table_se08_fold_local_standardization_summary.csv",
  "table_se08_grouped_fold_local_observation_scores.csv",
  "table_se08_row_fold_local_observation_scores.csv",
  "table_se08_grouped_fold_local_model_scores.csv",
  "table_se08_row_fold_local_model_scores.csv",
  "table_se08_grouped_fold_local_weights_ex_post.csv",
  "table_se08_grouped_fold_local_weights_no_lookahead.csv",
  "table_se08_row_fold_local_weights_ex_post.csv",
  "table_se08_row_fold_local_weights_no_lookahead.csv",
  "table_se08_fold_local_vs_global_weight_comparison.csv",
  "table_se08_fold_local_vs_global_firmre_shift_summary.csv",
  "table_se08_fold_local_vs_global_top_model_comparison.csv",
  "table_se08_fold_local_sensitivity_decision.csv",
  "se08c_decision_interpretation",
  "Fold-local preprocessing preserves the positive row-minus-grouped Firm-RE weight shift",
  "attenuates its magnitude below the 70% stability threshold",
  "do not claim aggregate RQ1 fold-local robustness",
  "top-model-axis evidence is stable",
  "do not claim full top-model-axis robustness",
  "do not use top-model identity or heterogeneity-axis stability",
  "se08_fold_local_preprocessing_collect_manifest.csv"
)
for (fragment in required_fragments) {
  if (!grepl(fragment, txt, fixed = TRUE)) {
    stop("SE08C missing base-R collector contract fragment: ", fragment)
  }
}

if (grepl("trimws\\s*\\(\\s*readLines\\s*\\(\\s*pin", txt, perl = TRUE)) {
  stop("SE08C must use BOM-safe completed-run pin reading, not raw trimws(readLines(pin)).")
}
if (grepl("PASS if", txt, fixed = TRUE)) {
  stop("SE08C decision interpretations must be decision-specific and must not use generic 'PASS if' threshold prose.")
}

source("scripts/ma00_setup.R")
expected <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
pin <- tempfile("se08c_bom_pin_")
con <- file(pin, open = "wb")
writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
writeBin(charToRaw(paste0(expected, "\n")), con)
close(con)
got <- read_single_line_no_bom(pin, "SE08C static BOM helper check")
if (!identical(got, expected)) {
  stop("read_single_line_no_bom did not return the clean path for SE08C BOM helper check.")
}

cat("test_se08c_base_r_static.R passed\n")
