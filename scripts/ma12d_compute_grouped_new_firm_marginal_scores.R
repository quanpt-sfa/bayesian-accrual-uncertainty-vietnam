# Script: ma12d_compute_grouped_new_firm_marginal_scores.R
# Purpose: Orchestration wrapper for MA12D v1.1 grouped-firm marginal
#          new-firm rescoring. Heavy computation lives in the MA12E worker.

source("scripts/ma00_setup.R")
phase_begin("ma12d", "Orchestrate grouped-firm marginal new-firm rescoring")

# Environment contract preserved by wrapper/stages:
# ACCRUAL_MA12D_SOURCE_KFOLD_RUN_ROOT
# ACCRUAL_MA12D_SOURCE_ROW_KFOLD_RUN_ROOT
# ACCRUAL_MA12D_OUTPUT_RUN_ROOT
# ACCRUAL_MA12D_NEW_FIRM_DRAWS
# ACCRUAL_MA12D_MAX_POSTERIOR_DRAWS
# ACCRUAL_MA12D_SEED
# ACCRUAL_MA12D_FORCE_RECOMPUTE
# ACCRUAL_MA12D_WEIGHT_CHANGE_MATERIAL
# ACCRUAL_MA12D_ALLOW_RESTACK_EXCLUDED

run_stage <- function(path, allow_failure = FALSE) {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  status <- system2(rscript, path)
  if (!identical(status, 0L) && !isTRUE(allow_failure)) {
    stop("[BLOCKER] MA12D stage failed: ", path, call. = FALSE)
  }
  status
}

latest_run_pin <- file.path(output_root, "grouped_new_firm_marginal", "LATEST_RUN.txt")

prepare_status <- run_stage("scripts/ma12d_prepare_grouped_new_firm_marginal_tasks.R")
prepared_root <- if (file.exists(latest_run_pin)) {
  x <- trimws(readLines(latest_run_pin, warn = FALSE))
  x <- x[nzchar(x)]
  if (length(x)) x[[1]] else ""
} else {
  ""
}
if (nzchar(prepared_root) && !nzchar(Sys.getenv("ACCRUAL_MA12D_OUTPUT_RUN_ROOT"))) {
  Sys.setenv(ACCRUAL_MA12D_OUTPUT_RUN_ROOT = prepared_root)
}

worker_status <- run_stage("scripts/ma12e_compute_grouped_new_firm_marginal_workers.R", allow_failure = TRUE)
collect_status <- run_stage("scripts/ma12f_collect_grouped_new_firm_marginal_scores.R", allow_failure = TRUE)

if (!identical(worker_status, 0L)) {
  stop("[BLOCKER] MA12D worker stage failed; MA12F attempted blocker collection. See task status and decision tables.", call. = FALSE)
}
if (!identical(collect_status, 0L)) {
  stop("[BLOCKER] MA12D collect stage failed. See MA12D run logs.", call. = FALSE)
}

cat("\n[SUCCESS] MA12D v1.1 grouped-firm marginal new-firm rescoring completed.\n")
cat("Output run root:", Sys.getenv("ACCRUAL_MA12D_OUTPUT_RUN_ROOT", prepared_root), "\n")
cat("Refits performed: FALSE\n")
phase_end("ma12d", "Orchestrate grouped-firm marginal new-firm rescoring")
