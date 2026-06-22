# -----------------------------------------------------------------------------
# Script: check_ma07_fit_draw_artifact_audit.R
# Purpose: Lightweight invariant checks for ma07 fit/draw artifact audit outputs.
#
# Intended use:
#   Rscript scripts/diagnostics/check_ma07_fit_draw_artifact_audit.R
# -----------------------------------------------------------------------------

source("scripts/ma00_setup.R")

audit_path <- file.path(output_root, "tables", "table_ma07_fit_draw_artifact_audit.csv")
failures_path <- file.path(output_root, "tables", "table_ma07_hard_gate_failures.csv")
helper_path <- file.path(output_root, "logs", "ma07_suggested_remediation_targets.ps1")

if (!file.exists(audit_path)) {
  stop("[CHECK BLOCKER] Missing ma07 artifact audit table: ", audit_path)
}

audit <- read.csv(audit_path, stringsAsFactors = FALSE, check.names = FALSE)
required_cols <- c(
  "diagnostic_key", "Main_Stack_Inclusion", "fit_exists_after", "draws_exists_after",
  "draw_generation_skip_reason", "hard_gate_status"
)
missing_cols <- setdiff(required_cols, names(audit))
if (length(missing_cols)) {
  stop("[CHECK BLOCKER] ma07 artifact audit lacks required column(s): ", paste(missing_cols, collapse = ", "))
}

bad_missing_draw_reason <- audit$fit_exists_after %in% TRUE &
  !(audit$draws_exists_after %in% TRUE) &
  !nzchar(trimws(ifelse(is.na(audit$draw_generation_skip_reason), "", audit$draw_generation_skip_reason)))
if (any(bad_missing_draw_reason)) {
  stop("[CHECK BLOCKER] Fit rows with missing draws lack draw_generation_skip_reason: ",
       paste(audit$diagnostic_key[bad_missing_draw_reason], collapse = "; "))
}

main_fail <- audit$Main_Stack_Inclusion %in% TRUE & audit$hard_gate_status == "FAIL"
if (any(main_fail)) {
  if (!file.exists(failures_path)) {
    stop("[CHECK BLOCKER] Main-stack hard gate failures exist but failure table is missing: ", failures_path)
  }
  if (!file.exists(helper_path)) {
    stop("[CHECK BLOCKER] Main-stack hard gate failures exist but remediation helper is missing: ", helper_path)
  }
}

strict_mode <- toupper(Sys.getenv("ACCRUAL_MA07_STRICT_REVIEW_BLOCKER", "FALSE")) %in% c("TRUE", "1", "YES", "Y")
main_review <- audit$Main_Stack_Inclusion %in% TRUE & audit$hard_gate_status == "REVIEW"
if (strict_mode && any(main_review)) {
  if (!file.exists(failures_path) || !file.exists(helper_path)) {
    stop("[CHECK BLOCKER] Strict REVIEW blocker mode requires failure table and remediation helper for main-stack REVIEW rows.")
  }
}

cat("ma07 artifact audit checks passed\n")
