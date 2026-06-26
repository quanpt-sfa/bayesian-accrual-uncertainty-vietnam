# test_di05_economic_validity_outcome_uniqueness_static.R
# Static regression test: prevents future_Earnings_persistence from re-entering
# di05 economic-validity outcomes, and enforces 4-outcome / 12-test denominators.

txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
di05 <- txt("scripts/diagnostics/di05_economic_validity_top_tail.R")

# ── 1. future_Earnings_persistence must not appear in the outcomes vector ─────
# The live outcomes vector assignment must be the corrected 4-outcome form.
if (grepl('"future_Earnings_persistence"', di05, fixed = TRUE) &&
    grepl('outcomes <- c\\(.*future_Earnings_persistence', di05, perl = TRUE)) {
  stop("di05 outcomes vector must not contain future_Earnings_persistence. ",
       "It duplicates future_Earnings (both = NI_lead / A). Remove it.")
}

# Stricter check: the active outcomes <- c(...) assignment must contain exactly
# the four corrected outcomes and must NOT include future_Earnings_persistence.
outcomes_line <- grep("^outcomes <- c\\(", strsplit(di05, "\n")[[1L]], value = TRUE, perl = TRUE)
if (!length(outcomes_line)) {
  stop("di05 must contain an 'outcomes <- c(...)' assignment.")
}
active_outcomes_line <- outcomes_line[1L]
if (grepl("future_Earnings_persistence", active_outcomes_line, fixed = TRUE)) {
  stop("The active outcomes <- c(...) in di05 must not list future_Earnings_persistence.")
}
required_outcomes <- c("future_CFO", "future_ROA", "future_Earnings", "accrual_reversal")
for (o in required_outcomes) {
  if (!grepl(o, active_outcomes_line, fixed = TRUE)) {
    stop("di05 active outcomes vector is missing required outcome: ", o)
  }
}

# ── 2. The duplicate variable must not be computed in raw_leads transmute ─────
# Guard against re-adding `future_Earnings_persistence = num(...)` to transmute.
# We allow it in comments (## NOTE:) but not as an active assignment.
non_comment_lines <- grep("^\\s*#", strsplit(di05, "\n")[[1L]], invert = TRUE, value = TRUE, perl = TRUE)
non_comment_body <- paste(non_comment_lines, collapse = "\n")
if (grepl("future_Earnings_persistence\\s*=\\s*num\\(", non_comment_body, perl = TRUE)) {
  stop("di05 must not compute future_Earnings_persistence = num(...) in transmute. ",
       "This would recreate the duplicate outcome. Remove it.")
}
if (grepl("future_Earnings_persistence\\s*=\\s*.data\\$NI_lead", non_comment_body, perl = TRUE)) {
  stop("di05 must not compute future_Earnings_persistence = .data$NI_lead / ... in mutate.")
}

# ── 3. Guardrail must be present in the script ────────────────────────────────
if (!grepl("[GUARDRAIL]", di05, fixed = TRUE)) {
  stop("di05 must contain an inline [GUARDRAIL] check for future_Earnings_persistence.")
}

# ── 4. Expected denominator: 4 outcomes × 3 terms = 12 tests ─────────────────
# Verify by counting distinct outcome names in the corrected vector.
n_outcomes <- length(required_outcomes[sapply(required_outcomes, function(o) grepl(o, active_outcomes_line, fixed = TRUE))])
if (n_outcomes != 4L) {
  stop("di05 active outcomes vector must contain exactly 4 outcomes (got ", n_outcomes, ").")
}
# 4 outcomes × 3 terms (RowOnlyTop5, GroupedOnlyTop5, CommonTop5) = 12 tests per score variable.
terms_interest <- c("RowOnlyTop5TRUE", "GroupedOnlyTop5TRUE", "CommonTop5TRUE")
for (t in terms_interest) {
  if (!grepl(t, di05, fixed = TRUE)) {
    stop("di05 must include membership term: ", t)
  }
}

# ── 5. Correction note must be present in the reviewer note ──────────────────
if (!grepl("Correction Note", di05, fixed = TRUE)) {
  stop("di05 reviewer note must include a 'Correction Note' section documenting the fix.")
}
if (!grepl("four independent outcomes", di05, fixed = TRUE)) {
  stop("di05 reviewer note must state 'four independent outcomes' after the correction.")
}

# ── 6. No active export in this file writes 5-outcome or 15-test language ────
# Use word-boundary regex: avoids false-positive from "di05 outcomes vector"
# (where "5 outcomes" appears as a substring of "di05 outcomes").
active_lines <- grep("^\\s*#", strsplit(di05, "\n")[[1L]], invert = TRUE, value = TRUE, perl = TRUE)
active_body <- paste(active_lines, collapse = "\n")
if (grepl("\\b5 outcomes\\b", active_body, perl = TRUE) &&
    !grepl("di05 outcomes", active_body, fixed = TRUE)) {
  stop("di05 active code must not report '5 outcomes' after the correction.")
}
if (grepl("\\b15 tests\\b", active_body, perl = TRUE)) {
  stop("di05 active code must not report '15 tests' after the correction.")
}

cat("test_di05_economic_validity_outcome_uniqueness_static.R passed\n")
