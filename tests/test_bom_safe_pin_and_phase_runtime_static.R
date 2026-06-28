# Static and lightweight behavioral guards for BOM-safe pins and phase runtime logging.

env_path <- "scripts/config/env_helpers.R"
se08a_path <- "scripts/sensitivity/se08a_plan_fold_local_preprocessing_kfold.R"

for (p in c(env_path, se08a_path, "scripts/ma00_setup.R")) {
  if (!file.exists(p)) stop("Missing required file: ", p)
}

txt <- function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
env_txt <- txt(env_path)
se08a_txt <- txt(se08a_path)

for (fragment in c(
  "read_single_line_no_bom",
  "^\\ufeff",
  "^ï»¿",
  "[BLOCKER]",
  "ACCRUAL_DISABLE_PHASE_RUNTIME_LOG",
  "safe_write_phase_runtime_log",
  "tryCatch"
)) {
  if (!grepl(fragment, env_txt, fixed = TRUE)) {
    stop("env_helpers.R missing BOM/phase-runtime fragment: ", fragment)
  }
}

if (grepl("append\\s*=\\s*file\\.exists\\s*\\(\\s*log_path\\s*\\)", env_txt, perl = TRUE)) {
  stop("phase_end must not append phase_runtime_log.csv with write.table(... append = file.exists(log_path)).")
}

for (fragment in c(
  "read_single_line_no_bom(pin",
  "cleaned value",
  "BOM cleanup"
)) {
  if (!grepl(fragment, se08a_txt, fixed = TRUE)) {
    stop("se08a missing BOM-safe completed-run pin fragment: ", fragment)
  }
}

if (grepl("trimws\\s*\\(\\s*readLines\\s*\\(\\s*pin", se08a_txt, perl = TRUE)) {
  stop("se08a must not read completed-run pins with raw trimws(readLines(pin)).")
}

source("scripts/ma00_setup.R")

expected <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)

pin_utf8_bom <- tempfile("pin_utf8_bom_")
con <- file(pin_utf8_bom, open = "wb")
writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
writeBin(charToRaw(paste0("  ", expected, "  \n")), con)
close(con)
got <- read_single_line_no_bom(pin_utf8_bom, "test UTF-8 BOM pin")
if (!identical(got, expected)) {
  stop("read_single_line_no_bom failed to strip UTF-8 BOM/whitespace. Got: ", got)
}

pin_mojibake_bom <- tempfile("pin_mojibake_bom_")
writeLines(paste0("ï»¿", expected), pin_mojibake_bom, useBytes = TRUE)
got <- read_single_line_no_bom(pin_mojibake_bom, "test mojibake BOM pin")
if (!identical(got, expected)) {
  stop("read_single_line_no_bom failed to strip mojibake BOM marker. Got: ", got)
}

empty_pin <- tempfile("pin_empty_")
writeLines(c("   "), empty_pin, useBytes = TRUE)
failed <- FALSE
tryCatch(
  read_single_line_no_bom(empty_pin, "test empty pin"),
  error = function(e) {
    failed <<- grepl("[BLOCKER]", conditionMessage(e), fixed = TRUE)
  }
)
if (!failed) stop("read_single_line_no_bom must fail with [BLOCKER] for empty cleaned pins.")

old_disable <- Sys.getenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG", unset = NA_character_)
old_log_root <- Sys.getenv("ACCRUAL_LOG_ROOT", unset = NA_character_)
on.exit({
  if (is.na(old_disable)) Sys.unsetenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG") else Sys.setenv(ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = old_disable)
  if (is.na(old_log_root)) Sys.unsetenv("ACCRUAL_LOG_ROOT") else Sys.setenv(ACCRUAL_LOG_ROOT = old_log_root)
}, add = TRUE)

log_root <- tempfile("phase_log_root_")
Sys.setenv(ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = "TRUE", ACCRUAL_LOG_ROOT = log_root)
phase_begin("test_phase_runtime_disable", "runtime log disabled")
phase_end("test_phase_runtime_disable", "runtime log disabled")
if (file.exists(file.path(log_root, "phase_runtime_log.csv"))) {
  stop("phase_end should not write phase_runtime_log.csv when ACCRUAL_DISABLE_PHASE_RUNTIME_LOG=TRUE.")
}

cat("test_bom_safe_pin_and_phase_runtime_static.R passed\n")
