# -----------------------------------------------------------------------------
# Environment helpers and phase logging
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Script: ma00_setup.R
# Purpose: Shared setup/config + phase logging for the accrual uncertainty pipeline.
# -----------------------------------------------------------------------------

env_value <- function(name, default) {
  val <- Sys.getenv(name, unset = default)
  if (!nzchar(val)) default else val
}

env_flag <- function(name, default = "FALSE") {
  toupper(env_value(name, default)) %in% c("TRUE", "1", "YES", "Y")
}

env_list <- function(name, sep = ",", default = character()) {
  raw <- trimws(env_value(name, ""))
  if (!nzchar(raw)) return(default)
  out <- trimws(strsplit(raw, sep, fixed = TRUE)[[1]])
  out[nzchar(out)]
}

env_choice <- function(name, default, allowed, case = c("upper", "lower", "asis")) {
  case <- match.arg(case)
  value <- env_value(name, default)
  value <- switch(
    case,
    upper = toupper(value),
    lower = tolower(value),
    asis = value
  )
  allowed_cmp <- switch(
    case,
    upper = toupper(allowed),
    lower = tolower(allowed),
    asis = allowed
  )
  if (!value %in% allowed_cmp) {
    stop("[BLOCKER] ", name, " must be one of: ", paste(allowed_cmp, collapse = ", "), ".")
  }
  value
}

env_first <- function(names, default) {
  for (nm in names) {
    val <- Sys.getenv(nm, unset = "")
    if (nzchar(val)) return(val)
  }
  default
}

# --- Phase logging & timing (single source for all pipeline lines) ---
.phase_clock <- new.env(parent = emptyenv())

phase_begin <- function(phase_id, phase_label = "") {
  t0 <- Sys.time()
  assign(phase_id, t0, envir = .phase_clock)
  message(sprintf("[%s] BEGIN  %s | start=%s",
                  phase_id, phase_label, format(t0, "%Y-%m-%d %H:%M:%S %Z")))
  invisible(t0)
}

phase_end <- function(phase_id, phase_label = "") {
  t1 <- Sys.time()
  t0 <- get0(phase_id, envir = .phase_clock, ifnotfound = t1)
  secs <- as.numeric(difftime(t1, t0, units = "secs"))
  message(sprintf("[%s] END    %s | end=%s | elapsed=%.1fs (%.2f min)",
                  phase_id, phase_label, format(t1, "%H:%M:%S"), secs, secs / 60))
  log_path <- file.path(env_value("ACCRUAL_LOG_ROOT", file.path("out", "logs")), "phase_runtime_log.csv")
  tryCatch({
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    row <- data.frame(phase_id = phase_id, phase_label = phase_label,
      start_time = format(t0, "%Y-%m-%d %H:%M:%S"), end_time = format(t1, "%Y-%m-%d %H:%M:%S"),
      elapsed_seconds = round(secs, 1), elapsed_minutes = round(secs / 60, 2),
      run_date = format(Sys.Date()), stringsAsFactors = FALSE)
    write.table(row, log_path, sep = ",", row.names = FALSE,
                col.names = !file.exists(log_path), append = file.exists(log_path))
  }, error = function(e) invisible(NULL))
  invisible(secs)
}

env_int <- function(name, default, min = NULL, allow_na = FALSE) {
  raw <- if (length(name) > 1) env_first(name, as.character(default)) else env_value(name, as.character(default))
  out <- suppressWarnings(as.integer(raw))
  if (is.na(out)) {
    if (allow_na) return(NA_integer_)
    stop("[BLOCKER] Invalid integer environment value for ", paste(name, collapse = "/"), ": ", raw)
  }
  if (!is.null(min) && out < min) {
    stop("[BLOCKER] Environment value for ", paste(name, collapse = "/"), " must be >= ", min, ". Got: ", out)
  }
  out
}

env_num <- function(name, default, min = NULL, allow_na = FALSE) {
  raw <- if (length(name) > 1) env_first(name, as.character(default)) else env_value(name, as.character(default))
  out <- suppressWarnings(as.numeric(raw))
  if (is.na(out)) {
    if (allow_na) return(NA_real_)
    stop("[BLOCKER] Invalid numeric environment value for ", paste(name, collapse = "/"), ": ", raw)
  }
  if (!is.null(min) && out < min) {
    stop("[BLOCKER] Environment value for ", paste(name, collapse = "/"), " must be >= ", min, ". Got: ", out)
  }
  out
}

env_num_list <- function(name, default, sep = ",") {
  raw <- env_list(name, sep = sep, default = character())
  if (!length(raw)) return(as.numeric(default))
  out <- suppressWarnings(as.numeric(raw))
  if (any(is.na(out))) stop("[BLOCKER] ", name, " must be ", sep, "-separated numeric values.")
  out
}

env_int_list <- function(name, default, sep = ",", min = NULL) {
  raw <- env_list(name, sep = sep, default = character())
  if (!length(raw)) return(as.integer(default))
  out <- suppressWarnings(as.integer(raw))
  if (any(is.na(out))) stop("[BLOCKER] ", name, " must be ", sep, "-separated integer values.")
  if (!is.null(min) && any(out < min)) stop("[BLOCKER] ", name, " values must be >= ", min, ".")
  out
}
