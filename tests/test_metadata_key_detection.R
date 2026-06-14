source("scripts/00_helpers.R")

cases <- list(
  list(columns = c("Mã", "Industry"), expected = "Mã"),
  list(columns = c("Ma", "Industry"), expected = "Ma"),
  list(columns = c("Mã CK", "Industry"), expected = "Mã CK"),
  list(columns = c("Ticker", "Industry"), expected = "Ticker"),
  list(columns = c("company", "Industry"), expected = "company"),
  list(columns = c("Symbol", "Industry"), expected = "Symbol")
)

for (case in cases) {
  detected <- detect_metadata_company_column(case$columns)
  if (!identical(detected, case$expected)) {
    stop(
      "Expected metadata key '", case$expected,
      "' but detected '", detected, "'."
    )
  }
}

missing_error <- tryCatch(
  {
    detect_metadata_company_column(c("Industry", "Sector"))
    NULL
  },
  error = function(err) err$message
)

if (is.null(missing_error) || !grepl("Available columns: Industry, Sector", missing_error, fixed = TRUE)) {
  stop("Missing-column blocker did not include available column names.")
}

cat("metadata key detection passed\n")