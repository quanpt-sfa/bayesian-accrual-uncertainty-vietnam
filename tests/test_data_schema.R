data_path <- Sys.getenv("V3_DATA_PATH", file.path("data", "raw", "data.xlsx"))

if (!file.exists(data_path)) {
  message("Data workbook not found at ", data_path, ". Skipping schema validation.")
  quit(save = "no", status = 0)
}

if (!requireNamespace("readxl", quietly = TRUE)) {
  message("Package 'readxl' is not installed. Skipping schema validation.")
  quit(save = "no", status = 0)
}

sheets <- readxl::excel_sheets(data_path)
required_sheets <- c("Sheet1", "Sheet2")
missing_sheets <- setdiff(required_sheets, sheets)
if (length(missing_sheets) > 0) {
  stop("Missing required sheets: ", paste(missing_sheets, collapse = ", "))
}

sheet1_cols <- names(readxl::read_excel(data_path, sheet = "Sheet1", n_max = 0))
required_cols <- c("company", "year", "A", "NI", "REV", "CFO", "REC", "PPE", "ROA", "COGS", "INV")
missing_cols <- setdiff(required_cols, sheet1_cols)
if (length(missing_cols) > 0) {
  stop("Missing required Sheet1 columns: ", paste(missing_cols, collapse = ", "))
}

cat("test_data_schema.R passed\n")
