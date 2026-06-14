source("scripts/v3/00_v3_winsor_helpers.R")

v3_data_workbook_path <- function() {
  v3_data_path
}

v3_read_raw_sheet <- function(sheet = "Sheet1") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read the workbook.")
  }
  readxl::read_excel(v3_data_path, sheet = sheet)
}

v3_read_winsor_sample_file <- function(file_name) {
  read_winsor_sample(file_name)
}
