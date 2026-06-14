source("scripts/00_helpers.R")

data_workbook_path <- function() {
  data_path
}

read_raw_sheet <- function(sheet = "Sheet1") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read the workbook.")
  }
  readxl::read_excel(data_path, sheet = sheet)
}

read_winsor_sample_file <- function(file_name) {
  read_winsor_sample(file_name)
}
