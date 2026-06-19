required_files <- c(
  "scripts/ma01_setup_and_registry.R",
  "scripts/ma04_define_named_models.R",
  "scripts/ma12_grouped_kfold_firm.R"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing model-registry files: ", paste(missing_files, collapse = ", "))
}

text_13 <- readLines("scripts/ma12_grouped_kfold_firm.R", warn = FALSE)
ex_post_line <- grep("ExPost_Model_IDs", text_13, value = TRUE)
no_lookahead_line <- grep("NoLookahead_Model_IDs", text_13, value = TRUE)
if (length(ex_post_line) == 0 || !grepl("M02", ex_post_line[1], fixed = TRUE)) {
  stop("M02 is missing from ExPost_Model_IDs in script 13.")
}
if (length(no_lookahead_line) == 0 || !grepl("M02", no_lookahead_line[1], fixed = TRUE)) {
  stop("M02 is missing from NoLookahead_Model_IDs in script 13.")
}

cat("test_model_registry.R passed\n")
