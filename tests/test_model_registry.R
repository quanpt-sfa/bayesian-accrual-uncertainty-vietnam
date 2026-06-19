# Validate the named-model registry after the ma/se reorg.
# Model sets are now centralized in ma00_setup via main_model_ids_for_space();
# this test checks the canonical sets and that ma12 wires them in.

required_files <- c(
  "scripts/ma00_setup.R",
  "scripts/ma01_setup_and_registry.R",
  "scripts/ma04_define_named_models.R",
  "scripts/ma12_grouped_kfold_firm.R"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing model-registry files: ", paste(missing_files, collapse = ", "))
}

# Canonical model sets from the centralized helper.
source("scripts/ma00_setup.R")
ep <- main_model_ids_for_space("ex_post")
rt <- main_model_ids_for_space("real_time")
expected_ep <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07")
expected_rt <- c("M01", "M02", "M03", "M07", "M09")
if (!setequal(ep, expected_ep)) {
  stop("ex_post model set mismatch: got {", paste(ep, collapse = ","), "}")
}
if (!setequal(rt, expected_rt)) {
  stop("real_time model set mismatch: got {", paste(rt, collapse = ","), "}")
}

# ma12 must derive its model-ID manifest fields from the centralized helper
# (or, for backward compatibility, list M02 inline).
text_12 <- readLines("scripts/ma12_grouped_kfold_firm.R", warn = FALSE)
ex_post_line <- grep("ExPost_Model_IDs", text_12, value = TRUE)
no_lookahead_line <- grep("NoLookahead_Model_IDs", text_12, value = TRUE)
ok_line <- function(x) length(x) > 0 &&
  (grepl("main_model_ids_for_space", x[1], fixed = TRUE) || grepl("M02", x[1], fixed = TRUE))
if (!ok_line(ex_post_line)) {
  stop("ExPost_Model_IDs in ma12 neither uses main_model_ids_for_space nor lists M02.")
}
if (!ok_line(no_lookahead_line)) {
  stop("NoLookahead_Model_IDs in ma12 neither uses main_model_ids_for_space nor lists M02.")
}

cat("test_model_registry.R passed\n")
