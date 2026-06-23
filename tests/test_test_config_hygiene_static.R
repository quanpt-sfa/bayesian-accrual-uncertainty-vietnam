txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

test_files <- list.files("tests", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
test_files <- gsub("\\\\", "/", test_files)

for (path in test_files) {
  body <- txt(path)
  generated_report_path <- paste0("reports", "/")
  if (grepl(generated_report_path, body, fixed = TRUE)) {
    stop("Tests must not require generated report paths: ", path)
  }
}

sampler_literal_patterns <- c(
  "ACCRUAL_.*(CHAINS|CORES|ITER|WARMUP|ADAPT_DELTA|MAX_TREEDEPTH).*\"[0-9]",
  "(chains|cores|iter|warmup|max_treedepth)\\s*,\\s*[0-9]+L",
  "adapt_delta\\s*,\\s*0\\.[0-9]+",
  paste0("(BASELINE|KFOLD|SENS).*", "120", "00"),
  paste0("(BASELINE|KFOLD|SENS).*", "40", "00")
)
for (path in test_files) {
  body <- txt(path)
  lines <- readLines(path, warn = FALSE)
  suspect <- character()
  for (pattern in sampler_literal_patterns) {
    hits <- grep(pattern, lines, value = TRUE, perl = TRUE)
    hits <- hits[!grepl("TEST_FIXTURE_NUMERIC_LITERAL_OK", hits, fixed = TRUE)]
    suspect <- c(suspect, hits)
  }
  if (length(suspect)) {
    stop("Sampler/runtime numeric literals must come from ma00 helpers, not tests: ",
         path, " :: ", paste(unique(trimws(suspect)), collapse = " | "))
  }
}

sys_getenv_files <- test_files[vapply(test_files, function(path) {
  grepl(paste0("Sys", ".getenv", "("), txt(path), fixed = TRUE)
}, logical(1))]
allowed_sys_getenv <- c(
  "tests/test_chapter3_method_alignment_static.R",
  "tests/test_data_schema.R",
  "tests/test_kfold_weights_sanity.R",
  "tests/test_ma07_fit_collect_refactor_static.R",
  "tests/test_no_script_local_env_config_static.R",
  "tests/test_psis_reliability_gate_schema.R",
  "tests/test_row_exact_kfold_schema.R",
  "tests/test_run_profile_production_5w4c_static.R"
)
unexpected_sys_getenv <- setdiff(sys_getenv_files, allowed_sys_getenv)
if (length(unexpected_sys_getenv)) {
  stop("Unexpected direct environment reads in tests: ", paste(unexpected_sys_getenv, collapse = ", "))
}

config_expectation_tests <- c(
  "tests/test_chapter3_method_alignment_static.R",
  "tests/test_centralized_runtime_config_static.R",
  "tests/test_ma07_fit_collect_refactor_static.R",
  "tests/test_run_profile_production_5w4c_static.R"
)
for (path in config_expectation_tests) {
  body <- txt(path)
  if (!grepl('source\\("scripts/ma00_setup.R"\\)', body, fixed = FALSE)) {
    stop("Config expectation test must source scripts/ma00_setup.R: ", path)
  }
}

profile_test <- txt("tests/test_run_profile_production_5w4c_static.R")
if (!grepl('accrual_run_profile_config(profile_name)', profile_test, fixed = TRUE)) {
  stop("Production run profile test must compare against accrual_run_profile_config().")
}

cat("test_test_config_hygiene_static.R passed\n")
