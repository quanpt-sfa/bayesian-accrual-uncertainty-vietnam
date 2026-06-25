source("scripts/ma00_setup.R")

assert_true <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
assert_equal <- function(x, y, msg, tolerance = sqrt(.Machine$double.eps)) {
  if (!isTRUE(all.equal(x, y, tolerance = tolerance, check.attributes = FALSE))) {
    stop(msg, call. = FALSE)
  }
}

years_gap <- c(2016, 2017, 2019)
x_gap <- c(10, 20, 40)
lag_gap <- get_lag_contiguous(x_gap, years_gap)
lead_gap <- get_lead_contiguous(x_gap, years_gap)
assert_equal(lag_gap[2], 10, "Lag for 2017 must use 2016.")
assert_true(is.na(lag_gap[3]), "Lag for 2019 must be NA when 2018 is missing.")
assert_true(is.na(lead_gap[2]), "Lead for 2017 must be NA when 2018 is missing.")
assert_equal(lead_gap[1], 20, "Lead for 2016 must use 2017.")

years_contig <- c(2016, 2017, 2018)
x_contig <- c(1, 2, 4)
sd_contig <- rolling_sd_contiguous_3(x_contig, years_contig)
assert_equal(sd_contig[3], stats::sd(x_contig), "Rolling SD at 2018 must use contiguous 2016-2018 values.")
assert_true(is.na(rolling_sd_contiguous_3(x_gap, years_gap)[3]), "Rolling SD at 2019 must be NA across a year gap.")
assert_true(is.na(rolling_sd_contiguous_3(c(1, NA, 4), years_contig)[3]), "Rolling SD must be NA when any window value is NA.")

w_raw <- c(NA, -100, 0, 1, 2, 100)
w_out <- winsorize_vec(w_raw)
w_cut <- stats::quantile(w_raw, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE, type = 7)
assert_true(is.na(w_out[1]), "Winsorization must preserve NA values.")
assert_equal(min(w_out, na.rm = TRUE), w_cut[1], "Winsorized minimum must equal 1% cutoff.")
assert_equal(max(w_out, na.rm = TRUE), w_cut[2], "Winsorized maximum must equal 99% cutoff.")
assert_equal(w_out[3:5], w_raw[3:5], "Interior values must remain unchanged after winsorization.")

one_col <- matrix(c(-1, -2, -3), ncol = 1)
colnames(one_col) <- "only_model"
w_one <- optimize_stacking_from_lpd(one_col)
assert_equal(unname(w_one), 1, "Single-model stacking must return weight exactly 1.")

dominant <- cbind(model_a = rep(5, 8), model_b = rep(-5, 8))
w_dom <- optimize_stacking_from_lpd(dominant)
assert_true(all(is.finite(w_dom)) && all(w_dom >= 0) && abs(sum(w_dom) - 1) < 1e-8,
            "Dominant-case stacking weights must be finite, non-negative, and sum to 1.")
assert_true(w_dom[["model_a"]] > 0.99, "Strictly dominant model must receive near-all stacking weight.")

symmetric <- cbind(model_a = c(1, 2, 3, 4), model_b = c(1, 2, 3, 4))
w_sym <- optimize_stacking_from_lpd(symmetric)
assert_true(all(is.finite(w_sym)) && all(w_sym >= 0) && abs(sum(w_sym) - 1) < 1e-8,
            "Symmetric-case stacking weights must be finite, non-negative, and sum to 1.")
assert_equal(unname(w_sym), c(0.5, 0.5), "Symmetric two-model case should return approximately equal weights.", tolerance = 1e-4)

tmp_csv_dir <- file.path(tempdir(), paste0("safe_csv_helper_test_", Sys.getpid()))
tmp_csv_named <- file.path(tmp_csv_dir, "named", "safe.csv")
tmp_csv_positional <- file.path(tmp_csv_dir, "positional", "safe.csv")
named_return <- write_csv_safely(data.frame(a = 1), file = tmp_csv_named)
positional_return <- write_csv_safely(data.frame(a = 1), tmp_csv_positional)
assert_equal(named_return, tmp_csv_named, "write_csv_safely(file=) must invisibly return the file path.")
assert_equal(positional_return, tmp_csv_positional, "write_csv_safely(positional file) must invisibly return the file path.")
assert_true(file.exists(tmp_csv_named), "write_csv_safely(file=) must create the CSV file.")
assert_true(file.exists(tmp_csv_positional), "write_csv_safely(positional file) must create the CSV file.")
missing_file_error <- tryCatch(
  {
    write_csv_safely(data.frame(a = 1))
    NA_character_
  },
  error = conditionMessage
)
assert_true(!is.na(missing_file_error) && grepl("[BLOCKER]", missing_file_error, fixed = TRUE),
            "write_csv_safely() without file must fail with [BLOCKER].")

test_lines <- readLines("tests/test_behavioral_core_helpers.R", warn = FALSE)
check_lines <- test_lines[!grepl("artifact_patterns|test_lines|check_lines|hits <-|Behavioral helper test", test_lines)]
test_body <- paste(check_lines, collapse = "\n")
artifact_patterns <- c("read[.]csv\\s*\\(", "readRDS\\s*\\(", paste0("out", "/"), paste0("out", "\\\\"), paste0("accruals", "/"), "LATEST_COMPLETED_RUN")
hits <- artifact_patterns[vapply(artifact_patterns, grepl, logical(1), x = test_body, perl = TRUE)]
if (length(hits)) stop("Behavioral helper test must not read or depend on pipeline artifacts: ", paste(hits, collapse = ", "))

cat("test_behavioral_core_helpers.R passed\n")
