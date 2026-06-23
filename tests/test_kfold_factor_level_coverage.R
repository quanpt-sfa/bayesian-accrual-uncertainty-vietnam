source("scripts/ma00_setup.R")

expect_blocker <- function(expr, pattern) {
  ok <- tryCatch({
    force(expr)
    FALSE
  }, error = function(e) {
    grepl("[BLOCKER]", conditionMessage(e), fixed = TRUE) &&
      grepl(pattern, conditionMessage(e), fixed = TRUE)
  })
  if (!isTRUE(ok)) stop("Expected [BLOCKER] containing: ", pattern, call. = FALSE)
}

train <- data.frame(industry = c("A", "B", "A"), year = c(2016, 2017, 2018))
test <- data.frame(industry = c("A", "B"), year = c(2017, 2018))
assert_ok <- assert_training_factor_level_coverage(train, test, context = "passing synthetic fold")
if (!isTRUE(assert_ok)) stop("Coverage helper should return invisible TRUE for covered factor levels.")

expect_blocker(
  assert_training_factor_level_coverage(
    train,
    data.frame(industry = "C", year = 2017),
    context = "missing industry fold"
  ),
  "column=industry"
)

expect_blocker(
  assert_training_factor_level_coverage(
    train,
    data.frame(industry = "A", year = 2019),
    context = "missing year fold"
  ),
  "column=year"
)

expect_blocker(
  assert_training_factor_level_coverage(
    data.frame(year = c(2016, 2017)),
    data.frame(year = 2018),
    context = "missing optional industry"
  ),
  "column=year"
)

cat("test_kfold_factor_level_coverage.R passed\n")
