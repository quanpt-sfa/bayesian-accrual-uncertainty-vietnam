# Static and behavioral guard for SE08C fast analytic-gradient stacking.

source("scripts/ma00_setup.R")

objective_value <- function(lpd_matrix, weights) {
  lpd_matrix <- as.matrix(lpd_matrix)
  row_max <- apply(lpd_matrix, 1L, max)
  centered <- exp(lpd_matrix - row_max)
  denom <- as.vector(centered %*% as.numeric(weights))
  sum(row_max + log(pmax(denom, .Machine$double.xmin)))
}

lpd <- matrix(
  c(
    -1.00, -1.40, -3.00,
    -1.20, -1.10, -2.80,
    -1.10, -1.30, -3.20,
    -2.80, -0.95, -1.20,
    -3.00, -1.05, -1.10,
    -2.90, -1.15, -1.00
  ),
  ncol = 3L,
  byrow = TRUE
)
colnames(lpd) <- c("model_a", "model_b", "model_c")

fit <- optimize_stacking_from_lpd_fast(lpd, maxit = 200L, reltol = 1e-8)
if (!is.list(fit) || !is.numeric(fit$weights)) {
  stop("optimize_stacking_from_lpd_fast must return a list with numeric weights.")
}
if (any(!is.finite(fit$weights)) || any(fit$weights < -1e-10)) {
  stop("fast_exact weights must be finite and nonnegative.")
}
if (abs(sum(fit$weights) - 1) > 1e-8) {
  stop("fast_exact weights must sum to 1.")
}
if (!is.finite(fit$objective) || !is.finite(fit$singleton_objective)) {
  stop("fast_exact objective metadata must be finite.")
}
if (fit$objective + 1e-6 < fit$singleton_objective) {
  stop("fast_exact objective must be at least the best singleton objective within tolerance.")
}
if (objective_value(lpd, fit$weights) + 1e-6 < max(colSums(lpd))) {
  stop("fast_exact returned weights worse than best singleton under the stacking objective.")
}

one_col <- lpd[, 1L, drop = FALSE]
one_fit <- optimize_stacking_from_lpd_fast(one_col)
if (!identical(unname(one_fit$weights), 1)) {
  stop("fast_exact single-model case must return weight 1.")
}

se08c_path <- "scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R"
se08c <- paste(readLines(se08c_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

required_fragments <- c(
  "ACCRUAL_SE08C_STACKING_METHOD",
  "unset = \"fast_exact\"",
  "allowed <- c(\"fast_exact\", \"singleton\", \"pseudo_bma\", \"exact_legacy\")",
  "optimize_stacking_from_lpd_fast",
  "forced_singleton",
  "pseudo_bma_softmax_elpd",
  "legacy_optimizer_explicitly_requested",
  "Stacking_Method_Fold_Local",
  "Stacking_Fallback_Used",
  "Stacking_Convergence_Code",
  "Stacking_Objective",
  "Singleton_Objective",
  "Stacking_Context"
)
for (fragment in required_fragments) {
  if (!grepl(fragment, se08c, fixed = TRUE)) {
    stop("SE08C fast stacking contract missing fragment: ", fragment)
  }
}

legacy_calls <- gregexpr("optimize_stacking_from_lpd\\(lpd_matrix\\)", se08c, perl = TRUE)[[1L]]
legacy_call_count <- if (identical(legacy_calls, -1L)) 0L else length(legacy_calls)
if (legacy_call_count != 1L) {
  stop("SE08C must call the legacy stacking optimizer only in the explicit exact_legacy branch.")
}
legacy_pos <- legacy_calls[[1L]]
branch_pos <- regexpr("identical\\(method, \"exact_legacy\"\\)|else \\{\\s*w <- optimize_stacking_from_lpd\\(lpd_matrix\\)", se08c, perl = TRUE)
if (branch_pos[[1L]] < 0L || legacy_pos < branch_pos[[1L]]) {
  stop("SE08C legacy optimizer call must be guarded by the exact_legacy branch.")
}

cat("test_se08c_fast_stacking_static_behavioral.R passed\n")
