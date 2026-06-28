# Behavioral unit tests for SE08D global-vs-fold-local RQ2 comparison helpers.

source("scripts/ma00_setup.R")

global_j <- data.frame(
  target_space = c("ex_post", "ex_post", "real_time", "real_time"),
  score_variable = c("abs(DA_raw)", "DA_z_estimation", "raw", "z_est"),
  jaccard = c(0.42, 0.55, 0.70, 0.76),
  spearman_rank_correlation = c(0.81, 0.84, 0.89, 0.91),
  stringsAsFactors = FALSE
)

fold_local_j <- data.frame(
  target_space = c("ex_post", "ex_post", "real_time", "real_time"),
  source_score_variable = c("DA_raw", "abs(DA_z_estimation)", "DA_raw", "z_estimation"),
  jaccard = c(0.45, 0.70, 0.77, 0.95),
  Spearman = c(0.82, 0.86, 0.90, 0.97),
  stringsAsFactors = FALSE
)

comparison <- build_se08d_rq2_global_fold_local_comparison(global_j, fold_local_j)
expected_cols <- c(
  "target_space",
  "metric",
  "global_jaccard",
  "fold_local_jaccard",
  "absolute_difference",
  "abs_absolute_difference",
  "global_material_turnover",
  "fold_local_material_turnover",
  "materiality_conclusion_unchanged",
  "global_spearman_rank_correlation",
  "fold_local_spearman_rank_correlation"
)
missing_cols <- setdiff(expected_cols, names(comparison))
if (length(missing_cols)) {
  stop("SE08D comparison helper missing expected columns: ", paste(missing_cols, collapse = ", "))
}
if (nrow(comparison) != 4L) {
  stop("SE08D comparison helper should join all four synthetic rows.")
}
if (any(!is.finite(comparison$global_jaccard))) {
  stop("SE08D comparison helper must not leave matched global Jaccard as NA.")
}
raw_ep <- comparison[comparison$target_space == "ex_post" & comparison$metric == "DA_raw", , drop = FALSE]
if (nrow(raw_ep) != 1L || abs(raw_ep$global_jaccard - 0.42) > 1e-12 || abs(raw_ep$absolute_difference - 0.03) > 1e-12) {
  stop("SE08D comparison helper failed to normalize/join abs(DA_raw) to DA_raw.")
}
z_rt <- comparison[comparison$target_space == "real_time" & comparison$metric == "DA_z_estimation", , drop = FALSE]
if (nrow(z_rt) != 1L || !identical(z_rt$fold_local_material_turnover, FALSE)) {
  stop("SE08D comparison helper failed to normalize z_est/z_estimation to DA_z_estimation.")
}

decision <- decide_se08d_rq2_global_fold_local(comparison)
if (any(!is.finite(decision$global_value))) {
  stop("SE08D decision helper must never emit NA global_value for matched comparison rows.")
}
if (!all(c("PASS", "WARN", "FAIL") %in% unique(decision$decision))) {
  stop("SE08D decision helper should exercise PASS/WARN/FAIL on the synthetic comparison fixture.")
}
if (!"abs_absolute_difference" %in% names(decision) ||
    !"materiality_conclusion_unchanged" %in% names(decision)) {
  stop("SE08D decision helper must carry global-vs-fold-local stability columns.")
}

missing_global <- global_j[global_j$target_space != "real_time" | global_j$score_variable != "z_est", , drop = FALSE]
blocked <- FALSE
tryCatch(
  build_se08d_rq2_global_fold_local_comparison(missing_global, fold_local_j),
  error = function(e) {
    blocked <<- grepl(
      "[BLOCKER] se08d global-vs-fold-local RQ2 comparison is incomplete; missing global Jaccard for:",
      conditionMessage(e),
      fixed = TRUE
    )
  }
)
if (!blocked) {
  stop("SE08D comparison helper must block when a fold-local metric lacks a matched global Jaccard row.")
}

cat("test_se08d_rq2_global_comparison_behavioral.R passed\n")
