default_root <- file.path(
  "out",
  "interim",
  "winsor",
  "kfold_firm",
  "K5_FULL_MODE_modelset_primary_v2026-06-15-stratified-sparse-industry-v2_pareto_problem_v1"
)

root <- Sys.getenv("ACCRUAL_KFOLD_CHECK_ROOT", unset = default_root)

if (!dir.exists(root)) {
  stop("K-fold root not found: ", root)
}

check_one <- function(space, weight_file) {
  weight_path <- file.path(root, "tables", weight_file)
  if (!file.exists(weight_path)) {
    stop("Missing weight table for ", space, ": ", weight_path)
  }

  w <- read.csv(weight_path, stringsAsFactors = FALSE)
  required_cols <- c("Model_ID", "Heterogeneity_Variant", "Weight_KFold", "elpd_kfold", "RMSE", "reliability_flag")
  missing_cols <- setdiff(required_cols, names(w))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in ", weight_path, ": ", paste(missing_cols, collapse = ", "))
  }

  cat("\n====", space, "====\n")
  print(w[, required_cols])

  weight_sum <- sum(w$Weight_KFold)
  cat("sum weights =", weight_sum, "\n")
  if (!is.finite(weight_sum) || abs(weight_sum - 1) > 1e-5) {
    stop("Weight sum check failed for ", space, ": ", weight_sum)
  }

  top_w <- w[which.max(w$Weight_KFold), ]
  best_elpd <- w[which.max(w$elpd_kfold), ]
  cat(
    "top weight model:", top_w$Model_ID, "|", top_w$Heterogeneity_Variant,
    "| weight =", top_w$Weight_KFold, "| elpd =", top_w$elpd_kfold, "\n"
  )
  cat(
    "best elpd model:", best_elpd$Model_ID, "|", best_elpd$Heterogeneity_Variant,
    "| weight =", best_elpd$Weight_KFold, "| elpd =", best_elpd$elpd_kfold, "\n"
  )

  if (top_w$Weight_KFold > 0.999 && top_w$elpd_kfold + 1e-6 < best_elpd$elpd_kfold) {
    warning(
      "SUSPICIOUS: weight is approximately 1 on a model whose elpd_kfold is below the best individual elpd. ",
      "Check weight/meta alignment or optimizer."
    )
  }
}

check_one("ex_post", "table_winsor_kfold_weights_ex_post.csv")
check_one("real_time", "table_winsor_kfold_weights_no_lookahead.csv")

cat("test_kfold_weights_sanity.R passed\n")