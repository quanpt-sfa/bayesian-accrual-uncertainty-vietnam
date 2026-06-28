# -----------------------------------------------------------------------------
# Analysis utility helpers
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

get_lag_contiguous <- function(x, year, n = 1) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) stop("[BLOCKER] n must be a positive integer for get_lag_contiguous().")
  if (length(x) != length(year)) stop("[BLOCKER] x and year must have equal length for get_lag_contiguous().")
  if (n >= length(x)) return(rep(NA, length(x)))
  lag_val <- c(rep(NA, n), head(x, -n))
  lag_year <- c(rep(NA, n), head(year, -n))
  ifelse(!is.na(lag_year) & lag_year == (year - n), lag_val, NA)
}

get_lead_contiguous <- function(x, year, n = 1) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) stop("[BLOCKER] n must be a positive integer for get_lead_contiguous().")
  if (length(x) != length(year)) stop("[BLOCKER] x and year must have equal length for get_lead_contiguous().")
  if (n >= length(x)) return(rep(NA, length(x)))
  lead_val <- c(tail(x, -n), rep(NA, n))
  lead_year <- c(tail(year, -n), rep(NA, n))
  ifelse(!is.na(lead_year) & lead_year == (year + n), lead_val, NA)
}

rolling_sd_contiguous_3 <- function(x, year) {
  out <- rep(NA_real_, length(x))
  if (length(x) < 3L) return(out)
  for (i in seq_along(x)) {
    if (i >= 3L) {
      idx <- (i - 2L):i
      yrs <- year[idx]
      vals <- x[idx]
      if (all(yrs == (year[i] - c(2, 1, 0))) && all(is.finite(vals))) {
        out[i] <- stats::sd(vals)
      }
    }
  }
  out
}

winsorize_vec <- function(x, probs = c(0.01, 0.99), na.rm = TRUE) {
  qs <- stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE, type = 7)
  pmin(pmax(x, qs[1]), qs[2])
}

winsorize_with_cutoffs <- function(x, probs = c(0.01, 0.99), na.rm = TRUE) {
  qs <- stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE, type = 7)
  list(values = pmin(pmax(x, qs[1]), qs[2]), cutoffs = qs)
}

optimize_stacking_from_lpd <- function(lpd_matrix) {
  lpd_matrix <- as.matrix(lpd_matrix)
  if (is.null(colnames(lpd_matrix))) {
    colnames(lpd_matrix) <- paste0("model_", seq_len(ncol(lpd_matrix)))
  }
  if (ncol(lpd_matrix) == 1) {
    out <- 1
    names(out) <- colnames(lpd_matrix)
    return(out)
  }

  softmax <- function(theta) {
    z <- c(theta, 0)
    z <- z - max(z)
    exp(z) / sum(exp(z))
  }
  log_sum_exp <- function(vals) {
    m <- max(vals)
    m + log(sum(exp(vals - m)))
  }
  mixture_objective_value <- function(w) {
    log_w <- log(pmax(w, .Machine$double.eps))
    adjusted <- sweep(lpd_matrix, 2, log_w, "+")
    sum(apply(adjusted, 1, log_sum_exp))
  }
  objective <- function(theta) -mixture_objective_value(softmax(theta))

  singleton_elpd <- colSums(lpd_matrix)
  best_singleton <- which.max(singleton_elpd)
  singleton_w <- rep(0, ncol(lpd_matrix))
  singleton_w[best_singleton] <- 1
  names(singleton_w) <- colnames(lpd_matrix)

  starts <- list(rep(0, ncol(lpd_matrix) - 1))
  for (j in seq_len(ncol(lpd_matrix))) {
    z <- rep(-8, ncol(lpd_matrix))
    z[j] <- 8
    starts[[length(starts) + 1]] <- z[-ncol(lpd_matrix)]
  }
  fits <- lapply(starts, function(st) {
    tryCatch(
      stats::optim(st, objective, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12)),
      error = function(e) NULL
    )
  })
  fits <- Filter(Negate(is.null), fits)
  if (length(fits) == 0) {
    warning("Stacking optimizer failed for all starts; falling back to best singleton elpd model.")
    return(singleton_w)
  }

  vals <- vapply(fits, function(f) -f$value, numeric(1))
  best_fit <- fits[[which.max(vals)]]
  w <- softmax(best_fit$par)
  names(w) <- colnames(lpd_matrix)
  if (mixture_objective_value(w) + 1e-6 < mixture_objective_value(singleton_w)) {
    warning("Stacking optimizer returned a solution worse than the best singleton; falling back to best singleton elpd model.")
    return(singleton_w)
  }
  w
}

optimize_stacking_from_lpd_fast <- function(lpd_matrix, maxit = 500L, reltol = 1e-8) {
  lpd_matrix <- as.matrix(lpd_matrix)
  if (!nrow(lpd_matrix) || !ncol(lpd_matrix)) {
    stop("[BLOCKER] optimize_stacking_from_lpd_fast() requires a non-empty matrix.")
  }
  if (is.null(colnames(lpd_matrix))) {
    colnames(lpd_matrix) <- paste0("model_", seq_len(ncol(lpd_matrix)))
  }
  if (any(!is.finite(lpd_matrix))) {
    stop("[BLOCKER] optimize_stacking_from_lpd_fast() requires finite log predictive densities.")
  }

  n <- nrow(lpd_matrix)
  k <- ncol(lpd_matrix)
  row_max <- apply(lpd_matrix, 1L, max)
  p <- exp(lpd_matrix - row_max)

  singleton_elpd <- colSums(lpd_matrix)
  best_singleton <- which.max(singleton_elpd)
  singleton_w <- rep(0, k)
  singleton_w[best_singleton] <- 1
  names(singleton_w) <- colnames(lpd_matrix)
  singleton_objective <- as.numeric(singleton_elpd[best_singleton])

  if (k == 1L) {
    return(list(
      weights = singleton_w,
      objective = singleton_objective,
      singleton_objective = singleton_objective,
      convergence = 0L,
      fallback_used = FALSE,
      method = "fast_exact",
      message = "single_model"
    ))
  }

  softmax_eta <- function(theta) {
    eta <- c(theta, 0)
    eta <- eta - max(eta)
    w <- exp(eta)
    w / sum(w)
  }

  mixture_objective_value <- function(w) {
    denom <- as.vector(p %*% w)
    sum(row_max + log(pmax(denom, .Machine$double.xmin)))
  }

  fn <- function(theta) {
    w <- softmax_eta(theta)
    -mixture_objective_value(w)
  }

  gr <- function(theta) {
    w <- softmax_eta(theta)
    denom <- as.vector(p %*% w)
    resp <- sweep(p, 2L, w, "*") / pmax(denom, .Machine$double.xmin)
    s <- colSums(resp)
    grad_eta <- s - n * w
    -grad_eta[seq_len(k - 1L)]
  }

  starts <- list(rep(0, k - 1L))
  if (k > 1L) {
    eta <- rep(-4, k)
    eta[best_singleton] <- 4
    starts[[2L]] <- eta[-k]
  }

  fits <- lapply(starts, function(start) {
    tryCatch(
      stats::optim(
        par = start,
        fn = fn,
        gr = gr,
        method = "BFGS",
        control = list(maxit = as.integer(maxit), reltol = reltol)
      ),
      error = function(e) e
    )
  })
  ok <- vapply(fits, function(x) is.list(x) && !inherits(x, "error") && is.finite(x$value), logical(1))
  if (!any(ok)) {
    return(list(
      weights = singleton_w,
      objective = singleton_objective,
      singleton_objective = singleton_objective,
      convergence = NA_integer_,
      fallback_used = TRUE,
      method = "fast_exact",
      message = "optim_failed_all_starts"
    ))
  }

  fits_ok <- fits[ok]
  vals <- vapply(fits_ok, function(fit) -fit$value, numeric(1))
  fit <- fits_ok[[which.max(vals)]]
  w <- softmax_eta(fit$par)
  names(w) <- colnames(lpd_matrix)
  objective <- mixture_objective_value(w)

  fallback_used <- FALSE
  message <- fit$message
  if (objective + 1e-6 < singleton_objective) {
    w <- singleton_w
    objective <- singleton_objective
    fallback_used <- TRUE
    message <- "optimized_objective_below_singleton"
  }

  list(
    weights = w,
    objective = objective,
    singleton_objective = singleton_objective,
    convergence = as.integer(fit$convergence),
    fallback_used = fallback_used,
    method = "fast_exact",
    message = if (is.null(message)) NA_character_ else as.character(message)
  )
}

assert_training_factor_level_coverage <- function(train, test, factor_cols = c("industry", "year"), context = "unknown") {
  for (col in factor_cols) {
    if (col %in% names(train) && col %in% names(test)) {
      train_levels <- unique(as.character(train[[col]][!is.na(train[[col]])]))
      test_levels <- unique(as.character(test[[col]][!is.na(test[[col]])]))
      missing_levels <- setdiff(test_levels, train_levels)
      if (length(missing_levels)) {
        stop("[BLOCKER] Missing training factor-level coverage for ", context,
             "; column=", col, "; missing levels=", paste(missing_levels, collapse = ", "))
      }
    }
  }
  invisible(TRUE)
}

safe_variant_name <- function(x) {
  gsub(" ", "_", gsub("[()|]", "", x))
}

model_key <- function(model_id, target_space, heterogeneity_variant, suffix = NULL) {
  key <- sprintf("%s_%s_%s", model_id, target_space, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

model_key_sampled <- function(model_id, target_space, sample_group, heterogeneity_variant, suffix = NULL) {
  if (is.null(sample_group) || is.na(sample_group) || !nzchar(sample_group)) sample_group <- "main_common"
  key <- sprintf("%s_%s_%s_%s", model_id, target_space, sample_group, safe_variant_name(heterogeneity_variant))
  if (!is.null(suffix) && nzchar(suffix)) key <- paste0(key, suffix)
  key
}

standardize_predictors <- function(df, predictor_vars = pred_vars) {
  for (v in predictor_vars) {
    if (v %in% colnames(df)) {
      m <- mean(df[[v]], na.rm = TRUE)
      s <- sd(df[[v]], na.rm = TRUE)
      df[[paste0(v, "_std")]] <- if (!is.na(s) && s > 0) (df[[v]] - m) / s else 0
    }
  }
  df
}

fix_formula <- function(formula_str, predictor_vars = pred_vars, prefactor = FALSE) {
  if (prefactor) {
    formula_str <- gsub("factor\\(industry\\)", "industry_f", formula_str)
    formula_str <- gsub("factor\\(year\\)", "year_f", formula_str)
  }
  for (v in predictor_vars) {
    formula_str <- gsub(paste0("\\b", v, "\\b"), paste0(v, "_std"), formula_str)
  }
  formula_str
}

read_winsor_sample <- function(sample_file, prefactor = FALSE, root = input_winsor_root) {
  path <- file.path(root, "tables", sample_file)
  if (!file.exists(path)) stop("[BLOCKER] Winsorized sample file missing: ", path)
  df <- read.csv(path, stringsAsFactors = FALSE)
  df <- standardize_predictors(df)
  if (prefactor) {
    df$industry_f <- factor(df$industry)
    df$year_f <- factor(df$year)
  }
  df
}

prepare_varying_slope_data <- function(df, group = varyslope_group) {
  if (identical(group, "industry_year")) {
    if (!all(c("industry", "year") %in% names(df))) {
      stop("[BLOCKER] industry_year varying slopes require industry and year columns.")
    }
    df$industry_year_id <- interaction(df$industry, df$year, drop = TRUE)
  }
  df
}

varying_slope_formula <- function(formula_str, group = varyslope_group) {
  parts <- strsplit(formula_str, "~", fixed = TRUE)[[1]]
  if (length(parts) != 2) stop("[BLOCKER] Cannot parse formula for varying slopes: ", formula_str)
  rhs <- trimws(parts[2])
  rhs <- gsub("\\+\\s*factor\\(industry\\)", "", rhs)
  rhs <- gsub("\\+\\s*factor\\(year\\)", "", rhs)
  rhs <- gsub("\\+\\s*\\(1\\s*\\|\\s*company\\)", "", rhs)
  rhs <- trimws(gsub("\\s+", " ", rhs))
  group_var <- if (identical(group, "firm")) "company" else "industry_year_id"
  sprintf("TA_scaled ~ 1 + %s + (1 + %s | %s)", rhs, rhs, group_var)
}

varying_slope_candidate <- function(model_id, target_space) {
  if (identical(varyslope_scope, "FULL")) return(TRUE)
  paste(model_id, target_space) %in% c(
    "M06 ex_post",
    "M07 ex_post",
    "M07 real_time",
    "M09 real_time",
    "M01 ex_post",
    "M01 real_time"
  )
}

describe_numeric <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(c(N = 0, Mean = NA, SD = NA, Min = NA, P01 = NA, P05 = NA, P25 = NA,
             Median = NA, P75 = NA, P95 = NA, P99 = NA, Max = NA))
  }
  qs <- quantile(x, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99),
                 na.rm = TRUE, names = FALSE, type = 7)
  c(
    N = length(x),
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    P01 = qs[1],
    P05 = qs[2],
    P25 = qs[3],
    Median = qs[4],
    P75 = qs[5],
    P95 = qs[6],
    P99 = qs[7],
    Max = max(x)
  )
}

log_mean_exp <- function(x) {
  m <- max(x)
  m + log(mean(exp(x - m)))
}

extract_weight_variant <- function(model_name, heterogeneity_variant = NULL) {
  if (!is.null(heterogeneity_variant) && !is.na(heterogeneity_variant) && nzchar(heterogeneity_variant)) {
    return(heterogeneity_variant)
  }
  if (grepl("Firm RE", model_name)) return("Firm RE (Random Intercept + Year FE)")
  if (grepl("Pooled", model_name)) return("Pooled (Industry + Year FE)")
  NA_character_
}

extract_base_model_name <- function(model_name) {
  sub(" \\((Firm RE|Pooled).*$", "", model_name)
}

read_original_weight_file <- function(space) {
  if (space == "ex_post") {
    candidates <- c(
      file.path(baseline_root, "tables", "table_stacking_weights_ex_post_corrected.csv"),
      file.path(baseline_root, "tables", "table_stacking_weights_ex_post.csv")
    )
  } else {
    candidates <- c(
      file.path(baseline_root, "tables", "table_stacking_weights_real_time_corrected.csv"),
      file.path(baseline_root, "tables", "table_stacking_weights_real_time.csv")
    )
  }
  source_path <- candidates[file.exists(candidates)][1]
  if (is.na(source_path)) stop("[BLOCKER] No original weight file found for space: ", space)
  df <- read.csv(source_path, stringsAsFactors = FALSE)
  df$Original_Weight_Source <- source_path
  df
}

normalize_rq2_metric_key <- function(x) {
  y <- tolower(trimws(as.character(x)))
  y <- gsub("\\s+", "", y)
  y <- gsub("^abs\\((.*)\\)$", "\\1", y)
  y <- gsub("_stacked$", "", y)
  out <- rep(NA_character_, length(y))
  out[grepl("^raw$|da_raw|^raw_magnitude$", y)] <- "DA_raw"
  out[grepl("^z_est$|^z_estimation$|da_z_est|estimation_scaled", y)] <- "DA_z_estimation"
  unknown <- is.na(out) & !is.na(y) & nzchar(y)
  out[unknown] <- as.character(x)[unknown]
  out
}

first_existing_name <- function(x, candidates, context = "table", required = TRUE) {
  hit <- candidates[candidates %in% names(x)][1]
  if ((is.na(hit) || !nzchar(hit)) && isTRUE(required)) {
    stop("[BLOCKER] Missing required column for ", context, ": one of ", paste(candidates, collapse = ", "))
  }
  if (is.na(hit)) NA_character_ else hit
}

standardize_rq2_jaccard_table <- function(x, source_label = "RQ2 Jaccard table") {
  if (!is.data.frame(x) || !nrow(x)) {
    stop("[BLOCKER] ", source_label, " is missing or empty.")
  }
  target_col <- first_existing_name(x, c("target_space", "Target_Space"), source_label)
  metric_col <- first_existing_name(
    x,
    c("source_score_variable", "score_variable", "reported_score_variable", "metric"),
    source_label
  )
  jaccard_col <- first_existing_name(x, c("jaccard", "Jaccard", "top_tail_jaccard", "top5_jaccard"), source_label)
  spearman_col <- first_existing_name(
    x,
    c("spearman_rank_correlation", "Spearman", "spearman", "rank_spearman"),
    source_label,
    required = FALSE
  )
  out <- data.frame(
    target_space = as.character(x[[target_col]]),
    metric = normalize_rq2_metric_key(x[[metric_col]]),
    jaccard = suppressWarnings(as.numeric(x[[jaccard_col]])),
    spearman_rank_correlation = if (!is.na(spearman_col)) suppressWarnings(as.numeric(x[[spearman_col]])) else NA_real_,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$target_space) & nzchar(out$target_space) & !is.na(out$metric) & nzchar(out$metric), , drop = FALSE]
  if (!nrow(out)) stop("[BLOCKER] ", source_label, " has no usable target_space/metric rows.")
  aggregate(
    cbind(jaccard, spearman_rank_correlation) ~ target_space + metric,
    data = out,
    FUN = function(v) {
      v <- v[is.finite(v)]
      if (!length(v)) NA_real_ else v[1]
    },
    na.action = na.pass
  )
}

build_se08d_rq2_global_fold_local_comparison <- function(global_jaccard, fold_local_jaccard) {
  global_std <- standardize_rq2_jaccard_table(global_jaccard, "SE08D global RQ2 Jaccard baseline")
  fold_std <- standardize_rq2_jaccard_table(fold_local_jaccard, "SE08D fold-local RQ2 Jaccard")
  names(global_std)[names(global_std) == "jaccard"] <- "global_jaccard"
  names(global_std)[names(global_std) == "spearman_rank_correlation"] <- "global_spearman_rank_correlation"
  names(fold_std)[names(fold_std) == "jaccard"] <- "fold_local_jaccard"
  names(fold_std)[names(fold_std) == "spearman_rank_correlation"] <- "fold_local_spearman_rank_correlation"
  comparison <- merge(
    fold_std,
    global_std,
    by = c("target_space", "metric"),
    all.x = TRUE,
    sort = FALSE
  )
  missing <- comparison[!is.finite(comparison$global_jaccard), c("target_space", "metric"), drop = FALSE]
  if (nrow(missing)) {
    missing_keys <- paste(missing$target_space, missing$metric, sep = "/")
    stop(
      "[BLOCKER] se08d global-vs-fold-local RQ2 comparison is incomplete; missing global Jaccard for: ",
      paste(missing_keys, collapse = ", "),
      "."
    )
  }
  comparison$absolute_difference <- comparison$fold_local_jaccard - comparison$global_jaccard
  comparison$abs_absolute_difference <- abs(comparison$absolute_difference)
  comparison$global_material_turnover <- is.finite(comparison$global_jaccard) & comparison$global_jaccard < 0.80
  comparison$fold_local_material_turnover <- is.finite(comparison$fold_local_jaccard) & comparison$fold_local_jaccard < 0.80
  comparison$materiality_conclusion_unchanged <- comparison$global_material_turnover == comparison$fold_local_material_turnover
  comparison[, c(
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
  ), drop = FALSE]
}

decide_se08d_rq2_global_fold_local <- function(comparison) {
  if (!is.data.frame(comparison) || !nrow(comparison)) {
    stop("[BLOCKER] SE08D RQ2 comparison is missing or empty.")
  }
  required <- c(
    "target_space", "metric", "global_jaccard", "fold_local_jaccard",
    "absolute_difference", "abs_absolute_difference", "global_material_turnover",
    "fold_local_material_turnover", "materiality_conclusion_unchanged"
  )
  missing <- setdiff(required, names(comparison))
  if (length(missing)) stop("[BLOCKER] SE08D RQ2 comparison lacks columns: ", paste(missing, collapse = ", "))
  decision <- rep("PASS", nrow(comparison))
  decision[!is.finite(comparison$global_jaccard) | !is.finite(comparison$fold_local_jaccard)] <- "FAIL"
  decision[is.finite(comparison$fold_local_jaccard) & comparison$fold_local_jaccard >= 0.80] <- "FAIL"
  unchanged <- !is.na(comparison$materiality_conclusion_unchanged) & comparison$materiality_conclusion_unchanged
  warn <- decision == "PASS" & (
    !unchanged |
      (is.finite(comparison$abs_absolute_difference) & comparison$abs_absolute_difference > 0.10)
  )
  decision[warn] <- "WARN"
  interpretation <- ifelse(
    decision == "PASS",
    "Fold-local row-vs-grouped DA object divergence remains material and stable relative to the primary global-preprocessing baseline.",
    ifelse(
      decision == "WARN",
      "Fold-local row-vs-grouped DA object divergence remains reviewer-relevant but changes materially relative to the global-preprocessing baseline; qualify RQ2 robustness.",
      "Fold-local preprocessing does not support the RQ2 robustness claim because the global-vs-fold-local comparison is incomplete or material top-tail turnover is not present."
    )
  )
  data.frame(
    decision_id = paste(comparison$target_space, comparison$metric, "rq2_fold_local_global_comparison", sep = "_"),
    target_space = comparison$target_space,
    metric = comparison$metric,
    fold_local_value = comparison$fold_local_jaccard,
    global_value = comparison$global_jaccard,
    absolute_difference = comparison$absolute_difference,
    abs_absolute_difference = comparison$abs_absolute_difference,
    global_material_turnover = comparison$global_material_turnover,
    fold_local_material_turnover = comparison$fold_local_material_turnover,
    materiality_conclusion_unchanged = comparison$materiality_conclusion_unchanged,
    decision = decision,
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}
