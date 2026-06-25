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

