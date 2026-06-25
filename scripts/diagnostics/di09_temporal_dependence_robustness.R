# -----------------------------------------------------------------------------
# Script: di09_temporal_dependence_robustness.R
# Purpose: Lightweight AR(1) temporal-dependence robustness simulation for the
#          row-minus-grouped Firm-RE premium.
#
# Intended use:
#   Rscript scripts/diagnostics/di09_temporal_dependence_robustness.R
#
# This is a non-BRMS mechanism diagnostic. It uses lm/lmer-style fast scoring
# and writes only simulation diagnostics under output_root.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di09", "Temporal-dependence robustness diagnostic")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

if (!env_flag("ACCRUAL_RUN_TEMPORAL_ROBUSTNESS", FALSE)) {
  stop("[BLOCKER] Temporal-dependence robustness is gated because it can run many lmer fits. ",
       "Set ACCRUAL_RUN_TEMPORAL_ROBUSTNESS=TRUE to run this diagnostic intentionally.")
}

script_start_time <- Sys.time()
script_name <- "scripts/diagnostics/di09_temporal_dependence_robustness.R"
script_version <- "2026-06-25-v1-temporal-dependence-robustness"

temporal_root <- file.path(output_root, "simulation", "temporal_dependence")
tables_dir <- file.path(temporal_root, "tables")
logs_dir <- file.path(temporal_root, "logs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

replications_path <- file.path(tables_dir, "table_temporal_dependence_replications.csv")
premium_path <- file.path(tables_dir, "table_temporal_dependence_firmre_premium.csv")
decision_path <- file.path(tables_dir, "table_temporal_dependence_decision.csv")
io_manifest_path <- file.path(tables_dir, "table_temporal_dependence_io_manifest.csv")
note_path <- file.path(logs_dir, "temporal_dependence_reviewer_note.md")

cfg <- accrual_simulation_runtime_config("temporal_robustness")
rho_grid <- cfg$rho_grid
sigma_firm_grid <- cfg$sigma_grid
T_grid <- as.integer(cfg$t_grid)
n_firms <- as.integer(cfg$n_firms)
n_industries <- as.integer(cfg$n_industries)
R <- as.integer(cfg$R)
K <- as.integer(cfg$K)
sigma_eps <- as.numeric(cfg$sigma_eps)
base_seed <- as.integer(cfg$seed)

git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}
file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

safe_seed <- function(offset) {
  seed <- (base_seed + as.integer(offset)) %% .Machine$integer.max
  set_accrual_effective_seed(seed, context = "di09_temporal_dependence")
  seed
}

simulate_panel_ar1 <- function(T, rho, sigma_firm, replication) {
  offset <- 700000L + as.integer(replication) +
    as.integer(T) * 1000L +
    as.integer(round(rho * 1000)) * 10L +
    as.integer(round(sigma_firm * 10000))
  seed <- safe_seed(offset)
  firms <- sprintf("F%04d", seq_len(n_firms))
  years <- seq_len(T)
  industries <- sprintf("IND%02d", seq_len(n_industries))
  industry_map <- sample(rep(industries, length.out = n_firms), n_firms)
  names(industry_map) <- firms
  df <- expand.grid(company = firms, year_num = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  df$industry <- industry_map[df$company]
  firm_effect <- stats::rnorm(n_firms, 0, sigma_firm)
  names(firm_effect) <- firms
  industry_year <- stats::rnorm(n_industries * T, 0, 0.020)
  dim(industry_year) <- c(n_industries, T)
  rownames(industry_year) <- industries
  colnames(industry_year) <- as.character(years)
  df$inv_A_lag <- stats::rnorm(nrow(df), 0.020, 0.010)
  df$dREV_dREC_scaled <- stats::rnorm(nrow(df), 0.050, 0.100)
  df$PPE_scaled <- stats::rnorm(nrow(df), 0.450, 0.200)
  df$ROA_lag <- stats::rnorm(nrow(df), 0.040, 0.080)
  epsilon <- numeric(nrow(df))
  for (firm in firms) {
    idx <- which(df$company == firm)
    idx <- idx[order(df$year_num[idx])]
    innovation <- stats::rnorm(length(idx), 0, sigma_eps)
    previous <- innovation[[1]] / sqrt(max(1 - rho^2, .Machine$double.eps))
    epsilon[idx[[1]]] <- previous
    if (length(idx) > 1) {
      for (pos in seq.int(2L, length(idx))) {
        previous <- rho * previous + innovation[[pos]]
        epsilon[idx[[pos]]] <- previous
      }
    }
  }
  mu <- 0.030 * df$inv_A_lag + 0.060 * df$dREV_dREC_scaled -
    0.040 * df$PPE_scaled + 0.080 * df$ROA_lag +
    firm_effect[df$company] +
    industry_year[cbind(df$industry, as.character(df$year_num))]
  df$epsilon <- epsilon
  df$TA_scaled <- mu + epsilon
  df$company <- factor(df$company, levels = firms)
  df$industry <- factor(df$industry, levels = industries)
  df$year <- factor(df$year_num, levels = as.character(years))
  attr(df, "seed") <- seed
  df
}

make_row_folds <- function(df, K, offset) {
  safe_seed(offset)
  sample(rep(seq_len(K), length.out = nrow(df)))
}

make_grouped_folds <- function(df, K, offset) {
  safe_seed(offset)
  firms <- sort(unique(as.character(df$company)))
  assignment <- data.frame(company = sample(firms), fold = rep(seq_len(K), length.out = length(firms)), stringsAsFactors = FALSE)
  fold_map <- assignment$fold
  names(fold_map) <- assignment$company
  unname(fold_map[as.character(df$company)])
}

normal_lpd <- function(y, mu, sigma) stats::dnorm(y, mu, max(sigma, .Machine$double.eps), log = TRUE)

score_fold <- function(train, test, validation_target, lme4_available) {
  pooled <- stats::lm(
    TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(industry) + factor(year),
    data = train
  )
  pred_pooled <- as.numeric(stats::predict(pooled, newdata = test))
  lpd_pooled <- normal_lpd(test$TA_scaled, pred_pooled, stats::sigma(pooled))
  if (!lme4_available) {
    return(data.frame(
      lpd_pooled = lpd_pooled,
      lpd_firmre = NA_real_,
      warning = "INSUFFICIENT_DEPENDENCY_lme4",
      stringsAsFactors = FALSE
    ))
  }
  firmre <- lme4::lmer(
    TA_scaled ~ inv_A_lag + dREV_dREC_scaled + PPE_scaled + ROA_lag + factor(year) + (1 | company),
    data = train,
    REML = FALSE,
    control = lme4::lmerControl(check.conv.singular = "ignore", check.conv.grad = "ignore", check.conv.hess = "ignore")
  )
  pred_firmre <- if (identical(validation_target, "row_level_kfold")) {
    as.numeric(stats::predict(firmre, newdata = test, re.form = NULL, allow.new.levels = TRUE))
  } else {
    as.numeric(stats::predict(firmre, newdata = test, re.form = NA, allow.new.levels = TRUE))
  }
  data.frame(
    lpd_pooled = lpd_pooled,
    lpd_firmre = normal_lpd(test$TA_scaled, pred_firmre, stats::sigma(firmre)),
    warning = ifelse(lme4::isSingular(firmre, tol = 1e-5), "singular_firmre_fit", NA_character_),
    stringsAsFactors = FALSE
  )
}

score_cv <- function(df, validation_target, scenario_offset, lme4_available) {
  folds <- if (identical(validation_target, "row_level_kfold")) {
    make_row_folds(df, K, scenario_offset + 11L)
  } else {
    make_grouped_folds(df, K, scenario_offset + 29L)
  }
  fold_scores <- lapply(seq_len(K), function(fold) {
    score_fold(df[folds != fold, , drop = FALSE], df[folds == fold, , drop = FALSE], validation_target, lme4_available)
  })
  scored <- bind_rows(fold_scores)
  data.frame(
    validation_target = validation_target,
    score_pooled = sum(scored$lpd_pooled, na.rm = TRUE),
    score_firmre = if (all(is.na(scored$lpd_firmre))) NA_real_ else sum(scored$lpd_firmre, na.rm = TRUE),
    firmre_premium = if (all(is.na(scored$lpd_firmre))) NA_real_ else sum(scored$lpd_firmre, na.rm = TRUE) - sum(scored$lpd_pooled, na.rm = TRUE),
    firmre_weight = NA_real_,
    warning = paste(unique(na.omit(scored$warning)), collapse = ";"),
    stringsAsFactors = FALSE
  )
}

run_replication <- function(T, rho, sigma_firm, replication, lme4_available) {
  scenario_offset <- 800000L + as.integer(replication) +
    as.integer(T) * 1000L + as.integer(round(rho * 1000)) * 10L +
    as.integer(round(sigma_firm * 10000))
  df <- simulate_panel_ar1(T = T, rho = rho, sigma_firm = sigma_firm, replication = replication)
  seed <- attr(df, "seed")
  row_score <- score_cv(df, "row_level_kfold", scenario_offset, lme4_available)
  grouped_score <- score_cv(df, "grouped_firm_kfold", scenario_offset, lme4_available)
  pair <- bind_rows(row_score, grouped_score)
  row_premium <- pair$firmre_premium[pair$validation_target == "row_level_kfold"]
  grouped_premium <- pair$firmre_premium[pair$validation_target == "grouped_firm_kfold"]
  replication_cols <- c(
    "Replication", "n_firms", "T", "rho", "sigma_firm", "K",
    "validation_target", "score_pooled", "score_firmre", "firmre_premium",
    "firmre_weight", "row_minus_grouped_firmre_premium",
    "row_minus_grouped_firmre_weight_premium", "seed", "fit_status", "warning"
  )
  pair %>%
    mutate(
      Replication = replication,
      n_firms = n_firms,
      T = T,
      rho = rho,
      sigma_firm = sigma_firm,
      K = K,
      row_minus_grouped_firmre_premium = row_premium - grouped_premium,
      row_minus_grouped_firmre_weight_premium = NA_real_,
      seed = seed,
      fit_status = ifelse(is.finite(.data$firmre_premium), "SUCCESS", "INSUFFICIENT_DEPENDENCY"),
      warning = ifelse(nzchar(.data$warning), .data$warning, NA_character_)
    ) %>%
    select(all_of(replication_cols))
}

lme4_available <- requireNamespace("lme4", quietly = TRUE)
grid <- expand.grid(
  T = T_grid,
  rho = rho_grid,
  sigma_firm = sigma_firm_grid,
  Replication = seq_len(R),
  KEEP.OUT.ATTRS = FALSE
)

replication_rows <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  replication_rows[[i]] <- tryCatch(
    run_replication(g$T, g$rho, g$sigma_firm, g$Replication, lme4_available),
    error = function(e) {
      bind_rows(lapply(c("row_level_kfold", "grouped_firm_kfold"), function(target) {
        data.frame(
          Replication = g$Replication,
          n_firms = n_firms,
          T = g$T,
          rho = g$rho,
          sigma_firm = g$sigma_firm,
          K = K,
          validation_target = target,
          score_pooled = NA_real_,
          score_firmre = NA_real_,
          firmre_premium = NA_real_,
          firmre_weight = NA_real_,
          row_minus_grouped_firmre_premium = NA_real_,
          row_minus_grouped_firmre_weight_premium = NA_real_,
          seed = NA_integer_,
          fit_status = "FAILED",
          warning = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }))
    }
  )
}

replications <- bind_rows(replication_rows)
write_csv_safely(replications, replications_path, row.names = FALSE, fileEncoding = "UTF-8")

usable_pairs <- replications %>%
  filter(.data$fit_status == "SUCCESS") %>%
  distinct(.data$Replication, .data$n_firms, .data$T, .data$rho, .data$sigma_firm, .data$K,
           .data$row_minus_grouped_firmre_premium) %>%
  group_by(.data$n_firms, .data$T, .data$rho, .data$sigma_firm, .data$K) %>%
  summarise(
    R = n(),
    mean_row_minus_grouped_firmre_premium = mean(.data$row_minus_grouped_firmre_premium, na.rm = TRUE),
    median_row_minus_grouped_firmre_premium = stats::median(.data$row_minus_grouped_firmre_premium, na.rm = TRUE),
    sd_row_minus_grouped_firmre_premium = stats::sd(.data$row_minus_grouped_firmre_premium, na.rm = TRUE),
    p05_row_minus_grouped_firmre_premium = as.numeric(stats::quantile(.data$row_minus_grouped_firmre_premium, 0.05, na.rm = TRUE, names = FALSE)),
    p95_row_minus_grouped_firmre_premium = as.numeric(stats::quantile(.data$row_minus_grouped_firmre_premium, 0.95, na.rm = TRUE, names = FALSE)),
    share_row_minus_grouped_positive = mean(.data$row_minus_grouped_firmre_premium > 0, na.rm = TRUE),
    .groups = "drop"
  )

if (nrow(usable_pairs) > 0) {
  target_long <- replications %>%
    filter(.data$fit_status == "SUCCESS") %>%
    group_by(.data$n_firms, .data$T, .data$rho, .data$sigma_firm, .data$validation_target) %>%
    summarise(mean_firmre_premium = mean(.data$firmre_premium, na.rm = TRUE), .groups = "drop")

  if (requireNamespace("tidyr", quietly = TRUE)) {
    target_summary <- target_long %>%
      tidyr::pivot_wider(names_from = "validation_target", values_from = "mean_firmre_premium")
  } else {
    target_summary <- target_long
    row_summary <- target_summary[target_summary$validation_target == "row_level_kfold", ]
    grouped_summary <- target_summary[target_summary$validation_target == "grouped_firm_kfold", ]
    target_summary <- merge(
      row_summary[, c("n_firms", "T", "rho", "sigma_firm", "mean_firmre_premium")],
      grouped_summary[, c("n_firms", "T", "rho", "sigma_firm", "mean_firmre_premium")],
      by = c("n_firms", "T", "rho", "sigma_firm"),
      all = TRUE,
      suffixes = c("_row_level_kfold", "_grouped_firm_kfold")
    )
    names(target_summary)[names(target_summary) == "mean_firmre_premium_row_level_kfold"] <- "row_level_kfold"
    names(target_summary)[names(target_summary) == "mean_firmre_premium_grouped_firm_kfold"] <- "grouped_firm_kfold"
  }

  premium_summary <- usable_pairs %>%
    left_join(target_summary, by = c("n_firms", "T", "rho", "sigma_firm")) %>%
    transmute(
      n_firms = .data$n_firms,
      T = .data$T,
      rho = .data$rho,
      sigma_firm = .data$sigma_firm,
      R = .data$R,
      mean_row_firmre_premium = .data$row_level_kfold,
      mean_grouped_firmre_premium = .data$grouped_firm_kfold,
      mean_row_minus_grouped_firmre_premium = .data$mean_row_minus_grouped_firmre_premium,
      median_row_minus_grouped_firmre_premium = .data$median_row_minus_grouped_firmre_premium,
      sd_row_minus_grouped_firmre_premium = .data$sd_row_minus_grouped_firmre_premium,
      p05_row_minus_grouped_firmre_premium = .data$p05_row_minus_grouped_firmre_premium,
      p95_row_minus_grouped_firmre_premium = .data$p95_row_minus_grouped_firmre_premium,
      share_row_minus_grouped_positive = .data$share_row_minus_grouped_positive,
      interpretation = ifelse(
        .data$mean_row_minus_grouped_firmre_premium > 0,
        "Row-level validation gives a larger Firm-RE premium; interpret as within-firm interpolation under persistent same-firm shocks.",
        "Grouped validation premium is not below row-level premium in this scenario."
      )
    )
} else {
  premium_summary <- data.frame(
    n_firms = integer(0),
    T = integer(0),
    rho = numeric(0),
    sigma_firm = numeric(0),
    R = integer(0),
    mean_row_firmre_premium = numeric(0),
    mean_grouped_firmre_premium = numeric(0),
    mean_row_minus_grouped_firmre_premium = numeric(0),
    median_row_minus_grouped_firmre_premium = numeric(0),
    sd_row_minus_grouped_firmre_premium = numeric(0),
    p05_row_minus_grouped_firmre_premium = numeric(0),
    p95_row_minus_grouped_firmre_premium = numeric(0),
    share_row_minus_grouped_positive = numeric(0),
    interpretation = character(0),
    stringsAsFactors = FALSE
  )
}
write_csv_safely(premium_summary, premium_path, row.names = FALSE, fileEncoding = "UTF-8")

usable_n <- nrow(usable_pairs)
decision_value <- "FAIL_TEMPORAL_ROBUSTNESS_UNAVAILABLE"
decision_reason <- "No usable Firm-RE replications were available."
if (!lme4_available) {
  decision_value <- "INSUFFICIENT_INPUTS"
  decision_reason <- "Package lme4 is unavailable, so Firm-RE scoring was not computed."
} else if (usable_n > 0) {
  rho_trend <- premium_summary %>%
    group_by(.data$T, .data$sigma_firm) %>%
    summarise(
      rho_slope = if (length(unique(.data$rho)) >= 2) stats::coef(stats::lm(mean_row_minus_grouped_firmre_premium ~ rho, data = cur_data()))[[2]] else NA_real_,
      .groups = "drop"
    )
  material_increase <- any(rho_trend$rho_slope > 0.05, na.rm = TRUE)
  mixed <- any(rho_trend$rho_slope > 0.02, na.rm = TRUE) && any(rho_trend$rho_slope < -0.02, na.rm = TRUE)
  decision_value <- dplyr::case_when(
    material_increase ~ "WARN_ROW_PREMIUM_INCREASES_WITH_TEMPORAL_DEPENDENCE",
    mixed ~ "WARN_TEMPORAL_RESULTS_MIXED",
    TRUE ~ "PASS_TEMPORAL_ROBUSTNESS_AVAILABLE"
  )
  decision_reason <- dplyr::case_when(
    material_increase ~ "The row-minus-grouped Firm-RE premium increases materially with rho in at least one scenario.",
    mixed ~ "The rho pattern is mixed across T and sigma_firm scenarios.",
    TRUE ~ "Temporal persistence does not materially increase the row-minus-grouped Firm-RE premium."
  )
}

decision <- data.frame(
  temporal_decision = decision_value,
  usable_replication_pairs = usable_n,
  requested_replications_per_cell = R,
  rho_grid = paste(rho_grid, collapse = ","),
  sigma_firm_grid = paste(sigma_firm_grid, collapse = ","),
  T_grid = paste(T_grid, collapse = ","),
  n_firms = n_firms,
  K = K,
  lme4_available = lme4_available,
  interpretation = decision_reason,
  row_validation_interpretation = "Row-level K-fold is within-firm interpolation when other years of the same firm remain in training.",
  grouped_validation_interpretation = "Grouped firm K-fold is out-of-firm prediction because held-out firms have no training observations.",
  stringsAsFactors = FALSE
)
write_csv_safely(decision, decision_path, row.names = FALSE, fileEncoding = "UTF-8")

note <- c(
  "# Temporal Dependence Robustness Note",
  "",
  "This lightweight AR(1) mechanism simulation tests whether the row-minus-grouped Firm-RE premium changes as same-firm residual shocks become temporally persistent.",
  "",
  "The simulated panel follows `TA_it = X_it beta + industry_year_effect + u_i + epsilon_it`, with `epsilon_it = rho * epsilon_i,t-1 + nu_it`.",
  "",
  "Row-level K-fold allows other years of the same firm to remain in training, so it should be interpreted as within-firm interpolation when persistent same-firm shocks are present.",
  "",
  "Grouped firm-level K-fold holds out entire firms and is therefore out-of-firm prediction.",
  "",
  paste0("Decision: `", decision_value, "`."),
  decision_reason,
  "",
  "A warning is not a failure of the paper. It means row-level validation is capturing within-firm temporal information and should not be interpreted as out-of-time or out-of-firm validity.",
  "",
  "Temporal persistence does not by itself prove leakage, earnings management, or managerial intent."
)
writeLines(note, note_path, useBytes = TRUE)

output_paths <- c(replications_path, premium_path, decision_path, note_path)
io_manifest <- rbind(
data.frame(
  script_name = script_name,
  script_version = script_version,
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  git_commit = git_commit_or_na(),
  output_root = output_root,
  io_class = "output",
  path = output_paths,
  exists = file.exists(output_paths),
  file_size_bytes = vapply(output_paths, file_size_or_na, numeric(1)),
  modified_time = vapply(output_paths, mtime_or_na, character(1)),
  md5 = vapply(output_paths, file_hash_or_na, character(1)),
  rho_grid = paste(rho_grid, collapse = ","),
  sigma_firm_grid = paste(sigma_firm_grid, collapse = ","),
  T_grid = paste(T_grid, collapse = ","),
  replications = R,
  n_firms = n_firms,
  K = K,
  seed = base_seed,
  stringsAsFactors = FALSE
),
data.frame(
  script_name = script_name,
  script_version = script_version,
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  git_commit = git_commit_or_na(),
  output_root = output_root,
  io_class = "output",
  path = io_manifest_path,
  exists = TRUE,
  file_size_bytes = NA_real_,
  modified_time = NA_character_,
  md5 = "self_referential_manifest",
  rho_grid = paste(rho_grid, collapse = ","),
  sigma_firm_grid = paste(sigma_firm_grid, collapse = ","),
  T_grid = paste(T_grid, collapse = ","),
  replications = R,
  n_firms = n_firms,
  K = K,
  seed = base_seed,
  stringsAsFactors = FALSE
)
)
write_csv_safely(io_manifest, io_manifest_path, row.names = FALSE, fileEncoding = "UTF-8")

cat("[SUCCESS] Temporal-dependence robustness outputs written under ", temporal_root, "\n", sep = "")
phase_end("di09", "Temporal-dependence robustness diagnostic")
