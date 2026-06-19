# -----------------------------------------------------------------------------
# Script: 12_lofo_stacking.R
# Purpose: Reviewer Priority 2 - grouped PSIS leave-one-firm-out stacking on
#          already winsorized brms models.
# -----------------------------------------------------------------------------

library(dplyr)
library(brms)
library(loo)

source("scripts/ma00_setup.R")
phase_begin("ro01", "Grouped PSIS-LOFO robustness")
ensure_analysis_dirs()

options(mc.cores = 1)

lofo_root <- file.path(winsor_root, "lofo")
lofo_dirs <- file.path(lofo_root, c("", "tables", "logs", "figures", "cache"))
for (d in lofo_dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

table_path <- function(file_name, prefer_input = FALSE) {
  candidates <- if (prefer_input) {
    c(file.path(input_winsor_root, "tables", file_name), file.path(output_root, "tables", file_name))
  } else {
    c(file.path(output_root, "tables", file_name), file.path(input_winsor_root, "tables", file_name))
  }
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) candidates[1] else hit
}

input_paths <- c(
  ex_post_sample = table_path("final_common_ex_post_sample_winsor.csv", prefer_input = TRUE),
  no_lookahead_sample = table_path("final_common_realtime_sample_winsor.csv", prefer_input = TRUE),
  formulas = table_path("table_named_model_formulas_winsor.csv"),
  diagnostics = file.path(output_root, "tables", "table_brms_diagnostics_winsor.csv"),
  rowloo_ex_post = file.path(output_root, "tables", "table_stacking_weights_ex_post_winsor_corrected.csv"),
  rowloo_no_lookahead = file.path(output_root, "tables", "table_stacking_weights_no_lookahead_winsor_corrected.csv")
)

missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs) > 0) {
  stop("[BLOCKER] Missing required Priority 1 winsor input(s): ",
       paste(names(missing_inputs), missing_inputs, sep = "=", collapse = "; "))
}

models_dir <- file.path(winsor_root, "models")
if (!dir.exists(models_dir)) stop("[BLOCKER] Winsorized model directory missing: ", models_dir)
if (length(list.files(models_dir, pattern = "\\.rds$", full.names = TRUE)) == 0) {
  stop("[BLOCKER] No winsorized model files found under: ", models_dir)
}

formulas_df <- read.csv(input_paths[["formulas"]], stringsAsFactors = FALSE)
diag_df <- read.csv(input_paths[["diagnostics"]], stringsAsFactors = FALSE)
rowloo_ep <- read.csv(input_paths[["rowloo_ex_post"]], stringsAsFactors = FALSE)
rowloo_rt <- read.csv(input_paths[["rowloo_no_lookahead"]], stringsAsFactors = FALSE)
sample_ep <- read.csv(input_paths[["ex_post_sample"]], stringsAsFactors = FALSE)
sample_rt <- read.csv(input_paths[["no_lookahead_sample"]], stringsAsFactors = FALSE)

ex_post_ids <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07")
no_lookahead_ids <- c("M01", "M02", "M03", "M07", "M09")

family_label <- function(model_id) {
  dplyr::case_when(
    model_id %in% c("M01", "M02", "M03") ~ "Jones-family",
    model_id %in% c("M04", "M05", "M06") ~ "Cash-flow/McNichols-family",
    model_id == "M07" ~ "Ball-Shivakumar/asymmetry",
    model_id == "M09" ~ "No-lookahead/real-time",
    model_id == "M08" ~ "Secondary volatility",
    model_id == "M10" ~ "Secondary operating-cycle",
    TRUE ~ "Other"
  )
}

space_label_for_output <- function(space) {
  ifelse(space == "real_time", "no_lookahead", space)
}

model_file_candidates <- function(model_id, target_space, sample_group, heterogeneity_variant) {
  key_no_suffix <- model_key(model_id, target_space, heterogeneity_variant)
  key_winsor <- model_key(model_id, target_space, heterogeneity_variant, "_winsor")
  key_sampled_winsor <- model_key_sampled(model_id, target_space, sample_group, heterogeneity_variant, "_winsor")
  file.path(models_dir, c(
    paste0("fit_", key_sampled_winsor, "_sp.rds"),
    paste0("fit_", key_sampled_winsor, ".rds"),
    paste0("fit_", key_winsor, "_sp.rds"),
    paste0("fit_", key_no_suffix, "_sp_winsor.rds"),
    paste0("fit_", key_winsor, ".rds"),
    paste0("fit_", key_no_suffix, "_winsor.rds"),
    paste0("fit_", key_no_suffix, ".rds")
  ))
}

discover_model_file <- function(model_id, target_space, sample_group, heterogeneity_variant) {
  candidates <- unique(model_file_candidates(model_id, target_space, sample_group, heterogeneity_variant))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(NA_character_)
  chosen <- existing[1]
  normalized <- normalizePath(chosen, winslash = "/", mustWork = TRUE)
  baseline_models_root <- normalizePath(file.path(baseline_root, "models"), winslash = "/", mustWork = FALSE)
  if (startsWith(normalized, baseline_models_root)) {
    stop("[BLOCKER] Refusing to read non-winsorized model file: ", normalized)
  }
  model_root <- normalizePath(models_dir, winslash = "/", mustWork = FALSE)
  if (!startsWith(normalized, model_root)) {
    stop("[BLOCKER] Model file is not under current ACCRUAL_OUTPUT_ROOT models directory: ", normalized)
  }
  chosen
}

aggregate_log_lik_by_firm <- function(ll_obs, firm_ids, firm_levels) {
  if (length(dim(ll_obs)) != 2) stop("[BLOCKER] log_lik output is not a draws x observations matrix.")
  if (ncol(ll_obs) != length(firm_ids)) {
    stop(sprintf("[BLOCKER] log_lik observation count mismatch: ncol=%d, sample rows=%d.",
                 ncol(ll_obs), length(firm_ids)))
  }
  ll_firm <- sapply(firm_levels, function(f) {
    rowSums(ll_obs[, firm_ids == f, drop = FALSE])
  })
  if (is.null(dim(ll_firm))) ll_firm <- matrix(ll_firm, ncol = length(firm_levels))
  colnames(ll_firm) <- firm_levels
  if (ncol(ll_firm) != length(firm_levels)) {
    stop("[BLOCKER] Grouped log-likelihood column count differs from number of firms.")
  }
  ll_firm
}

reliability_from_k <- function(k_values, failed = FALSE) {
  if (failed) return("FAILED")
  if (length(k_values) == 0 || all(is.na(k_values))) return("FAILED")
  gt07 <- sum(k_values > 0.7, na.rm = TRUE)
  gt10 <- sum(k_values > 1.0, na.rm = TRUE)
  share_gt07 <- gt07 / length(k_values)
  if (gt10 > 0 || share_gt07 > 0.10) return("LOW_RELIABILITY")
  if (gt07 > 0) return("CAUTION")
  "OK"
}

obs_per_firm_stats <- function(firm_ids) {
  counts <- as.integer(table(firm_ids))
  c(
    N_Firms = length(counts),
    Min_Obs_Per_Firm = min(counts),
    Median_Obs_Per_Firm = median(counts),
    Max_Obs_Per_Firm = max(counts)
  )
}

load_or_compute_lofo <- function(model_row, sample_df, target_space, primary = TRUE) {
  model_file <- discover_model_file(model_row$Model_ID, target_space, model_row$Sample_Group, model_row$Heterogeneity_Variant)
  firm_ids <- sample_df$company
  firm_levels <- unique(firm_ids)
  stats <- obs_per_firm_stats(firm_ids)

  base_key <- model_key_sampled(model_row$Model_ID, target_space, model_row$Sample_Group, model_row$Heterogeneity_Variant, "_winsor")
  cache_path <- file.path(lofo_root, "cache", paste0(base_key, "_grouped_psis_lofo.rds"))

  empty_diag <- data.frame(
    Target_Space = target_space,
    Model_ID = model_row$Model_ID,
    Model_Name = model_row$Model_Name,
    Sample_Group = model_row$Sample_Group,
    Main_Stack_Inclusion = model_row$Main_Stack_Inclusion,
    Secondary_Robustness = model_row$Secondary_Robustness,
    Heterogeneity_Variant = model_row$Heterogeneity_Variant,
    Model_File = ifelse(is.na(model_file), NA_character_, model_file),
    N_Obs = nrow(sample_df),
    N_Firms = as.integer(stats["N_Firms"]),
    Min_Obs_Per_Firm = as.integer(stats["Min_Obs_Per_Firm"]),
    Median_Obs_Per_Firm = as.numeric(stats["Median_Obs_Per_Firm"]),
    Max_Obs_Per_Firm = as.integer(stats["Max_Obs_Per_Firm"]),
    LogLik_Method = "grouped_PSIS_LOFO",
    Re_Formula_Used = "NA",
    elpd_lofo = NA_real_,
    se_elpd_lofo = NA_real_,
    p_lofo = NA_real_,
    looic_lofo = NA_real_,
    pareto_k_gt_0_7 = NA_integer_,
    pareto_k_gt_1_0 = NA_integer_,
    max_pareto_k = NA_real_,
    ParetoK_Status = "FAILED",
    Family_Level_Conclusion = NA_character_,
    Model_Level_Caution = NA_character_,
    reliability_flag = "FAILED",
    included_in_stack = FALSE,
    exclusion_reason = NA_character_,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )

  if (is.na(model_file)) {
    empty_diag$exclusion_reason <- "Winsorized model file not found."
    return(list(diag = empty_diag, loo = NULL, firms = firm_levels))
  }

  if (file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    if (!"ParetoK_Status" %in% names(cached$diag)) {
      cached$diag$ParetoK_Status <- if (isTRUE(cached$diag$pareto_k_gt_1_0 > 0)) {
        "POOR_GT_1_0"
      } else if (isTRUE(cached$diag$pareto_k_gt_0_7 > 0)) {
        "CAUTION_GT_0_7"
      } else {
        "OK"
      }
    }
    if (!"Model_Level_Caution" %in% names(cached$diag)) {
      cached$diag$Model_Level_Caution <- if (isTRUE(cached$diag$pareto_k_gt_0_7 > 0)) {
        "Exact grouped K-fold is recommended for affected models."
      } else {
        "No Pareto-k caution."
      }
    }
    for (nm in c("Prior_Set_ID", "Likelihood_Family", "Model_Structure", "Output_Root")) {
      if (!nm %in% names(cached$diag)) cached$diag[[nm]] <- metadata_columns()[[nm]]
    }
    return(cached)
  }

  message(sprintf("Computing grouped PSIS-LOFO: %s %s %s", target_space, model_row$Model_ID, model_row$Heterogeneity_Variant))
  fit <- tryCatch(readRDS(model_file), error = function(e) e)
  if (inherits(fit, "error")) {
    empty_diag$exclusion_reason <- paste("Failed to read model:", fit$message)
    result <- list(diag = empty_diag, loo = NULL, firms = firm_levels)
    saveRDS(result, cache_path)
    return(result)
  }

  ll_obs <- tryCatch(
    brms::log_lik(fit, re_formula = NA),
    error = function(e) e
  )

  loglik_method <- "grouped_PSIS_LOFO_population_level"
  re_formula_used <- "NA"
  fallback_used <- FALSE
  fallback_error <- NA_character_

  if (inherits(ll_obs, "error")) {
    fallback_error <- ll_obs$message
    message("  Primary log_lik(re_formula = NA) failed; trying diagnostic fallback log_lik(fit).")
    ll_obs <- tryCatch(brms::log_lik(fit), error = function(e) e)
    loglik_method <- "conditional_grouped_LOFO_not_primary"
    re_formula_used <- "model_default"
    fallback_used <- TRUE
  }

  if (inherits(ll_obs, "error")) {
    empty_diag$LogLik_Method <- loglik_method
    empty_diag$Re_Formula_Used <- re_formula_used
    empty_diag$exclusion_reason <- paste("log_lik failed; primary error:", fallback_error, "; fallback error:", ll_obs$message)
    result <- list(diag = empty_diag, loo = NULL, firms = firm_levels)
    saveRDS(result, cache_path)
    return(result)
  }

  ll_firm <- tryCatch(
    aggregate_log_lik_by_firm(ll_obs, firm_ids, firm_levels),
    error = function(e) e
  )
  rm(ll_obs)
  gc()

  if (inherits(ll_firm, "error")) {
    empty_diag$LogLik_Method <- loglik_method
    empty_diag$Re_Formula_Used <- re_formula_used
    empty_diag$exclusion_reason <- ll_firm$message
    result <- list(diag = empty_diag, loo = NULL, firms = firm_levels)
    saveRDS(result, cache_path)
    return(result)
  }

  loo_firm <- tryCatch(loo::loo(ll_firm, cores = 1), error = function(e) e)
  if (inherits(loo_firm, "error")) {
    empty_diag$LogLik_Method <- loglik_method
    empty_diag$Re_Formula_Used <- re_formula_used
    empty_diag$exclusion_reason <- paste("loo() failed:", loo_firm$message)
    result <- list(diag = empty_diag, loo = NULL, firms = firm_levels)
    saveRDS(result, cache_path)
    return(result)
  }

  k_values <- loo_firm$diagnostics$pareto_k
  reliability <- reliability_from_k(k_values)
  included <- !fallback_used && reliability != "FAILED"
  exclusion_reason <- if (fallback_used) {
    "Primary population-level log_lik failed; conditional fallback computed only for diagnostics."
  } else if (!included) {
    "LOFO reliability failed."
  } else {
    NA_character_
  }

  diag <- empty_diag
  diag$LogLik_Method <- loglik_method
  diag$Re_Formula_Used <- re_formula_used
  diag$elpd_lofo <- loo_firm$estimates["elpd_loo", "Estimate"]
  diag$se_elpd_lofo <- loo_firm$estimates["elpd_loo", "SE"]
  diag$p_lofo <- loo_firm$estimates["p_loo", "Estimate"]
  diag$looic_lofo <- loo_firm$estimates["looic", "Estimate"]
  diag$pareto_k_gt_0_7 <- sum(k_values > 0.7, na.rm = TRUE)
  diag$pareto_k_gt_1_0 <- sum(k_values > 1.0, na.rm = TRUE)
  diag$max_pareto_k <- max(k_values, na.rm = TRUE)
  diag$ParetoK_Status <- if (diag$pareto_k_gt_1_0 > 0) {
    "POOR_GT_1_0"
  } else if (diag$pareto_k_gt_0_7 > 0) {
    "CAUTION_GT_0_7"
  } else {
    "OK"
  }
  diag$Model_Level_Caution <- if (diag$pareto_k_gt_0_7 > 0) {
    "Exact grouped K-fold is recommended for affected models."
  } else {
    "No Pareto-k caution."
  }
  diag$reliability_flag <- reliability
  diag$included_in_stack <- included
  diag$exclusion_reason <- exclusion_reason

  result <- list(diag = diag, loo = loo_firm, firms = firm_levels)
  saveRDS(result, cache_path)
  result
}

build_model_rows <- function(target_space, model_ids) {
  diag_df %>%
    filter(stacking_eligible == TRUE,
           Target_Space == target_space,
           Model_ID %in% model_ids,
           Sample_Group == "main_common",
           Main_Stack_Inclusion == TRUE) %>%
    left_join(
      formulas_df %>% select(Model_ID, Model_Name, Target_Space, Heterogeneity_Variant, Target_Sample) %>% distinct(),
      by = c("Model_ID", "Model_Name", "Target_Space", "Heterogeneity_Variant")
    ) %>%
    arrange(Model_ID, Heterogeneity_Variant)
}

run_space_lofo <- function(target_space, model_ids, sample_df) {
  rows <- build_model_rows(target_space, model_ids)
  if (nrow(rows) == 0) stop("[BLOCKER] No eligible winsorized models found for ", target_space)

  results <- vector("list", nrow(rows))
  for (i in seq_len(nrow(rows))) {
    results[[i]] <- load_or_compute_lofo(rows[i, ], sample_df, target_space)
  }

  diag_space <- bind_rows(lapply(results, `[[`, "diag"))
  included_results <- results[vapply(results, function(x) isTRUE(x$diag$included_in_stack), logical(1))]

  if (length(included_results) == 0) {
    stop("[BLOCKER] No models included in grouped PSIS-LOFO stack for ", target_space)
  }

  firm_reference <- included_results[[1]]$firms
  for (res in included_results) {
    if (!identical(res$firms, firm_reference)) {
      stop("[BLOCKER] Firm set/order differs across included models in ", target_space, " stack.")
    }
  }

  loo_list <- list()
  meta <- list()
  for (res in included_results) {
    d <- res$diag
    key <- model_key_sampled(d$Model_ID, d$Target_Space, d$Sample_Group, d$Heterogeneity_Variant, "_winsor")
    loo_list[[key]] <- res$loo
    meta[[key]] <- d
  }

  weights <- as.numeric(loo::loo_model_weights(loo_list, method = "stacking"))
  if (abs(sum(weights) - 1) > 1e-5) {
    stop("[BLOCKER] Grouped PSIS-LOFO weights do not sum to 1 for ", target_space)
  }

  weights_df <- data.frame(
    Target_Space = target_space,
    Model_ID = vapply(names(loo_list), function(k) meta[[k]]$Model_ID, character(1)),
    Model_Name = vapply(names(loo_list), function(k) meta[[k]]$Model_Name, character(1)),
    Sample_Group = "main_common",
    M10_Included = FALSE,
    M08_Included = FALSE,
    Heterogeneity_Variant = vapply(names(loo_list), function(k) meta[[k]]$Heterogeneity_Variant, character(1)),
    Weight_LOFO = weights,
    LogLik_Method = vapply(names(loo_list), function(k) meta[[k]]$LogLik_Method, character(1)),
    ParetoK_Status = vapply(names(loo_list), function(k) meta[[k]]$ParetoK_Status, character(1)),
    Model_Level_Caution = vapply(names(loo_list), function(k) meta[[k]]$Model_Level_Caution, character(1)),
    reliability_flag = vapply(names(loo_list), function(k) meta[[k]]$reliability_flag, character(1)),
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(Weight_LOFO)) %>%
    mutate(Rank_LOFO = row_number()) %>%
    select(Target_Space, Sample_Group, M10_Included, M08_Included, Model_ID, Model_Name, Heterogeneity_Variant, Weight_LOFO, Rank_LOFO,
           LogLik_Method, ParetoK_Status, Model_Level_Caution, reliability_flag,
           Prior_Set_ID, Likelihood_Family, Model_Structure, Output_Root)

  list(diagnostics = diag_space, weights = weights_df, firms = firm_reference)
}

message("\n========= GROUPED PSIS-LOFO: EX-POST =========")
lofo_ep <- run_space_lofo("ex_post", ex_post_ids, sample_ep)

message("\n========= GROUPED PSIS-LOFO: NO-LOOK-AHEAD FEATURE SPACE =========")
lofo_rt <- run_space_lofo("real_time", no_lookahead_ids, sample_rt)

diagnostics_df <- bind_rows(lofo_ep$diagnostics, lofo_rt$diagnostics)
write.csv(diagnostics_df, file.path(lofo_root, "tables", "table_winsor_lofo_model_diagnostics.csv"), row.names = FALSE)

write.csv(lofo_ep$weights, file.path(lofo_root, "tables", "table_winsor_lofo_weights_ex_post.csv"), row.names = FALSE)
write.csv(lofo_rt$weights, file.path(lofo_root, "tables", "table_winsor_lofo_weights_no_lookahead.csv"), row.names = FALSE)

# Secondary M08 robustness only, if eligible model files exist.
m08_rows <- diag_df %>%
  filter(stacking_eligible == TRUE, Model_ID == "M08") %>%
  left_join(
    formulas_df %>% select(Model_ID, Model_Name, Target_Space, Heterogeneity_Variant, Target_Sample) %>% distinct(),
    by = c("Model_ID", "Model_Name", "Target_Space", "Heterogeneity_Variant")
  )

m08_diag <- data.frame()
if (nrow(m08_rows) > 0) {
  for (i in seq_len(nrow(m08_rows))) {
    sample_path <- table_path(m08_rows$Target_Sample[i], prefer_input = TRUE)
    if (!file.exists(sample_path)) next
    m08_sample <- read.csv(sample_path, stringsAsFactors = FALSE)
    res <- load_or_compute_lofo(m08_rows[i, ], m08_sample, m08_rows$Target_Space[i])
    m08_diag <- bind_rows(m08_diag, res$diag)
  }
}
write.csv(m08_diag, file.path(lofo_root, "tables", "table_winsor_lofo_m08_secondary.csv"), row.names = FALSE)

prepare_rowloo <- function(df) {
  df %>%
    mutate(
      Rank_RowLOO_Winsor = rank(-Weight, ties.method = "first")
    ) %>%
    select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant,
           Weight_RowLOO_Winsor = Weight, Rank_RowLOO_Winsor)
}

rowloo_all <- bind_rows(prepare_rowloo(rowloo_ep), prepare_rowloo(rowloo_rt))
lofo_all <- bind_rows(lofo_ep$weights, lofo_rt$weights)

stability_df <- full_join(
  rowloo_all,
  lofo_all %>% select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant,
                      Weight_LOFO_Winsor = Weight_LOFO, Rank_LOFO_Winsor = Rank_LOFO,
                      ParetoK_Status, Model_Level_Caution, reliability_flag),
  by = c("Target_Space", "Model_ID", "Model_Name", "Heterogeneity_Variant")
) %>%
  mutate(
    Weight_RowLOO_Winsor = ifelse(is.na(Weight_RowLOO_Winsor), 0, Weight_RowLOO_Winsor),
    Weight_LOFO_Winsor = ifelse(is.na(Weight_LOFO_Winsor), 0, Weight_LOFO_Winsor),
    Weight_Difference = Weight_LOFO_Winsor - Weight_RowLOO_Winsor,
    Abs_Weight_Difference = abs(Weight_Difference),
    Rank_Change = Rank_LOFO_Winsor - Rank_RowLOO_Winsor,
    Family = vapply(Model_ID, family_label, character(1)),
    ParetoK_Status = ifelse(is.na(ParetoK_Status), "NOT_AVAILABLE", ParetoK_Status),
    Model_Level_Caution = ifelse(is.na(Model_Level_Caution), "No LOFO model-level diagnostic available.", Model_Level_Caution)
  )

family_weights <- stability_df %>%
  group_by(Target_Space, Family) %>%
  summarise(
    Weight_RowLOO_Winsor = sum(Weight_RowLOO_Winsor, na.rm = TRUE),
    Weight_LOFO_Winsor = sum(Weight_LOFO_Winsor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Target_Space) %>%
  mutate(
    Weight_Difference = Weight_LOFO_Winsor - Weight_RowLOO_Winsor,
    Family_Rank_RowLOO = rank(-Weight_RowLOO_Winsor, ties.method = "first"),
    Family_Rank_LOFO = rank(-Weight_LOFO_Winsor, ties.method = "first"),
    Interpretation = case_when(
      Family == "Jones-family" & Weight_LOFO_Winsor >= 0.50 ~ "Jones-family becomes dominant under grouped PSIS-LOFO.",
      Family == "Jones-family" & Weight_LOFO_Winsor > Weight_RowLOO_Winsor ~ "Jones-family gains weight but remains below dominance threshold unless noted.",
      Weight_LOFO_Winsor >= Weight_RowLOO_Winsor ~ "Family support increases under firm-level holdout.",
      TRUE ~ "Family support attenuates under firm-level holdout."
    ),
    Family_Level_Conclusion = case_when(
      Family == "Jones-family" & Weight_LOFO_Winsor >= 0.50 ~ "Jones-family dominates under grouped PSIS-LOFO.",
      Family_Rank_LOFO == 1 ~ "Dominant grouped PSIS-LOFO family.",
      TRUE ~ "Secondary grouped PSIS-LOFO family."
    )
  ) %>%
  ungroup() %>%
  arrange(Target_Space, Family_Rank_LOFO)

dominant_family <- function(space) {
  family_weights %>%
    filter(Target_Space == space) %>%
    arrange(desc(Weight_LOFO_Winsor)) %>%
    slice(1)
}

dom_ep <- dominant_family("ex_post")
dom_rt <- dominant_family("real_time")
jones_ep <- family_weights %>% filter(Target_Space == "ex_post", Family == "Jones-family")
jones_rt <- family_weights %>% filter(Target_Space == "real_time", Family == "Jones-family")

top_model_reliability_bad <- function(weights_df) {
  top <- weights_df %>% arrange(desc(Weight_LOFO)) %>% slice_head(n = min(3, nrow(weights_df)))
  all(top$reliability_flag == "LOW_RELIABILITY")
}

firm_re_collapse <- stability_df %>%
  filter(grepl("Firm RE", Heterogeneity_Variant)) %>%
  group_by(Target_Space) %>%
  summarise(
    Firm_RE_RowLOO = sum(Weight_RowLOO_Winsor, na.rm = TRUE),
    Firm_RE_LOFO = sum(Weight_LOFO_Winsor, na.rm = TRUE),
    Firm_RE_Difference = Firm_RE_LOFO - Firm_RE_RowLOO,
    .groups = "drop"
  )

all_main_same_firms <- length(lofo_ep$firms) == length(unique(sample_ep$company)) &&
  length(lofo_rt$firms) == length(unique(sample_rt$company))

diag_counts <- diagnostics_df %>% count(reliability_flag, name = "N_Models")
failed_or_low_share <- mean(diagnostics_df$reliability_flag %in% c("FAILED", "LOW_RELIABILITY"), na.rm = TRUE)
inconclusive <- failed_or_low_share > 0.50 ||
  top_model_reliability_bad(lofo_ep$weights) ||
  top_model_reliability_bad(lofo_rt$weights)

ex_survives <- dom_ep$Family %in% c("Cash-flow/McNichols-family", "Ball-Shivakumar/asymmetry") &&
  nrow(jones_ep) == 1 && jones_ep$Weight_LOFO_Winsor < 0.50
rt_survives <- dom_rt$Family %in% c("Cash-flow/McNichols-family", "Ball-Shivakumar/asymmetry", "No-lookahead/real-time") &&
  nrow(jones_rt) == 1 && jones_rt$Weight_LOFO_Winsor < 0.50
jones_dominant_both <- nrow(jones_ep) == 1 && nrow(jones_rt) == 1 &&
  jones_ep$Weight_LOFO_Winsor >= 0.50 && jones_rt$Weight_LOFO_Winsor >= 0.50

final_decision <- if (!all_main_same_firms || inconclusive) {
  "INCONCLUSIVE_DUE_TO_LOFO_DIAGNOSTICS"
} else if (jones_dominant_both || (!ex_survives && !rt_survives)) {
  "DOES_NOT_SURVIVE_WINSOR_AND_LOFO"
} else if (ex_survives && rt_survives) {
  "SURVIVES_WINSOR_AND_LOFO"
} else {
  "PARTIALLY_SURVIVES_WINSOR_AND_LOFO"
}

stability_flag <- if (final_decision == "SURVIVES_WINSOR_AND_LOFO") {
  "Stable"
} else if (final_decision == "PARTIALLY_SURVIVES_WINSOR_AND_LOFO") {
  "Partially Stable"
} else {
  "Unstable or inconclusive"
}

stability_df <- stability_df %>%
  mutate(
    Headline_Stability_Flag = stability_flag,
    Interpretation = case_when(
      Family == "Jones-family" & Weight_LOFO_Winsor >= 0.50 ~ "Jones-family dominates after grouped PSIS-LOFO.",
      Family == "Jones-family" & Weight_LOFO_Winsor > Weight_RowLOO_Winsor ~ "Jones-family gains weight under grouped PSIS-LOFO but dominance depends on family total.",
      grepl("Firm RE", Heterogeneity_Variant) & Weight_LOFO_Winsor < Weight_RowLOO_Winsor ~ "Firm random-intercept support attenuates under new-firm evaluation.",
      TRUE ~ "Compare row-level winsor LOO and grouped PSIS-LOFO support."
    ),
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root
  ) %>%
  arrange(Target_Space, Rank_LOFO_Winsor)

family_weights <- family_weights %>%
  mutate(
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root
  )

write.csv(stability_df, file.path(lofo_root, "tables", "table_winsor_weight_stability_rowloo_vs_lofo.csv"), row.names = FALSE)
write.csv(family_weights, file.path(lofo_root, "tables", "table_winsor_lofo_family_weights.csv"), row.names = FALSE)

decision_table <- data.frame(
  Criterion = c(
    "Did all main-stack models share the same firm set?",
    "Was LOFO computed at firm level?",
    "Was re_formula = NA used for primary Firm-RE evaluation?",
    "Did ex-post headline survive LOFO?",
    "Did no-look-ahead headline survive LOFO?",
    "Did Jones-family become dominant?",
    "Did Firm-RE weights collapse under new-firm evaluation?",
    "Were Pareto-k diagnostics acceptable?",
    "Overall Priority 2 decision."
  ),
  Evidence = c(
    sprintf("Ex-post firms=%d; no-look-ahead firms=%d; aligned=%s.",
            length(lofo_ep$firms), length(lofo_rt$firms), all_main_same_firms),
    "Observation-level log-likelihood draws were summed by company before loo::loo().",
    "Primary log-likelihood call uses brms::log_lik(fit, re_formula = NA); conditional fallback is excluded from main stacks.",
    sprintf("Ex-post dominant LOFO family: %s (%.4f).", dom_ep$Family, dom_ep$Weight_LOFO_Winsor),
    sprintf("No-look-ahead dominant LOFO family: %s (%.4f).", dom_rt$Family, dom_rt$Weight_LOFO_Winsor),
    sprintf("Jones-family LOFO weight: ex-post %.4f; no-look-ahead %.4f.",
            ifelse(nrow(jones_ep) == 1, jones_ep$Weight_LOFO_Winsor, NA_real_),
            ifelse(nrow(jones_rt) == 1, jones_rt$Weight_LOFO_Winsor, NA_real_)),
    paste(apply(firm_re_collapse, 1, function(x) {
      sprintf("%s Firm-RE rowLOO=%s LOFO=%s diff=%s", x[["Target_Space"]], x[["Firm_RE_RowLOO"]], x[["Firm_RE_LOFO"]], x[["Firm_RE_Difference"]])
    }), collapse = "; "),
    paste(apply(diag_counts, 1, function(x) paste(x[["reliability_flag"]], x[["N_Models"]], sep = "=")), collapse = "; "),
    final_decision
  ),
  Decision = c(
    ifelse(all_main_same_firms, "Yes", "No"),
    "Yes",
    "Yes",
    ifelse(ex_survives, "Yes", "No or partial"),
    ifelse(rt_survives, "Yes", "No or partial"),
    ifelse(jones_ep$Weight_LOFO_Winsor >= 0.50 || jones_rt$Weight_LOFO_Winsor >= 0.50, "Yes", "No"),
    ifelse(any(firm_re_collapse$Firm_RE_Difference < -0.30), "Material attenuation", "No full collapse"),
    ifelse(inconclusive, "Problematic", "Usable with reported cautions"),
    final_decision
  ),
  Severity = c("High", "High", "High", "High", "High", "High", "Medium", "High", "High"),
  Manuscript_Action = c(
    "Report firm-set alignment.",
    "Describe as grouped PSIS-LOFO, not exact LOFO.",
    "State that Firm-RE models are evaluated as new-firm predictions.",
    "Report family-level LOFO weights and any attenuation.",
    "Report no-look-ahead feature-space LOFO weights.",
    "Use as key stability criterion.",
    "Discuss change in Firm-RE support if material.",
    "Report Pareto-k limitations next to LOFO weights.",
    "Use conservative Priority 2 wording in manuscript and response letter."
  ),
  stringsAsFactors = FALSE
)
decision_table$Family_Level_Conclusion <- final_decision
decision_table$Model_Level_Caution <- if (any(diagnostics_df$pareto_k_gt_0_7 > 0, na.rm = TRUE)) {
  "Exact grouped K-fold is recommended for affected models."
} else {
  "No model-level Pareto-k caution."
}
decision_table$ParetoK_Status <- if (any(diagnostics_df$pareto_k_gt_1_0 > 0, na.rm = TRUE)) {
  "POOR_GT_1_0"
} else if (any(diagnostics_df$pareto_k_gt_0_7 > 0, na.rm = TRUE)) {
  "CAUTION_GT_0_7"
} else {
  "OK"
}
decision_table$Prior_Set_ID <- prior_set_id
decision_table$Likelihood_Family <- likelihood_family
decision_table$Model_Structure <- model_structure
decision_table$Output_Root <- output_root
write.csv(decision_table, file.path(lofo_root, "tables", "table_reviewer_priority2_lofo_decision.csv"), row.names = FALSE)

recommended_paragraph <- switch(
  final_decision,
  SURVIVES_WINSOR_AND_LOFO = "To address the panel dependence concern, we re-estimated stacking weights using firm-level grouped PSIS leave-one-firm-out validation on the winsorized sample. The dominant model families remain materially unchanged, with cash-flow mapping and asymmetric accrual models receiving the largest support. This suggests that the main model-uncertainty conclusion is not solely an artifact of row-level LOO in panel data.",
  PARTIALLY_SURVIVES_WINSOR_AND_LOFO = "Firm-level grouped PSIS leave-one-firm-out validation attenuates some of the row-level LOO results, especially for firm random-intercept variants. However, the dominant support does not revert to the traditional Jones-family specifications. We therefore interpret the evidence as supportive but not definitive: model uncertainty remains material, although the exact stacking weights are sensitive to the cross-validation unit.",
  DOES_NOT_SURVIVE_WINSOR_AND_LOFO = "Firm-level grouped PSIS leave-one-firm-out validation materially changes the stacking weights, indicating that the row-level LOO results overstate the predictive advantage of the original leading models. Consequently, the paper reframes the stacking evidence as exploratory and reports the row-level results as non-primary.",
  INCONCLUSIVE_DUE_TO_LOFO_DIAGNOSTICS = "Grouped PSIS leave-one-firm-out diagnostics indicate that firm-level holdout is unstable for several high-weight models. We therefore report the firm-level analysis as a diagnostic stress test and avoid making strong claims based solely on stacking weights."
)

reviewer_notes <- c(
  "Reviewer Priority 2 grouped PSIS-LOFO response notes",
  "",
  "Row-level LOO can be optimistic in panel data because leaving out one firm-year still leaves adjacent years of the same firm in the training set.",
  "Grouped PSIS-LOFO aggregates log-likelihood contributions by firm, then evaluates predictive fit over firm-level held-out units.",
  "This is approximate leave-one-firm-out validation, not exact LOFO, because the models are not refit after dropping each firm.",
  "For Firm-RE models, the primary log-likelihood uses re_formula = NA so held-out firms are evaluated through population-level predictions rather than their fitted random intercepts.",
  "Conditional grouped LOFO, if triggered as a fallback, is labeled conditional_grouped_LOFO_not_primary and excluded from main stacks.",
  sprintf("Ex-post dominant grouped PSIS-LOFO family: %s (weight %.4f).", dom_ep$Family, dom_ep$Weight_LOFO_Winsor),
  sprintf("No-look-ahead feature-space dominant grouped PSIS-LOFO family: %s (weight %.4f).", dom_rt$Family, dom_rt$Weight_LOFO_Winsor),
  sprintf("Final Priority 2 decision: %s.", final_decision),
  "Pareto-k limitations are reported in table_winsor_lofo_model_diagnostics.csv and should be discussed wherever LOFO weights are cited.",
  if (any(diagnostics_df$pareto_k_gt_0_7 > 0, na.rm = TRUE)) "Exact grouped K-fold is recommended for affected models." else "No grouped PSIS-LOFO Pareto-k values exceeded 0.7.",
  "Recommended manuscript wording:",
  recommended_paragraph
)
writeLines(reviewer_notes, file.path(lofo_root, "logs", "reviewer_priority2_lofo_response_notes.txt"))

technical_log <- c(
  "ro01 grouped PSIS-LOFO technical log",
  paste("Run date/time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "Input files used:",
  paste(" -", names(input_paths), input_paths),
  "",
  sprintf("Ex-post observations=%d, firms=%d.", nrow(sample_ep), length(unique(sample_ep$company))),
  sprintf("No-look-ahead observations=%d, firms=%d.", nrow(sample_rt), length(unique(sample_rt$company))),
  "",
  "Method details:",
  " - Primary method: grouped PSIS-LOFO / approximate leave-one-firm-out stacking.",
  " - log_lik called with re_formula = NA for population-level new-firm prediction.",
  " - Observation-level log-likelihood draws summed by company before loo::loo().",
  " - Model weights computed with loo_model_weights(method = 'stacking') separately by target space.",
  " - M08 excluded from main stacks and written to secondary diagnostics only.",
  " - M10 excluded from main grouped PSIS-LOFO because operating_cycle is secondary robustness only.",
  "",
  "Model files used:",
  paste(" -", diagnostics_df$Model_File),
  "",
  "Reliability counts:",
  paste(apply(diag_counts, 1, function(x) paste(" -", x[["reliability_flag"]], x[["N_Models"]])), collapse = "\n"),
  "",
  if (any(diagnostics_df$pareto_k_gt_0_7 > 0, na.rm = TRUE)) "Exact grouped K-fold is recommended for affected models." else "No exact grouped K-fold recommendation triggered by Pareto-k.",
  "",
  paste("Final decision:", final_decision)
)
writeLines(technical_log, file.path(lofo_root, "logs", "phase4d_lofo_stacking_winsor_notes.txt"))

cat("\n===== REVIEWER PRIORITY 2 GROUPED PSIS-LOFO SUMMARY =====\n")
cat(sprintf("1. Ex-post N observations: %d; N firms: %d\n", nrow(sample_ep), length(unique(sample_ep$company))))
cat(sprintf("2. No-look-ahead N observations: %d; N firms: %d\n", nrow(sample_rt), length(unique(sample_rt$company))))
cat("3. Top 5 ex-post LOFO weights:\n")
print(head(lofo_ep$weights, 5), row.names = FALSE)
cat("4. Top 5 no-look-ahead LOFO weights:\n")
print(head(lofo_rt$weights, 5), row.names = FALSE)
cat("5. Family-level LOFO weights:\n")
print(as.data.frame(family_weights %>% select(Target_Space, Family, Weight_LOFO_Winsor, Family_Rank_LOFO)), row.names = FALSE)
cat("6. Largest absolute changes from row-level winsor LOO to grouped PSIS-LOFO:\n")
print(head(stability_df %>% arrange(desc(Abs_Weight_Difference)), 10), row.names = FALSE)
cat("7. Pareto-k diagnostic summary:\n")
print(as.data.frame(diag_counts), row.names = FALSE)
cat("8. Final Priority 2 decision: ", final_decision, "\n", sep = "")

cat("\n[SUCCESS] ro01 grouped PSIS-LOFO stacking completed.\n")
phase_end("ro01", "Grouped PSIS-LOFO robustness")
