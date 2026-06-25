# -----------------------------------------------------------------------------
# Script: ma14_construct_exact_kfold_DA.R
# Purpose: Construct exact-K-fold-weighted winsorized DA from pinned completed
#          grouped and row exact K-fold runs. This is separate from ma10,
#          which remains the secondary PSIS/LOO DA constructor.
# -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args || "-h" %in% args) {
  cat("Usage: Rscript scripts/ma14_construct_exact_kfold_DA.R\n")
  cat("Optional env vars:\n")
  cat("  ACCRUAL_GROUPED_KFOLD_RUN_ROOT: completed grouped exact K-fold run root\n")
  cat("  ACCRUAL_ROW_KFOLD_RUN_ROOT: completed row exact K-fold run root\n")
  cat("  ACCRUAL_STACKING_MIXTURE_DRAWS: mixture draw count, default from helpers\n")
  quit(save = "no", status = 0)
}

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("ma14", "Construct exact-KFold primary DA")
ensure_analysis_dirs()
validate_final_analysis_config("ma14 exact-KFold primary DA", final_mode = TRUE)

script_name <- "scripts/ma14_construct_exact_kfold_DA.R"
script_version <- "2026-06-19-v2-provenance-inclusion-gate"
script_start_time <- Sys.time()
mixture_draws <- stacking_mixture_draws

tables_dir <- file.path(output_root, "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

read_root_pin <- function(env_name, pin_path, label) {
  env_root <- trimws(env_value(env_name, ""))
  if (nzchar(env_root)) return(env_root)
  if (!file.exists(pin_path)) {
    stop("[BLOCKER] Missing completed-run pin for ", label, ": ", pin_path,
         ". Run the corresponding exact K-fold script to completion or set ", env_name, ".")
  }
  root <- trimws(readLines(pin_path, warn = FALSE)[1])
  if (!nzchar(root)) stop("[BLOCKER] Empty completed-run pin for ", label, ": ", pin_path)
  root
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) stop("[BLOCKER] Missing required input: ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

file_size_or_na <- function(path) if (file.exists(path)) as.numeric(file.info(path)$size) else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}
git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}
same_normalized_path <- function(a, b) {
  identical(normalizePath(a, winslash = "/", mustWork = FALSE),
            normalizePath(b, winslash = "/", mustWork = FALSE))
}
manifest_seed <- function(manifest) {
  if ("Seed" %in% names(manifest)) return(as.integer(manifest$Seed[1]))
  if ("Canonical_Seed" %in% names(manifest)) return(as.integer(manifest$Canonical_Seed[1]))
  if ("Effective_Seed" %in% names(manifest)) return(as.integer(manifest$Effective_Seed[1]))
  NA_integer_
}

grouped_pin_path <- file.path(output_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt")
row_pin_path <- file.path(output_root, "row_exact_kfold", "LATEST_COMPLETED_RUN.txt")
grouped_root <- read_root_pin("ACCRUAL_GROUPED_KFOLD_RUN_ROOT", grouped_pin_path, "grouped exact K-fold")
row_root <- read_root_pin("ACCRUAL_ROW_KFOLD_RUN_ROOT", row_pin_path, "row exact K-fold")

validate_grouped_run <- function(root) {
  manifest_path <- file.path(root, "logs", "run_config_manifest.csv")
  manifest <- safe_read_csv(manifest_path)
  status <- if ("Status" %in% names(manifest)) manifest$Status[1] else NA_character_
  run_mode <- if ("Run_Mode" %in% names(manifest)) manifest$Run_Mode[1] else NA_character_
  preflight <- if ("Preflight_Only" %in% names(manifest)) isTRUE(as.logical(manifest$Preflight_Only[1])) else FALSE
  partial <- if ("Partial_Run" %in% names(manifest)) isTRUE(as.logical(manifest$Partial_Run[1])) else FALSE
  if (!"Completed_Run_Pin_Eligible" %in% names(manifest)) {
    stop("[BLOCKER] Missing Completed_Run_Pin_Eligible in grouped exact K-fold manifest: ", manifest_path)
  }
  pin_ok <- isTRUE(as.logical(manifest$Completed_Run_Pin_Eligible[1]))
  K_manifest <- if ("K" %in% names(manifest)) as.integer(manifest$K[1]) else NA_integer_
  seed_manifest <- manifest_seed(manifest)
  if ("Kfold_Run_Root" %in% names(manifest) && !same_normalized_path(manifest$Kfold_Run_Root[1], root)) {
    stop("[BLOCKER] Grouped exact K-fold manifest root disagrees with selected root: ", root)
  }
  if (!identical(status, "COMPLETED") || !identical(run_mode, "FULL_MODE") || preflight || partial ||
      !pin_ok || !identical(K_manifest, 5L) || !identical(seed_manifest, accrual_seed("grouped_kfold"))) {
    stop("[BLOCKER] Grouped exact K-fold run is not a completed primary-eligible run: ", root)
  }
  manifest_path
}

validate_row_run <- function(root) {
  manifest_path <- file.path(root, "logs", "row_exact_kfold_run_manifest.csv")
  manifest <- safe_read_csv(manifest_path)
  status <- if ("Status" %in% names(manifest)) manifest$Status[1] else NA_character_
  run_mode <- if ("Run_Mode" %in% names(manifest)) manifest$Run_Mode[1] else NA_character_
  preflight <- if ("Preflight_Only" %in% names(manifest)) isTRUE(as.logical(manifest$Preflight_Only[1])) else FALSE
  primary_allowed <- if ("Primary_Inference_Allowed" %in% names(manifest)) isTRUE(as.logical(manifest$Primary_Inference_Allowed[1])) else FALSE
  if (!"Completed_Run_Pin_Eligible" %in% names(manifest)) {
    stop("[BLOCKER] Missing Completed_Run_Pin_Eligible in row exact K-fold manifest: ", manifest_path)
  }
  pin_ok <- isTRUE(as.logical(manifest$Completed_Run_Pin_Eligible[1]))
  K_manifest <- if ("K" %in% names(manifest)) as.integer(manifest$K[1]) else NA_integer_
  seed_manifest <- manifest_seed(manifest)
  if ("Row_KFold_Root" %in% names(manifest) && !same_normalized_path(manifest$Row_KFold_Root[1], root)) {
    stop("[BLOCKER] Row exact K-fold manifest root disagrees with selected root: ", root)
  }
  if (!identical(status, "COMPLETED") || !identical(run_mode, "FULL_MODE") || preflight ||
      !primary_allowed || !pin_ok || !identical(K_manifest, 5L) || !identical(seed_manifest, accrual_seed("row_kfold"))) {
    stop("[BLOCKER] Row exact K-fold run is not a completed primary-eligible run: ", root)
  }
  manifest_path
}

grouped_manifest_path <- validate_grouped_run(grouped_root)
row_manifest_path <- validate_row_run(row_root)

ep_sample_path <- file.path(input_winsor_root, "tables", "final_common_ex_post_sample_winsor.csv")
rt_sample_path <- file.path(input_winsor_root, "tables", "final_common_realtime_sample_winsor.csv")
df_ep <- safe_read_csv(ep_sample_path)
df_rt <- safe_read_csv(rt_sample_path)

mcmc_gate_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_gate_winsor.csv")
mcmc_gate <- safe_read_csv(mcmc_gate_path)
required_mcmc_cols <- c("model_id", "model_name", "Target_Space", "Heterogeneity_Variant", "diagnostics_status")
if (!all(required_mcmc_cols %in% names(mcmc_gate))) {
  stop("[BLOCKER] MCMC diagnostics gate lacks required columns: ", mcmc_gate_path)
}
psis_status_path <- file.path(output_root, "tables", "table_loo_comparison_winsor_corrected.csv")
psis_status <- if (file.exists(psis_status_path)) safe_read_csv(psis_status_path) else data.frame()
if (nrow(psis_status) > 0 && !"PSIS_Status" %in% names(psis_status)) {
  psis_status$PSIS_Status <- if ("moment_match_note" %in% names(psis_status)) {
    psis_status$moment_match_note
  } else if ("Moment_Match_Note" %in% names(psis_status)) {
    psis_status$Moment_Match_Note
  } else {
    NA_character_
  }
}
inclusion_gate_rows <- list()

apply_primary_inclusion_gate <- function(active, source, validation_target) {
  active$Exact_KFold_Reliability_Status <- if ("reliability_flag" %in% names(active)) {
    as.character(active$reliability_flag)
  } else {
    "OK"
  }
  gate <- active %>%
    left_join(
      mcmc_gate %>%
        transmute(
          Model_ID = model_id,
          Target_Space,
          Heterogeneity_Variant,
          Full_Sample_MCMC_Status = diagnostics_status
        ),
      by = c("Model_ID", "Target_Space", "Heterogeneity_Variant")
    )
  if (nrow(psis_status) > 0 && all(c("Model_ID", "Target_Space", "Heterogeneity_Variant") %in% names(psis_status))) {
    gate <- gate %>%
      left_join(
        psis_status %>%
          transmute(Model_ID, Target_Space, Heterogeneity_Variant, PSIS_Status = as.character(PSIS_Status)) %>%
          distinct(),
        by = c("Model_ID", "Target_Space", "Heterogeneity_Variant")
      )
  } else {
    gate$PSIS_Status <- NA_character_
  }
  gate <- gate %>%
    mutate(
      Full_Sample_MCMC_Status = ifelse(is.na(Full_Sample_MCMC_Status), "MISSING", Full_Sample_MCMC_Status),
      Exact_KFold_Reliability_Status = ifelse(is.na(Exact_KFold_Reliability_Status), "MISSING", Exact_KFold_Reliability_Status),
      Primary_Inclusion_Decision = case_when(
        Full_Sample_MCMC_Status %in% c("PASS", "OK") &
          Exact_KFold_Reliability_Status %in% c("OK", "PASS", "CAUTION") ~ "INCLUDE_PRIMARY",
        Full_Sample_MCMC_Status %in% c("REVIEW", "CAUTION", "PSIS_REVIEW_REQUIRED") &
          Exact_KFold_Reliability_Status %in% c("OK", "PASS", "CAUTION") ~ "MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS",
        Full_Sample_MCMC_Status %in% c("FAIL", "LOW_RELIABILITY") ~ "EXCLUDE_FULL_SAMPLE_MCMC_FAIL",
        TRUE ~ "EXCLUDE_RELIABILITY_NOT_PRIMARY"
      ),
      Inclusion_Rationale = case_when(
        Primary_Inclusion_Decision == "INCLUDE_PRIMARY" ~ "Full-sample MCMC diagnostics pass and exact-KFold reliability is acceptable.",
        Primary_Inclusion_Decision == "MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS" ~ "Full-sample MCMC status is REVIEW/CAUTION, but exact refit reliability is acceptable; included only with explicit flag.",
        Primary_Inclusion_Decision == "EXCLUDE_FULL_SAMPLE_MCMC_FAIL" ~ "Full-sample MCMC diagnostics fail or are low reliability.",
        TRUE ~ "Model is not primary-eligible under combined MCMC and exact-KFold reliability gates."
      ),
      Primary_Inference_Allowed = Primary_Inclusion_Decision %in% c("INCLUDE_PRIMARY", "MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS"),
      DA_Source = source,
      Validation_Target = validation_target,
      Exact_KFold_Weight = Weight
    )
  inclusion_gate_rows[[length(inclusion_gate_rows) + 1]] <<- gate %>%
    transmute(
      Target_Space,
      Model_ID,
      Model_Name,
      Heterogeneity_Variant,
      DA_Source,
      Exact_KFold_Weight,
      Exact_KFold_Reliability_Status,
      Full_Sample_MCMC_Status,
      PSIS_Status,
      Primary_Inclusion_Decision,
      Inclusion_Rationale,
      Primary_Inference_Allowed
    )
  kept <- gate %>% filter(Primary_Inference_Allowed)
  if (nrow(kept) == 0) {
    stop("[BLOCKER] No models remain after primary inclusion gate for ", source, " / ", validation_target)
  }
  kept$Weight <- kept$Weight / sum(kept$Weight)
  kept
}

clean_weights <- function(weights_df, source, target_space) {
  if (identical(source, "exact_grouped_kfold")) {
    out <- weights_df %>%
      transmute(
        Target_Space = Target_Space,
        Sample_Group = if ("Sample_Group" %in% names(weights_df)) Sample_Group else "main_common",
        Model_ID = Model_ID,
        Model_Name = Model_Name,
        Heterogeneity_Variant = Heterogeneity_Variant,
        Weight = Weight_KFold,
        reliability_flag = if ("reliability_flag" %in% names(weights_df)) reliability_flag else "OK"
      )
  } else {
    out <- weights_df %>%
      transmute(
        Target_Space = target_space,
        Sample_Group = if ("sample_group" %in% names(weights_df)) sample_group else "main_common",
        Model_ID = model_id,
        Model_Name = model_name,
        Heterogeneity_Variant = heterogeneity_variant,
        Weight = weight_row_exact_kfold,
        reliability_flag = if ("reliability_flag" %in% names(weights_df)) reliability_flag else "OK"
      )
  }
  out <- out %>%
    filter(Target_Space == target_space, Sample_Group == "main_common", is.finite(Weight), Weight > 1e-6) %>%
    arrange(desc(Weight))
  if (nrow(out) == 0) stop("[BLOCKER] No active ", source, " weights for ", target_space)
  weight_sum <- sum(out$Weight)
  if (!is.finite(weight_sum) || abs(weight_sum - 1) > 1e-4) {
    stop("[BLOCKER] ", source, " weights for ", target_space, " do not sum to 1. Sum=", weight_sum)
  }
  out$Weight <- out$Weight / weight_sum
  apply_primary_inclusion_gate(out, source, target_space)
}

draws_path_for <- function(row, target_space) {
  file.path(
    output_root,
    "draws",
    paste0("draws_", model_key_sampled(row$Model_ID, target_space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor"), ".rds")
  )
}

compute_exact_kfold_da <- function(df_sample, weights_df, source, target_space, validation_target) {
  active <- clean_weights(weights_df, source, target_space)
  active$Draw_File <- vapply(seq_len(nrow(active)), function(i) draws_path_for(active[i, ], target_space), character(1))
  N <- nrow(df_sample)
  mixture_rng_context <- paste0("exact_kfold_da_", source, "_", target_space)
  mixture_rng_offset <- match(target_space, c("ex_post", "real_time"), nomatch = 10L) +
    ifelse(identical(source, "exact_row_kfold"), 1000L, 0L)
  mixture_rng_meta <- accrual_rng_metadata_list(mixture_rng_context, offset = mixture_rng_offset)
  set_accrual_seed(
    mixture_rng_context,
    offset = mixture_rng_offset
  )
  sampled_model_indices <- sample(seq_len(nrow(active)), size = mixture_draws, replace = TRUE, prob = active$Weight)
  stacked_epred <- matrix(NA_real_, nrow = mixture_draws, ncol = N)
  stacked_predict <- matrix(NA_real_, nrow = mixture_draws, ncol = N)
  draw_counts <- integer(nrow(active))

  for (m in seq_len(nrow(active))) {
    row <- active[m, ]
    mix_rows <- which(sampled_model_indices == m)
    draws_path <- row$Draw_File
    if (!file.exists(draws_path)) stop("[BLOCKER] Missing full-sample posterior draw file: ", draws_path)
    draws <- readRDS(draws_path)
    if (is.null(draws$epred) || is.null(draws$predict)) {
      stop("[BLOCKER] Draw file lacks epred/predict matrices: ", draws_path)
    }
    if (ncol(draws$epred) != N || ncol(draws$predict) != N) {
      stop("[BLOCKER] Draw N mismatch for ", draws_path, ": expected ", N,
           ", epred=", ncol(draws$epred), ", predict=", ncol(draws$predict))
    }
    draw_counts[m] <- nrow(draws$epred)
    if (length(mix_rows) > 0) {
      selected_draws <- sample(seq_len(nrow(draws$epred)), size = length(mix_rows), replace = nrow(draws$epred) < length(mix_rows))
      stacked_epred[mix_rows, ] <- draws$epred[selected_draws, ]
      stacked_predict[mix_rows, ] <- draws$predict[selected_draws, ]
    }
    rm(draws)
    gc()
  }

  if (anyNA(stacked_epred) || anyNA(stacked_predict)) {
    stop("[BLOCKER] Exact-KFold DA mixture draw matrix contains NA after stacking.")
  }

  NDA_mean_stacked <- colMeans(stacked_epred)
  NDA_sd_epred_stacked <- apply(stacked_epred, 2, sd)
  NDA_sd_predict_stacked <- apply(stacked_predict, 2, sd)
  NDA_q025_stacked <- apply(stacked_predict, 2, quantile, probs = 0.025)
  NDA_q975_stacked <- apply(stacked_predict, 2, quantile, probs = 0.975)
  NDA_q010_stacked <- apply(stacked_predict, 2, quantile, probs = 0.010)
  NDA_q990_stacked <- apply(stacked_predict, 2, quantile, probs = 0.990)
  DA_raw_stacked <- df_sample$TA_scaled - NDA_mean_stacked
  DA_ppd_percentile <- colMeans(sweep(stacked_predict, 2, df_sample$TA_scaled, FUN = "<="), na.rm = TRUE)
  DA_ppd_tail_prob_two_sided <- 2 * pmin(DA_ppd_percentile, 1 - DA_ppd_percentile)

  result <- data.frame(
    DA_Source = source,
    Validation_Target = validation_target,
    target_space = target_space,
    company = df_sample$company,
    year = df_sample$year,
    industry = if ("industry" %in% names(df_sample)) df_sample$industry else NA_character_,
    TA_scaled = df_sample$TA_scaled,
    NDA_mean_stacked = NDA_mean_stacked,
    NDA_sd_epred_stacked = NDA_sd_epred_stacked,
    NDA_sd_predict_stacked = NDA_sd_predict_stacked,
    NDA_q025_stacked = NDA_q025_stacked,
    NDA_q975_stacked = NDA_q975_stacked,
    DA_raw_stacked = DA_raw_stacked,
    DA_z_estimation_stacked = DA_raw_stacked / NDA_sd_epred_stacked,
    DA_z_predictive_stacked = DA_raw_stacked / NDA_sd_predict_stacked,
    DA_tail_flag_95 = as.integer(df_sample$TA_scaled < NDA_q025_stacked | df_sample$TA_scaled > NDA_q975_stacked),
    DA_tail_flag_98 = as.integer(df_sample$TA_scaled < NDA_q010_stacked | df_sample$TA_scaled > NDA_q990_stacked),
    DA_ppd_tail_prob_two_sided = DA_ppd_tail_prob_two_sided,
    DA_ppd_percentile = DA_ppd_percentile,
    N_Mixture_Draws = mixture_draws,
    RNG_Context = mixture_rng_meta$RNG_Context,
    RNG_Offset = mixture_rng_meta$RNG_Offset,
    Canonical_Seed = mixture_rng_meta$Canonical_Seed,
    Mixture_Seed = mixture_rng_meta$Effective_Seed,
    RNG_Source = mixture_rng_meta$RNG_Source,
    Script_Version = script_version,
    Primary_Inference_Allowed = TRUE,
    stringsAsFactors = FALSE
  )

  list(
    result = result,
    audit = active %>%
      mutate(
        DA_Source = source,
        Validation_Target = validation_target,
        Weight_Sum = sum(active$Weight),
        N_Models_Active = nrow(active),
        N_Draws_Per_Model_Available = draw_counts,
        N_Mixture_Draws = mixture_draws,
        RNG_Context = mixture_rng_meta$RNG_Context,
        RNG_Offset = mixture_rng_meta$RNG_Offset,
        Canonical_Seed = mixture_rng_meta$Canonical_Seed,
        Mixture_Seed = mixture_rng_meta$Effective_Seed,
        RNG_Source = mixture_rng_meta$RNG_Source,
        Script_Version = script_version,
        Primary_Inference_Allowed = TRUE
      )
  )
}

grouped_ep_weights_path <- file.path(grouped_root, "tables", "table_winsor_kfold_weights_ex_post.csv")
grouped_rt_weights_path <- file.path(grouped_root, "tables", "table_winsor_kfold_weights_no_lookahead.csv")
row_ep_weights_path <- file.path(row_root, "tables", "table_winsor_row_exact_kfold_weights_ex_post.csv")
row_rt_weights_path <- file.path(row_root, "tables", "table_winsor_row_exact_kfold_weights_no_lookahead.csv")

grouped_ep_weights <- safe_read_csv(grouped_ep_weights_path)
grouped_rt_weights <- safe_read_csv(grouped_rt_weights_path)
row_rt_weights <- safe_read_csv(row_rt_weights_path)
row_ep_weights <- if (file.exists(row_ep_weights_path)) safe_read_csv(row_ep_weights_path) else NULL

grouped_ep <- compute_exact_kfold_da(df_ep, grouped_ep_weights, "exact_grouped_kfold", "ex_post", "firm_grouped_ex_post")
grouped_rt <- compute_exact_kfold_da(df_rt, grouped_rt_weights, "exact_grouped_kfold", "real_time", "firm_grouped_real_time")
row_results <- list()
row_audits <- list()
if (!is.null(row_ep_weights) && nrow(row_ep_weights) > 0) {
  row_ep <- compute_exact_kfold_da(df_ep, row_ep_weights, "exact_row_kfold", "ex_post", "row_level_ex_post")
  row_results[[length(row_results) + 1]] <- row_ep$result
  row_audits[[length(row_audits) + 1]] <- row_ep$audit
}
row_rt <- compute_exact_kfold_da(df_rt, row_rt_weights, "exact_row_kfold", "real_time", "row_level_real_time")
row_results[[length(row_results) + 1]] <- row_rt$result
row_audits[[length(row_audits) + 1]] <- row_rt$audit

grouped_out <- bind_rows(grouped_ep$result, grouped_rt$result)
row_out <- bind_rows(row_results)
grouped_out_path <- file.path(tables_dir, "final_uncertainty_adjusted_accruals_exact_kfold_grouped_winsor.csv")
row_out_path <- file.path(tables_dir, "final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv")
write_csv_safely(grouped_out, grouped_out_path, row.names = FALSE)
write_csv_safely(row_out, row_out_path, row.names = FALSE)

weight_audit <- bind_rows(grouped_ep$audit, grouped_rt$audit, row_audits)
weight_files <- data.frame(
  DA_Source = c("exact_grouped_kfold", "exact_grouped_kfold", rep("exact_row_kfold", if (!is.null(row_ep_weights) && nrow(row_ep_weights) > 0) 2 else 1)),
  Validation_Target = c("firm_grouped_ex_post", "firm_grouped_real_time", if (!is.null(row_ep_weights) && nrow(row_ep_weights) > 0) c("row_level_ex_post", "row_level_real_time") else "row_level_real_time"),
  Weight_File = c(grouped_ep_weights_path, grouped_rt_weights_path, if (!is.null(row_ep_weights) && nrow(row_ep_weights) > 0) c(row_ep_weights_path, row_rt_weights_path) else row_rt_weights_path),
  stringsAsFactors = FALSE
)

draw_hash_manifest <- weight_audit %>%
  distinct(DA_Source, Validation_Target, Model_ID, Target_Space, Heterogeneity_Variant, Draw_File) %>%
  mutate(
    Draw_File_Exists = file.exists(Draw_File),
    Draw_File_Size = vapply(Draw_File, file_size_or_na, numeric(1)),
    Draw_File_MTime = vapply(Draw_File, mtime_or_na, character(1)),
    Draw_File_Hash = vapply(Draw_File, file_hash_or_na, character(1))
  )
draw_hash_manifest_path <- file.path(tables_dir, "table_DA_exact_kfold_draw_file_hash_manifest.csv")
write_csv_safely(draw_hash_manifest, draw_hash_manifest_path, row.names = FALSE)

model_inclusion_gate <- bind_rows(inclusion_gate_rows)
write_csv_safely(model_inclusion_gate, file.path(tables_dir, "table_model_primary_inclusion_gate.csv"), row.names = FALSE)

weight_audit <- weight_audit %>%
  left_join(
    model_inclusion_gate %>%
      select(Target_Space, Model_ID, Heterogeneity_Variant, DA_Source,
             Primary_Inclusion_Decision, Full_Sample_MCMC_Status, PSIS_Status),
    by = c("Target_Space", "Model_ID", "Heterogeneity_Variant", "DA_Source")
  )

source_manifest <- weight_audit %>%
  group_by(DA_Source, Validation_Target) %>%
  summarise(
    Grouped_KFold_Run_Root = grouped_root,
    Row_KFold_Run_Root = row_root,
    Grouped_KFold_Run_Manifest = grouped_manifest_path,
    Row_KFold_Run_Manifest = row_manifest_path,
    Weight_Sum = sum(Weight),
    N_Models_Active = n(),
    N_Draws_Per_Model_Available = min(N_Draws_Per_Model_Available, na.rm = TRUE),
    N_Mixture_Draws = first(N_Mixture_Draws),
    RNG_Context = first(RNG_Context),
    RNG_Offset = first(RNG_Offset),
    Canonical_Seed = first(Canonical_Seed),
    Mixture_Seed = first(Mixture_Seed),
    RNG_Source = first(RNG_Source),
    Script_Version = first(Script_Version),
    Primary_Inference_Allowed = all(Primary_Inference_Allowed),
    .groups = "drop"
  ) %>%
  left_join(weight_files, by = c("DA_Source", "Validation_Target")) %>%
  mutate(
    Grouped_Weight_File = ifelse(DA_Source == "exact_grouped_kfold", Weight_File, NA_character_),
    Row_Weight_File = ifelse(DA_Source == "exact_row_kfold", Weight_File, NA_character_),
    Weight_File_Size = vapply(Weight_File, file_size_or_na, numeric(1)),
    Weight_File_MTime = vapply(Weight_File, mtime_or_na, character(1)),
    Weight_File_Hash = vapply(Weight_File, file_hash_or_na, character(1)),
    Draw_File_Count = vapply(Validation_Target, function(v) sum(draw_hash_manifest$Validation_Target == v), integer(1)),
    Draw_File_Hash_Manifest = draw_hash_manifest_path,
    Script_Name = script_name
  ) %>%
  select(
    DA_Source, Validation_Target, Grouped_KFold_Run_Root, Row_KFold_Run_Root,
    Grouped_KFold_Run_Manifest, Row_KFold_Run_Manifest, Grouped_Weight_File,
    Row_Weight_File, Weight_File, Weight_File_Size, Weight_File_MTime, Weight_File_Hash,
    Draw_File_Count, Draw_File_Hash_Manifest, Weight_Sum, N_Models_Active,
    N_Draws_Per_Model_Available, N_Mixture_Draws, RNG_Context, RNG_Offset,
    Canonical_Seed, Mixture_Seed, RNG_Source, Script_Name,
    Script_Version, Primary_Inference_Allowed
  )

write_csv_safely(source_manifest, file.path(tables_dir, "table_DA_exact_kfold_source_manifest.csv"), row.names = FALSE)
write_csv_safely(weight_audit, file.path(tables_dir, "table_DA_exact_kfold_weight_audit.csv"), row.names = FALSE)

primary_columns <- c(
  "NDA_mean_stacked", "NDA_sd_epred_stacked", "NDA_sd_predict_stacked",
  "DA_raw_stacked", "DA_z_estimation_stacked", "DA_z_predictive_stacked",
  "DA_tail_flag_95", "DA_tail_flag_98", "DA_ppd_tail_prob_two_sided"
)

audit_nonfinite <- function(path, df) {
  bind_rows(lapply(intersect(primary_columns, names(df)), function(col) {
    x <- df[[col]]
    data.frame(
      output_file = path,
      DA_Source = paste(unique(df$DA_Source), collapse = ","),
      column = col,
      n_rows = length(x),
      n_nonfinite = sum(!is.finite(x)),
      share_nonfinite = mean(!is.finite(x)),
      primary_column = TRUE,
      stringsAsFactors = FALSE
    )
  }))
}
nonfinite_audit <- bind_rows(
  audit_nonfinite(grouped_out_path, grouped_out),
  audit_nonfinite(row_out_path, row_out)
)
write_csv_safely(nonfinite_audit, file.path(tables_dir, "table_DA_exact_kfold_nonfinite_audit.csv"), row.names = FALSE)

gate_decision <- if (any(nonfinite_audit$n_nonfinite > 0)) "FAIL_NONFINITE_PRIMARY_COLUMNS" else "PASS"
gate <- data.frame(
  gate = "exact_kfold_DA_construction",
  gate_decision = gate_decision,
  n_nonfinite_primary = sum(nonfinite_audit$n_nonfinite),
  grouped_output = grouped_out_path,
  row_output = row_out_path,
  Script_Version = script_version,
  stringsAsFactors = FALSE
)
write_csv_safely(gate, file.path(tables_dir, "table_DA_exact_kfold_gate_decision.csv"), row.names = FALSE)

manifest_paths <- c(
  grouped_manifest_path, row_manifest_path, grouped_ep_weights_path, grouped_rt_weights_path,
  row_ep_weights_path, row_rt_weights_path, mcmc_gate_path,
  grouped_out_path, row_out_path, file.path(tables_dir, "table_DA_exact_kfold_source_manifest.csv"),
  file.path(tables_dir, "table_DA_exact_kfold_weight_audit.csv"),
  file.path(tables_dir, "table_DA_exact_kfold_nonfinite_audit.csv"),
  file.path(tables_dir, "table_DA_exact_kfold_gate_decision.csv"),
  file.path(tables_dir, "table_model_primary_inclusion_gate.csv"),
  draw_hash_manifest_path
)
script_end_time <- Sys.time()
io_manifest <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = as.character(script_start_time),
  End_Time = as.character(script_end_time),
  Runtime_Seconds = as.numeric(difftime(script_end_time, script_start_time, units = "secs")),
  Git_Commit = git_commit_or_na(),
  Classification = c(rep("input", 7), rep("output", length(manifest_paths) - 7)),
  Path = manifest_paths,
  Exists = file.exists(manifest_paths),
  Size = vapply(manifest_paths, file_size_or_na, numeric(1)),
  MTime = vapply(manifest_paths, mtime_or_na, character(1)),
  Hash = vapply(manifest_paths, file_hash_or_na, character(1)),
  Gate_Decision = gate_decision,
  Primary_Secondary = "primary_exact_kfold",
  stringsAsFactors = FALSE
)
write_csv_safely(io_manifest, file.path(tables_dir, "table_DA_exact_kfold_io_manifest.csv"), row.names = FALSE)

cat("\n[SUCCESS] Exact K-fold DA construction completed.\n")
cat("Grouped output:", grouped_out_path, "\n")
cat("Row output:", row_out_path, "\n")
cat("Gate decision:", gate_decision, "\n")
phase_end("ma14", "Construct exact-KFold primary DA")
