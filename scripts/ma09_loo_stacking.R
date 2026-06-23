# -----------------------------------------------------------------------------
# Script: ma09_loo_stacking.R
# Purpose: Corrected LOO and stacking for winsorized models.
# -----------------------------------------------------------------------------

library(dplyr)
library(brms)
library(loo)

source("scripts/ma00_setup.R")
phase_begin("ma09", "LOO stacking (secondary)")
ensure_analysis_dirs()
write_prior_registry()
validate_final_analysis_config("ma09 LOO stacking (secondary)", final_mode = TRUE)

loo_cfg <- accrual_loo_config()
options(mc.cores = loo_cfg$mc_cores)
compare_original_weights <- loo_cfg$compare_original_weights

formulas_path <- file.path(output_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
diag_path <- file.path(output_root, "tables", "table_brms_diagnostics_winsor.csv")
coeff_path <- file.path(output_root, "tables", "table_coefficient_summary_winsor.csv")
loo_comp_path <- file.path(output_root, "tables", "table_loo_comparison_winsor_corrected.csv")
loo_cache_dir <- file.path(output_root, "draws", "loo_cache")
models_dir <- file.path(output_root, "models")

if (!file.exists(formulas_path)) stop("[BLOCKER] Missing winsor formula table.")
if (!file.exists(diag_path)) stop("[BLOCKER] Missing winsor diagnostics table. Run ma07 first.")
if (!file.exists(coeff_path)) stop("[BLOCKER] Missing winsor coefficient table. Run ma07 first.")
if (!dir.exists(loo_cache_dir)) dir.create(loo_cache_dir, recursive = TRUE)

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
diag_df <- read.csv(diag_path, stringsAsFactors = FALSE)
coeff_df <- read.csv(coeff_path, stringsAsFactors = FALSE)

# Check MCMC diagnostics gate status
diagnostics_gate_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_gate_winsor.csv")
if (!file.exists(diagnostics_gate_path)) {
  stop("[BLOCKER] MCMC diagnostics gate table 'table_mcmc_diagnostics_gate_winsor.csv' not found. Run 08_mcmc_diagnostics.R first.")
}
gate_df <- read.csv(diagnostics_gate_path, stringsAsFactors = FALSE)

joined_diag_gate <- diag_df %>%
  left_join(
    gate_df %>% select(model_id, Target_Space, Heterogeneity_Variant, diagnostics_status, fail_reason),
    by = c("Model_ID" = "model_id", "Target_Space", "Heterogeneity_Variant")
  )

# Exclude FAIL models
exclusions_df <- joined_diag_gate %>%
  filter(diagnostics_status == "FAIL" | is.na(diagnostics_status)) %>%
  transmute(
    model_id = Model_ID,
    model_name = Model_Name,
    Target_Space = Target_Space,
    Sample_Group = Sample_Group,
    Heterogeneity_Variant = Heterogeneity_Variant,
    diagnostics_status = ifelse(is.na(diagnostics_status), "FAIL", diagnostics_status),
    fail_reason = ifelse(is.na(fail_reason), "Missing diagnostics / failed on MCMC checks", fail_reason)
  )

exclusions_path <- file.path(output_root, "tables", "table_stacking_model_exclusions_winsor.csv")
write.csv(exclusions_df, exclusions_path, row.names = FALSE)
message("Saved stacking model exclusions list to: ", exclusions_path)

eligible_models <- joined_diag_gate %>%
  filter(diagnostics_status %in% c("PASS", "REVIEW"))

if (nrow(eligible_models) == 0) {
  stop("[BLOCKER] No stacking-eligible winsor models found after filtering by MCMC diagnostics gate status.")
}

chains <- loo_cfg$chains
cores <- loo_cfg$cores
iter <- loo_cfg$iter
warmup <- loo_cfg$warmup
adapt_delta <- loo_cfg$adapt_delta
max_treedepth <- loo_cfg$max_treedepth
refresh <- loo_cfg$refresh
options(mc.cores = cores)

eligible_joined <- eligible_models %>%
  left_join(
    formulas_df %>% select(Model_ID, Target_Space, Heterogeneity_Variant, Target_Sample, brms_Formula) %>% distinct(),
    by = c("Model_ID", "Target_Space", "Heterogeneity_Variant")
  )

loo_list <- list()
loo_comparison <- data.frame(
  Model_ID = character(),
  Model_Name = character(),
  Target_Space = character(),
  Sample_Group = character(),
  Main_Stack_Inclusion = logical(),
  Secondary_Robustness = logical(),
  Heterogeneity_Variant = character(),
  N_Obs = integer(),
  original_elpd = double(),
  refit_raw_elpd = double(),
  corrected_elpd = double(),
  original_k_above_07 = integer(),
  refit_raw_k_above_07 = integer(),
  corrected_k_above_07 = integer(),
  elpd_diff_refit = double(),
  moment_match_applied = logical(),
  moment_match_note = character(),
  Prior_Set_ID = character(),
  Likelihood_Family = character(),
  Model_Structure = character(),
  Output_Root = character(),
  stringsAsFactors = FALSE
)

message("\n========= WINSOR STAGES 1 & 2: REFIT, SANITY CHECK & LOO =========")

for (i in seq_len(nrow(eligible_joined))) {
  row <- eligible_joined[i, ]
  base_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
  sp_filename <- file.path(models_dir, paste0("fit_", base_key, "_sp.rds"))
  loo_cache <- file.path(loo_cache_dir, paste0(base_key, "_loo.rds"))

  message(sprintf("\n[%d/%d] Winsor LOO model: %s", i, nrow(eligible_joined), base_key))

  if (file.exists(loo_cache)) {
    message("  Loading cached winsor LOO from: ", loo_cache)
    loo_corrected <- readRDS(loo_cache)
    loo_list[[base_key]] <- loo_corrected
    loo_comparison <- rbind(loo_comparison, data.frame(
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Main_Stack_Inclusion = row$Main_Stack_Inclusion,
      Secondary_Robustness = row$Secondary_Robustness,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      N_Obs = length(loo_corrected$diagnostics$pareto_k),
      original_elpd = row$elpd_loo,
      refit_raw_elpd = NA_real_,
      corrected_elpd = loo_corrected$estimates["elpd_loo", "Estimate"],
      original_k_above_07 = row$pareto_k_above_07,
      refit_raw_k_above_07 = NA_integer_,
      corrected_k_above_07 = sum(loo_corrected$diagnostics$pareto_k > 0.7),
      elpd_diff_refit = NA_real_,
      moment_match_applied = NA,
      moment_match_note = "Loaded cached LOO",
      Prior_Set_ID = if ("Prior_Set_ID" %in% names(row)) row$Prior_Set_ID else prior_set_id,
      Likelihood_Family = if ("Likelihood_Family" %in% names(row)) row$Likelihood_Family else likelihood_family,
      Model_Structure = if ("Model_Structure" %in% names(row)) row$Model_Structure else model_structure,
      Output_Root = output_root,
      stringsAsFactors = FALSE
    ))
    next
  }

  fit_sp <- NULL
  if (file.exists(sp_filename)) {
    message("  Loading existing winsor save_pars refit...")
    fit_sp <- tryCatch(readRDS(sp_filename), error = function(e) NULL)
  }

  if (is.null(fit_sp)) {
    message("  Refitting winsor model with save_pars(all=TRUE) and pre-factored variables...")
    df_scaled <- read_winsor_sample(row$Target_Sample, prefactor = TRUE)
    formula_str <- fix_formula(row$brms_Formula, prefactor = TRUE)
    message("  Formula: ", formula_str)
    message(
      "  brms/rstan sampler controls: chains=", chains,
      ", cores=", cores,
      ", iter=", iter,
      ", warmup=", warmup,
      ", adapt_delta=", adapt_delta,
      ", max_treedepth=", max_treedepth,
      ", refresh=", refresh
    )

    prior_list <- default_prior_list(row$Heterogeneity_Variant)

    fit_sp <- tryCatch({
      brm(
        formula = bf(as.formula(formula_str)),
        data = df_scaled,
        family = brms_family(),
        prior = prior_list,
        chains = chains,
        cores = cores,
        iter = iter,
        warmup = warmup,
        control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
        seed = accrual_seed_for(
          paste0("baseline_loo_refit_", row$Target_Space, "_", row$Model_ID, "_", row$Heterogeneity_Variant),
          offset = i
        ),
        save_pars = save_pars(all = TRUE),
        refresh = refresh
      )
    }, error = function(e) {
      message("[ERROR] Winsor save_pars refit crashed: ", e$message)
      NULL
    })

    if (is.null(fit_sp)) stop("[BLOCKER] Winsor save_pars refit failed for model: ", base_key)
    saveRDS(fit_sp, sp_filename)
    message("  Saved winsor save_pars refit to: ", sp_filename)
  }

  n_obs <- nobs(fit_sp)
  message("  Computing raw LOO for winsor refit (cores=1)...")
  loo_raw <- loo(fit_sp, cores = 1)
  refit_raw_elpd <- loo_raw$estimates["elpd_loo", "Estimate"]
  refit_raw_k <- sum(loo_raw$diagnostics$pareto_k > 0.7)

  orig_coefs <- coeff_df %>%
    filter(Model_ID == row$Model_ID,
           Target_Space == row$Target_Space,
           Heterogeneity_Variant == row$Heterogeneity_Variant)

  if (nrow(orig_coefs) > 0) {
    refit_coef_summary <- fixef(fit_sp)
    refit_coef_names <- rownames(refit_coef_summary)
    orig_coef_names_mapped <- gsub("factoryear", "year_f", orig_coefs$Parameter)
    orig_coef_names_mapped <- gsub("factorindustry", "industry_f", orig_coef_names_mapped)
    max_diff_coef <- 0
    for (p_name in refit_coef_names) {
      match_idx <- which(orig_coef_names_mapped == p_name)
      if (length(match_idx) == 1) {
        diff_val <- abs(refit_coef_summary[p_name, "Estimate"] - orig_coefs$Estimate[match_idx])
        if (diff_val > max_diff_coef) max_diff_coef <- diff_val
      }
    }
    message(sprintf("  Max coefficient diff = %.6f", max_diff_coef))
    if (max_diff_coef >= 0.005) {
      stop(sprintf("[BLOCKER] Coefficient shift %.5f detected for winsor model %s.", max_diff_coef, base_key))
    }
  } else {
    warning("  [WARNING] No winsor coefficients found for sanity check.")
  }

  diff_elpd <- refit_raw_elpd - row$elpd_loo
  message(sprintf("  Refit raw ELPD = %.5f (ma07 = %.5f, Diff = %.5f)",
                  refit_raw_elpd, row$elpd_loo, diff_elpd))
  if (abs(diff_elpd) >= 10.0) {
    stop(sprintf("[BLOCKER] Winsor elpd shifted materially by %.4f for %s.", diff_elpd, base_key))
  }

  loo_corrected <- loo_raw
  mm_applied <- FALSE
  mm_note <- "No high Pareto-k observations"
  if (refit_raw_k > 0) {
    message(sprintf("  Applying moment matching on %d high-k observations...", refit_raw_k))
    loo_mm <- tryCatch({
      loo(fit_sp, moment_match = TRUE, cores = 1)
    }, error = function(e) {
      message("  [ERROR] Moment matching failed: ", e$message)
      NULL
    })
    if (is.null(loo_mm)) {
      stop("[BLOCKER] Moment matching failed for winsor model: ", base_key)
    }
    loo_corrected <- loo_mm
    mm_applied <- TRUE
    mm_note <- sprintf("Moment matching applied; high-k before=%d after=%d",
                       refit_raw_k, sum(loo_corrected$diagnostics$pareto_k > 0.7))
  }

  corrected_elpd <- loo_corrected$estimates["elpd_loo", "Estimate"]
  corrected_k <- sum(loo_corrected$diagnostics$pareto_k > 0.7)

  loo_comparison <- rbind(loo_comparison, data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Main_Stack_Inclusion = row$Main_Stack_Inclusion,
    Secondary_Robustness = row$Secondary_Robustness,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    N_Obs = n_obs,
    original_elpd = row$elpd_loo,
    refit_raw_elpd = refit_raw_elpd,
    corrected_elpd = corrected_elpd,
    original_k_above_07 = row$pareto_k_above_07,
    refit_raw_k_above_07 = refit_raw_k,
    corrected_k_above_07 = corrected_k,
    elpd_diff_refit = diff_elpd,
    moment_match_applied = mm_applied,
    moment_match_note = mm_note,
    Prior_Set_ID = if ("Prior_Set_ID" %in% names(row)) row$Prior_Set_ID else prior_set_id,
    Likelihood_Family = if ("Likelihood_Family" %in% names(row)) row$Likelihood_Family else likelihood_family,
    Model_Structure = if ("Model_Structure" %in% names(row)) row$Model_Structure else model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  ))

  saveRDS(loo_corrected, loo_cache)
  loo_list[[base_key]] <- loo_corrected
  rm(fit_sp)
  gc()
}

write.csv(loo_comparison, loo_comp_path, row.names = FALSE)
message("\nSaved winsor LOO comparison table to ", loo_comp_path)

message("\n========= WINSOR STAGE 3: STACKING WEIGHTS =========")

sample_n <- function(file_name) {
  candidates <- c(
    file.path(output_root, "tables", file_name),
    file.path(input_winsor_root, "tables", file_name)
  )
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path)) stop("[BLOCKER] Missing sample file: ", file_name)
  nrow(read.csv(path, stringsAsFactors = FALSE))
}

expected_n_ep <- sample_n("final_common_ex_post_sample_winsor.csv")
expected_n_rt <- sample_n("final_common_realtime_sample_winsor.csv")

run_space_stacking <- function(space_name, expected_N, eligible_ids) {
  message(sprintf("\n=== WINSOR STACKING: %s (Expected N = %d) ===", toupper(space_name), expected_N))

  space_eligible <- eligible_models %>%
    filter(Target_Space == space_name,
           Model_ID %in% eligible_ids,
           Sample_Group == "main_common",
           Main_Stack_Inclusion == TRUE)
  if (nrow(space_eligible) < 2) {
    stop(sprintf("[BLOCKER] Insufficient eligible models (found %d, need at least 2) for stacking in space: %s", nrow(space_eligible), space_name))
  }

  space_loos <- list()
  n_check <- c()
  meta_rows <- list()

  for (i in seq_len(nrow(space_eligible))) {
    m <- space_eligible[i, ]
    key <- model_key_sampled(m$Model_ID, m$Target_Space, m$Sample_Group, m$Heterogeneity_Variant, "_winsor")
    if (!(key %in% names(loo_list))) {
      warning("LOO not found for winsor model: ", key, " - excluded from stack.")
      next
    }
    comp_row <- loo_comparison %>%
      filter(Model_ID == m$Model_ID, Target_Space == m$Target_Space,
             Heterogeneity_Variant == m$Heterogeneity_Variant)
    if (nrow(comp_row) == 1) n_check <- c(n_check, comp_row$N_Obs)
    space_loos[[key]] <- loo_list[[key]]
    meta_rows[[key]] <- m
  }

  unique_ns <- unique(n_check)
  if (length(unique_ns) > 1) stop(sprintf("[BLOCKER] N mismatch in %s stack: %s", space_name, paste(unique_ns, collapse = ", ")))
  if (length(unique_ns) != 1 || unique_ns[1] != expected_N) {
    stop(sprintf("[BLOCKER] N mismatch in %s stack: expected %d, got %s",
                 space_name, expected_N, paste(unique_ns, collapse = ", ")))
  }

  weights_vec <- as.numeric(loo_model_weights(space_loos, method = "stacking"))
  if (abs(sum(weights_vec) - 1) > 1e-5) {
    stop(sprintf("[BLOCKER] Winsor weights do not sum to 1 in %s stack.", space_name))
  }

  df_weights <- data.frame(
    Model_ID = sapply(names(space_loos), function(k) meta_rows[[k]]$Model_ID),
    Model_Name = sapply(names(space_loos), function(k) meta_rows[[k]]$Model_Name),
    Target_Space = space_name,
    Sample_Group = "main_common",
    M10_Included = FALSE,
    M08_Included = FALSE,
    Heterogeneity_Variant = sapply(names(space_loos), function(k) meta_rows[[k]]$Heterogeneity_Variant),
    Weight = weights_vec,
    Full_Sample_MCMC_Status = sapply(names(space_loos), function(k) meta_rows[[k]]$diagnostics_status),
    Primary_Secondary = "secondary_psis_loo",
    Primary_Inference_Allowed = FALSE,
    Inclusion_Note = "PSIS/LOO weights are secondary; REVIEW models are not silently primary and exact-KFold DA applies table_model_primary_inclusion_gate.csv.",
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  ) %>% arrange(desc(Weight))

  print(df_weights, row.names = FALSE)
  df_weights
}

w_ep <- run_space_stacking("ex_post", expected_n_ep, c("M01", "M02", "M03", "M04", "M05", "M06", "M07"))
w_rt <- run_space_stacking("real_time", expected_n_rt, c("M01", "M02", "M03", "M07", "M09"))

ep_weights_path <- file.path(output_root, "tables", "table_stacking_weights_ex_post_winsor_corrected.csv")
rt_weights_path <- file.path(output_root, "tables", "table_stacking_weights_no_lookahead_winsor_corrected.csv")
write.csv(w_ep, ep_weights_path, row.names = FALSE)
write.csv(w_rt, rt_weights_path, row.names = FALSE)

secondary_oc_scores <- loo_comparison %>%
  filter(Sample_Group == "secondary_operating_cycle") %>%
  mutate(
    Comparison_Scope = "secondary_operating_cycle_sample",
    Comparable_Weights_Computed = FALSE,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    Notes = "M10 is secondary robustness only. Main-stack models were not rerun on the exact secondary operating-cycle sample in this script, so no comparable secondary stacking weights are computed here."
  )
write.csv(secondary_oc_scores, file.path(winsor_root, "tables", "table_secondary_operating_cycle_model_scores.csv"), row.names = FALSE)

write_skipped_stability <- function(reason, original_required, original_found, winsor_found, notes) {
  skipped_df <- data.frame(
    Comparison_Name = "Original_vs_Winsor",
    Status = "SKIPPED",
    Reason = reason,
    Original_Weights_Required = original_required,
    Original_Weights_Found = original_found,
    Winsor_Weights_Found = winsor_found,
    Notes = notes,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
  write.csv(
    skipped_df,
    file.path(output_root, "tables", "table_weight_stability_original_vs_winsor_SKIPPED.csv"),
    row.names = FALSE
  )
  skipped_df
}

make_stability <- function(winsor_df, space) {
  original_df <- read_original_weight_file(space)
  original_df <- original_df %>%
    mutate(
      Model_Name_Base = extract_base_model_name(Model_Name),
      Heterogeneity_Variant = vapply(Model_Name, extract_weight_variant, character(1)),
      Rank_Original = rank(-Weight, ties.method = "first")
    ) %>%
    select(Model_ID, Model_Name_Base, Heterogeneity_Variant, Weight_Original = Weight,
           Rank_Original, Original_Weight_Source)

  winsor_aug <- winsor_df %>%
    mutate(Rank_Winsor = rank(-Weight, ties.method = "first")) %>%
    select(Model_ID, Model_Name, Heterogeneity_Variant, Target_Space,
           Weight_Winsor = Weight, Rank_Winsor)

  joined <- winsor_aug %>%
    full_join(original_df, by = c("Model_ID", "Heterogeneity_Variant")) %>%
    mutate(
      Target_Space = ifelse(is.na(Target_Space), space, Target_Space),
      Model_Name = ifelse(is.na(Model_Name), Model_Name_Base, Model_Name),
      Weight_Original = ifelse(is.na(Weight_Original), 0, Weight_Original),
      Weight_Winsor = ifelse(is.na(Weight_Winsor), 0, Weight_Winsor),
      Weight_Difference = Weight_Winsor - Weight_Original,
      Abs_Weight_Difference = abs(Weight_Difference),
      Rank_Change = Rank_Winsor - Rank_Original
    )

  top_orig <- joined %>% filter(Rank_Original == min(Rank_Original, na.rm = TRUE)) %>% slice(1)
  top_win <- joined %>% filter(Rank_Winsor == min(Rank_Winsor, na.rm = TRUE)) %>% slice(1)
  orig_family <- classify_model_family(top_orig$Model_ID, top_orig$Model_Name)
  win_family <- classify_model_family(top_win$Model_ID, top_win$Model_Name)
  max_abs_change <- max(joined$Abs_Weight_Difference, na.rm = TRUE)
  flag <- if (win_family == "Jones-family" || max_abs_change >= 0.50) {
    "Unstable"
  } else if (orig_family == win_family && max_abs_change < 0.20) {
    "Stable"
  } else {
    "Partially Stable"
  }

  joined$Family_Level_Stability <- flag
  joined$Model_Level_Stability <- dplyr::case_when(
    is.na(joined$Rank_Change) ~ "Not comparable",
    joined$Abs_Weight_Difference >= 0.20 | abs(joined$Rank_Change) >= 3 ~ "Unstable",
    joined$Abs_Weight_Difference >= 0.10 | abs(joined$Rank_Change) >= 1 ~ "Partially Stable",
    TRUE ~ "Stable"
  )
  joined$Headline_Stability_Flag <- joined$Family_Level_Stability
  joined$Prior_Set_ID <- prior_set_id
  joined$Likelihood_Family <- likelihood_family
  joined$Model_Structure <- model_structure
  joined$Output_Root <- output_root
  joined %>%
    select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant,
           Weight_Original, Weight_Winsor, Weight_Difference, Abs_Weight_Difference,
           Rank_Original, Rank_Winsor, Rank_Change, Family_Level_Stability,
           Model_Level_Stability, Headline_Stability_Flag,
           Prior_Set_ID, Likelihood_Family, Model_Structure, Output_Root,
           Original_Weight_Source) %>%
    arrange(Target_Space, Rank_Winsor)
}

winsor_weights_found <- file.exists(ep_weights_path) && file.exists(rt_weights_path)
original_sources <- character()
stability_note <- "Original/no-winsor comparison was skipped by default."

if (!compare_original_weights) {
  write_skipped_stability(
    reason = "Original/no-winsor weights are unavailable or intentionally not used after corrected rerun",
    original_required = FALSE,
    original_found = FALSE,
    winsor_found = winsor_weights_found,
    notes = "Main winsorized stacking outputs remain valid."
  )
} else {
  original_candidates <- c(
    file.path(baseline_root, "tables", "table_stacking_weights_ex_post_corrected.csv"),
    file.path(baseline_root, "tables", "table_stacking_weights_ex_post.csv"),
    file.path(baseline_root, "tables", "table_stacking_weights_real_time_corrected.csv"),
    file.path(baseline_root, "tables", "table_stacking_weights_real_time.csv"),
    file.path(baseline_root, "tables", "table_stacking_weights_no_lookahead_corrected.csv"),
    file.path(baseline_root, "tables", "table_stacking_weights_no_lookahead.csv")
  )
  existing_originals <- unique(original_candidates[file.exists(original_candidates)])

  if (length(existing_originals) == 0) {
    warning("Original/no-winsor weights not found. Skipping original-vs-winsor comparison.")
    write_skipped_stability(
      reason = "Original/no-winsor weights requested but not found",
      original_required = TRUE,
      original_found = FALSE,
      winsor_found = winsor_weights_found,
      notes = "Set ACCRUAL_COMPARE_ORIGINAL_WEIGHTS=TRUE only when original weight files are present."
    )
    stability_note <- "Original/no-winsor comparison was requested but skipped because original files were missing."
  } else {
    stability_df <- bind_rows(make_stability(w_ep, "ex_post"), make_stability(w_rt, "real_time"))
    write.csv(
      stability_df,
      file.path(output_root, "tables", "table_weight_stability_original_vs_winsor.csv"),
      row.names = FALSE
    )
    original_sources <- unique(stability_df$Original_Weight_Source)
    stability_note <- "Original/no-winsor comparison was performed because ACCRUAL_COMPARE_ORIGINAL_WEIGHTS=TRUE and original files were available."
  }
}

notes <- c(
  "ma09 winsor corrected LOO/stacking notes",
  sprintf("Output root: %s", output_root),
  sprintf("Input winsor root for sample sizes/formulas when needed: %s", input_winsor_root),
  sprintf("Prior set: %s; likelihood family: %s; model structure: %s", prior_set_id, likelihood_family, model_structure),
  "ma09 is based only on current-root winsorized model files and LOO cache.",
  "Authoritative logic adapted from the earlier corrected refit workflow.",
  "Features retained: pre-factored industry/year variables, save_pars(all=TRUE), coefficient sanity check, raw elpd sanity check, moment matching, N-check before stacking, separate ex-post and no-look-ahead stacks.",
  sprintf("Ex-post expected N: %d", expected_n_ep),
  sprintf("No-look-ahead expected N: %d", expected_n_rt),
  "Main ex-post stack and no-lookahead stack were computed in sample_group = main_common.",
  "M08 is excluded from main stacks because it uses rolling-volatility subsamples.",
  "M10 is excluded from main stacks because it requires operating_cycle and uses the secondary operating-cycle sample.",
  "Main stacking outputs therefore exclude M08 and M10.",
  "Family labels: M01-M03 Jones-family; M04-M06 Cash-flow/McNichols-family; M07 Ball-Shivakumar/asymmetry; M09 No-lookahead/real-time; M08 and M10 secondary only.",
  "Moment-matching failures are blockers. Remaining high Pareto-k counts are reported in table_loo_comparison_winsor_corrected.csv.",
  "Firm random-effect models can still have high Pareto-k observations; this limitation should be documented when interpreting LOO.",
  stability_note,
  "Original/no-winsor comparison is skipped by default.",
  "To enable optional comparison, set ACCRUAL_COMPARE_ORIGINAL_WEIGHTS=TRUE.",
  "Missing original/no-winsor weights are not a fatal error.",
  if (length(original_sources) > 0) {
    paste("Original weight source files used:", paste(original_sources, collapse = "; "))
  } else {
    "Original weight source files used: none"
  }
)
writeLines(notes, file.path(output_root, "logs", "ma09_loo_stacking_winsor_notes.txt"))

cat("\n[SUCCESS] ma09 winsor corrected LOO and stacking completed.\n")
phase_end("ma09", "LOO stacking (secondary)")
