# Script: ma09c_collect_loo_stacking.R
# Purpose: Collect secondary PSIS/LOO outputs after save_pars worker refits.

source("scripts/ma00_setup.R")
phase_begin("ma09c", "Collect LOO stacking outputs")

tables_dir <- file.path(output_root, "tables")
manifest_path <- file.path(tables_dir, "table_ma09_savepars_refit_task_manifest.csv")
status_path <- file.path(tables_dir, "table_ma09_savepars_refit_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) {
  stop("[BLOCKER] ma09c requires ma09a manifest and ma09b task status.")
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "ma09c loo collect")

if (!"result_path" %in% names(manifest)) {
  manifest$result_path <- sub("_metadata\\.csv$", "_loo_result.rds", manifest$metadata_path)
}
results <- lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] ma09c missing task LOO result: ", path)
  readRDS(path)
})
loo_list <- setNames(lapply(results, `[[`, "loo_corrected"), manifest$Task_Key)
loo_comparison <- do.call(rbind, lapply(seq_along(results), function(i) {
  r <- results[[i]]
  task <- r$task
  data.frame(
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Main_Stack_Inclusion = if ("Main_Stack_Inclusion" %in% names(task)) task$Main_Stack_Inclusion else TRUE,
    Secondary_Robustness = if ("Secondary_Robustness" %in% names(task)) task$Secondary_Robustness else FALSE,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    N_Obs = r$n_obs,
    original_elpd = r$original_elpd,
    refit_raw_elpd = r$refit_raw_elpd,
    corrected_elpd = r$corrected_elpd,
    elpd_diff_refit = r$elpd_diff_refit,
    max_diff_coef = r$max_diff_coef,
    original_k_above_07 = if ("original_k_above_07" %in% names(task)) task$original_k_above_07 else NA_integer_,
    refit_raw_k_above_07 = r$refit_raw_k_above_07,
    corrected_k_above_07 = r$corrected_k_above_07,
    moment_match_applied = r$moment_match_applied,
    moment_match_note = r$moment_match_note,
    Prior_Set_ID = if ("Prior_Set_ID" %in% names(task)) task$Prior_Set_ID else prior_set_id,
    Likelihood_Family = if ("Likelihood_Family" %in% names(task)) task$Likelihood_Family else likelihood_family,
    Model_Structure = if ("Model_Structure" %in% names(task)) task$Model_Structure else model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
}))
write.csv(loo_comparison, file.path(tables_dir, "table_loo_comparison_winsor_corrected.csv"), row.names = FALSE)

make_weights <- function(space) {
  main_inclusion <- if ("Main_Stack_Inclusion" %in% names(manifest)) {
    manifest$Main_Stack_Inclusion %in% c(TRUE, "TRUE", "true", "1", 1L)
  } else {
    rep(TRUE, nrow(manifest))
  }
  sample_main <- if ("Sample_Group" %in% names(manifest)) {
    manifest$Sample_Group == "main_common"
  } else {
    rep(TRUE, nrow(manifest))
  }
  idx <- which(manifest$Target_Space == space & main_inclusion & sample_main)
  if (length(idx) < 2L) return(data.frame())
  n_obs <- vapply(results[idx], `[[`, numeric(1), "n_obs")
  if (length(unique(n_obs)) > 1L) {
    stop("[BLOCKER] N mismatch in ma09c stack for ", space, ": ", paste(unique(n_obs), collapse = ", "))
  }
  weights <- as.numeric(loo::loo_model_weights(loo_list[idx], method = "stacking"))
  data.frame(
    Model_ID = manifest$Model_ID[idx],
    Model_Name = manifest$Model_Name[idx],
    Target_Space = manifest$Target_Space[idx],
    Sample_Group = manifest$Sample_Group[idx],
    Heterogeneity_Variant = manifest$Heterogeneity_Variant[idx],
    stacking_weight = weights,
    Primary_Secondary = "secondary_psis_loo",
    stringsAsFactors = FALSE
  )
}
write.csv(make_weights("ex_post"), file.path(tables_dir, "table_stacking_weights_ex_post_winsor_corrected.csv"), row.names = FALSE)
write.csv(make_weights("real_time"), file.path(tables_dir, "table_stacking_weights_no_lookahead_winsor_corrected.csv"), row.names = FALSE)
message("ma09c collected task-level LOO results and wrote shared PSIS/LOO outputs.")
phase_end("ma09c", "Collect LOO stacking outputs")
