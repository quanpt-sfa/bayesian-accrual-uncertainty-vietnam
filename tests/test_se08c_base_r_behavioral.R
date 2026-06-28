# Lightweight behavioral smoke test for SE08C using synthetic task outputs.

root <- normalizePath(file.path(tempdir(), paste0("se08c_smoke_", Sys.getpid())), winslash = "/", mustWork = FALSE)
dir.create(root, recursive = TRUE, showWarnings = FALSE)
tables_dir <- file.path(root, "sensitivity", "fold_local_preprocessing", "tables")
cache_dir <- file.path(root, "sensitivity", "fold_local_preprocessing", "cache")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

primary_grouped_root <- file.path(root, "kfold_firm", "completed")
primary_row_root <- file.path(root, "row_exact_kfold", "completed")
dir.create(file.path(primary_grouped_root, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(primary_row_root, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "kfold_firm"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "row_exact_kfold"), recursive = TRUE, showWarnings = FALSE)

con <- file(file.path(root, "kfold_firm", "LATEST_COMPLETED_RUN.txt"), open = "wb")
writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
writeBin(charToRaw(paste0(primary_grouped_root, "\n")), con)
close(con)
writeLines(paste0("ï»¿", primary_row_root), file.path(root, "row_exact_kfold", "LATEST_COMPLETED_RUN.txt"), useBytes = TRUE)

model_rows <- data.frame(
  Model_ID = c("M01", "M02"),
  Model_Name = c("Model 1", "Model 2"),
  Heterogeneity_Variant = c("Pooled (Industry + Year FE)", "Firm RE (Random Intercept + Year FE)"),
  stringsAsFactors = FALSE
)

write.csv(
  transform(model_rows, Weight_KFold = c(0.8, 0.2), Rank_KFold = c(1L, 2L)),
  file.path(primary_grouped_root, "tables", "table_winsor_kfold_weights_ex_post.csv"),
  row.names = FALSE
)
write.csv(
  transform(model_rows, Weight_KFold = c(0.7, 0.3), Rank_KFold = c(1L, 2L)),
  file.path(primary_grouped_root, "tables", "table_winsor_kfold_weights_no_lookahead.csv"),
  row.names = FALSE
)
write.csv(
  transform(model_rows, weight_row_exact_kfold = c(0.25, 0.75), rank_row_exact_kfold = c(2L, 1L)),
  file.path(primary_row_root, "tables", "table_winsor_row_exact_kfold_weights_ex_post.csv"),
  row.names = FALSE
)
write.csv(
  transform(model_rows, weight_row_exact_kfold = c(0.35, 0.65), rank_row_exact_kfold = c(2L, 1L)),
  file.path(primary_row_root, "tables", "table_winsor_row_exact_kfold_weights_no_lookahead.csv"),
  row.names = FALSE
)

task_specs <- data.frame(
  Task_Key = c("g_ep_m01", "g_ep_m02", "r_ep_m01", "r_ep_m02", "g_rt_m01", "g_rt_m02", "r_rt_m01", "r_rt_m02"),
  Validation_Scheme = rep(c("grouped_firm_kfold", "row_exact_kfold"), each = 2L, times = 2L),
  Target_Space = rep(c("ex_post", "real_time"), each = 4L),
  Model_ID = rep(c("M01", "M02"), times = 4L),
  Heterogeneity_Variant = rep(model_rows$Heterogeneity_Variant, times = 4L),
  stringsAsFactors = FALSE
)
task_specs$Model_Name <- ifelse(task_specs$Model_ID == "M01", "Model 1", "Model 2")
task_specs$Sample_Group <- "main_common"
task_specs$Fold_ID <- 1L
task_specs$K <- 1L
task_specs$Required <- TRUE
task_specs$result_path <- file.path(cache_dir, paste0(task_specs$Task_Key, ".rds"))

make_audit <- function(spec) {
  data.frame(
    validation_scheme = spec$Validation_Scheme,
    target_space = spec$Target_Space,
    fold_id = 1L,
    model_id = spec$Model_ID,
    heterogeneity_variant = spec$Heterogeneity_Variant,
    variable = c("TA_scaled", "dREV_scaled"),
    preprocessing_step = c("winsorization", "standardization"),
    train_cutoff_p01 = c(-1, NA),
    train_cutoff_p99 = c(1, NA),
    train_mean = c(NA, 0.1),
    train_sd = c(NA, 1.2),
    delta_train_vs_global_p01 = c(0.01, NA),
    delta_train_vs_global_p99 = c(0.02, NA),
    share_test_capped_low = c(0, NA),
    share_test_capped_high = c(0.1, NA),
    stringsAsFactors = FALSE
  )
}

make_diag <- function(spec) {
  data.frame(
    Validation_Scheme = spec$Validation_Scheme,
    Target_Space = spec$Target_Space,
    Sample_Group = "main_common",
    Fold_ID = 1L,
    Model_ID = spec$Model_ID,
    Model_Name = spec$Model_Name,
    Heterogeneity_Variant = spec$Heterogeneity_Variant,
    Completed = TRUE,
    Max_Rhat = 1.0,
    Min_ESS_Bulk = 500,
    Min_ESS_Tail = 500,
    ESS_Warning = FALSE,
    Divergences = 0L,
    Treedepth_Warnings = 0L,
    Runtime_Seconds = 1,
    stringsAsFactors = FALSE
  )
}

make_obs <- function(spec) {
  if (identical(spec$Validation_Scheme, "grouped_firm_kfold")) {
    data.frame(
      Target_Space = spec$Target_Space,
      Sample_Group = "main_common",
      Fold_ID = 1L,
      Model_ID = spec$Model_ID,
      Model_Name = spec$Model_Name,
      Heterogeneity_Variant = spec$Heterogeneity_Variant,
      company = c("A", "B", "C"),
      year = 2020:2022,
      lpd_obs = if (spec$Model_ID == "M01") c(-1.0, -1.1, -1.2) else c(-0.7, -0.8, -0.9),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      target_space = spec$Target_Space,
      sample_group = "main_common",
      fold_id = 1L,
      model_id = spec$Model_ID,
      model_name = spec$Model_Name,
      heterogeneity_variant = spec$Heterogeneity_Variant,
      observation_id = paste0("obs", 1:3),
      company = c("A", "B", "C"),
      year = 2020:2022,
      log_predictive_density = if (spec$Model_ID == "M01") c(-1.4, -1.3, -1.2) else c(-0.5, -0.6, -0.7),
      primary_row_target_inclusion = TRUE,
      new_company_in_row_fold = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

for (i in seq_len(nrow(task_specs))) {
  spec <- task_specs[i, , drop = FALSE]
  saveRDS(
    list(
      fold_diag = make_diag(spec),
      preprocessing_audit = make_audit(spec),
      obs_scores = make_obs(spec)
    ),
    spec$result_path
  )
}

write.csv(task_specs, file.path(tables_dir, "table_se08_fold_local_preprocessing_task_manifest.csv"), row.names = FALSE)
write.csv(
  data.frame(
    Task_Key = task_specs$Task_Key,
    status = "SUCCESS",
    Required = TRUE,
    result_path = task_specs$result_path,
    stringsAsFactors = FALSE
  ),
  file.path(tables_dir, "table_se08_fold_local_preprocessing_task_status.csv"),
  row.names = FALSE
)

old_root <- Sys.getenv("ACCRUAL_OUTPUT_ROOT", unset = NA_character_)
old_input <- Sys.getenv("ACCRUAL_INPUT_WINSOR_ROOT", unset = NA_character_)
old_disable <- Sys.getenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG", unset = NA_character_)
on.exit({
  if (is.na(old_root)) Sys.unsetenv("ACCRUAL_OUTPUT_ROOT") else Sys.setenv(ACCRUAL_OUTPUT_ROOT = old_root)
  if (is.na(old_input)) Sys.unsetenv("ACCRUAL_INPUT_WINSOR_ROOT") else Sys.setenv(ACCRUAL_INPUT_WINSOR_ROOT = old_input)
  if (is.na(old_disable)) Sys.unsetenv("ACCRUAL_DISABLE_PHASE_RUNTIME_LOG") else Sys.setenv(ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = old_disable)
}, add = TRUE)
Sys.setenv(
  ACCRUAL_OUTPUT_ROOT = root,
  ACCRUAL_INPUT_WINSOR_ROOT = root,
  ACCRUAL_DISABLE_PHASE_RUNTIME_LOG = "TRUE"
)

source("scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R")

expected_outputs <- file.path(tables_dir, c(
  "table_se08_fold_local_preprocessing_audit.csv",
  "table_se08_fold_local_cutoff_summary.csv",
  "table_se08_fold_local_standardization_summary.csv",
  "table_se08_grouped_fold_local_observation_scores.csv",
  "table_se08_row_fold_local_observation_scores.csv",
  "table_se08_grouped_fold_local_model_scores.csv",
  "table_se08_row_fold_local_model_scores.csv",
  "table_se08_grouped_fold_local_weights_ex_post.csv",
  "table_se08_grouped_fold_local_weights_no_lookahead.csv",
  "table_se08_row_fold_local_weights_ex_post.csv",
  "table_se08_row_fold_local_weights_no_lookahead.csv",
  "table_se08_fold_local_vs_global_weight_comparison.csv",
  "table_se08_fold_local_vs_global_firmre_shift_summary.csv",
  "table_se08_fold_local_vs_global_top_model_comparison.csv",
  "table_se08_fold_local_sensitivity_decision.csv"
))
missing <- expected_outputs[!file.exists(expected_outputs)]
if (length(missing)) stop("SE08C smoke missing expected outputs: ", paste(missing, collapse = ", "))

weight_check <- read.csv(file.path(tables_dir, "table_se08_grouped_fold_local_weights_ex_post.csv"), stringsAsFactors = FALSE)
required_weight_cols <- c(
  "Stacking_Method_Fold_Local",
  "Stacking_Fallback_Used",
  "Stacking_Convergence_Code",
  "Stacking_Objective",
  "Singleton_Objective",
  "Stacking_Context"
)
missing_weight_cols <- setdiff(required_weight_cols, names(weight_check))
if (length(missing_weight_cols)) {
  stop("SE08C smoke grouped weight table missing stacking metadata: ", paste(missing_weight_cols, collapse = ", "))
}
if (!all(weight_check$Stacking_Method_Fold_Local == "fast_exact")) {
  stop("SE08C smoke should use fast_exact stacking by default.")
}
if (abs(sum(weight_check$Weight_Fold_Local) - 1) > 1e-8) {
  stop("SE08C smoke grouped fast_exact weights must sum to 1.")
}

decision <- read.csv(file.path(tables_dir, "table_se08_fold_local_sensitivity_decision.csv"), stringsAsFactors = FALSE)
if (!nrow(decision)) stop("SE08C smoke decision table is empty.")

collect_manifest <- file.path(root, "sensitivity", "fold_local_preprocessing", "logs", "se08_fold_local_preprocessing_collect_manifest.csv")
if (!file.exists(collect_manifest)) stop("SE08C smoke missing collect manifest.")

cat("test_se08c_base_r_behavioral.R passed\n")
