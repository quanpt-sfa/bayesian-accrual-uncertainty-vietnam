txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

ma12a <- txt("scripts/ma12a_plan_grouped_kfold_firm.R")
ma12b <- txt("scripts/ma12b_fit_grouped_kfold_firm_workers.R")
ma12c <- txt("scripts/ma12c_collect_grouped_kfold_firm_scores.R")
ma13a <- txt("scripts/ma13a_plan_row_level_exact_kfold.R")
ma13b <- txt("scripts/ma13b_fit_row_level_exact_kfold_workers.R")
ma13c <- txt("scripts/ma13c_collect_row_level_exact_kfold_scores.R")
ma14 <- txt("scripts/ma14_construct_exact_kfold_DA.R")

required <- list(
  ma12a = c("accrual_exact_kfold_run_context(\"grouped_firm\"", "Kfold_Run_Root", "Config_Tag", "Run_ID"),
  ma12b = c("accrual_extract_brms_mcmc_diagnostics", "Max_Rhat", "Min_ESS_Bulk", "Min_ESS_Tail", "Divergences", "Treedepth_Warnings"),
  ma12c = c("LATEST_COMPLETED_RUN.txt", "run_config_manifest.csv", "Weight_KFold", "Kfold_Run_Root", "Completed_Run_Pin_Eligible"),
  ma13a = c("accrual_exact_kfold_run_context(\"row\"", "Row_KFold_Root", "Config_Tag", "Run_ID"),
  ma13b = c("accrual_extract_brms_mcmc_diagnostics", "Max_Rhat", "Min_ESS_Bulk", "Min_ESS_Tail", "Divergences", "Treedepth_Warnings"),
  ma13c = c("LATEST_COMPLETED_RUN.txt", "row_exact_kfold_run_manifest.csv", "weight_row_exact_kfold", "Row_KFold_Root", "Primary_Inference_Allowed"),
  ma14 = c("LATEST_COMPLETED_RUN.txt", "run_config_manifest.csv", "row_exact_kfold_run_manifest.csv", "Weight_KFold", "weight_row_exact_kfold")
)

bodies <- list(
  ma12a = ma12a,
  ma12b = ma12b,
  ma12c = ma12c,
  ma13a = ma13a,
  ma13b = ma13b,
  ma13c = ma13c,
  ma14 = ma14
)

for (nm in names(required)) {
  for (fragment in required[[nm]]) {
    if (!grepl(fragment, bodies[[nm]], fixed = TRUE)) {
      stop(nm, " missing exact-KFold ma14 contract fragment: ", fragment)
    }
  }
}

run_body <- txt("run.R")
if (grepl("scripts/ma12_grouped_kfold_firm.R", run_body, fixed = TRUE)) {
  stop("run.R must not call monolithic ma12_grouped_kfold_firm.R in the main pipeline.")
}

if (grepl("scripts/ma13_row_level_exact_kfold.R", run_body, fixed = TRUE)) {
  stop("run.R must not call monolithic ma13_row_level_exact_kfold.R in the main pipeline.")
}

cat("test_exact_kfold_ma14_contract_static.R passed\n")
