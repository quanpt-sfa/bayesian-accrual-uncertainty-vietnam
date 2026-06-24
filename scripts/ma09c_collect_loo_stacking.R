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

collector_contract <- data.frame(
  output = c("table_loo_comparison_winsor_corrected.csv", "table_stacking_weights_ex_post_winsor_corrected.csv",
             "table_stacking_weights_no_lookahead_winsor_corrected.csv"),
  owner = "ma09c_collect_loo_stacking.R",
  evidence_role = "secondary_psis_loo",
  task_manifest_rows = nrow(manifest),
  stringsAsFactors = FALSE
)
write.csv(collector_contract, file.path(tables_dir, "table_ma09_collect_contract.csv"), row.names = FALSE)
message("ma09c collector owns shared PSIS/LOO outputs; no brms fitting occurs in collector.")
phase_end("ma09c", "Collect LOO stacking outputs")
