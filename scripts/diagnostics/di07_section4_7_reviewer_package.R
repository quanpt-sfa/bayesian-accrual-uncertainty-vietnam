# -----------------------------------------------------------------------------
# Script: di07_section4_7_reviewer_package.R
# Purpose: Assemble Section 4.7 reviewer-required evidence package and fail when
#          required artifacts are missing.
#
# Intended use:
#   Rscript scripts/diagnostics/di07_section4_7_reviewer_package.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di07", "Assemble Section 4.7 reviewer package")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

reviewer_root <- file.path(output_root, "reviewer_v6")
tables_dir <- file.path(reviewer_root, "tables")
figures_dir <- file.path(reviewer_root, "figures")
notes_dir <- file.path(reviewer_root, "notes")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(notes_dir, recursive = TRUE, showWarnings = FALSE)

artifact_spec <- data.frame(
  artifact_id = c(
    "full_vs_strict_weights", "full_vs_strict_shift", "full_vs_strict_exclusions", "full_vs_strict_note",
    "denominator_sd_mu_distribution", "denominator_capped_jaccard", "da_z_est_vs_z_pred_comparison", "denominator_decision",
    "economic_validity", "economic_validity_decision", "economic_validity_means", "economic_validity_counts", "economic_validity_note",
    "temporal_firmre_premium", "temporal_decision"
  ),
  source_path = c(
    file.path(output_root, "diagnostics", "table_full_vs_strict_stacking_weights.csv"),
    file.path(output_root, "diagnostics", "table_full_vs_strict_firmre_shift.csv"),
    file.path(output_root, "diagnostics", "table_strict_model_exclusions.csv"),
    file.path(output_root, "diagnostics", "full_vs_strict_stacking_note.md"),
    file.path(output_root, "diagnostics", "table_denominator_sd_mu_distribution.csv"),
    file.path(output_root, "diagnostics", "table_denominator_capped_jaccard.csv"),
    file.path(output_root, "diagnostics", "table_da_z_est_vs_z_pred_comparison.csv"),
    file.path(output_root, "diagnostics", "table_denominator_diagnostics_decision.csv"),
    file.path(output_root, "diagnostics", "table_top_tail_group_economic_validity.csv"),
    file.path(output_root, "diagnostics", "table_top_tail_group_economic_validity_decision.csv"),
    file.path(output_root, "diagnostics", "table_top_tail_group_outcome_means.csv"),
    file.path(output_root, "diagnostics", "table_top_tail_set_counts_exact_kfold.csv"),
    file.path(output_root, "diagnostics", "economic_validity_top_tail_reviewer_note.md"),
    file.path(output_root, "simulation", "temporal_dependence", "tables", "table_temporal_dependence_firmre_premium.csv"),
    file.path(output_root, "simulation", "temporal_dependence", "tables", "table_temporal_dependence_decision.csv")
  ),
  artifact_class = c(
    rep("full_vs_strict_legacy_reviewer", 4),
    rep("di04_denominator", 4),
    rep("di05_economic_validity", 5),
    rep("temporal_dependence_optional", 2)
  ),
  required = c(rep(TRUE, 13), rep(FALSE, 2)),
  stringsAsFactors = FALSE
)

destination_for <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "tsv")) return(file.path(tables_dir, basename(path)))
  if (ext %in% c("png", "jpg", "jpeg", "pdf")) return(file.path(figures_dir, basename(path)))
  file.path(notes_dir, basename(path))
}

artifact_spec$exists <- file.exists(artifact_spec$source_path)
artifact_spec$file_size <- ifelse(artifact_spec$exists, file.info(artifact_spec$source_path)$size, NA_real_)
artifact_spec$mtime <- ifelse(artifact_spec$exists, as.character(file.info(artifact_spec$source_path)$mtime), NA_character_)
artifact_spec$destination_path <- vapply(artifact_spec$source_path, destination_for, character(1))

missing <- artifact_spec$required & !artifact_spec$exists
if (any(missing)) {
  manifest_path <- file.path(tables_dir, "reviewer_required_artifact_manifest.csv")
  write.csv(artifact_spec, manifest_path, row.names = FALSE)
  stop("[BLOCKER] Missing reviewer-required artifact(s): ",
       paste(artifact_spec$source_path[missing], collapse = "; "),
       ". Partial manifest written to: ", manifest_path)
}

for (i in which(artifact_spec$exists)) {
  file.copy(artifact_spec$source_path[[i]], artifact_spec$destination_path[[i]], overwrite = TRUE)
}

checklist <- artifact_spec %>%
  transmute(
    artifact_id = .data$artifact_id,
    artifact_class = .data$artifact_class,
    required = .data$required,
    source_path = .data$source_path,
    package_path = .data$destination_path,
    status = ifelse(.data$exists, "PASS", "FAIL")
  )

summary_lines <- c(
  "# Section 4.7 Reviewer Evidence Package",
  "",
  "This package assembles reviewer-facing diagnostics for exact-KFold model-space sensitivity, denominator mechanics, economic-validity top-tail membership checks, and optional temporal-dependence robustness.",
  "",
  paste0("Generated at: ", Sys.time()),
  paste0("Output root: ", output_root),
  "",
  "## Included Artifact Classes",
  "- full_vs_strict_legacy_reviewer: full admissible versus strict clean model-space stacking weights.",
  "- di04_denominator: canonical SD(mu) denominator distribution, capped Jaccard, z-est versus z-pred comparison, and decision.",
  "- di05_economic_validity: canonical RowOnlyTop5, GroupedOnlyTop5, and CommonTop5 supplementary economic-validity outputs.",
  "- temporal_dependence_optional: gated AR(1) temporal-dependence robustness outputs, included when ACCRUAL_RUN_TEMPORAL_ROBUSTNESS=TRUE has been run.",
  "",
  "## Execution Status",
  paste0("- Required artifacts present: ", sum(artifact_spec$exists), " / ", nrow(artifact_spec))
)

write.csv(checklist, file.path(tables_dir, "table_4_7_execution_checklist.csv"), row.names = FALSE)
write.csv(artifact_spec, file.path(tables_dir, "reviewer_required_artifact_manifest.csv"), row.names = FALSE)
writeLines(summary_lines, file.path(notes_dir, "section4_7_results_summary.md"), useBytes = TRUE)

cat("[SUCCESS] Reviewer package written under ", reviewer_root, "\n", sep = "")
phase_end("di07", "Assemble Section 4.7 reviewer package")
