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
    "di04_weights", "di04_shift", "di04_exclusions", "di04_note",
    "di05_distribution", "di05_correlation", "di05_decomposition", "di05_profile", "di05_capping", "di05_note",
    "di06_results", "di06_framework", "di06_counts", "di06_means", "di06_note",
    "si05_rep_results", "si05_grid_summary", "si06_mechanism_summary", "si06_note"
  ),
  source_path = c(
    file.path(output_root, "diagnostics", "table_full_vs_strict_stacking_weights.csv"),
    file.path(output_root, "diagnostics", "table_full_vs_strict_firmre_shift.csv"),
    file.path(output_root, "diagnostics", "table_strict_model_exclusions.csv"),
    file.path(output_root, "diagnostics", "full_vs_strict_stacking_note.md"),
    file.path(output_root, "diagnostics", "table_denominator_distribution_by_target.csv"),
    file.path(output_root, "diagnostics", "table_denominator_rank_correlation.csv"),
    file.path(output_root, "diagnostics", "table_z_est_decomposition_jaccard.csv"),
    file.path(output_root, "diagnostics", "table_top5_membership_denominator_profile.csv"),
    file.path(output_root, "diagnostics", "table_z_est_denominator_capping_sensitivity.csv"),
    file.path(output_root, "diagnostics", "denominator_diagnostics_note.md"),
    file.path(output_root, "validation", "top5_membership", "table_outcome_validation_top5_membership.csv"),
    file.path(output_root, "validation", "top5_membership", "table_outcome_validation_preinterpretation_matrix.csv"),
    file.path(output_root, "validation", "top5_membership", "table_outcome_validation_n_by_membership.csv"),
    file.path(output_root, "validation", "top5_membership", "table_outcome_validation_marginal_means.csv"),
    file.path(output_root, "validation", "top5_membership", "outcome_validation_top5_note.md"),
    file.path(output_root, "simulation", "lmer_temporal_dependence", "tables", "table_lmer_temporal_dependence_rep_results.csv"),
    file.path(output_root, "simulation", "lmer_temporal_dependence", "tables", "table_lmer_temporal_dependence_grid_summary.csv"),
    file.path(output_root, "simulation", "lmer_temporal_dependence", "tables", "table_temporal_dependence_mechanism_summary.csv"),
    file.path(output_root, "simulation", "lmer_temporal_dependence", "notes", "temporal_dependence_mechanism_note.md")
  ),
  artifact_class = c(
    rep("di04_full_vs_strict", 4),
    rep("di05_denominator", 6),
    rep("di06_outcome_validation", 5),
    rep("si05_si06_temporal_dependence", 4)
  ),
  required = TRUE,
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

for (i in seq_len(nrow(artifact_spec))) {
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
  "This package assembles the reviewer-required diagnostics for exact-KFold model-space sensitivity, denominator mechanics, membership-based outcome validation, and temporal-dependence simulation.",
  "",
  paste0("Generated at: ", Sys.time()),
  paste0("Output root: ", output_root),
  "",
  "## Included Artifact Classes",
  "- di04_full_vs_strict: full admissible versus strict clean model-space stacking weights.",
  "- di05_denominator: SD(mu) denominator distribution, rank correlation, decomposition, and capping sensitivity.",
  "- di06_outcome_validation: RowOnlyTop5, GroupedOnlyTop5, and CommonTop5 supplementary outcome validation.",
  "- si05_si06_temporal_dependence: AR(1)/persistent-shock mechanism simulation outputs.",
  "",
  "## Execution Status",
  paste0("- Required artifacts present: ", sum(artifact_spec$exists), " / ", nrow(artifact_spec))
)

write.csv(checklist, file.path(tables_dir, "table_4_7_execution_checklist.csv"), row.names = FALSE)
write.csv(artifact_spec, file.path(tables_dir, "reviewer_required_artifact_manifest.csv"), row.names = FALSE)
writeLines(summary_lines, file.path(notes_dir, "section4_7_results_summary.md"), useBytes = TRUE)

cat("[SUCCESS] Reviewer package written under ", reviewer_root, "\n", sep = "")
phase_end("di07", "Assemble Section 4.7 reviewer package")
