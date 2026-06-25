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

artifact_spec <- accrual_section47_reviewer_artifact_spec(output_root)

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
  write_csv_safely(artifact_spec, manifest_path, row.names = FALSE)
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
  "This package assembles reviewer-facing diagnostics for denominator mechanics, economic-validity top-tail membership checks, and optional temporal-dependence robustness.",
  "",
  paste0("Generated at: ", Sys.time()),
  paste0("Output root: ", output_root),
  "",
  "## Included Artifact Classes",
  "- di04_denominator: canonical SD(mu) denominator distribution, capped Jaccard, z-est versus z-pred comparison, and decision.",
  "- di05_economic_validity: canonical RowOnlyTop5, GroupedOnlyTop5, and CommonTop5 supplementary economic-validity outputs.",
  "- temporal_dependence_optional: gated AR(1) temporal-dependence robustness outputs, included when ACCRUAL_RUN_TEMPORAL_ROBUSTNESS=TRUE has been run.",
  "",
  "## Execution Status",
  paste0("- Required artifacts present: ", sum(artifact_spec$exists), " / ", nrow(artifact_spec))
)

write_csv_safely(checklist, file.path(tables_dir, "table_4_7_execution_checklist.csv"), row.names = FALSE)
write_csv_safely(artifact_spec, file.path(tables_dir, "reviewer_required_artifact_manifest.csv"), row.names = FALSE)
writeLines(summary_lines, file.path(notes_dir, "section4_7_results_summary.md"), useBytes = TRUE)

cat("[SUCCESS] Reviewer package written under ", reviewer_root, "\n", sep = "")
phase_end("di07", "Assemble Section 4.7 reviewer package")
