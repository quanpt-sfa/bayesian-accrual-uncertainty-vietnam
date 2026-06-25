txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

parse_file <- function(path) {
  if (!file.exists(path)) stop("Missing parse target: ", path)
  tryCatch(parse(path), error = function(e) stop("Parse failed for ", path, ": ", conditionMessage(e)))
}

extract_block <- function(text, start_marker, end_marker) {
  start <- regexpr(start_marker, text, fixed = TRUE)[1]
  end <- regexpr(end_marker, text, fixed = TRUE)[1]
  if (start < 0 || end < 0 || end <= start) stop("Could not isolate block: ", start_marker)
  substr(text, start, end - 1L)
}

extract_script_paths <- function(text) {
  hits <- regmatches(text, gregexpr('"scripts/[^"]+\\.R"', text, perl = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  unique(gsub('"', "", hits, fixed = TRUE))
}

phase_ids <- function(path) {
  body <- txt(path)
  hits <- regmatches(body, gregexpr('phase_begin\\("[^"]+"', body, perl = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  sub('phase_begin\\("', "", hits)
}

step_pairs <- function(text) {
  hits <- regmatches(text, gregexpr('step\\("[^"]+",\\s*"[^"]+"', text, perl = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) {
    return(data.frame(id = character(), path = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    id = sub('step\\("([^"]+)".*', "\\1", hits),
    path = sub('step\\("[^"]+",\\s*"([^"]+)".*', "\\1", hits),
    stringsAsFactors = FALSE
  )
}

run_text <- txt("run.R")
main_text <- extract_block(run_text, "main_steps <- list", "robustness_steps <- list")
reviewer_text <- extract_block(run_text, "reviewer_steps <- list", "diagnostics_steps <- list")
diagnostics_text <- extract_block(run_text, "diagnostics_steps <- list", "diagnostics_steps_for_all <- list")

all_referenced <- unique(c(
  "run.R",
  "scripts/ma17_export_tables_figures.R",
  extract_script_paths(main_text),
  extract_script_paths(reviewer_text),
  extract_script_paths(diagnostics_text)
))
for (path in all_referenced) parse_file(path)

for (stale in c(
  "scripts/diagnostics/di04_full_vs_strict_model_space_stacking.R",
  "scripts/diagnostics/di05_denominator_diagnostics_z_est.R",
  "scripts/diagnostics/di06_outcome_validation_top5_membership.R",
  "scripts/diagnostics/di06_temporal_dependence_robustness.R",
  "scripts/diagnostics/rv04_full_vs_strict_model_space_stacking.R",
  "scripts/diagnostics/rv05_legacy_denominator_diagnostics_z_est.R",
  "scripts/diagnostics/rv06_legacy_outcome_validation_top5_membership.R",
  "archive/legacy_diagnostics"
)) {
  if (grepl(stale, run_text, fixed = TRUE)) {
    stop("run.R must not reference stale diagnostic namespace path: ", stale)
  }
}

rv_in_scripts <- list.files("scripts/diagnostics", pattern = "^rv.*\\.R$", full.names = TRUE)
if (length(rv_in_scripts)) {
  stop("Legacy rv*.R scripts must not exist directly under scripts/diagnostics: ",
       paste(rv_in_scripts, collapse = ", "))
}

if (grepl("scripts/diagnostics/di09_temporal_dependence_robustness.R", main_text, fixed = TRUE)) {
  stop("Temporal robustness must not be a default main step.")
}
if (!grepl("scripts/diagnostics/di05_economic_validity_top_tail.R", main_text, fixed = TRUE)) {
  stop("Main pipeline must include canonical di05 economic-validity diagnostic.")
}
if (!grepl("scripts/diagnostics/di04_denominator_diagnostics.R", reviewer_text, fixed = TRUE) ||
    !grepl("scripts/diagnostics/di05_economic_validity_top_tail.R", reviewer_text, fixed = TRUE) ||
    !grepl("scripts/diagnostics/di09_temporal_dependence_robustness.R", reviewer_text, fixed = TRUE) ||
    !grepl("scripts/diagnostics/di07_section4_7_reviewer_package.R", reviewer_text, fixed = TRUE)) {
  stop("Reviewer branch must use only canonical di04/di05/di09/di07 diagnostics.")
}

main_review_text <- paste(main_text, reviewer_text, sep = "\n")
main_review_steps <- step_pairs(main_review_text)
for (id in unique(main_review_steps$id)) {
  mapped_paths <- unique(main_review_steps$path[main_review_steps$id == id])
  if (length(mapped_paths) > 1L) {
    stop("run.R step id maps to multiple script roles: ", id, " -> ", paste(mapped_paths, collapse = ", "))
  }
}

canonical_diag_paths <- unique(grep("^scripts/diagnostics/di[0-9][0-9a-z]*_.*\\.R$", all_referenced, value = TRUE))
canonical_prefix <- sub("^scripts/diagnostics/(di[0-9][0-9a-z]*).*", "\\1", canonical_diag_paths)
prefix_map <- split(basename(canonical_diag_paths), canonical_prefix)
for (prefix in names(prefix_map)) {
  if (length(unique(prefix_map[[prefix]])) > 1L) {
    stop("Duplicate canonical diagnostic script prefix ", prefix, ": ", paste(prefix_map[[prefix]], collapse = ", "))
  }
}

phase_map <- list()
for (path in canonical_diag_paths) {
  ids <- unique(phase_ids(path))
  for (id in ids) phase_map[[id]] <- unique(c(phase_map[[id]], basename(path)))
}
for (id in names(phase_map)) {
  if (length(phase_map[[id]]) > 1L) {
    stop("Duplicate canonical phase_begin id ", id, ": ", paste(phase_map[[id]], collapse = ", "))
  }
}

rv_paths <- unique(grep("^scripts/diagnostics/rv[0-9][0-9a-z]*_.*\\.R$", all_referenced, value = TRUE))
for (path in rv_paths) {
  ids <- phase_ids(path)
  if (!length(ids) || any(!grepl("^rv[0-9]", ids))) {
    stop("Reviewer legacy diagnostic must use rv* phase_begin id: ", path)
  }
}

di05 <- txt("scripts/diagnostics/di05_economic_validity_top_tail.R")
for (fragment in c(
  "table_top_tail_set_membership_exact_kfold.csv",
  "table_top_tail_set_counts_exact_kfold.csv",
  "table_top_tail_group_outcome_means.csv",
  "table_top_tail_group_economic_validity.csv",
  "table_top_tail_group_economic_validity_decision.csv",
  "table_top_tail_group_economic_validity_io_manifest.csv",
  "economic_validity_top_tail_reviewer_note.md",
  "get_lead_contiguous",
  "Primary exact-KFold magnitude evidence"
)) {
  if (!grepl(fragment, di05, fixed = TRUE)) {
    stop("di05 economic-validity script missing required fragment: ", fragment)
  }
}

di07 <- txt("scripts/diagnostics/di07_section4_7_reviewer_package.R")
for (fragment in c(
  "table_denominator_sd_mu_distribution.csv",
  "table_denominator_capped_jaccard.csv",
  "table_da_z_est_vs_z_pred_comparison.csv",
  "table_denominator_diagnostics_decision.csv",
  "table_top_tail_group_economic_validity.csv",
  "table_top_tail_group_economic_validity_decision.csv",
  "table_top_tail_group_outcome_means.csv",
  "table_temporal_dependence_firmre_premium.csv",
  "table_temporal_dependence_decision.csv"
)) {
  if (!grepl(fragment, di07, fixed = TRUE)) {
    stop("di07 reviewer package missing canonical artifact fragment: ", fragment)
  }
}

di04_body <- txt("scripts/diagnostics/di04_denominator_diagnostics.R")
di05_body <- txt("scripts/diagnostics/di05_economic_validity_top_tail.R")
di07_pos <- regexpr('step("di07"', reviewer_text, fixed = TRUE)[1]
if (di07_pos < 0) stop("reviewer_steps must include di07 reviewer package.")
di07_step_text <- substr(reviewer_text, di07_pos, nchar(reviewer_text))
di04_reviewer_pos <- regexpr("scripts/diagnostics/di04_denominator_diagnostics.R", reviewer_text, fixed = TRUE)[1]
di05_reviewer_pos <- regexpr("scripts/diagnostics/di05_economic_validity_top_tail.R", reviewer_text, fixed = TRUE)[1]
if (!(di04_reviewer_pos > 0 && di04_reviewer_pos < di07_pos)) {
  stop("reviewer_steps must run canonical di04 before di07 or explicitly satisfy all di04 package artifacts.")
}
if (!(di05_reviewer_pos > 0 && di05_reviewer_pos < di07_pos)) {
  stop("reviewer_steps must run canonical di05 before di07 or explicitly satisfy all di05 package artifacts.")
}

canonical_packaged <- c(
  "table_denominator_sd_mu_distribution.csv",
  "table_denominator_capped_jaccard.csv",
  "table_da_z_est_vs_z_pred_comparison.csv",
  "table_denominator_diagnostics_decision.csv",
  "table_top_tail_group_economic_validity.csv",
  "table_top_tail_group_economic_validity_decision.csv",
  "table_top_tail_group_outcome_means.csv",
  "table_top_tail_set_counts_exact_kfold.csv"
)
for (artifact in canonical_packaged) {
  produced_earlier <- (grepl(artifact, di04_body, fixed = TRUE) && di04_reviewer_pos < di07_pos) ||
    (grepl(artifact, di05_body, fixed = TRUE) && di05_reviewer_pos < di07_pos)
  declared_require <- grepl(artifact, di07_step_text, fixed = TRUE)
  if (!produced_earlier && !declared_require) {
    stop("di07 canonical artifact is neither produced by an earlier reviewer step nor declared as a di07 require: ", artifact)
  }
}

for (path in c("scripts/diagnostics/di04_denominator_diagnostics.R",
               "scripts/diagnostics/di05_economic_validity_top_tail.R",
               "scripts/diagnostics/di09_temporal_dependence_robustness.R")) {
  body <- txt(path)
  if (grepl('status = "pending"', body, fixed = TRUE)) {
    stop("IO manifest must not be built from a placeholder in: ", path)
  }
  if (!grepl("self_referential_manifest", body, fixed = TRUE)) {
    stop("IO manifest must mark its self row as self_referential_manifest in: ", path)
  }
}

cat("test_diagnostics_integration_static.R passed\n")
