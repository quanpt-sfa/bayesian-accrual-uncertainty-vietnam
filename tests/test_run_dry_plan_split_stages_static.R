dry <- system2("Rscript", c("run.R", "all", "--dry-run"), stdout = TRUE, stderr = TRUE)
dry_text <- paste(dry, collapse = "\n")

ordered_ids <- c(
  "ma07a", "ma07b", "ma07c",
  "ma09a", "ma09b", "ma09c",
  "ma12a", "ma12b", "ma12c",
  "ma13a", "ma13b", "ma13c",
  "se02a", "se02b", "se02c",
  "si03a", "si03b", "si03c",
  "si04a", "si04b", "si04c",
  "di08a", "di08b", "di08c"
)
positions <- vapply(ordered_ids, function(id) regexpr(paste0(id, " "), dry_text, fixed = TRUE)[1], numeric(1))
missing <- ordered_ids[positions < 0]
if (length(missing)) stop("run.R all --dry-run missing split stage(s): ", paste(missing, collapse = ", "))

groups <- list(
  c("ma07a", "ma07b", "ma07c"),
  c("ma09a", "ma09b", "ma09c"),
  c("ma12a", "ma12b", "ma12c"),
  c("ma13a", "ma13b", "ma13c"),
  c("se02a", "se02b", "se02c"),
  c("si03a", "si03b", "si03c"),
  c("si04a", "si04b", "si04c"),
  c("di08a", "di08b", "di08c")
)
for (group in groups) {
  pos <- positions[group]
  if (is.unsorted(pos)) stop("Split stages out of order: ", paste(group, collapse = " -> "))
}

obsolete_stage_lines <- c(
  "ma09 scripts/ma09_loo_stacking.R",
  "ma12 scripts/ma12_grouped_kfold_firm.R",
  "ma13 scripts/ma13_row_level_exact_kfold.R",
  "se02 scripts/sensitivity/se02_refit_prior_scenarios.R",
  "si03 scripts/simulation/si03_brms_leakage_confirmation.R",
  "si04 scripts/simulation/si04_brms_parameter_recovery.R",
  "di08 scripts/diagnostics/di08_mcmc_sampler_calibration.R"
)
hits <- obsolete_stage_lines[vapply(obsolete_stage_lines, grepl, logical(1), x = dry_text, fixed = TRUE)]
if (length(hits)) stop("Dry-run plan still includes obsolete mixed fit/collect stage(s): ", paste(hits, collapse = ", "))

for (id in c("ma07a", "ma07b", "ma09b", "ma12b", "ma13b", "se02b", "si03b", "si04b", "di08b")) {
  line <- dry[grepl(paste0(id, " "), dry, fixed = TRUE)]
  if (!length(line) || !grepl("heavy", line, fixed = TRUE)) stop(id, " must be marked heavy in dry-run plan.")
}
for (id in c("ma07c", "ma09c", "ma12c", "ma13c", "se02c", "si03c", "si04c", "di08c")) {
  line <- dry[grepl(paste0(id, " "), dry, fixed = TRUE)]
  if (!length(line) || !grepl("requires artifacts", line, fixed = TRUE)) stop(id, " must be marked requires artifacts in dry-run plan.")
}

cat("test_run_dry_plan_split_stages_static.R passed\n")
