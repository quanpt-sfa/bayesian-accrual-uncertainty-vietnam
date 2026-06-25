txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

script_paths <- list.files("scripts", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
script_paths <- gsub("\\\\", "/", script_paths)
active_scripts <- script_paths[
  !grepl("^scripts/archive/", script_paths) &
    !grepl("^scripts/legacy_diagnostics/", script_paths)
]

if (!length(active_scripts)) stop("No active scripts found under scripts/.")

ma00_path <- "scripts/ma00_setup.R"
ma00 <- txt(ma00_path)
for (fragment in c(
  "write_csv_safely <- function(x, path, row.names = FALSE, ...)",
  "dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)",
  "write.csv(x, path, row.names = row.names, ...)",
  "invisible(path)"
)) {
  if (!grepl(fragment, ma00, fixed = TRUE)) {
    stop("ma00 missing central safe CSV writer fragment: ", fragment)
  }
}

direct_write_hits <- unlist(lapply(active_scripts, function(path) {
  lines <- readLines(path, warn = FALSE)
  hit_idx <- grep("write\\.csv\\(", lines)
  if (!length(hit_idx)) return(character())
  if (identical(path, ma00_path)) {
    helper_start <- grep("^write_csv_safely <- function\\(", lines)
    helper_hits <- hit_idx[
      length(helper_start) == 1L &
        hit_idx > helper_start &
        hit_idx <= helper_start + 4L
    ]
    hit_idx <- setdiff(hit_idx, helper_hits)
  }
  if (!length(hit_idx)) return(character())
  paste0(path, ":", hit_idx, ": ", trimws(lines[hit_idx]))
}), use.names = FALSE)

if (length(direct_write_hits)) {
  stop("Active scripts must not call write.csv() directly outside write_csv_safely():\n",
       paste(direct_write_hits, collapse = "\n"))
}

local_helper_hits <- unlist(lapply(active_scripts, function(path) {
  if (identical(path, ma00_path)) return(character())
  lines <- readLines(path, warn = FALSE)
  hit_idx <- grep("\\bwrite_csv_[A-Za-z0-9_]*\\s*<-\\s*function\\(", lines, perl = TRUE)
  if (!length(hit_idx)) return(character())
  paste0(path, ":", hit_idx, ": ", trimws(lines[hit_idx]))
}), use.names = FALSE)

if (length(local_helper_hits)) {
  stop("Active scripts must use central write_csv_safely(), not local CSV helpers:\n",
       paste(local_helper_hits, collapse = "\n"))
}

coverage_required <- c(
  "scripts/ma01_setup_and_registry.R",
  "scripts/ma02_build_common_sample.R",
  "scripts/ma03_audit_data_integrity.R",
  "scripts/ma04_define_named_models.R",
  "scripts/ma05_winsorize_common_samples.R",
  "scripts/ma06_prior_predictive_checks.R",
  "scripts/ma07a_fit_brms_named_models.R",
  "scripts/ma07b_extract_brms_fit_outputs_workers.R",
  "scripts/ma07c_collect_brms_fit_outputs.R",
  "scripts/ma08_mcmc_diagnostics.R",
  "scripts/ma09_loo_stacking.R",
  "scripts/ma09a_plan_loo_savepars_refits.R",
  "scripts/ma09b_fit_loo_savepars_refits.R",
  "scripts/ma09c_collect_loo_stacking.R"
)
missing_required <- setdiff(coverage_required, active_scripts)
if (length(missing_required)) {
  stop("Repo-wide CSV writer test did not cover required script(s): ",
       paste(missing_required, collapse = ", "))
}

for (dir in c("scripts/diagnostics", "scripts/simulation", "scripts/robustness", "scripts/sensitivity")) {
  dir_scripts <- active_scripts[startsWith(active_scripts, paste0(dir, "/"))]
  if (!length(dir_scripts)) stop("Repo-wide CSV writer test did not cover directory: ", dir)
}

writer_usage <- active_scripts[vapply(active_scripts, function(path) {
  grepl("write_csv_safely(", txt(path), fixed = TRUE)
}, logical(1))]
if (!length(writer_usage)) stop("No active scripts use write_csv_safely(); migration test is not meaningful.")

cat("test_repo_wide_csv_writer_static.R passed\n")
