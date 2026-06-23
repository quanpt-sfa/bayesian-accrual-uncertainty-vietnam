source("scripts/ma00_setup.R")

profile_name <- "full_clean_production_5w4c"
profile_path <- file.path("run_profiles", "run_full_clean_production_5w4c.ps1")
if (!file.exists(profile_path)) stop("Missing production run profile: ", profile_path)

expected <- accrual_run_profile_config(profile_name)
profile_text <- paste(readLines(profile_path, warn = FALSE), collapse = "\n")

for (name in names(expected)) {
  assignment <- paste0("$env:", name, " = \"", expected[[name]], "\"")
  if (!grepl(assignment, profile_text, fixed = TRUE)) {
    stop("Production run profile assignment differs from ma00 helper for ", name)
  }
}

with_profile_env <- function(expr) {
  old_values <- Sys.getenv(names(expected), unset = NA_character_)
  names(old_values) <- names(expected)
  on.exit({
    for (nm in names(old_values)) {
      if (is.na(old_values[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, as.list(stats::setNames(old_values[[nm]], nm)))
      }
    }
  }, add = TRUE)
  do.call(Sys.setenv, as.list(expected))
  force(expr)
}

as_int <- function(name) as.integer(expected[[name]])
as_num <- function(name) as.numeric(expected[[name]])

with_profile_env({
  parallel_cfg <- accrual_model_parallel_config(
    cores_per_fit = as_int("ACCRUAL_BASELINE_CORES"),
    context = "production profile static test"
  )
  if (!identical(parallel_cfg$workers, as_int("ACCRUAL_MODEL_PARALLEL_WORKERS"))) {
    stop("production profile worker count does not resolve through ma00.")
  }
  if (!identical(parallel_cfg$total_core_budget, as_int("ACCRUAL_TOTAL_CORE_BUDGET"))) {
    stop("production profile total core budget does not resolve through ma00.")
  }

  baseline_cfg <- accrual_sampler_config("baseline")
  if (!identical(baseline_cfg$chains, as_int("ACCRUAL_BASELINE_CHAINS")) ||
      !identical(baseline_cfg$cores, as_int("ACCRUAL_BASELINE_CORES")) ||
      !identical(baseline_cfg$iter, as_int("ACCRUAL_BASELINE_ITER")) ||
      !identical(baseline_cfg$warmup, as_int("ACCRUAL_BASELINE_WARMUP")) ||
      !isTRUE(all.equal(baseline_cfg$adapt_delta, as_num("ACCRUAL_BASELINE_ADAPT_DELTA"))) ||
      !identical(baseline_cfg$max_treedepth, as_int("ACCRUAL_BASELINE_MAX_TREEDEPTH"))) {
    stop("baseline sampler config does not resolve to production profile values.")
  }

  grouped_cfg <- accrual_kfold_config("grouped_firm")
  row_cfg <- accrual_kfold_config("row")
  for (cfg_name in c("grouped_cfg", "row_cfg")) {
    cfg <- get(cfg_name)
    prefix <- if (identical(cfg_name, "grouped_cfg")) "ACCRUAL_KFOLD_FIRM" else "ACCRUAL_ROW_KFOLD"
    if (!identical(cfg$K, as_int(paste0(prefix, "_K"))) ||
        !identical(cfg$chains, as_int(paste0(prefix, "_CHAINS"))) ||
        !identical(cfg$cores, as_int(paste0(prefix, "_CORES"))) ||
        !identical(cfg$iter, as_int(paste0(prefix, "_ITER"))) ||
        !identical(cfg$warmup, as_int(paste0(prefix, "_WARMUP"))) ||
        !isTRUE(all.equal(cfg$adapt_delta, as_num(paste0(prefix, "_ADAPT_DELTA")))) ||
        !identical(cfg$max_treedepth, as_int(paste0(prefix, "_MAX_TREEDEPTH")))) {
      stop(cfg_name, " does not resolve to production profile values.")
    }
  }

  sensitivity_cfg <- accrual_sampler_config("sensitivity")
  if (!identical(sensitivity_cfg$chains, as_int("ACCRUAL_SENS_CHAINS")) ||
      !identical(sensitivity_cfg$cores, as_int("ACCRUAL_SENS_CORES")) ||
      !identical(sensitivity_cfg$iter, as_int("ACCRUAL_SENS_ITER")) ||
      !identical(sensitivity_cfg$warmup, as_int("ACCRUAL_SENS_WARMUP")) ||
      !isTRUE(all.equal(sensitivity_cfg$adapt_delta, as_num("ACCRUAL_SENS_ADAPT_DELTA"))) ||
      !identical(sensitivity_cfg$max_treedepth, as_int("ACCRUAL_SENS_MAX_TREEDEPTH"))) {
    stop("sensitivity sampler config does not resolve to production profile values.")
  }
})

cat("test_run_profile_production_5w4c_static.R passed\n")
