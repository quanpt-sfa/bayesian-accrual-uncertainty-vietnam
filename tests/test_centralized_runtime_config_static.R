source("scripts/ma00_setup.R")

txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

ma00 <- txt("scripts/ma00_setup.R")
ma06 <- txt("scripts/ma06_prior_predictive_checks.R")
se01 <- txt("scripts/sensitivity/se01_prior_predictive.R")

if (!grepl('"prior_predictive"', ma00, fixed = TRUE) ||
    !grepl("ACCRUAL_PRIOR_PRED_CHAINS", ma00, fixed = TRUE) ||
    !grepl("ACCRUAL_PRIOR_PRED_CORES", ma00, fixed = TRUE) ||
    !grepl("ACCRUAL_PRIOR_PRED_REFRESH", ma00, fixed = TRUE)) {
  stop("ma00_setup.R must define the centralized prior_predictive sampler profile.")
}

prior_cfg <- accrual_sampler_config("prior_predictive")
if (!identical(prior_cfg$chains, 2L)) stop("Default prior_predictive chains must be 2.")
if (!identical(prior_cfg$cores, prior_cfg$chains)) stop("Default prior_predictive cores must equal chains.")
if (!identical(prior_cfg$iter, 1000L)) stop("Default prior_predictive iter must be 1000.")
if (!identical(prior_cfg$warmup, 500L)) stop("Default prior_predictive warmup must be 500.")
if (!(prior_cfg$warmup < prior_cfg$iter)) stop("Default prior_predictive warmup must be smaller than iter.")
if (!identical(prior_cfg$refresh, 0L)) stop("Default prior_predictive refresh must be 0.")
if (!identical(prior_cfg$backend, "rstan")) stop("Default prior_predictive backend must be rstan.")

if (grepl("chains\\s*<-\\s*2", ma06, perl = TRUE)) {
  stop("ma06 must not hard-code chains <- 2.")
}
if (grepl("cores\\s*<-\\s*env_int\\(\"ACCRUAL_BASELINE_CORES", ma06, perl = TRUE)) {
  stop("ma06 must not read ACCRUAL_BASELINE_CORES for prior predictive sampling.")
}
for (fragment in c(
  'prior_cfg <- accrual_sampler_config("prior_predictive")',
  "prior_cfg$chains",
  "prior_cfg$cores",
  "prior_cfg$iter",
  "prior_cfg$warmup",
  "prior_cfg$refresh"
)) {
  if (!grepl(fragment, ma06, fixed = TRUE)) {
    stop("ma06 missing centralized prior predictive config fragment: ", fragment)
  }
}

if (!grepl('prior_cfg <- accrual_sampler_config("prior_predictive")', se01, fixed = TRUE) ||
    !grepl("prior_cfg$refresh", se01, fixed = TRUE)) {
  stop("se01 prior predictive sampling must use centralized prior_predictive config.")
}

blocked <- tryCatch({
  validate_rstan_cores(.Machine$integer.max, 1L, "static impossible core test")
  FALSE
}, error = function(e) grepl("[BLOCKER]", conditionMessage(e), fixed = TRUE))
if (!isTRUE(blocked)) stop("validate_rstan_cores must block impossible core settings.")

registry_path <- tempfile(fileext = ".csv")
write_execution_config_registry(registry_path)
registry <- read.csv(registry_path, stringsAsFactors = FALSE)
need <- c("chains", "cores", "iter", "warmup", "adapt_delta", "max_treedepth", "refresh", "backend")
missing <- need[!vapply(need, function(param) {
  any(registry$Scope == "prior_predictive" & registry$Parameter == param)
}, logical(1))]
if (length(missing)) {
  stop("Execution config registry missing prior_predictive parameter(s): ", paste(missing, collapse = ", "))
}

cat("test_centralized_runtime_config_static.R passed\n")
