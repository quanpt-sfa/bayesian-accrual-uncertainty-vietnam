source("scripts/ma00_setup.R")

txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

sampler_config <- txt("scripts/config/sampler_config.R")
ma06 <- txt("scripts/ma06_prior_predictive_checks.R")
se01 <- txt("scripts/sensitivity/se01_prior_predictive.R")

if (!grepl('"prior_predictive"', sampler_config, fixed = TRUE) ||
    !grepl("ACCRUAL_PRIOR_PRED_CHAINS", sampler_config, fixed = TRUE) ||
    !grepl("ACCRUAL_PRIOR_PRED_CORES", sampler_config, fixed = TRUE) ||
    !grepl("ACCRUAL_PRIOR_PRED_REFRESH", sampler_config, fixed = TRUE)) {
  stop("ma00_setup.R must define the centralized prior_predictive sampler profile.")
}

prior_cfg <- accrual_sampler_config("prior_predictive")
required_prior_fields <- c(
  "chains", "cores", "iter", "warmup", "adapt_delta", "max_treedepth",
  "refresh", "backend", "sampler_profile", "config_source"
)
missing_prior_fields <- setdiff(required_prior_fields, names(prior_cfg))
if (length(missing_prior_fields)) {
  stop("prior_predictive sampler config missing field(s): ", paste(missing_prior_fields, collapse = ", "))
}
if (!is.finite(prior_cfg$chains) || prior_cfg$chains < 1L) stop("prior_predictive chains must be >= 1.")
if (!is.finite(prior_cfg$cores) || prior_cfg$cores < 1L) stop("prior_predictive cores must be >= 1.")
if (!is.finite(prior_cfg$iter) || !is.finite(prior_cfg$warmup) || !(prior_cfg$warmup < prior_cfg$iter)) {
  stop("prior_predictive warmup must be smaller than iter.")
}
if (prior_cfg$cores > prior_cfg$chains) stop("prior_predictive cores should not exceed chains unless explicitly redesigned.")
if (!identical(prior_cfg$backend, "rstan")) stop("prior_predictive backend must be rstan.")
if (!grepl("scripts/ma00_setup.R", prior_cfg$config_source, fixed = TRUE)) {
  stop("prior_predictive config_source must point to scripts/ma00_setup.R.")
}

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
