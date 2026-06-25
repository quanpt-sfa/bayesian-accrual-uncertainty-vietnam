txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
norm_path <- function(path) gsub("\\\\", "/", path)

script_files <- norm_path(list.files("scripts", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
script_files <- script_files[!grepl("^scripts/archive/", script_files)]
script_files <- script_files[!grepl("^scripts/renv/", script_files)]
script_files <- script_files[basename(script_files) != "activate.R"]

ma00 <- txt("scripts/ma00_setup.R")
for (fragment in c(
  "env_list <- function",
  "env_choice <- function",
  "accrual_orchestrator_config <- function",
  "accrual_loo_config <- function",
  "accrual_kfold_filter_config <- function",
  "accrual_simulation_runtime_config <- function",
  "accrual_calibration_profile_grid <- function"
)) {
  if (!grepl(fragment, ma00, fixed = TRUE)) {
    stop("ma00_setup.R missing centralized runtime/config helper: ", fragment)
  }
}

non_ma00 <- setdiff(script_files, "scripts/ma00_setup.R")
sys_getenv_hits <- non_ma00[vapply(non_ma00, function(path) grepl("Sys\\.getenv\\s*\\(", txt(path), perl = TRUE), logical(1))]
if (length(sys_getenv_hits)) {
  stop("Sys.getenv() outside ma00_setup.R is not allowed in scripts: ", paste(sys_getenv_hits, collapse = ", "))
}

parser_hits <- non_ma00[vapply(non_ma00, function(path) {
  grepl("\\b(parse_[A-Za-z0-9_]*_env|split_env|flag_from_env)\\s*<-\\s*function", txt(path), perl = TRUE)
}, logical(1))]
if (length(parser_hits)) {
  stop("Script-local env parser/helper definitions are not allowed: ", paste(parser_hits, collapse = ", "))
}

set_seed_hits <- non_ma00[vapply(non_ma00, function(path) grepl("\\bset\\.seed\\s*\\(", txt(path), perl = TRUE), logical(1))]
if (length(set_seed_hits)) {
  stop("Direct set.seed() outside ma00_setup.R is not allowed; use set_accrual_seed() or set_accrual_effective_seed(): ",
       paste(set_seed_hits, collapse = ", "))
}

required_usage <- list(
  "run.R" = c("source(\"scripts/ma00_setup.R\")", "accrual_orchestrator_config()"),
  "scripts/ma09a_plan_loo_savepars_refits.R" = c("accrual_loo_config()", "loo_cfg$chains", "loo_cfg$warmup"),
  "scripts/ma12a_plan_grouped_kfold_firm.R" = c("accrual_exact_kfold_run_context(\"grouped_firm\"", "filter_cfg$target_space_filter"),
  "scripts/ma13a_plan_row_level_exact_kfold.R" = c("accrual_exact_kfold_run_context(\"row\"", "filter_cfg$target_space_filter"),
  "scripts/simulation/si01_lmer_pilot_run.R" = c("accrual_simulation_runtime_config(\"lmer_pilot\")"),
  "scripts/simulation/si03a_plan_brms_leakage_confirmation.R" = c("accrual_simulation_runtime_config(\"brms_leakage\")"),
  "scripts/simulation/si04a_plan_brms_parameter_recovery.R" = c("accrual_simulation_runtime_config(\"brms_recovery\")"),
  "scripts/simulation/si05_lmer_temporal_dependence_run.R" = c("accrual_simulation_runtime_config(\"lmer_temporal\")"),
  "scripts/diagnostics/di08a_plan_mcmc_sampler_calibration.R" = c("accrual_calibration_profile_grid()"),
  "scripts/ma15_audit_DA_finite_outputs.R" = c("env_flag(\"ACCRUAL_DA_FINITE_GATE_STRICT\"")
)
for (path in names(required_usage)) {
  body <- txt(path)
  missing <- required_usage[[path]][!vapply(required_usage[[path]], grepl, logical(1), x = body, fixed = TRUE)]
  if (length(missing)) {
    stop(path, " missing centralized config usage fragment(s): ", paste(missing, collapse = ", "))
  }
}

runtime_default_patterns <- c(
  "\\bchains\\s*<-\\s*[0-9]+",
  "\\bcores\\s*<-\\s*[0-9]+",
  "\\biter\\s*<-\\s*[0-9]+",
  "\\bwarmup\\s*<-\\s*[0-9]+",
  "ACCRUAL_[A-Z0-9_]*(CHAINS|CORES|ITER|WARMUP)[A-Z0-9_]*\"\\s*,\\s*[0-9]"
)
runtime_default_hits <- non_ma00[vapply(non_ma00, function(path) {
  body <- txt(path)
  any(vapply(runtime_default_patterns, grepl, logical(1), x = body, perl = TRUE))
}, logical(1))]
if (length(runtime_default_hits)) {
  stop("Sampler/runtime chains/cores/iter/warmup defaults must live in ma00_setup.R: ",
       paste(runtime_default_hits, collapse = ", "))
}

cat("test_no_script_local_env_config_static.R passed\n")
