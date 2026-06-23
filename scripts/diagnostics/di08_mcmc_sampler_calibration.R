# -----------------------------------------------------------------------------
# Script: di08_mcmc_sampler_calibration.R
# Purpose: Temporary sampler-calibration diagnostic for worst failing Firm-RE
#          baseline models. This is not a manuscript-result script.
#
# Intended use:
#   Rscript scripts/diagnostics/di08_mcmc_sampler_calibration.R
#
# This script writes only under ACCRUAL_CALIBRATION_ROOT, defaulting to
# out/interim/winsor/diagnostics/mcmc_sampler_calibration. It never writes to
# production models/ or draws/ directories.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(posterior)
})

source("scripts/ma00_setup.R")
phase_begin("di08", "MCMC sampler calibration diagnostic")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()
validate_final_analysis_config("di08 MCMC sampler calibration", final_mode = TRUE)

script_start <- Sys.time()
script_name <- "scripts/diagnostics/di08_mcmc_sampler_calibration.R"
script_version <- "2026-06-23-v1-temporary-sampler-calibration"

default_targets <- c(
  "M09|real_time|main_common|Firm RE (Random Intercept + Year FE)",
  "M05|ex_post|main_common|Firm RE (Random Intercept + Year FE)"
)
calibration_targets <- env_list("ACCRUAL_CALIBRATION_TARGETS", sep = ";", default = default_targets)
calibration_targets <- unique(calibration_targets[nzchar(calibration_targets)])

profile_grid <- accrual_calibration_profile_grid()

calibration_root <- env_value(
  "ACCRUAL_CALIBRATION_ROOT",
  file.path(output_root, "diagnostics", "mcmc_sampler_calibration")
)
tables_dir <- file.path(calibration_root, "tables")
logs_dir <- file.path(calibration_root, "logs")
fits_dir <- file.path(calibration_root, "fits")
for (d in c(calibration_root, tables_dir, logs_dir, fits_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

production_model_dir <- normalizePath(file.path(output_root, "models"), winslash = "/", mustWork = FALSE)
production_draw_dir <- normalizePath(file.path(output_root, "draws"), winslash = "/", mustWork = FALSE)
calibration_fit_dir <- normalizePath(fits_dir, winslash = "/", mustWork = FALSE)
if (startsWith(calibration_fit_dir, production_model_dir) || startsWith(calibration_fit_dir, production_draw_dir)) {
  stop("[DI08 INPUT BLOCKER] Calibration fit directory must not be inside production models/draws directories: ",
       calibration_fit_dir)
}

formulas_path <- file.path(output_root, "tables", "table_named_model_formulas_winsor.csv")
gate_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_gate_winsor.csv")
if (!file.exists(formulas_path)) stop("[DI08 INPUT BLOCKER] Missing formula table: ", formulas_path)
if (!file.exists(gate_path)) stop("[DI08 INPUT BLOCKER] Missing MCMC diagnostics gate table: ", gate_path)

results_path <- file.path(tables_dir, "table_di08_mcmc_sampler_calibration_results.csv")
recommend_path <- file.path(tables_dir, "table_di08_recommended_sampler_profile.csv")
manifest_path <- file.path(logs_dir, "di08_mcmc_sampler_calibration_manifest.csv")
env_path <- file.path(logs_dir, "di08_recommended_env.ps1")
note_path <- file.path(logs_dir, "di08_interpretation_note.md")
preprocess_audit_path <- file.path(tables_dir, "table_di08_preprocessing_audit.csv")

diagnostic_key_for_row <- function(row) {
  paste(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, sep = "|")
}

parse_target_key <- function(key) {
  parts <- trimws(unlist(strsplit(key, "|", fixed = TRUE)))
  if (length(parts) != 4 || any(!nzchar(parts))) {
    stop("[DI08 INPUT BLOCKER] Invalid calibration target key: ", key,
         ". Expected Model_ID|Target_Space|Sample_Group|Heterogeneity_Variant")
  }
  parts
}
invisible(lapply(calibration_targets, parse_target_key))

classify_di08_mcmc <- function(max_rhat, n_divergent, min_bulk_ess, min_tail_ess) {
  if (!is.finite(max_rhat) || max_rhat > 1.01) return("FAIL")
  if (!is.finite(n_divergent) || n_divergent > 0) return("FAIL")
  if (!is.finite(min_bulk_ess) || min_bulk_ess < 400) return("FAIL")
  if (!is.finite(min_tail_ess) || min_tail_ess < 400) return("FAIL")
  if (min_bulk_ess < 1000 || min_tail_ess < 1000) return("REVIEW")
  "PASS"
}

di08_reason <- function(max_rhat, n_divergent, min_bulk_ess, min_tail_ess) {
  reasons <- c(
    if (!is.finite(max_rhat)) "max_rhat is non-finite" else if (max_rhat > 1.01) sprintf("max_rhat %.6f > 1.01", max_rhat),
    if (!is.finite(n_divergent)) "n_divergent is non-finite" else if (n_divergent > 0) sprintf("n_divergent=%d", as.integer(n_divergent)),
    if (!is.finite(min_bulk_ess)) "min_bulk_ess is non-finite" else if (min_bulk_ess < 400) sprintf("min_bulk_ess %.2f < 400", min_bulk_ess),
    if (!is.finite(min_tail_ess)) "min_tail_ess is non-finite" else if (min_tail_ess < 400) sprintf("min_tail_ess %.2f < 400", min_tail_ess),
    if (is.finite(min_bulk_ess) && min_bulk_ess >= 400 && min_bulk_ess < 1000) sprintf("min_bulk_ess %.2f below strict marker 1000", min_bulk_ess),
    if (is.finite(min_tail_ess) && min_tail_ess >= 400 && min_tail_ess < 1000) sprintf("min_tail_ess %.2f below strict marker 1000", min_tail_ess)
  )
  reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
  if (!length(reasons)) "MCMC diagnostics passed calibration gate."
  else paste(reasons, collapse = "; ")
}

file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

pkg_version <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(pkg))
}

extract_fit_diagnostics <- function(fit, max_treedepth) {
  draws <- posterior::as_draws_array(fit)
  draw_summ <- as.data.frame(posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail"))
  draw_summ <- draw_summ[!grepl("__$", draw_summ$variable), , drop = FALSE]
  max_rhat <- suppressWarnings(max(draw_summ$rhat, na.rm = TRUE))
  min_bulk <- suppressWarnings(min(draw_summ$ess_bulk, na.rm = TRUE))
  min_tail <- suppressWarnings(min(draw_summ$ess_tail, na.rm = TRUE))
  if (!is.finite(max_rhat)) max_rhat <- NA_real_
  if (!is.finite(min_bulk)) min_bulk <- NA_real_
  if (!is.finite(min_tail)) min_tail <- NA_real_
  np <- brms::nuts_params(fit)
  n_divergent <- sum(np$Parameter == "divergent__" & np$Value > 0, na.rm = TRUE)
  treedepth_vals <- np$Value[np$Parameter == "treedepth__"]
  max_treedepth_hits <- sum(treedepth_vals >= max_treedepth, na.rm = TRUE)
  list(
    max_rhat = max_rhat,
    min_bulk_ess = min_bulk,
    min_tail_ess = min_tail,
    n_divergent = n_divergent,
    max_treedepth_hits = max_treedepth_hits
  )
}

empty_results <- function() {
  data.frame(
    model_key = character(),
    Model_ID = character(),
    Model_Name = character(),
    Target_Space = character(),
    Sample_Group = character(),
    Heterogeneity_Variant = character(),
    sampler_profile = character(),
    chains = integer(),
    cores = integer(),
    iter = integer(),
    warmup = integer(),
    adapt_delta = numeric(),
    max_treedepth = integer(),
    seed = integer(),
    preprocessing_mode = character(),
    formula = character(),
    fit_status = character(),
    max_rhat = numeric(),
    min_bulk_ess = numeric(),
    min_tail_ess = numeric(),
    n_divergent = integer(),
    max_treedepth_hits = integer(),
    diagnostics_status = character(),
    diagnostics_reason = character(),
    elapsed_seconds = numeric(),
    fit_path = character(),
    error_message = character(),
    stringsAsFactors = FALSE
  )
}

results <- if (file.exists(results_path)) {
  read.csv(results_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  empty_results()
}
write_results <- function() write.csv(results, results_path, row.names = FALSE)

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE, check.names = FALSE)
gate_df <- read.csv(gate_path, stringsAsFactors = FALSE, check.names = FALSE)
formulas_df$.diagnostic_key <- vapply(seq_len(nrow(formulas_df)), function(i) diagnostic_key_for_row(formulas_df[i, ]), character(1))
missing_targets <- setdiff(calibration_targets, formulas_df$.diagnostic_key)
if (length(missing_targets)) {
  stop("[DI08 INPUT BLOCKER] Calibration target(s) not found in formula table: ",
       paste(missing_targets, collapse = "; "))
}

assert_grand_mean_standardization <- function(raw_df, scaled_df, target_key, sample_file) {
  rows <- list()
  for (v in pred_vars) {
    std_name <- paste0(v, "_std")
    if (!v %in% names(raw_df) || !std_name %in% names(scaled_df)) next
    m <- mean(raw_df[[v]], na.rm = TRUE)
    s <- sd(raw_df[[v]], na.rm = TRUE)
    expected <- if (!is.na(s) && s > 0) (raw_df[[v]] - m) / s else rep(0, nrow(raw_df))
    max_abs_diff <- suppressWarnings(max(abs(scaled_df[[std_name]] - expected), na.rm = TRUE))
    if (!is.finite(max_abs_diff)) max_abs_diff <- NA_real_
    firm_mean_max_abs <- NA_real_
    if ("company" %in% names(scaled_df)) {
      firm_means <- tapply(scaled_df[[std_name]], scaled_df$company, mean, na.rm = TRUE)
      firm_mean_max_abs <- suppressWarnings(max(abs(firm_means), na.rm = TRUE))
      if (!is.finite(firm_mean_max_abs)) firm_mean_max_abs <- NA_real_
    }
    rows[[length(rows) + 1]] <- data.frame(
      model_key = target_key,
      sample_file = sample_file,
      predictor = v,
      preprocessing_mode = "grand_mean_zscore_predictors_no_firm_demean",
      raw_grand_mean = m,
      raw_grand_sd = s,
      max_abs_diff_from_grand_zscore = max_abs_diff,
      firm_mean_max_abs_after_standardization = firm_mean_max_abs,
      firm_demeaning_used = FALSE,
      group_centering_used = FALSE,
      stringsAsFactors = FALSE
    )
  }
  bind_rows(rows)
}

preprocess_audit_rows <- list()
for (target_key in calibration_targets) {
  row <- formulas_df[formulas_df$.diagnostic_key == target_key, , drop = FALSE][1, ]
  sample_path <- file.path(input_winsor_root, "tables", row$Target_Sample)
  if (!file.exists(sample_path)) stop("[DI08 INPUT BLOCKER] Missing winsor sample: ", sample_path)
  raw_sample <- read.csv(sample_path, stringsAsFactors = FALSE, check.names = FALSE)
  scaled_sample <- read_winsor_sample(row$Target_Sample, prefactor = FALSE)
  audit <- assert_grand_mean_standardization(raw_sample, scaled_sample, target_key, row$Target_Sample)
  if (nrow(audit) && any(is.na(audit$max_abs_diff_from_grand_zscore) | audit$max_abs_diff_from_grand_zscore > 1e-8)) {
    stop("[DI08 INPUT BLOCKER] Predictor standardization audit failed for ", target_key,
         ". Expected grand-mean z-score predictors only.")
  }
  preprocess_audit_rows[[length(preprocess_audit_rows) + 1]] <- audit
}
preprocess_audit <- bind_rows(preprocess_audit_rows)
write.csv(preprocess_audit, preprocess_audit_path, row.names = FALSE)

for (target_index in seq_along(calibration_targets)) {
  target_key <- calibration_targets[[target_index]]
  row <- formulas_df[formulas_df$.diagnostic_key == target_key, , drop = FALSE][1, ]
  model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
  df_scaled <- read_winsor_sample(row$Target_Sample, prefactor = FALSE)
  formula_str <- fix_formula(row$brms_Formula, prefactor = FALSE)
  brms_form <- brms::bf(stats::as.formula(formula_str))
  prior_list <- default_prior_list(row$Heterogeneity_Variant, model_structure = model_structure)
  seed <- accrual_seed_for(paste0("di08_mcmc_sampler_calibration_", target_key), offset = target_index)

  for (profile_index in seq_len(nrow(profile_grid))) {
    prof <- profile_grid[profile_index, ]
    fit_path <- file.path(fits_dir, model_key, prof$sampler_profile, "fit.rds")
    dir.create(dirname(fit_path), recursive = TRUE, showWarnings = FALSE)

    message("[DI08] Calibrating ", target_key, " with profile ", prof$sampler_profile)
    message(
      "brms/rstan sampler controls: chains=", prof$chains,
      ", cores=", prof$cores,
      ", iter=", prof$iter,
      ", warmup=", prof$warmup,
      ", adapt_delta=", prof$adapt_delta,
      ", max_treedepth=", prof$max_treedepth
    )
    t0 <- Sys.time()
    err_msg <- NA_character_
    fit_status <- "SUCCESS"
    diag <- list(max_rhat = NA_real_, min_bulk_ess = NA_real_, min_tail_ess = NA_real_,
                 n_divergent = NA_integer_, max_treedepth_hits = NA_integer_)
    fit <- tryCatch({
      brms::brm(
        formula = brms_form,
        data = df_scaled,
        family = brms_family(),
        prior = prior_list,
        chains = prof$chains,
        cores = prof$cores,
        iter = prof$iter,
        warmup = prof$warmup,
        control = list(adapt_delta = prof$adapt_delta, max_treedepth = prof$max_treedepth),
        seed = seed,
        save_pars = brms::save_pars(all = TRUE),
        refresh = 500
      )
    }, error = function(e) {
      err_msg <<- conditionMessage(e)
      fit_status <<- "FAILED"
      NULL
    })

    if (!is.null(fit)) {
      saveRDS(fit, fit_path)
      diag <- tryCatch(
        extract_fit_diagnostics(fit, prof$max_treedepth),
        error = function(e) {
          err_msg <<- conditionMessage(e)
          fit_status <<- "DIAGNOSTICS_FAILED"
          list(max_rhat = NA_real_, min_bulk_ess = NA_real_, min_tail_ess = NA_real_,
               n_divergent = NA_integer_, max_treedepth_hits = NA_integer_)
        }
      )
    }

    diagnostics_status <- if (identical(fit_status, "SUCCESS")) {
      classify_di08_mcmc(diag$max_rhat, diag$n_divergent, diag$min_bulk_ess, diag$min_tail_ess)
    } else {
      "FAIL"
    }
    diagnostics_reason <- if (identical(fit_status, "SUCCESS")) {
      di08_reason(diag$max_rhat, diag$n_divergent, diag$min_bulk_ess, diag$min_tail_ess)
    } else {
      paste0("fit or diagnostics failed: ", err_msg)
    }

    result_row <- data.frame(
      model_key = target_key,
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      sampler_profile = prof$sampler_profile,
      chains = prof$chains,
      cores = prof$cores,
      iter = prof$iter,
      warmup = prof$warmup,
      adapt_delta = prof$adapt_delta,
      max_treedepth = prof$max_treedepth,
      seed = seed,
      preprocessing_mode = "grand_mean_zscore_predictors_no_firm_demean",
      formula = formula_str,
      fit_status = fit_status,
      max_rhat = diag$max_rhat,
      min_bulk_ess = diag$min_bulk_ess,
      min_tail_ess = diag$min_tail_ess,
      n_divergent = diag$n_divergent,
      max_treedepth_hits = diag$max_treedepth_hits,
      diagnostics_status = diagnostics_status,
      diagnostics_reason = diagnostics_reason,
      elapsed_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
      fit_path = fit_path,
      error_message = ifelse(is.na(err_msg), "", err_msg),
      stringsAsFactors = FALSE
    )
    results <- results %>%
      filter(!(.data$model_key == target_key & .data$sampler_profile == prof$sampler_profile)) %>%
      bind_rows(result_row)
    write_results()
  }
}

recommend_one <- function(df) {
  acceptable <- df[df$diagnostics_status %in% c("PASS", "REVIEW"), , drop = FALSE]
  non_baseline <- acceptable[acceptable$sampler_profile != "baseline_current", , drop = FALSE]
  if (nrow(non_baseline)) acceptable <- non_baseline
  if (!nrow(acceptable)) {
    return(data.frame(
      model_key = df$model_key[[1]],
      recommended_sampler_profile = "NO_ACCEPTABLE_PROFILE",
      recommendation_status = "NO_ACCEPTABLE_PROFILE",
      diagnostics_status = "FAIL",
      diagnostics_reason = paste(df$diagnostics_reason, collapse = " | "),
      chains = NA_integer_, cores = NA_integer_, iter = NA_integer_, warmup = NA_integer_,
      adapt_delta = NA_real_, max_treedepth = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }
  acceptable$diagnostics_rank <- ifelse(acceptable$diagnostics_status == "PASS", 1L, 2L)
  acceptable$cost_rank <- match(
    acceptable$sampler_profile,
    c("remediation_default", "longer_warmup", "very_long_if_needed", "baseline_current")
  )
  best <- acceptable[order(acceptable$diagnostics_rank, acceptable$cost_rank), , drop = FALSE][1, ]
  data.frame(
    model_key = best$model_key,
    recommended_sampler_profile = best$sampler_profile,
    recommendation_status = "ACCEPTABLE_PROFILE_AVAILABLE",
    diagnostics_status = best$diagnostics_status,
    diagnostics_reason = best$diagnostics_reason,
    chains = best$chains,
    cores = best$cores,
    iter = best$iter,
    warmup = best$warmup,
    adapt_delta = best$adapt_delta,
    max_treedepth = best$max_treedepth,
    stringsAsFactors = FALSE
  )
}

recommendations <- bind_rows(lapply(split(results, results$model_key), recommend_one))
write.csv(recommendations, recommend_path, row.names = FALSE)

common_profile <- if (nrow(recommendations) && all(recommendations$recommendation_status == "ACCEPTABLE_PROFILE_AVAILABLE")) {
  unique(recommendations$recommended_sampler_profile)
} else {
  character()
}
if (length(common_profile) == 1) {
  common_cfg <- profile_grid[profile_grid$sampler_profile == common_profile, , drop = FALSE][1, ]
  env_lines <- c(
    paste0("$env:ACCRUAL_REMEDIATION_CORES = \"", common_cfg$cores, "\""),
    paste0("$env:ACCRUAL_REMEDIATION_ITER = \"", common_cfg$iter, "\""),
    paste0("$env:ACCRUAL_REMEDIATION_WARMUP = \"", common_cfg$warmup, "\""),
    paste0("$env:ACCRUAL_REMEDIATION_ADAPT_DELTA = \"", common_cfg$adapt_delta, "\""),
    paste0("$env:ACCRUAL_REMEDIATION_MAX_TREEDEPTH = \"", common_cfg$max_treedepth, "\"")
  )
} else {
  conservative <- recommendations[recommendations$recommendation_status == "ACCEPTABLE_PROFILE_AVAILABLE", , drop = FALSE]
  if (nrow(conservative)) {
    rank_map <- match(conservative$recommended_sampler_profile, c("remediation_default", "longer_warmup", "very_long_if_needed", "baseline_current"))
    chosen <- conservative[which.max(rank_map), , drop = FALSE][1, ]
    env_lines <- c(
      "# WARNING: Targets require different recommended profiles; production remediation should use the most conservative profile that passes all targets.",
      paste0("$env:ACCRUAL_REMEDIATION_CORES = \"", chosen$cores, "\""),
      paste0("$env:ACCRUAL_REMEDIATION_ITER = \"", chosen$iter, "\""),
      paste0("$env:ACCRUAL_REMEDIATION_WARMUP = \"", chosen$warmup, "\""),
      paste0("$env:ACCRUAL_REMEDIATION_ADAPT_DELTA = \"", chosen$adapt_delta, "\""),
      paste0("$env:ACCRUAL_REMEDIATION_MAX_TREEDEPTH = \"", chosen$max_treedepth, "\"")
    )
  } else {
    env_lines <- "# WARNING: No acceptable calibration profile was found; inspect table_di08_mcmc_sampler_calibration_results.csv before remediation."
  }
}
env_lines <- c(
  env_lines,
  paste0("$env:ACCRUAL_MCMC_REMEDIATION_TARGETS = \"", paste(calibration_targets, collapse = ";"), "\""),
  "Remove-Item Env:\\ACCRUAL_FORCE_REFIT -ErrorAction SilentlyContinue",
  "Rscript .\\scripts\\ma07_fit_brms_named_models.R",
  "Rscript .\\scripts\\ma08_mcmc_diagnostics.R"
)
writeLines(env_lines, env_path, useBytes = TRUE)

git_commit <- tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
manifest <- data.frame(
  timestamp = as.character(Sys.time()),
  script = script_name,
  script_version = script_version,
  git_commit = git_commit,
  r_version = R.version.string,
  brms_version = pkg_version("brms"),
  posterior_version = pkg_version("posterior"),
  dplyr_version = pkg_version("dplyr"),
  formulas_path = formulas_path,
  formulas_md5 = file_hash_or_na(formulas_path),
  gate_path = gate_path,
  gate_md5 = file_hash_or_na(gate_path),
  target_list = paste(calibration_targets, collapse = ";"),
  profile_list = paste(profile_grid$sampler_profile, collapse = ","),
  canonical_seed = accrual_base_seed(),
  output_root = calibration_root,
  preprocessing_note = "grand_mean_zscore_predictors_no_firm_demean; no firm demeaning or group centering used",
  design_note = "No prior, formula, likelihood, winsorization, sample, target-space, or seed-search changes were made.",
  stringsAsFactors = FALSE
)
write.csv(manifest, manifest_path, row.names = FALSE)

note <- c(
  "# di08 MCMC Sampler Calibration Note",
  "",
  "This is a temporary sampler-calibration diagnostic, not a manuscript-result script.",
  "Its only purpose is to compare safe sampler settings for failed Firm-RE baseline models before production remediation.",
  "",
  "Predictor preprocessing remains grand-mean z-score standardization only. The script does not firm-demean predictors, group-center predictors, or remove firm-level variation.",
  "The script does not change model formulas, priors, likelihood family, dependent variable, winsorization, sample construction, seed strategy, or target-space definitions.",
  "No seed search is performed.",
  "",
  paste0("Results: ", results_path),
  paste0("Recommendations: ", recommend_path),
  paste0("PowerShell remediation helper: ", env_path)
)
writeLines(note, note_path, useBytes = TRUE)

cat("[SUCCESS] di08 calibration outputs written under ", calibration_root, "\n", sep = "")
phase_end("di08", "MCMC sampler calibration diagnostic")
