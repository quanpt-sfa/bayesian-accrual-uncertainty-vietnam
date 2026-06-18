# -----------------------------------------------------------------------------
# Script: 30_new_firm_predictive_integration_audit.R
# Purpose: Audit whether primary out-of-firm posterior predictive quantities from
#          Firm-RE models verify integration over new-firm effects u_new.
#
# Intended use:
#   Rscript scripts/30_new_firm_predictive_integration_audit.R
#
# This is a verification/audit script. It does not fit models and does not modify
# existing pipeline outputs.
#
# Key design rule:
#   For any Firm-RE posterior predictive quantity used as primary out-of-firm
#   evidence, the held-out/new-firm prediction must integrate over a new firm
#   effect:
#
#     u_new^(s) ~ Normal(0, sigma_u^(s)^2)
#
#   If this cannot be verified from source metadata or output manifests, the
#   quantity is suppressed from primary reporting by default.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/00_helpers.R")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

script_start_time <- Sys.time()
script_name <- "scripts/30_new_firm_predictive_integration_audit.R"
script_version <- "2026-06-18-v1-new-firm-integration-audit"

audit_root <- file.path(output_root, "new_firm_predictive_audit")
tables_dir <- file.path(audit_root, "tables")
logs_dir <- file.path(audit_root, "logs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

env_flag_local <- function(name, default = "FALSE") {
  raw <- Sys.getenv(name, default)
  toupper(raw) %in% c("TRUE", "1", "YES", "Y")
}

strict_mode <- env_flag_local("ACCRUAL_NEW_FIRM_AUDIT_STRICT", "TRUE")
allow_uncertainty_mode <- env_flag_local("ACCRUAL_NEW_FIRM_ALLOW_BRMS_UNCERTAINTY", "TRUE")

read_text_if_exists <- function(path) {
  if (!file.exists(path)) return(character())
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

collapse_text <- function(x) paste(x, collapse = "\n")

contains_any <- function(x, patterns, fixed = TRUE) {
  if (!length(x)) return(FALSE)
  txt <- collapse_text(x)
  any(vapply(patterns, function(p) grepl(p, txt, fixed = fixed), logical(1)))
}

extract_matching_lines <- function(x, patterns, fixed = TRUE, max_n = 8) {
  if (!length(x)) return("")
  hits <- rep(FALSE, length(x))
  for (p in patterns) hits <- hits | grepl(p, x, fixed = fixed)
  out <- trimws(x[hits])
  out <- unique(out[nzchar(out)])
  if (!length(out)) return("")
  paste(utils::head(out, max_n), collapse = " | ")
}

path_exists <- function(path) file.exists(path)

nrows_or_na <- function(path) {
  if (!file.exists(path) || !grepl("\\.csv$", path, ignore.case = TRUE)) return(NA_integer_)
  out <- tryCatch(nrow(read.csv(path, stringsAsFactors = FALSE)), error = function(e) NA_integer_)
  out
}

file_size_or_na <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  as.numeric(file.info(path)$size)
}

mtime_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

detect_latest_kfold_root <- function() {
  latest <- file.path(winsor_root, "kfold_firm", "LATEST_RUN.txt")
  if (!file.exists(latest)) return(NA_character_)
  x <- trimws(readLines(latest, warn = FALSE))
  x <- x[nzchar(x)]
  if (!length(x)) return(NA_character_)
  x[[1]]
}

detect_latest_row_kfold_root <- function() {
  # Supports likely future names from the reviewer-final method-matching scripts.
  candidates <- c(
    file.path(winsor_root, "row_exact_kfold", "LATEST_RUN.txt"),
    file.path(winsor_root, "kfold_row", "LATEST_RUN.txt"),
    file.path(winsor_root, "row_kfold", "LATEST_RUN.txt")
  )
  for (p in candidates) {
    if (file.exists(p)) {
      x <- trimws(readLines(p, warn = FALSE))
      x <- x[nzchar(x)]
      if (length(x)) return(x[[1]])
    }
  }
  NA_character_
}

classify_source_code_evidence <- function(path, role) {
  lines <- read_text_if_exists(path)

  has_allow_new_levels <- contains_any(lines, c("allow_new_levels = TRUE", "allow_new_levels=TRUE"))
  has_sample_new_levels_gaussian <- contains_any(lines, c(
    'sample_new_levels = "gaussian"', "sample_new_levels='gaussian'",
    'sample_new_levels = "uncertainty"', "sample_new_levels='uncertainty'"
  ))
  has_sample_new_levels_old_levels <- contains_any(lines, c(
    'sample_new_levels = "old_levels"', "sample_new_levels='old_levels'"
  ))
  has_re_formula_na <- contains_any(lines, c("re_formula = NA", "re.form = NA", "re_formula=NA", "re.form=NA"))
  has_re_formula_null_or_default <- contains_any(lines, c("re_formula = NULL", "re.form = NULL", "re_formula=NULL", "re.form=NULL"))
  has_custom_unew <- contains_any(lines, c(
    "u_new", "nu_new",
    "rnorm(", "sigma_u", "sd_company", "sd_company__Intercept"
  ))
  has_posterior_predict <- contains_any(lines, c("posterior_predict", "posterior_epred"))
  has_log_lik_newdata <- contains_any(lines, c("log_lik(", "newdata"))

  evidence_lines <- extract_matching_lines(lines, c(
    "allow_new_levels", "sample_new_levels", "re_formula", "re.form",
    "posterior_predict", "posterior_epred", "log_lik(", "u_new", "sigma_u",
    "sd_company"
  ))

  data.frame(
    source_type = "source_code",
    source_role = role,
    path = path,
    exists = file.exists(path),
    n_lines = length(lines),
    has_allow_new_levels = has_allow_new_levels,
    has_sample_new_levels_gaussian_or_uncertainty = has_sample_new_levels_gaussian,
    has_sample_new_levels_old_levels = has_sample_new_levels_old_levels,
    has_re_formula_na_population_only = has_re_formula_na,
    has_re_formula_conditional = has_re_formula_null_or_default,
    has_custom_u_new_draw_logic = has_custom_unew && contains_any(lines, c("u_new", "sigma_u")),
    has_posterior_predict_or_epred = has_posterior_predict,
    has_log_lik_newdata = has_log_lik_newdata,
    evidence_snippet = evidence_lines,
    stringsAsFactors = FALSE
  )
}

source_paths <- data.frame(
  source_role = c(
    "stacked_DA_draw_constructor",
    "grouped_PSIS_LOFO",
    "exact_grouped_kfold",
    "BRMS_leakage_confirmation",
    "exact_row_kfold_candidate",
    "psis_reliability_gate_candidate"
  ),
  path = c(
    file.path("scripts", "10_construct_uncertainty_adjusted_DA.R"),
    file.path("scripts", "12_lofo_stacking.R"),
    file.path("scripts", "13_grouped_kfold_firm.R"),
    file.path("scripts", "26_sim_brms_leakage_confirmation.R"),
    file.path("scripts", "28_row_level_exact_kfold.R"),
    file.path("scripts", "29_psis_reliability_gate.R")
  ),
  stringsAsFactors = FALSE
)

source_audit <- bind_rows(lapply(seq_len(nrow(source_paths)), function(i) {
  classify_source_code_evidence(source_paths$path[[i]], source_paths$source_role[[i]])
}))

# Output/artifact inventory ----------------------------------------------------

kfold_root_latest <- detect_latest_kfold_root()
row_kfold_root_latest <- detect_latest_row_kfold_root()

candidate_outputs <- data.frame(
  output_role = c(
    "stacked_DA_master_output_table",
    "stacked_DA_accruals_output_table",
    "grouped_kfold_observation_scores",
    "grouped_kfold_model_scores",
    "grouped_kfold_weights_ex_post",
    "grouped_kfold_weights_no_lookahead",
    "row_exact_kfold_observation_scores",
    "row_exact_kfold_model_scores",
    "psis_reliability_gate"
  ),
  path = c(
    file.path(output_root, "tables", "final_uncertainty_adjusted_accruals_winsor.csv"),
    tryCatch(baseline_accruals_path(), error = function(e) file.path("accruals", "baseline", "final_uncertainty_adjusted_accruals_winsor.csv")),
    ifelse(is.na(kfold_root_latest), file.path(winsor_root, "kfold_firm", "UNKNOWN", "tables", "table_winsor_kfold_observation_scores.csv"), file.path(kfold_root_latest, "tables", "table_winsor_kfold_observation_scores.csv")),
    ifelse(is.na(kfold_root_latest), file.path(winsor_root, "kfold_firm", "UNKNOWN", "tables", "table_winsor_kfold_model_scores.csv"), file.path(kfold_root_latest, "tables", "table_winsor_kfold_model_scores.csv")),
    ifelse(is.na(kfold_root_latest), file.path(winsor_root, "kfold_firm", "UNKNOWN", "tables", "table_winsor_kfold_weights_ex_post.csv"), file.path(kfold_root_latest, "tables", "table_winsor_kfold_weights_ex_post.csv")),
    ifelse(is.na(kfold_root_latest), file.path(winsor_root, "kfold_firm", "UNKNOWN", "tables", "table_winsor_kfold_weights_no_lookahead.csv"), file.path(kfold_root_latest, "tables", "table_winsor_kfold_weights_no_lookahead.csv")),
    ifelse(is.na(row_kfold_root_latest), file.path(winsor_root, "row_exact_kfold", "UNKNOWN", "tables", "table_winsor_row_exact_kfold_observation_scores.csv"), file.path(row_kfold_root_latest, "tables", "table_winsor_row_exact_kfold_observation_scores.csv")),
    ifelse(is.na(row_kfold_root_latest), file.path(winsor_root, "row_exact_kfold", "UNKNOWN", "tables", "table_winsor_row_exact_kfold_model_scores.csv"), file.path(row_kfold_root_latest, "tables", "table_winsor_row_exact_kfold_model_scores.csv")),
    file.path(output_root, "new_firm_predictive_audit", "tables", "table_new_firm_predictive_integration_audit.csv")
  ),
  stringsAsFactors = FALSE
)

artifact_inventory <- candidate_outputs %>%
  mutate(
    exists = file.exists(path),
    n_rows = vapply(path, nrows_or_na, integer(1)),
    file_size_bytes = vapply(path, file_size_or_na, numeric(1)),
    modified_time = vapply(path, mtime_or_na, character(1))
  )

# Quantity-level audit ---------------------------------------------------------

infer_quantity_rows_from_artifact <- function(output_role, path) {
  x <- safe_read_csv(path)
  if (is.null(x)) {
    return(data.frame(
      target_space = NA_character_,
      model_id = NA_character_,
      heterogeneity_variant = NA_character_,
      quantity = output_role,
      uses_firm_re = NA,
      out_of_firm_target = NA,
      source_artifact = path,
      artifact_exists = FALSE,
      artifact_columns = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  nm <- names(x)
  artifact_columns <- paste(nm, collapse = ",")

  # Flexible column detection.
  target_space <- if ("Target_Space" %in% nm) unique(as.character(x$Target_Space)) else
    if ("target_space" %in% nm) unique(as.character(x$target_space)) else NA_character_
  model_id <- if ("Model_ID" %in% nm) unique(as.character(x$Model_ID)) else
    if ("model_id" %in% nm) unique(as.character(x$model_id)) else NA_character_
  heterogeneity_variant <- if ("Heterogeneity_Variant" %in% nm) unique(as.character(x$Heterogeneity_Variant)) else
    if ("heterogeneity_variant" %in% nm) unique(as.character(x$heterogeneity_variant)) else NA_character_

  if (length(target_space) == 0) target_space <- NA_character_
  if (length(model_id) == 0) model_id <- NA_character_
  if (length(heterogeneity_variant) == 0) heterogeneity_variant <- NA_character_

  target_space <- utils::head(target_space, 20)
  model_id <- utils::head(model_id, 50)
  heterogeneity_variant <- utils::head(heterogeneity_variant, 20)

  # Identify quantities.
  q_cols <- character()
  if (output_role %in% c("stacked_DA_master_output_table", "stacked_DA_accruals_output_table")) {
    q_cols <- grep("tail_flag|tail_prob|ppd|predictive|DA_z_predictive", nm, value = TRUE, ignore.case = TRUE)
  } else if (grepl("observation_scores|model_scores", output_role)) {
    q_cols <- grep("log_predictive_density|elpd|lpd|score", nm, value = TRUE, ignore.case = TRUE)
  } else if (grepl("weights", output_role)) {
    q_cols <- grep("Weight|weight", nm, value = TRUE, ignore.case = TRUE)
  }
  if (!length(q_cols)) q_cols <- output_role

  expand.grid(
    target_space = target_space,
    model_id = model_id,
    heterogeneity_variant = heterogeneity_variant,
    quantity = utils::head(q_cols, 40),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      uses_firm_re = grepl("firm|random|RE|intercept", heterogeneity_variant, ignore.case = TRUE),
      out_of_firm_target = grepl("group|firm|lofo|out_of_firm", output_role, ignore.case = TRUE),
      source_artifact = path,
      artifact_exists = TRUE,
      artifact_columns = artifact_columns
    )
}

quantity_inventory <- bind_rows(lapply(seq_len(nrow(candidate_outputs)), function(i) {
  infer_quantity_rows_from_artifact(candidate_outputs$output_role[[i]], candidate_outputs$path[[i]])
}))

# Source-level interpretation --------------------------------------------------

source_summary <- source_audit %>%
  summarise(
    any_sample_new_levels_gaussian_or_uncertainty = any(has_sample_new_levels_gaussian_or_uncertainty, na.rm = TRUE),
    any_custom_u_new_draw_logic = any(has_custom_u_new_draw_logic, na.rm = TRUE),
    any_allow_new_levels = any(has_allow_new_levels, na.rm = TRUE),
    any_population_only_re_formula_na = any(has_re_formula_na_population_only, na.rm = TRUE),
    any_conditional_re_formula = any(has_re_formula_conditional, na.rm = TRUE),
    .groups = "drop"
  )

verified_draw_integration <- isTRUE(source_summary$any_sample_new_levels_gaussian_or_uncertainty) ||
  isTRUE(source_summary$any_custom_u_new_draw_logic)

has_partial_new_level_evidence <- isTRUE(source_summary$any_allow_new_levels) ||
  isTRUE(source_summary$any_population_only_re_formula_na)

# A quantity is primary-allowed only if not FirmRE out-of-firm, or if integration is verified.
audit <- quantity_inventory %>%
  mutate(
    new_firm_integration_required = ifelse(isTRUE(out_of_firm_target) & isTRUE(uses_firm_re), TRUE, FALSE),
    allow_new_levels_detected_in_source = isTRUE(source_summary$any_allow_new_levels),
    sample_new_levels_detected_in_source = isTRUE(source_summary$any_sample_new_levels_gaussian_or_uncertainty),
    custom_u_new_draw_logic_detected_in_source = isTRUE(source_summary$any_custom_u_new_draw_logic),
    population_only_re_formula_detected_in_source = isTRUE(source_summary$any_population_only_re_formula_na),
    u_new_integrated_verified = ifelse(new_firm_integration_required, verified_draw_integration, NA),
    verification_status = dplyr::case_when(
      !artifact_exists ~ "ARTIFACT_MISSING",
      !new_firm_integration_required ~ "NOT_REQUIRED",
      verified_draw_integration ~ "PASS_VERIFIED_U_NEW_INTEGRATION",
      has_partial_new_level_evidence ~ "VERIFY_PARTIAL_NEW_LEVEL_EVIDENCE_ONLY",
      TRUE ~ "FAIL_NOT_VERIFIED"
    ),
    primary_reporting_allowed = dplyr::case_when(
      !artifact_exists ~ FALSE,
      !new_firm_integration_required ~ TRUE,
      verified_draw_integration ~ TRUE,
      TRUE ~ FALSE
    ),
    suppression_reason = dplyr::case_when(
      artifact_exists == FALSE ~ "Source artifact missing.",
      !new_firm_integration_required ~ NA_character_,
      verified_draw_integration ~ NA_character_,
      has_partial_new_level_evidence ~ "Firm-RE out-of-firm quantity requires u_new integration; source has partial new-level/population-level evidence but no verified posterior predictive u_new draw integration.",
      TRUE ~ "Firm-RE out-of-firm quantity requires u_new integration; no verified u_new integration evidence found."
    ),
    design_requirement = "Primary Firm-RE out-of-firm posterior predictive quantities must integrate over u_new ~ Normal(0, sigma_u^2).",
    required_action = dplyr::case_when(
      primary_reporting_allowed ~ "report_if_other_gates_pass",
      verification_status == "ARTIFACT_MISSING" ~ "do_not_report_missing_artifact",
      TRUE ~ "suppress_from_primary_RQ2_until_verified"
    )
  )

# Add a compact decision table.
decision <- data.frame(
  audit_decision = if (any(audit$new_firm_integration_required & !audit$primary_reporting_allowed, na.rm = TRUE)) {
    "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES"
  } else if (any(audit$new_firm_integration_required & audit$primary_reporting_allowed, na.rm = TRUE)) {
    "PASS_FOR_AVAILABLE_FIRMRE_OUT_OF_FIRM_QUANTITIES"
  } else {
    "NO_FIRMRE_OUT_OF_FIRM_PRIMARY_QUANTITIES_DETECTED"
  },
  verified_draw_integration = verified_draw_integration,
  partial_new_level_evidence = has_partial_new_level_evidence,
  strict_mode = strict_mode,
  allow_uncertainty_mode = allow_uncertainty_mode,
  n_quantities_audited = nrow(audit),
  n_required = sum(audit$new_firm_integration_required, na.rm = TRUE),
  n_required_primary_allowed = sum(audit$new_firm_integration_required & audit$primary_reporting_allowed, na.rm = TRUE),
  n_required_suppressed = sum(audit$new_firm_integration_required & !audit$primary_reporting_allowed, na.rm = TRUE),
  stringsAsFactors = FALSE
)

# Manuscript/appendix note.
note <- c(
  "# New-firm posterior predictive integration audit",
  "",
  paste0("Script: `", script_name, "`"),
  paste0("Version: `", script_version, "`"),
  "",
  "## Design requirement",
  "",
  "Any Firm-RE posterior predictive quantity used as primary out-of-firm evidence must integrate over a new firm effect:",
  "",
  "`u_new^(s) ~ Normal(0, sigma_u^(s)^2)`.",
  "",
  "The audit is fail-safe. If integration over `u_new` is not verified, Firm-RE out-of-firm posterior predictive tail flags are suppressed from primary RQ2 reporting.",
  "",
  "## Decision",
  "",
  paste0("- audit_decision: `", decision$audit_decision[[1]], "`"),
  paste0("- verified_draw_integration: `", decision$verified_draw_integration[[1]], "`"),
  paste0("- partial_new_level_evidence: `", decision$partial_new_level_evidence[[1]], "`"),
  paste0("- n_required: ", decision$n_required[[1]]),
  paste0("- n_required_primary_allowed: ", decision$n_required_primary_allowed[[1]]),
  paste0("- n_required_suppressed: ", decision$n_required_suppressed[[1]]),
  "",
  "## Interpretation",
  "",
  "Population-level grouped scores or `allow_new_levels=TRUE` are useful evidence, but they do not by themselves prove that posterior predictive tail draws integrated over `u_new` unless `sample_new_levels` or an equivalent custom draw construction is verified.",
  "",
  "This audit should be reported in Chapter 4 or the technical appendix before any Firm-RE out-of-firm posterior predictive tail flags are treated as primary evidence."
)

# Manifests and outputs.
input_manifest <- data.frame(
  input_type = c(rep("source_code", nrow(source_paths)), rep("artifact", nrow(candidate_outputs))),
  role = c(source_paths$source_role, candidate_outputs$output_role),
  path = c(source_paths$path, candidate_outputs$path),
  exists = file.exists(c(source_paths$path, candidate_outputs$path)),
  file_size_bytes = vapply(c(source_paths$path, candidate_outputs$path), file_size_or_na, numeric(1)),
  modified_time = vapply(c(source_paths$path, candidate_outputs$path), mtime_or_na, character(1)),
  n_rows = c(rep(NA_integer_, nrow(source_paths)), vapply(candidate_outputs$path, nrows_or_na, integer(1))),
  stringsAsFactors = FALSE
)

run_manifest <- data.frame(
  script = script_name,
  script_version = script_version,
  start_time = as.character(script_start_time),
  end_time = as.character(Sys.time()),
  runtime_seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  output_root = output_root,
  winsor_root = winsor_root,
  audit_root = audit_root,
  strict_mode = strict_mode,
  allow_uncertainty_mode = allow_uncertainty_mode,
  design_requirement = "Primary Firm-RE out-of-firm posterior predictive quantities must integrate over u_new.",
  stringsAsFactors = FALSE
)

write.csv(source_audit, file.path(tables_dir, "table_new_firm_predictive_source_code_audit.csv"), row.names = FALSE)
write.csv(artifact_inventory, file.path(tables_dir, "table_new_firm_predictive_artifact_inventory.csv"), row.names = FALSE)
write.csv(audit, file.path(tables_dir, "table_new_firm_predictive_integration_audit.csv"), row.names = FALSE)
write.csv(decision, file.path(tables_dir, "table_new_firm_predictive_integration_decision.csv"), row.names = FALSE)
write.csv(input_manifest, file.path(logs_dir, "new_firm_predictive_input_manifest.csv"), row.names = FALSE)
write.csv(run_manifest, file.path(logs_dir, "new_firm_predictive_run_manifest.csv"), row.names = FALSE)
writeLines(note, file.path(logs_dir, "new_firm_predictive_integration_reviewer_note.md"))
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))

cat("\n[SUCCESS] New-firm posterior predictive integration audit completed.\n")
cat("Decision:", decision$audit_decision[[1]], "\n")
cat("Audit table:", file.path(tables_dir, "table_new_firm_predictive_integration_audit.csv"), "\n")
cat("Decision table:", file.path(tables_dir, "table_new_firm_predictive_integration_decision.csv"), "\n")
cat("Reviewer note:", file.path(logs_dir, "new_firm_predictive_integration_reviewer_note.md"), "\n")

if (any(audit$new_firm_integration_required & !audit$primary_reporting_allowed, na.rm = TRUE)) {
  cat("\n[NOTICE] Some Firm-RE out-of-firm quantities are not primary-reporting eligible until u_new integration is verified.\n")
}
