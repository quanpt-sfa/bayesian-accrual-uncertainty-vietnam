# -----------------------------------------------------------------------------
# Script: di02_new_firm_predictive_integration_audit.R
# Purpose: Audit whether primary out-of-firm posterior predictive quantities from
#          Firm-RE models verify integration over new-firm effects u_new.
#
# Intended use:
#   Rscript scripts/diagnostics/di02_new_firm_predictive_integration_audit.R
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
#   If this cannot be verified from the source that generated the specific
#   quantity, the quantity is suppressed from primary reporting by default.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di02", "New-firm predictive integration audit")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

script_start_time <- Sys.time()
script_name <- "scripts/diagnostics/di02_new_firm_predictive_integration_audit.R"
script_version <- "2026-06-19-v3-active-reorg-source-paths"

audit_root <- file.path(output_root, "new_firm_predictive_audit")
tables_dir <- file.path(audit_root, "tables")
logs_dir <- file.path(audit_root, "logs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

env_flag_local <- function(name, default = "FALSE") {
  raw <- Sys.getenv(name, default)
  toupper(raw) %in% c("TRUE", "1", "YES", "Y")
}

# strict_mode=TRUE makes ambiguous stacked predictive/tail quantities fail-safe:
# they are treated as potentially Firm-RE out-of-firm quantities unless their own
# source verifies u_new integration.
strict_mode <- env_flag_local("ACCRUAL_NEW_FIRM_AUDIT_STRICT", "TRUE")

# allow_uncertainty_mode controls whether brms sample_new_levels="uncertainty"
# qualifies as verified integration over new group effects. gaussian always
# qualifies; old_levels never qualifies for new-firm integration.
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

extract_matching_lines <- function(x, patterns, fixed = TRUE, max_n = 10) {
  if (!length(x)) return("")
  hits <- rep(FALSE, length(x))
  for (p in patterns) hits <- hits | grepl(p, x, fixed = fixed)
  out <- trimws(x[hits])
  out <- unique(out[nzchar(out)])
  if (!length(out)) return("")
  paste(utils::head(out, max_n), collapse = " | ")
}

nrows_or_na <- function(path) {
  if (!file.exists(path) || !grepl("\\.csv$", path, ignore.case = TRUE)) return(NA_integer_)
  tryCatch(nrow(read.csv(path, stringsAsFactors = FALSE)), error = function(e) NA_integer_)
}

file_size_or_na <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  as.numeric(file.info(path)$size)
}

mtime_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime)
}

file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

detect_latest_root <- function(latest_path) {
  if (!file.exists(latest_path)) return(NA_character_)
  x <- trimws(readLines(latest_path, warn = FALSE))
  x <- x[nzchar(x)]
  if (!length(x)) return(NA_character_)
  x[[1]]
}

detect_latest_kfold_root <- function() {
  completed <- detect_latest_root(file.path(winsor_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt"))
  if (!is.na(completed)) return(completed)
  NA_character_
}

detect_latest_row_kfold_root <- function() {
  candidates <- c(
    file.path(winsor_root, "row_exact_kfold", "LATEST_COMPLETED_RUN.txt")
  )
  for (p in candidates) {
    root <- detect_latest_root(p)
    if (!is.na(root)) return(root)
  }
  NA_character_
}

classify_source_code_evidence <- function(path, role) {
  lines <- read_text_if_exists(path)

  has_allow_new_levels <- contains_any(lines, c("allow_new_levels = TRUE", "allow_new_levels=TRUE"))
  has_sample_new_levels_gaussian <- contains_any(lines, c(
    'sample_new_levels = "gaussian"', "sample_new_levels='gaussian'"
  ))
  has_sample_new_levels_uncertainty <- contains_any(lines, c(
    'sample_new_levels = "uncertainty"', "sample_new_levels='uncertainty'"
  ))
  has_sample_new_levels_old_levels <- contains_any(lines, c(
    'sample_new_levels = "old_levels"', "sample_new_levels='old_levels'"
  ))
  has_re_formula_na <- contains_any(lines, c("re_formula = NA", "re.form = NA", "re_formula=NA", "re.form=NA"))
  has_re_formula_null_or_default <- contains_any(lines, c("re_formula = NULL", "re.form = NULL", "re_formula=NULL", "re.form=NULL"))
  has_custom_unew <- contains_any(lines, c("u_new", "sigma_u")) &&
    contains_any(lines, c("rnorm(", "sd_company", "sd_company__Intercept", "sigma_u"))
  has_posterior_predict <- contains_any(lines, c("posterior_predict", "posterior_epred"))
  has_log_lik_newdata <- contains_any(lines, c("log_lik(", "newdata"))

  source_verified <- has_sample_new_levels_gaussian ||
    (allow_uncertainty_mode && has_sample_new_levels_uncertainty) ||
    has_custom_unew

  source_partial <- has_allow_new_levels || has_re_formula_na || has_sample_new_levels_uncertainty

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
    has_sample_new_levels_gaussian = has_sample_new_levels_gaussian,
    has_sample_new_levels_uncertainty = has_sample_new_levels_uncertainty,
    has_sample_new_levels_gaussian_or_uncertainty = has_sample_new_levels_gaussian || has_sample_new_levels_uncertainty,
    has_sample_new_levels_old_levels = has_sample_new_levels_old_levels,
    has_re_formula_na_population_only = has_re_formula_na,
    has_re_formula_conditional = has_re_formula_null_or_default,
    has_custom_u_new_draw_logic = has_custom_unew,
    has_posterior_predict_or_epred = has_posterior_predict,
    has_log_lik_newdata = has_log_lik_newdata,
    source_verified_u_new_integration = source_verified,
    source_partial_new_level_evidence = source_partial,
    verification_basis = dplyr::case_when(
      has_custom_unew ~ "custom_u_new_draw_logic",
      has_sample_new_levels_gaussian ~ "brms_sample_new_levels_gaussian",
      allow_uncertainty_mode && has_sample_new_levels_uncertainty ~ "brms_sample_new_levels_uncertainty_allowed",
      has_sample_new_levels_uncertainty && !allow_uncertainty_mode ~ "uncertainty_mode_detected_but_not_allowed_by_env",
      source_partial ~ "partial_new_level_or_population_evidence_only",
      TRUE ~ "none"
    ),
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
    file.path("scripts", "ma10_construct_psis_loo_DA.R"),
    file.path("scripts", "robustness", "ro01_lofo_stacking.R"),
    file.path("scripts", "ma12_grouped_kfold_firm.R"),
    file.path("scripts", "simulation", "si03_brms_leakage_confirmation.R"),
    file.path("scripts", "ma13_row_level_exact_kfold.R"),
    file.path("scripts", "diagnostics", "di01_psis_reliability_gate.R")
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
  expected_source_role = c(
    "stacked_DA_draw_constructor",
    "stacked_DA_draw_constructor",
    "exact_grouped_kfold",
    "exact_grouped_kfold",
    "exact_grouped_kfold",
    "exact_grouped_kfold",
    "exact_row_kfold_candidate",
    "exact_row_kfold_candidate",
    "psis_reliability_gate_candidate"
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
    file.path(output_root, "psis_reliability_gate", "tables", "table_psis_reliability_gate.csv")
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

quantity_class_from_name <- function(quantity, output_role) {
  q <- tolower(as.character(quantity))
  r <- tolower(as.character(output_role))
  dplyr::case_when(
    grepl("tail_flag|tail_prob|ppd|predictive|da_z_predictive|posterior_predict|posterior_epred", q) ~ "posterior_predictive_tail_or_distribution",
    grepl("log_predictive_density|elpd|lpd|score", q) ~ "predictive_score",
    grepl("weight", q) | grepl("weights", r) ~ "model_weight",
    TRUE ~ "other"
  )
}

infer_quantity_rows_from_artifact <- function(output_role, expected_source_role, path) {
  x <- safe_read_csv(path)
  if (is.null(x)) {
    return(data.frame(
      output_role = output_role,
      expected_source_role = expected_source_role,
      target_space = NA_character_,
      model_id = NA_character_,
      heterogeneity_variant = NA_character_,
      quantity = output_role,
      quantity_class = quantity_class_from_name(output_role, output_role),
      uses_firm_re = NA,
      uses_firm_re_basis = "artifact_missing",
      out_of_firm_target = NA,
      out_of_firm_basis = "artifact_missing",
      source_artifact = path,
      artifact_exists = FALSE,
      artifact_columns = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  nm <- names(x)
  artifact_columns <- paste(nm, collapse = ",")

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

  q_cols <- character()
  if (output_role %in% c("stacked_DA_master_output_table", "stacked_DA_accruals_output_table")) {
    q_cols <- grep("tail_flag|tail_prob|ppd|predictive|DA_z_predictive|posterior", nm, value = TRUE, ignore.case = TRUE)
  } else if (grepl("observation_scores|model_scores", output_role)) {
    q_cols <- grep("log_predictive_density|elpd|lpd|score", nm, value = TRUE, ignore.case = TRUE)
  } else if (grepl("weights", output_role)) {
    q_cols <- grep("Weight|weight", nm, value = TRUE, ignore.case = TRUE)
  } else if (output_role == "psis_reliability_gate") {
    q_cols <- grep("pareto|psis|action|status", nm, value = TRUE, ignore.case = TRUE)
  }
  if (!length(q_cols)) q_cols <- output_role

  out <- expand.grid(
    target_space = target_space,
    model_id = model_id,
    heterogeneity_variant = heterogeneity_variant,
    quantity = utils::head(q_cols, 60),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      output_role = output_role,
      expected_source_role = expected_source_role,
      quantity_class = quantity_class_from_name(quantity, output_role),
      heterogeneity_indicates_firm_re = grepl("firm|random|RE|intercept", heterogeneity_variant, ignore.case = TRUE),
      stacked_predictive_tail_ambiguous = strict_mode &
        output_role %in% c("stacked_DA_master_output_table", "stacked_DA_accruals_output_table") &
        quantity_class == "posterior_predictive_tail_or_distribution",
      uses_firm_re = dplyr::case_when(
        heterogeneity_indicates_firm_re ~ TRUE,
        stacked_predictive_tail_ambiguous ~ TRUE,
        TRUE ~ FALSE
      ),
      uses_firm_re_basis = dplyr::case_when(
        heterogeneity_indicates_firm_re ~ "heterogeneity_variant_indicates_firm_re",
        stacked_predictive_tail_ambiguous ~ "strict_mode_stacked_predictive_tail_treated_as_potential_firmre",
        TRUE ~ "no_firmre_indicator_detected"
      ),
      out_of_firm_target = dplyr::case_when(
        output_role %in% c("grouped_kfold_observation_scores", "grouped_kfold_model_scores") ~ TRUE,
        grepl("lofo|out_of_firm", output_role, ignore.case = TRUE) ~ TRUE,
        stacked_predictive_tail_ambiguous ~ TRUE,
        TRUE ~ FALSE
      ),
      out_of_firm_basis = dplyr::case_when(
        output_role %in% c("grouped_kfold_observation_scores", "grouped_kfold_model_scores") ~ "grouped_firm_validation_target",
        grepl("lofo|out_of_firm", output_role, ignore.case = TRUE) ~ "role_name_indicates_out_of_firm",
        stacked_predictive_tail_ambiguous ~ "strict_mode_stacked_predictive_tail_assumed_out_of_firm_reporting_risk",
        TRUE ~ "not_out_of_firm_primary_quantity"
      ),
      source_artifact = path,
      artifact_exists = TRUE,
      artifact_columns = artifact_columns
    )

  out %>% select(output_role, expected_source_role, target_space, model_id, heterogeneity_variant,
                 quantity, quantity_class, uses_firm_re, uses_firm_re_basis,
                 out_of_firm_target, out_of_firm_basis, source_artifact,
                 artifact_exists, artifact_columns)
}

quantity_inventory <- bind_rows(lapply(seq_len(nrow(candidate_outputs)), function(i) {
  infer_quantity_rows_from_artifact(
    candidate_outputs$output_role[[i]],
    candidate_outputs$expected_source_role[[i]],
    candidate_outputs$path[[i]]
  )
}))

source_verification <- source_audit %>%
  transmute(
    expected_source_role = source_role,
    source_code_path = path,
    source_code_exists = exists,
    source_verified_u_new_integration = source_verified_u_new_integration,
    source_partial_new_level_evidence = source_partial_new_level_evidence,
    source_verification_basis = verification_basis,
    source_has_allow_new_levels = has_allow_new_levels,
    source_has_sample_new_levels_gaussian = has_sample_new_levels_gaussian,
    source_has_sample_new_levels_uncertainty = has_sample_new_levels_uncertainty,
    source_has_re_formula_na_population_only = has_re_formula_na_population_only,
    source_has_custom_u_new_draw_logic = has_custom_u_new_draw_logic,
    source_evidence_snippet = evidence_snippet
  )

# Quantity-level audit ---------------------------------------------------------

audit <- quantity_inventory %>%
  left_join(source_verification, by = "expected_source_role") %>%
  mutate(
    source_code_exists = ifelse(is.na(source_code_exists), FALSE, source_code_exists),
    source_verified_u_new_integration = ifelse(is.na(source_verified_u_new_integration), FALSE, source_verified_u_new_integration),
    source_partial_new_level_evidence = ifelse(is.na(source_partial_new_level_evidence), FALSE, source_partial_new_level_evidence),
    new_firm_integration_required = artifact_exists & uses_firm_re & out_of_firm_target &
      quantity_class == "posterior_predictive_tail_or_distribution",
    u_new_integrated_verified = ifelse(new_firm_integration_required, source_verified_u_new_integration, NA),
    verification_status = dplyr::case_when(
      !artifact_exists ~ "ARTIFACT_MISSING",
      !new_firm_integration_required ~ "NOT_REQUIRED",
      source_verified_u_new_integration ~ "PASS_VERIFIED_U_NEW_INTEGRATION_FROM_MATCHED_SOURCE",
      source_partial_new_level_evidence ~ "VERIFY_PARTIAL_MATCHED_SOURCE_EVIDENCE_ONLY",
      TRUE ~ "FAIL_NOT_VERIFIED_FROM_MATCHED_SOURCE"
    ),
    primary_reporting_allowed = dplyr::case_when(
      !artifact_exists ~ FALSE,
      !new_firm_integration_required ~ TRUE,
      source_verified_u_new_integration ~ TRUE,
      TRUE ~ FALSE
    ),
    suppression_reason = dplyr::case_when(
      artifact_exists == FALSE ~ "Source artifact missing.",
      !new_firm_integration_required ~ NA_character_,
      source_verified_u_new_integration ~ NA_character_,
      source_partial_new_level_evidence ~ paste0(
        "Firm-RE out-of-firm posterior predictive quantity requires u_new integration. Matched source role '",
        expected_source_role,
        "' has partial new-level/population-level evidence but no verified posterior predictive u_new draw integration."
      ),
      TRUE ~ paste0(
        "Firm-RE out-of-firm posterior predictive quantity requires u_new integration. Matched source role '",
        expected_source_role,
        "' does not verify u_new integration. Evidence from other source roles is not accepted."
      )
    ),
    design_requirement = "Primary Firm-RE out-of-firm posterior predictive quantities must integrate over u_new ~ Normal(0, sigma_u^2) in their own generating source.",
    required_action = dplyr::case_when(
      primary_reporting_allowed ~ "report_if_other_gates_pass",
      verification_status == "ARTIFACT_MISSING" ~ "do_not_report_missing_artifact",
      TRUE ~ "suppress_from_primary_RQ2_until_verified"
    ),
    strict_mode_applied = strict_mode,
    allow_uncertainty_mode_applied = allow_uncertainty_mode
  )

# Compact decision table -------------------------------------------------------

decision <- data.frame(
  audit_decision = if (any(audit$new_firm_integration_required & !audit$primary_reporting_allowed, na.rm = TRUE)) {
    "PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES"
  } else if (any(audit$new_firm_integration_required & audit$primary_reporting_allowed, na.rm = TRUE)) {
    "PASS_FOR_AVAILABLE_FIRMRE_OUT_OF_FIRM_QUANTITIES"
  } else {
    "NO_FIRMRE_OUT_OF_FIRM_PRIMARY_QUANTITIES_DETECTED"
  },
  verification_scope = "source_role_specific_not_global",
  strict_mode = strict_mode,
  allow_uncertainty_mode = allow_uncertainty_mode,
  n_quantities_audited = nrow(audit),
  n_required = sum(audit$new_firm_integration_required, na.rm = TRUE),
  n_required_primary_allowed = sum(audit$new_firm_integration_required & audit$primary_reporting_allowed, na.rm = TRUE),
  n_required_suppressed = sum(audit$new_firm_integration_required & !audit$primary_reporting_allowed, na.rm = TRUE),
  n_stacked_predictive_tail_required = sum(audit$output_role %in% c("stacked_DA_master_output_table", "stacked_DA_accruals_output_table") & audit$new_firm_integration_required, na.rm = TRUE),
  n_stacked_predictive_tail_suppressed = sum(audit$output_role %in% c("stacked_DA_master_output_table", "stacked_DA_accruals_output_table") & audit$new_firm_integration_required & !audit$primary_reporting_allowed, na.rm = TRUE),
  stringsAsFactors = FALSE
)

# Manuscript/appendix note -----------------------------------------------------

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
  "The audit is fail-safe. If integration over `u_new` is not verified in the matched source that generated the quantity, Firm-RE out-of-firm posterior predictive tail flags are suppressed from primary RQ2 reporting.",
  "",
  "## Source-specific verification",
  "",
  "Verification is not global. Evidence from `scripts/ma13_row_level_exact_kfold.R` can verify only row-level K-fold new-level scoring behavior; it does not verify posterior predictive tail draws created by `scripts/ma10_construct_psis_loo_DA.R`.",
  "",
  "Stacked DA predictive/tail quantities are matched to `scripts/ma10_construct_psis_loo_DA.R`. In strict mode, ambiguous stacked predictive/tail columns are treated as potentially Firm-RE out-of-firm quantities and require source-specific u_new verification.",
  "",
  "## Decision",
  "",
  paste0("- audit_decision: `", decision$audit_decision[[1]], "`"),
  paste0("- verification_scope: `", decision$verification_scope[[1]], "`"),
  paste0("- strict_mode: `", decision$strict_mode[[1]], "`"),
  paste0("- allow_uncertainty_mode: `", decision$allow_uncertainty_mode[[1]], "`"),
  paste0("- n_required: ", decision$n_required[[1]]),
  paste0("- n_required_primary_allowed: ", decision$n_required_primary_allowed[[1]]),
  paste0("- n_required_suppressed: ", decision$n_required_suppressed[[1]]),
  paste0("- n_stacked_predictive_tail_required: ", decision$n_stacked_predictive_tail_required[[1]]),
  paste0("- n_stacked_predictive_tail_suppressed: ", decision$n_stacked_predictive_tail_suppressed[[1]]),
  "",
  "## Interpretation",
  "",
  "Population-level grouped scores or `allow_new_levels=TRUE` are useful evidence, but they do not by themselves prove that posterior predictive tail draws integrated over `u_new` unless `sample_new_levels` or an equivalent custom draw construction is verified in the matched source that generated the tail quantity.",
  "",
  "This audit should be reported in Chapter 4 or the technical appendix before any Firm-RE out-of-firm posterior predictive tail flags are treated as primary evidence."
)

# Manifests and outputs --------------------------------------------------------

input_manifest <- data.frame(
  input_type = c(rep("source_code", nrow(source_paths)), rep("artifact", nrow(candidate_outputs))),
  role = c(source_paths$source_role, candidate_outputs$output_role),
  expected_source_role = c(source_paths$source_role, candidate_outputs$expected_source_role),
  path = c(source_paths$path, candidate_outputs$path),
  exists = file.exists(c(source_paths$path, candidate_outputs$path)),
  file_size_bytes = vapply(c(source_paths$path, candidate_outputs$path), file_size_or_na, numeric(1)),
  modified_time = vapply(c(source_paths$path, candidate_outputs$path), mtime_or_na, character(1)),
  file_hash = vapply(c(source_paths$path, candidate_outputs$path), file_hash_or_na, character(1)),
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
  kfold_root_source = "LATEST_COMPLETED_RUN.txt_only",
  row_kfold_root_source = "LATEST_COMPLETED_RUN.txt_only",
  verification_scope = "source_role_specific_not_global",
  design_requirement = "Primary Firm-RE out-of-firm posterior predictive quantities must integrate over u_new in their matched generating source.",
  stringsAsFactors = FALSE
)

write.csv(source_audit, file.path(tables_dir, "table_new_firm_predictive_source_code_audit.csv"), row.names = FALSE)
write.csv(artifact_inventory, file.path(tables_dir, "table_new_firm_predictive_artifact_inventory.csv"), row.names = FALSE)
write.csv(quantity_inventory, file.path(tables_dir, "table_new_firm_predictive_quantity_inventory.csv"), row.names = FALSE)
write.csv(audit, file.path(tables_dir, "table_new_firm_predictive_integration_audit.csv"), row.names = FALSE)
write.csv(decision, file.path(tables_dir, "table_new_firm_predictive_integration_decision.csv"), row.names = FALSE)
write.csv(input_manifest, file.path(logs_dir, "new_firm_predictive_input_manifest.csv"), row.names = FALSE)
write.csv(run_manifest, file.path(logs_dir, "new_firm_predictive_run_manifest.csv"), row.names = FALSE)
writeLines(note, file.path(logs_dir, "new_firm_predictive_integration_reviewer_note.md"))
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))

cat("\n[SUCCESS] New-firm posterior predictive integration audit completed.\n")
cat("Decision:", decision$audit_decision[[1]], "\n")
cat("Verification scope:", decision$verification_scope[[1]], "\n")
cat("Audit table:", file.path(tables_dir, "table_new_firm_predictive_integration_audit.csv"), "\n")
cat("Decision table:", file.path(tables_dir, "table_new_firm_predictive_integration_decision.csv"), "\n")
cat("Reviewer note:", file.path(logs_dir, "new_firm_predictive_integration_reviewer_note.md"), "\n")

if (any(audit$new_firm_integration_required & !audit$primary_reporting_allowed, na.rm = TRUE)) {
  cat("\n[NOTICE] Some Firm-RE out-of-firm posterior predictive quantities are not primary-reporting eligible until source-specific u_new integration is verified.\n")
}
phase_end("di02", "New-firm predictive integration audit")
