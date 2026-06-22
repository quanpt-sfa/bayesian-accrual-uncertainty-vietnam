# -----------------------------------------------------------------------------
# Script: di04_full_vs_strict_model_space_stacking.R
# Purpose: Compare exact-KFold stacking weights under the full admissible model
#          space versus a strict clean MCMC diagnostics model space.
#
# Intended use:
#   Rscript scripts/diagnostics/di04_full_vs_strict_model_space_stacking.R
#
# This is an artifact-only reviewer diagnostic. It does not fit or refit models.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("di04", "Full vs strict model-space stacking diagnostic")
if (exists("ensure_analysis_dirs", mode = "function")) ensure_analysis_dirs()

script_start <- Sys.time()
script_name <- "scripts/diagnostics/di04_full_vs_strict_model_space_stacking.R"
script_version <- "2026-06-22-v2-read-weight-audit"

diagnostics_dir <- file.path(output_root, "diagnostics")
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

mcmc_gate_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_gate_winsor.csv")
weight_audit_path <- file.path(output_root, "tables", "table_DA_exact_kfold_weight_audit.csv")
primary_gate_path <- file.path(output_root, "tables", "table_model_primary_inclusion_gate.csv")

weights_out_path <- file.path(diagnostics_dir, "table_full_vs_strict_stacking_weights.csv")
shift_out_path <- file.path(diagnostics_dir, "table_full_vs_strict_firmre_shift.csv")
exclusions_out_path <- file.path(diagnostics_dir, "table_strict_model_exclusions.csv")
figure_out_path <- file.path(diagnostics_dir, "figure_full_vs_strict_firmre_shift.png")
note_out_path <- file.path(diagnostics_dir, "full_vs_strict_stacking_note.md")

strict_max_rhat <- as.numeric(Sys.getenv("ACCRUAL_STRICT_MODEL_MAX_RHAT", "1.01"))
strict_min_ess <- as.numeric(Sys.getenv("ACCRUAL_STRICT_MODEL_MIN_ESS", "1000"))

read_csv_required <- function(path, label) {
  if (!file.exists(path)) stop("[BLOCKER] Missing ", label, ": ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}

normalise_variant <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    grepl("Firm RE|Random Intercept|firmre", x, ignore.case = TRUE) ~ "firm_re",
    grepl("Pooled", x, ignore.case = TRUE) ~ "pooled",
    TRUE ~ gsub("[^A-Za-z0-9]+", "_", tolower(x))
  )
}

write_empty_outputs <- function(reason) {
  empty_weights <- data.frame(
    validation_source = character(), target_space = character(), model_id = character(),
    model_name = character(), heterogeneity_variant = character(), model_variant_class = character(),
    original_weight = numeric(), full_admissible_included = logical(),
    strict_clean_included = logical(), full_weight_renormalized = numeric(),
    strict_weight_renormalized = numeric(), diagnostics_status = character(),
    primary_inclusion_decision = character(), strict_exclusion_reason = character(),
    stringsAsFactors = FALSE
  )
  empty_shift <- data.frame(
    validation_source = character(), target_space = character(), full_firmre_weight = numeric(),
    strict_firmre_weight = numeric(), delta_firmre_weight = numeric(),
    full_pooled_weight = numeric(), strict_pooled_weight = numeric(),
    firmre_weight_ratio_strict_over_full = numeric(), n_models_full = integer(),
    n_models_strict = integer(), n_models_excluded_by_strict = integer(),
    stringsAsFactors = FALSE
  )
  write.csv(empty_weights, weights_out_path, row.names = FALSE)
  write.csv(empty_shift, shift_out_path, row.names = FALSE)
  write.csv(empty_weights[0, ], exclusions_out_path, row.names = FALSE)
  writeLines(c("# Full vs Strict Model-Space Stacking Note", "", reason), note_out_path, useBytes = TRUE)
  phase_end("di04", "Full vs strict model-space stacking diagnostic")
  quit(save = "no", status = 0)
}

mcmc_gate <- read_csv_required(mcmc_gate_path, "MCMC diagnostics gate")
weight_audit <- read_csv_required(weight_audit_path, "exact-KFold DA weight audit")
primary_gate <- read_csv_required(primary_gate_path, "model primary inclusion gate")

read_weight_audit_rows <- function(x) {
  target_col <- first_existing_col(x, c("Target_Space", "target_space"))
  source_col <- first_existing_col(x, c("DA_Source", "validation_source", "source"))
  weight_col <- first_existing_col(x, c("Weight", "Exact_KFold_Weight", "Weight_KFold", "weight", "stacking_weight"))
  model_col <- first_existing_col(x, c("Model_ID", "model_id"))
  variant_col <- first_existing_col(x, c("Heterogeneity_Variant", "heterogeneity_variant"))
  if (any(is.na(c(target_col, source_col, weight_col, model_col, variant_col)))) return(NULL)
  out <- data.frame(
    validation_source = dplyr::case_when(
      grepl("row", as.character(x[[source_col]]), ignore.case = TRUE) ~ "row_exact_kfold",
      grepl("group", as.character(x[[source_col]]), ignore.case = TRUE) ~ "grouped_exact_kfold",
      TRUE ~ as.character(x[[source_col]])
    ),
    source_path = if ("Weight_File" %in% names(x)) as.character(x$Weight_File) else weight_audit_path,
    target_space = as.character(x[[target_col]]),
    model_id = as.character(x[[model_col]]),
    model_name = if ("Model_Name" %in% names(x)) as.character(x$Model_Name) else as.character(x[[model_col]]),
    heterogeneity_variant = as.character(x[[variant_col]]),
    model_variant_class = normalise_variant(x[[variant_col]]),
    original_weight = suppressWarnings(as.numeric(x[[weight_col]])),
    stringsAsFactors = FALSE
  )
  out[is.finite(out$original_weight) & out$original_weight > 0, , drop = FALSE]
}

weights <- read_weight_audit_rows(weight_audit)
if (is.null(weights) || !nrow(weights)) {
  write_empty_outputs("No usable exact-KFold model weight rows were available in table_DA_exact_kfold_weight_audit.csv.")
}

mcmc_key_cols <- c("model_id", "target_space", "model_variant_class")
mcmc_norm <- mcmc_gate %>%
  mutate(
    model_id = as.character(.data$model_id),
    target_space = as.character(if ("Target_Space" %in% names(.)) .data$Target_Space else .data$target_space),
    model_variant_class = normalise_variant(if ("Heterogeneity_Variant" %in% names(.)) .data$Heterogeneity_Variant else .data$heterogeneity_variant),
    diagnostics_status = as.character(.data$diagnostics_status),
    max_rhat = suppressWarnings(as.numeric(.data$max_rhat)),
    n_divergent = suppressWarnings(as.numeric(.data$n_divergent)),
    min_bulk_ess = suppressWarnings(as.numeric(.data$min_bulk_ess)),
    min_tail_ess = suppressWarnings(as.numeric(.data$min_tail_ess))
  ) %>%
  select(any_of(c(mcmc_key_cols, "diagnostics_status", "max_rhat", "n_divergent", "min_bulk_ess", "min_tail_ess", "fail_reason", "warning_reason"))) %>%
  distinct()

primary_decision_col <- first_existing_col(primary_gate, c("Primary_Inclusion_Decision", "primary_inclusion_decision"))
primary_model_col <- first_existing_col(primary_gate, c("model_id", "Model_ID"))
primary_norm <- if (!is.na(primary_decision_col) && !is.na(primary_model_col)) {
  primary_gate %>%
    mutate(
      model_id = as.character(.data[[primary_model_col]]),
      target_space = as.character(if ("target_space" %in% names(.)) .data$target_space else if ("Target_Space" %in% names(.)) .data$Target_Space else NA_character_),
      model_variant_class = normalise_variant(if ("Heterogeneity_Variant" %in% names(.)) .data$Heterogeneity_Variant else if ("heterogeneity_variant" %in% names(.)) .data$heterogeneity_variant else ""),
      primary_inclusion_decision = as.character(.data[[primary_decision_col]])
    ) %>%
    select(any_of(c(mcmc_key_cols, "primary_inclusion_decision"))) %>%
    distinct()
} else {
  weights %>% distinct(across(all_of(mcmc_key_cols))) %>% mutate(primary_inclusion_decision = "INCLUDE_PRIMARY")
}

joined <- weights %>%
  left_join(mcmc_norm, by = mcmc_key_cols) %>%
  left_join(primary_norm, by = mcmc_key_cols) %>%
  mutate(
    primary_inclusion_decision = ifelse(is.na(.data$primary_inclusion_decision), "INCLUDE_PRIMARY", .data$primary_inclusion_decision),
    full_admissible_included = .data$primary_inclusion_decision %in% c("INCLUDE_PRIMARY", "MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS"),
    strict_clean_included = .data$full_admissible_included &
      .data$diagnostics_status == "PASS" &
      is.finite(.data$max_rhat) & .data$max_rhat <= strict_max_rhat &
      is.finite(.data$n_divergent) & .data$n_divergent == 0 &
      is.finite(.data$min_bulk_ess) & .data$min_bulk_ess >= strict_min_ess &
      is.finite(.data$min_tail_ess) & .data$min_tail_ess >= strict_min_ess,
    strict_exclusion_reason = case_when(
      !.data$full_admissible_included ~ paste0("not_full_admissible:", .data$primary_inclusion_decision),
      is.na(.data$diagnostics_status) ~ "missing_mcmc_diagnostics",
      .data$diagnostics_status != "PASS" ~ paste0("diagnostics_status_", .data$diagnostics_status),
      !is.finite(.data$max_rhat) | .data$max_rhat > strict_max_rhat ~ "rhat_above_strict_threshold",
      !is.finite(.data$n_divergent) | .data$n_divergent != 0 ~ "divergences_present",
      !is.finite(.data$min_bulk_ess) | .data$min_bulk_ess < strict_min_ess ~ "bulk_ess_below_strict_threshold",
      !is.finite(.data$min_tail_ess) | .data$min_tail_ess < strict_min_ess ~ "tail_ess_below_strict_threshold",
      TRUE ~ ""
    )
  ) %>%
  group_by(.data$validation_source, .data$target_space) %>%
  mutate(
    full_weight_renormalized = ifelse(.data$full_admissible_included, .data$original_weight / sum(.data$original_weight[.data$full_admissible_included], na.rm = TRUE), 0),
    strict_weight_renormalized = ifelse(.data$strict_clean_included, .data$original_weight / sum(.data$original_weight[.data$strict_clean_included], na.rm = TRUE), 0)
  ) %>%
  ungroup()

shift <- joined %>%
  group_by(.data$validation_source, .data$target_space) %>%
  summarise(
    full_firmre_weight = sum(.data$full_weight_renormalized[.data$model_variant_class == "firm_re"], na.rm = TRUE),
    strict_firmre_weight = sum(.data$strict_weight_renormalized[.data$model_variant_class == "firm_re"], na.rm = TRUE),
    delta_firmre_weight = .data$strict_firmre_weight - .data$full_firmre_weight,
    full_pooled_weight = sum(.data$full_weight_renormalized[.data$model_variant_class == "pooled"], na.rm = TRUE),
    strict_pooled_weight = sum(.data$strict_weight_renormalized[.data$model_variant_class == "pooled"], na.rm = TRUE),
    firmre_weight_ratio_strict_over_full = ifelse(.data$full_firmre_weight > 0, .data$strict_firmre_weight / .data$full_firmre_weight, NA_real_),
    n_models_full = sum(.data$full_admissible_included, na.rm = TRUE),
    n_models_strict = sum(.data$strict_clean_included, na.rm = TRUE),
    n_models_excluded_by_strict = sum(.data$full_admissible_included & !.data$strict_clean_included, na.rm = TRUE),
    .groups = "drop"
  )

exclusions <- joined %>%
  filter(.data$full_admissible_included & !.data$strict_clean_included) %>%
  arrange(.data$validation_source, .data$target_space, .data$model_id, .data$model_variant_class)

write.csv(joined, weights_out_path, row.names = FALSE)
write.csv(shift, shift_out_path, row.names = FALSE)
write.csv(exclusions, exclusions_out_path, row.names = FALSE)

if (requireNamespace("ggplot2", quietly = TRUE) && nrow(shift)) {
  fig <- ggplot2::ggplot(shift, ggplot2::aes(x = target_space, y = delta_firmre_weight, fill = validation_source)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::labs(x = "Target space", y = "Strict minus full Firm-RE weight", fill = "Validation source") +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(figure_out_path, fig, width = 7, height = 4, dpi = 160)
}

note <- c(
  "# Full vs Strict Model-Space Stacking Note",
  "",
  paste0("Script version: ", script_version),
  "",
  "Full admissible space keeps models marked INCLUDE_PRIMARY or MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS.",
  paste0("Strict clean space additionally requires diagnostics_status == PASS, zero divergences, max Rhat <= ",
         strict_max_rhat, ", and min bulk/tail ESS >= ", strict_min_ess, "."),
  "",
  "Weights are renormalized within validation source and target space. The Firm-RE shift is strict Firm-RE weight minus full Firm-RE weight.",
  "",
  paste0("Weight rows: ", nrow(joined)),
  paste0("Strict exclusions: ", nrow(exclusions)),
  paste0("Runtime seconds: ", round(as.numeric(difftime(Sys.time(), script_start, units = "secs")), 2))
)
writeLines(note, note_out_path, useBytes = TRUE)

cat("[SUCCESS] di04 outputs written under ", diagnostics_dir, "\n", sep = "")
phase_end("di04", "Full vs strict model-space stacking diagnostic")
