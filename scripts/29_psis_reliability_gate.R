# -----------------------------------------------------------------------------
# Script: 29_psis_reliability_gate.R
# Purpose: Reviewer-final PSIS Pareto-k reliability gate for empirical row-LOO
#          and optional BRMS simulation diagnostics.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/00_helpers.R")
ensure_analysis_dirs()

script_name <- "scripts/29_psis_reliability_gate.R"
script_version <- "2026-06-18-psis-reliability-gate-v2-schema-complete"
script_start_time <- Sys.time()
format_time <- function(x) format(x, "%Y-%m-%d %H:%M:%S %Z")

gate_root <- file.path(output_root, "psis_reliability_gate")
tables_dir <- file.path(gate_root, "tables")
logs_dir <- file.path(gate_root, "logs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

paths <- c(
  loo_comparison = file.path(output_root, "tables", "table_loo_comparison_winsor_corrected.csv"),
  brms_diagnostics = file.path(output_root, "tables", "table_brms_diagnostics_winsor.csv"),
  simulation_brms = file.path(output_root, "simulation", "brms_leakage_confirmation", "tables",
                              "table_brms_leakage_confirmation_rep_results.csv"),
  loo_cache_dir = file.path(output_root, "draws", "loo_cache")
)

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
}

get_col <- function(df, candidates, default = NA) {
  if (nrow(df) == 0) return(default)
  for (nm in candidates) if (nm %in% names(df)) return(df[[nm]])
  rep(default, nrow(df))
}

get_scalar <- function(row, candidates, default = NA) {
  for (nm in candidates) if (nm %in% names(row)) return(row[[nm]][1])
  default
}

num_or_na <- function(x) suppressWarnings(as.numeric(x))
int_or_na <- function(x) suppressWarnings(as.integer(x))

loo_comp <- safe_read_csv(paths[["loo_comparison"]])
diag <- safe_read_csv(paths[["brms_diagnostics"]])
sim <- safe_read_csv(paths[["simulation_brms"]])

if (nrow(loo_comp) == 0 && nrow(diag) == 0 && nrow(sim) == 0) {
  stop("[BLOCKER] No PSIS source table exists for the reliability gate.")
}

source_exists <- data.frame(
  input_name = names(paths),
  path = unname(paths),
  exists = file.exists(paths) | dir.exists(paths),
  row_count = c(nrow(loo_comp), nrow(diag), nrow(sim), NA_integer_),
  stringsAsFactors = FALSE
)

extract_cache_pareto <- function(row) {
  cache_dir <- paths[["loo_cache_dir"]]
  if (!dir.exists(cache_dir)) {
    return(list(max_k = NA_real_, n_gt_07 = NA_integer_, n_gt_10 = NA_integer_, n = NA_integer_,
                source = NA_character_))
  }

  model_id <- as.character(get_scalar(row, c("Model_ID", "model_id"), NA_character_))
  target_space <- as.character(get_scalar(row, c("Target_Space", "target_space"), NA_character_))
  sample_group <- as.character(get_scalar(row, c("Sample_Group", "sample_group"), "main_common"))
  heterogeneity_variant <- as.character(get_scalar(row, c("Heterogeneity_Variant", "heterogeneity_variant"), ""))

  if (any(is.na(c(model_id, target_space)))) {
    return(list(max_k = NA_real_, n_gt_07 = NA_integer_, n_gt_10 = NA_integer_, n = NA_integer_,
                source = NA_character_))
  }

  base_key <- tryCatch(
    model_key_sampled(model_id, target_space, sample_group, heterogeneity_variant, "_winsor"),
    error = function(e) NA_character_
  )

  candidates <- character()
  if (!is.na(base_key)) {
    candidates <- c(candidates, file.path(cache_dir, paste0(base_key, "_loo.rds")))
    pattern <- paste0("^", gsub("([+().])", "\\\\\\1", base_key), ".*\\.rds$")
    candidates <- c(candidates, list.files(cache_dir, pattern = pattern, full.names = TRUE))
  }
  candidates <- unique(candidates[file.exists(candidates)])
  if (length(candidates) == 0) {
    return(list(max_k = NA_real_, n_gt_07 = NA_integer_, n_gt_10 = NA_integer_, n = NA_integer_,
                source = NA_character_))
  }

  obj <- tryCatch(readRDS(candidates[1]), error = function(e) NULL)
  pk <- tryCatch(obj$diagnostics$pareto_k, error = function(e) NULL)
  if (is.null(pk)) {
    return(list(max_k = NA_real_, n_gt_07 = NA_integer_, n_gt_10 = NA_integer_, n = NA_integer_,
                source = candidates[1]))
  }
  list(
    max_k = suppressWarnings(max(pk, na.rm = TRUE)),
    n_gt_07 = sum(pk > 0.7, na.rm = TRUE),
    n_gt_10 = sum(pk > 1.0, na.rm = TRUE),
    n = length(pk),
    source = candidates[1]
  )
}

status_from_psis <- function(max_k, n_gt_07, n_gt_10, corrected_k_above_07, pareto_available = TRUE) {
  if (!isTRUE(pareto_available)) return("FAIL")
  if (!is.na(n_gt_10) && n_gt_10 > 0) return("FAIL")
  if (!is.na(max_k) && max_k > 1.0) return("FAIL")
  if ((!is.na(n_gt_07) && n_gt_07 > 0) ||
      (!is.na(corrected_k_above_07) && corrected_k_above_07 > 0) ||
      (!is.na(max_k) && max_k > 0.7)) return("REVIEW")
  if (!is.na(max_k) && max_k <= 0.7) return("PASS")
  if (!is.na(corrected_k_above_07) && corrected_k_above_07 == 0) return("PASS")
  "FAIL"
}

action_from_status <- function(status, pareto_available = TRUE) {
  if (!isTRUE(pareto_available)) return("missing_psis_diagnostics")
  dplyr::case_when(
    status == "PASS" ~ "use_psis_score",
    status == "REVIEW" ~ "exact_refit_sensitivity_required",
    status == "FAIL" ~ "exclude_from_primary_psis_inference",
    TRUE ~ "missing_psis_diagnostics"
  )
}

empirical_rows <- data.frame()
if (nrow(loo_comp) > 0) {
  empirical_rows <- bind_rows(lapply(seq_len(nrow(loo_comp)), function(i) {
    row <- loo_comp[i, , drop = FALSE]
    cs <- extract_cache_pareto(row)

    n_obs_source <- num_or_na(get_scalar(row, c("N_Obs", "n_obs", "n", "N"), NA_real_))
    n_obs <- if (!is.na(cs$n)) cs$n else n_obs_source

    corrected <- int_or_na(get_scalar(row, c("corrected_k_above_07", "Corrected_K_Above_07",
                                             "corrected_pareto_k_gt_0_7"), NA_integer_))
    n_k_gt_07_source <- int_or_na(get_scalar(row, c("pareto_k_gt_0_7", "n_k_gt_0_7",
                                                    "N_K_GT_0_7", "k_gt_0_7"), NA_integer_))
    max_k_source <- num_or_na(get_scalar(row, c("max_pareto_k", "Max_Pareto_K", "pareto_k_max"), NA_real_))

    n_k_gt_07 <- if (!is.na(cs$n_gt_07)) cs$n_gt_07 else if (!is.na(n_k_gt_07_source)) n_k_gt_07_source else corrected
    n_k_gt_10 <- cs$n_gt_10
    max_k <- if (!is.na(cs$max_k)) cs$max_k else max_k_source

    pareto_available <- !is.na(max_k) || !is.na(n_k_gt_07) || !is.na(corrected)
    status <- status_from_psis(max_k, n_k_gt_07, n_k_gt_10, corrected, pareto_available)

    data.frame(
      source_script = "scripts/09_loo_stacking.R",
      source_context = "empirical_row_loo",
      model_id = as.character(get_scalar(row, c("Model_ID", "model_id"), NA_character_)),
      model_name = as.character(get_scalar(row, c("Model_Name", "model_name"), NA_character_)),
      target_space = as.character(get_scalar(row, c("Target_Space", "target_space"), NA_character_)),
      sample_group = as.character(get_scalar(row, c("Sample_Group", "sample_group"), NA_character_)),
      heterogeneity_variant = as.character(get_scalar(row, c("Heterogeneity_Variant", "heterogeneity_variant"), NA_character_)),
      n_obs = n_obs,
      max_pareto_k = max_k,
      n_k_gt_0_7 = n_k_gt_07,
      share_k_gt_0_7 = ifelse(is.na(n_obs) || n_obs == 0 || is.na(n_k_gt_07), NA_real_, n_k_gt_07 / n_obs),
      n_k_gt_1_0 = n_k_gt_10,
      share_k_gt_1_0 = ifelse(is.na(n_obs) || n_obs == 0 || is.na(n_k_gt_10), NA_real_, n_k_gt_10 / n_obs),
      moment_match_applied = get_scalar(row, c("moment_match_applied", "Moment_Match_Applied"), NA),
      corrected_k_above_07 = corrected,
      psis_reliability_status = status,
      action = action_from_status(status, pareto_available),
      reviewer_relevance = "R2_Pareto_k_gate",
      pareto_source = ifelse(is.na(cs$source), "table_or_missing", cs$source),
      stringsAsFactors = FALSE
    )
  }))
}

sim_rows <- data.frame()
if (nrow(sim) > 0) {
  make_sim_rows <- function(kind) {
    max_col <- paste0("max_pareto_k_", kind)
    if (!max_col %in% names(sim)) return(data.frame())
    bind_rows(lapply(seq_len(nrow(sim)), function(i) {
      row <- sim[i, , drop = FALSE]
      max_k <- num_or_na(row[[max_col]][1])
      pareto_available <- !is.na(max_k)
      n_obs <- num_or_na(get_scalar(row, c("n_obs", "N_Obs"), NA_real_))
      n_gt_07 <- ifelse(!is.na(max_k), as.integer(max_k > 0.7), NA_integer_)
      n_gt_10 <- ifelse(!is.na(max_k), as.integer(max_k > 1.0), NA_integer_)
      status <- status_from_psis(max_k, n_gt_07, n_gt_10, NA_integer_, pareto_available)
      data.frame(
        source_script = "scripts/26_sim_brms_leakage_confirmation.R",
        source_context = "brms_simulation",
        model_id = paste0("SIM_", toupper(kind)),
        model_name = paste("Simulation", kind),
        target_space = "simulation",
        sample_group = paste0("rep_", get_scalar(row, c("rep_id", "Rep_ID"), i)),
        heterogeneity_variant = kind,
        n_obs = n_obs,
        max_pareto_k = max_k,
        n_k_gt_0_7 = n_gt_07,
        share_k_gt_0_7 = ifelse(is.na(n_obs) || n_obs == 0 || is.na(n_gt_07), NA_real_, n_gt_07 / n_obs),
        n_k_gt_1_0 = n_gt_10,
        share_k_gt_1_0 = ifelse(is.na(n_obs) || n_obs == 0 || is.na(n_gt_10), NA_real_, n_gt_10 / n_obs),
        moment_match_applied = NA,
        corrected_k_above_07 = NA_integer_,
        psis_reliability_status = status,
        action = action_from_status(status, pareto_available),
        reviewer_relevance = "R2_Pareto_k_gate",
        pareto_source = "simulation_table",
        stringsAsFactors = FALSE
      )
    }))
  }
  sim_rows <- bind_rows(make_sim_rows("pooled"), make_sim_rows("firmre"))
}

gate <- bind_rows(empirical_rows, sim_rows)
if (nrow(gate) == 0) stop("[BLOCKER] PSIS source tables existed but no gate rows could be constructed.")

summary_gate <- gate %>%
  group_by(target_space, heterogeneity_variant) %>%
  summarise(
    n_models = n(),
    n_pass = sum(psis_reliability_status == "PASS", na.rm = TRUE),
    n_review = sum(psis_reliability_status == "REVIEW", na.rm = TRUE),
    n_fail = sum(psis_reliability_status == "FAIL", na.rm = TRUE),
    max_pareto_k_overall = suppressWarnings(max(max_pareto_k, na.rm = TRUE)),
    total_high_k_obs = sum(n_k_gt_0_7, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(max_pareto_k_overall = ifelse(is.infinite(max_pareto_k_overall), NA_real_, max_pareto_k_overall)) %>%
  arrange(target_space, heterogeneity_variant)

input_manifest <- source_exists

run_manifest <- data.frame(
  Script_Name = script_name,
  Script_Version = script_version,
  Start_Time = format_time(script_start_time),
  End_Time = format_time(Sys.time()),
  Runtime_Seconds = as.numeric(difftime(Sys.time(), script_start_time, units = "secs")),
  Output_Root = output_root,
  Gate_Root = gate_root,
  N_Gate_Rows = nrow(gate),
  N_FAIL = sum(gate$psis_reliability_status == "FAIL", na.rm = TRUE),
  N_REVIEW = sum(gate$psis_reliability_status == "REVIEW", na.rm = TRUE),
  N_PASS = sum(gate$psis_reliability_status == "PASS", na.rm = TRUE),
  stringsAsFactors = FALSE
)

write.csv(gate, file.path(tables_dir, "table_psis_reliability_gate.csv"), row.names = FALSE)
write.csv(summary_gate, file.path(tables_dir, "table_psis_reliability_summary.csv"), row.names = FALSE)
write.csv(input_manifest, file.path(tables_dir, "psis_reliability_gate_input_manifest.csv"), row.names = FALSE)
write.csv(run_manifest, file.path(tables_dir, "psis_reliability_gate_manifest.csv"), row.names = FALSE)
write.csv(run_manifest, file.path(logs_dir, "psis_reliability_gate_manifest.csv"), row.names = FALSE)

note <- c(
  "# PSIS reliability gate reviewer note",
  "",
  paste("- Script:", script_name),
  paste("- Version:", script_version),
  paste("- Gate rows:", nrow(gate)),
  "",
  "PSIS-LOO is retained as a conventional row-level Bayesian diagnostic.",
  "The Pareto-k gate determines whether a PSIS score is reliable enough to report as PSIS evidence.",
  "PASS rows can use PSIS scores if all other gates pass.",
  "REVIEW rows require exact-refit sensitivity or explicit caution.",
  "FAIL rows are excluded from primary PSIS inference when exact-refit alternatives are available.",
  "",
  "Exact row-level K-fold provides the method-matched validation-unit comparison when PSIS is unreliable or when the primary RQ1 comparison requires exact-vs-exact refits.",
  "High Pareto-k in Firm-RE models is reported as part of the structured-CV diagnostic, not hidden."
)
writeLines(note, file.path(tables_dir, "psis_reliability_reviewer_note.md"))
writeLines(note, file.path(logs_dir, "psis_reliability_reviewer_note.md"))
writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))

message("PSIS reliability gate completed: ", gate_root)
