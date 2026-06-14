# -----------------------------------------------------------------------------
# Script: 05_winsorize_common_samples.R
# Purpose: Winsorize common samples at 1st/99th percentiles before any
#          z-standardization, and create winsorized formula/registry tables.
# -----------------------------------------------------------------------------

source("scripts/00_helpers.R")
ensure_analysis_dirs()

set.seed(42)

sample_specs <- data.frame(
  Sample = c(
    "Main Ex-Post Winsor",
    "Main No-Lookahead Winsor",
    "Secondary OperatingCycle Ex-Post Winsor",
    "Secondary OperatingCycle No-Lookahead Winsor",
    "M08 ex-post subsample",
    "M08 no-look-ahead subsample"
  ),
  Input = c(
    "final_common_ex_post_sample.csv",
    "final_common_realtime_sample.csv",
    "final_secondary_operating_cycle_ex_post_sample.csv",
    "final_secondary_operating_cycle_realtime_sample.csv",
    "final_M08_ex_post_subsample.csv",
    "final_M08_realtime_subsample.csv"
  ),
  Output = c(
    "final_common_ex_post_sample_winsor.csv",
    "final_common_realtime_sample_winsor.csv",
    "final_secondary_operating_cycle_ex_post_sample_winsor.csv",
    "final_secondary_operating_cycle_realtime_sample_winsor.csv",
    "final_M08_ex_post_subsample_winsor.csv",
    "final_M08_realtime_subsample_winsor.csv"
  ),
  Sample_Group = c(
    "main_common",
    "main_common",
    "secondary_operating_cycle",
    "secondary_operating_cycle",
    "secondary_volatility",
    "secondary_volatility"
  ),
  stringsAsFactors = FALSE
)

desc_rows <- list()
cutoff_rows <- list()
sd_rows <- list()
notes <- c(
  "Phase 1b winsorization notes",
  "Winsorization rule: continuous accrual/model variables capped at sample-specific 1st/99th percentiles.",
  "Winsorization is applied before downstream z-standardization.",
  paste("Continuous variables considered:", paste(continuous_vars_to_winsor, collapse = ", ")),
  paste("Binary variables explicitly not winsorized:", paste(binary_vars_do_not_winsor, collapse = ", ")),
  ""
)

for (i in seq_len(nrow(sample_specs))) {
  spec <- sample_specs[i, ]
  input_path <- file.path(baseline_root, "tables", spec$Input)
  output_path <- file.path(winsor_root, "tables", spec$Output)
  if (!file.exists(input_path)) stop("[BLOCKER] Missing input sample: ", input_path)

  df_before <- read.csv(input_path, stringsAsFactors = FALSE)
  df_after <- df_before
  n_before <- nrow(df_before)

  notes <- c(notes, sprintf("Sample: %s | input rows: %d", spec$Sample, n_before))
  message(sprintf("\n=== %s ===", spec$Sample))
  message("Rows before winsorization: ", n_before)

  vars_for_sample <- continuous_vars_to_winsor
  if (spec$Sample_Group != "secondary_operating_cycle") {
    vars_for_sample <- setdiff(vars_for_sample, "operating_cycle")
  }
  present_vars <- intersect(vars_for_sample, colnames(df_before))
  missing_vars <- setdiff(continuous_vars_to_winsor, colnames(df_before))
  if (length(missing_vars) > 0) {
    notes <- c(notes, sprintf("  Missing continuous variables in this sample: %s", paste(missing_vars, collapse = ", ")))
  }

  for (v in present_vars) {
    before <- df_before[[v]]
    after <- winsorize_vec(before)
    df_after[[v]] <- after

    b <- describe_numeric(before)
    a <- describe_numeric(after)
    sd_shrinkage_pct <- if (!is.na(b["SD"]) && b["SD"] != 0) 100 * (b["SD"] - a["SD"]) / b["SD"] else NA_real_
    max_reduction_ratio <- if (!is.na(b["Max"]) && b["Max"] != 0) abs(a["Max"] / b["Max"]) else NA_real_
    desc_rows[[length(desc_rows) + 1]] <- data.frame(
      Sample = spec$Sample,
      Variable = v,
      N = as.integer(b["N"]),
      Mean_Before = b["Mean"],
      SD_Before = b["SD"],
      Min_Before = b["Min"],
      P01_Before = b["P01"],
      P05_Before = b["P05"],
      P25_Before = b["P25"],
      Median_Before = b["Median"],
      P75_Before = b["P75"],
      P95_Before = b["P95"],
      P99_Before = b["P99"],
      Max_Before = b["Max"],
      Mean_After = a["Mean"],
      SD_After = a["SD"],
      Min_After = a["Min"],
      P01_After = a["P01"],
      P05_After = a["P05"],
      P25_After = a["P25"],
      Median_After = a["Median"],
      P75_After = a["P75"],
      P95_After = a["P95"],
      P99_After = a["P99"],
      Max_After = a["Max"],
      SD_Shrinkage_Pct = sd_shrinkage_pct,
      Max_Reduction_Ratio = max_reduction_ratio,
      Notes = "Winsorized at sample-specific 1/99 percentiles",
      stringsAsFactors = FALSE
    )

    qs <- quantile(before, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE, type = 7)
    n_nonmissing <- sum(!is.na(before))
    n_below <- sum(before < qs[1], na.rm = TRUE)
    n_above <- sum(before > qs[2], na.rm = TRUE)
    cutoff_rows[[length(cutoff_rows) + 1]] <- data.frame(
      Sample = spec$Sample,
      Variable = v,
      P01_Cutoff = qs[1],
      P99_Cutoff = qs[2],
      N_Below_P01 = n_below,
      N_Above_P99 = n_above,
      Share_Below_P01 = n_below / n_nonmissing,
      Share_Above_P99 = n_above / n_nonmissing,
      stringsAsFactors = FALSE
    )

    sd_rows[[length(sd_rows) + 1]] <- data.frame(
      Sample = spec$Sample,
      Variable = v,
      SD_Before = b["SD"],
      SD_After = a["SD"],
      SD_Shrinkage_Pct = sd_shrinkage_pct,
      stringsAsFactors = FALSE
    )
  }

  touched_binary <- intersect(binary_vars_do_not_winsor, present_vars)
  if (length(touched_binary) > 0) stop("[BLOCKER] Binary variables entered winsorization set: ", paste(touched_binary, collapse = ", "))

  df_after$winsorization_1_99_applied <- TRUE
  df_after$winsorization_scope <- "sample_specific_continuous_variables"
  write.csv(df_after, output_path, row.names = FALSE)

  n_after <- nrow(df_after)
  message("Rows after winsorization:  ", n_after)
  if (n_before != n_after) stop("[BLOCKER] Row count changed for ", spec$Sample)
  notes <- c(notes, sprintf("  output rows: %d | row count unchanged: %s", n_after, n_before == n_after), "")
}

desc_df <- do.call(rbind, desc_rows)
cutoff_df <- do.call(rbind, cutoff_rows)
sd_df <- do.call(rbind, sd_rows)

write.csv(desc_df, file.path(winsor_root, "tables", "table_winsor_before_after_descriptives.csv"), row.names = FALSE)
write.csv(cutoff_df, file.path(winsor_root, "tables", "table_winsor_cutoffs.csv"), row.names = FALSE)
write.csv(sd_df, file.path(winsor_root, "tables", "table_winsor_sd_shrinkage.csv"), row.names = FALSE)

appendix_df <- desc_df[desc_df$Variable %in% appendix1_vars, ]
appendix_df <- appendix_df[order(appendix_df$Sample, match(appendix_df$Variable, appendix1_vars)), ]
write.csv(appendix_df, file.path(winsor_root, "tables", "table_winsor_appendix1_descriptives_corrected.csv"), row.names = FALSE)

sample_summary <- do.call(rbind, lapply(seq_len(nrow(sample_specs)), function(i) {
  spec <- sample_specs[i, ]
  path <- file.path(winsor_root, "tables", spec$Output)
  df <- read.csv(path, stringsAsFactors = FALSE)
  data.frame(
    Sample_Name = spec$Sample,
    N_Obs = nrow(df),
    N_Firms = length(unique(df$company)),
    Min_Year = if (nrow(df) == 0) NA_integer_ else min(df$year),
    Max_Year = if (nrow(df) == 0) NA_integer_ else max(df$year),
    Sample_Group = spec$Sample_Group,
    Requires_Operating_Cycle = spec$Sample_Group == "secondary_operating_cycle",
    Notes = ifelse(spec$Sample_Group == "main_common",
                   "Main sample excludes operating_cycle as a required filter.",
                   ifelse(spec$Sample_Group == "secondary_operating_cycle",
                          "Secondary operating-cycle sample for M10 robustness.",
                          "Secondary rolling-volatility sample for M08 robustness.")),
    stringsAsFactors = FALSE
  )
}))
write.csv(sample_summary, file.path(winsor_root, "tables", "table_winsor_sample_summary.csv"), row.names = FALSE)

tail_audit_rows <- list()
for (i in which(sample_specs$Sample_Group == "secondary_operating_cycle")) {
  spec <- sample_specs[i, ]
  before <- read.csv(file.path(baseline_root, "tables", spec$Input), stringsAsFactors = FALSE)
  after <- read.csv(file.path(winsor_root, "tables", spec$Output), stringsAsFactors = FALSE)
  before$INV_over_COGS <- ifelse(!is.na(before$COGS) & before$COGS != 0, before$INV / before$COGS, NA_real_)
  after$INV_over_COGS <- ifelse(!is.na(after$COGS) & after$COGS != 0, after$INV / after$COGS, NA_real_)
  for (v in c("COGS", "INV", "INV_over_COGS", "operating_cycle")) {
    b <- describe_numeric(before[[v]])
    a <- describe_numeric(after[[v]])
    tail_audit_rows[[length(tail_audit_rows) + 1]] <- data.frame(
      Sample_Name = spec$Sample,
      Variable = v,
      N_Before = b["N"],
      Min_Before = b["Min"],
      P01_Before = b["P01"],
      Median_Before = b["Median"],
      Mean_Before = b["Mean"],
      P99_Before = b["P99"],
      Max_Before = b["Max"],
      N_After = a["N"],
      Min_After = a["Min"],
      P01_After = a["P01"],
      Median_After = a["Median"],
      Mean_After = a["Mean"],
      P99_After = a["P99"],
      Max_After = a["Max"],
      Notes = "COGS/INV audit variables; operating_cycle is winsorized only in secondary operating-cycle samples.",
      stringsAsFactors = FALSE
    )
  }
}
write.csv(do.call(rbind, tail_audit_rows), file.path(winsor_root, "tables", "table_operating_cycle_tail_audit_winsor.csv"), row.names = FALSE)

formulas_path <- file.path(baseline_root, "tables", "table_named_model_formulas.csv")
registry_path <- file.path(baseline_root, "tables", "table_model_registry.csv")
if (!file.exists(formulas_path)) stop("[BLOCKER] Missing formula table: ", formulas_path)
if (!file.exists(registry_path)) stop("[BLOCKER] Missing model registry: ", registry_path)

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
sample_map <- setNames(sample_specs$Output, sample_specs$Input)
formulas_df$Target_Sample <- ifelse(
  formulas_df$Target_Sample %in% names(sample_map),
  unname(sample_map[formulas_df$Target_Sample]),
  formulas_df$Target_Sample
)

if (any(!grepl("^TA_scaled\\s*~", formulas_df$brms_Formula))) {
  stop("[BLOCKER] Non-TA_scaled dependent variable detected in winsor formula table.")
}

write.csv(formulas_df, file.path(winsor_root, "tables", "table_named_model_formulas_winsor.csv"), row.names = FALSE)

registry_df <- read.csv(registry_path, stringsAsFactors = FALSE)
for (col in colnames(registry_df)) {
  if (is.character(registry_df[[col]])) {
    registry_df[[col]] <- gsub("Ball & Shivakumar \\(2005\\)", "Ball & Shivakumar (2006)", registry_df[[col]])
    registry_df[[col]] <- gsub("Breuer-Schutt \\(2023\\)", "Breuer & SchÃƒÂ¼tt (2021/2022)", registry_df[[col]])
    registry_df[[col]] <- gsub("Real-time No-lead", "No-look-ahead feature set", registry_df[[col]])
  }
}
write.csv(registry_df, file.path(winsor_root, "tables", "table_model_registry_winsor.csv"), row.names = FALSE)

notes <- c(
  notes,
  "Formula table:",
  "  Formulas unchanged; only Target_Sample paths redirected to *_winsor.csv.",
  "Registry:",
  "  Registry copied to winsor with requested label corrections.",
  "",
  paste("Outputs written under", winsor_root, "only.")
)
writeLines(notes, con = file.path(winsor_root, "logs", "phase1b_winsor_notes.txt"))

cat("\n[SUCCESS] Phase 1b winsorized samples and audit tables created under ", winsor_root, "/.\n", sep = "")
