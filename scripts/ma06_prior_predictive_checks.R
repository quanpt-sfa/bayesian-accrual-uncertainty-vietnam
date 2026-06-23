# -----------------------------------------------------------------------------
# Script: ma06_prior_predictive_checks.R
# Purpose: Document prior specification and run representative prior predictive
#          checks for the corrected winsorized accrual uncertainty pipeline.
# -----------------------------------------------------------------------------

library(dplyr)
library(brms)

source("scripts/ma00_setup.R")
phase_begin("ma06", "Prior predictive checks")
ensure_analysis_dirs()
write_method_design_files()
write_prior_registry()

mode <- prior_predictive_mode
include_m10 <- identical(mode, "FULL") || identical(mode, "EXTENDED")

formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) {
  stop("[BLOCKER] Winsor formula table not found. Run ma05 first.")
}

prior_spec_path <- file.path(output_root, "tables", "table_prior_specification.csv")
prior_summary_path <- file.path(output_root, "tables", "table_prior_predictive_summary.csv")
prior_extreme_path <- file.path(output_root, "tables", "table_prior_predictive_extreme_rates.csv")
prior_task_manifest_path <- file.path(output_root, "tables", "table_ma06_prior_predictive_task_manifest.csv")
prior_task_status_path <- file.path(output_root, "tables", "table_ma06_prior_predictive_task_status.csv")
prior_notes_path <- file.path(output_root, "logs", "phase3a_prior_predictive_notes.txt")
prior_method_note_path <- file.path(output_root, "logs", "method_note_scale_aware_student_priors.txt")

prior_spec <- default_prior_specification()
write.csv(prior_spec, prior_spec_path, row.names = FALSE)

formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)
representative_ids <- c("M01", "M06", "M07", "M09")
if (include_m10) representative_ids <- c(representative_ids, "M10")

representative_rows <- formulas_df %>%
  filter(Model_ID %in% representative_ids) %>%
  filter(
    (Model_ID == "M01" & Target_Space %in% c("ex_post", "real_time")) |
      (Model_ID == "M06" & Target_Space == "ex_post") |
      (Model_ID == "M07" & Target_Space %in% c("ex_post", "real_time")) |
      (Model_ID == "M09" & Target_Space == "real_time") |
      (Model_ID == "M10" & Target_Space == "ex_post")
  ) %>%
  arrange(match(Model_ID, c("M01", "M06", "M07", "M09", "M10")), Target_Space, Heterogeneity_Variant)

if (nrow(representative_rows) == 0) {
  stop("[BLOCKER] No representative rows were selected for prior predictive checks.")
}

prior_cfg <- accrual_sampler_config("prior_predictive")
options(mc.cores = prior_cfg$cores)
parallel_cfg <- accrual_fit_worker_config("prior_predictive", prior_cfg$cores, "ma06 prior predictive")

summarize_quantiles <- function(x) {
  qs <- quantile(x, probs = c(0.01, 0.50, 0.99), na.rm = TRUE, names = FALSE, type = 7)
  list(
    min = min(x, na.rm = TRUE),
    p01 = qs[1],
    median = qs[2],
    p99 = qs[3],
    max = max(x, na.rm = TRUE)
  )
}

write_overlay_plot <- function(observed, simulated, figure_path, title_text) {
  png(filename = figure_path, width = 1200, height = 800, res = 140)
  on.exit(dev.off(), add = TRUE)
  obs_density <- density(observed, na.rm = TRUE)
  sim_rows <- seq_len(min(20, nrow(simulated)))
  plot(
    obs_density,
    main = title_text,
    xlab = "TA_scaled",
    ylab = "Density",
    lwd = 3,
    col = "black"
  )
  for (idx in sim_rows) {
    lines(density(simulated[idx, ]), col = rgb(0.2, 0.4, 0.8, 0.18), lwd = 1)
  }
  sim_flat <- as.vector(simulated[seq_len(min(200, nrow(simulated))), , drop = FALSE])
  lines(density(sim_flat), col = "#3366AA", lwd = 2, lty = 2)
  legend(
    "topright",
    legend = c("Observed", "Prior predictive draws", "Flattened prior predictive"),
    col = c("black", rgb(0.2, 0.4, 0.8, 0.35), "#3366AA"),
    lwd = c(3, 1, 2),
    lty = c(1, 1, 2),
    bty = "n"
  )
}

build_prior_task_manifest <- function(rows) {
  out <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor")
    rng <- accrual_rng_metadata_list("baseline_prior_predictive_fit", offset = i)
    data.frame(
      task_index = i,
      task_key = stable_task_key("ma06_prior_predictive", model_key, i),
      model_key = model_key,
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Target_Sample = row$Target_Sample,
      brms_Formula = row$brms_Formula,
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      chains = prior_cfg$chains,
      cores = prior_cfg$cores,
      iter = prior_cfg$iter,
      warmup = prior_cfg$warmup,
      adapt_delta = prior_cfg$adapt_delta,
      max_treedepth = prior_cfg$max_treedepth,
      refresh = prior_cfg$refresh,
      backend = prior_cfg$backend,
      sampler_profile = prior_cfg$sampler_profile,
      RNG_Context = rng$RNG_Context,
      RNG_Offset = rng$RNG_Offset,
      Canonical_Seed = rng$Canonical_Seed,
      Effective_Seed = rng$Effective_Seed,
      RNG_Source = rng$RNG_Source,
      Required = TRUE,
      log_path = file.path(output_root, "logs", paste0("prior_predictive_", model_key, ".log")),
      stringsAsFactors = FALSE
    )
  })
  bind_rows(out)
}

fit_ma06_prior_task_worker <- function(task) {
  suppressPackageStartupMessages({
    library(brms)
    library(dplyr)
  })
  source("scripts/ma00_setup.R")
  started_at <- as.character(Sys.time())
  status <- "FAILED"
  error_message <- ""
  summary_row <- NULL
  extreme_row <- NULL
  observed <- numeric()
  simulated <- matrix(numeric(), nrow = 0)
  dir.create(dirname(task$log_path), recursive = TRUE, showWarnings = FALSE)

  tryCatch({
    message("[MA06] Prior predictive task: ", task$model_key)
    df_scaled <- read_winsor_sample(task$Target_Sample)
    observed <- df_scaled$TA_scaled
    formula_str <- fix_formula(task$brms_Formula)
    message(
      "brms/rstan sampler controls: chains=", task$chains,
      ", cores=", task$cores,
      ", iter=", task$iter,
      ", warmup=", task$warmup,
      ", refresh=", task$refresh
    )
    brm_args <- list(
      formula = brms::bf(stats::as.formula(formula_str)),
      data = df_scaled,
      family = brms_family(),
      prior = default_prior_list(task$Heterogeneity_Variant),
      sample_prior = "only",
      chains = task$chains,
      cores = task$cores,
      iter = task$iter,
      warmup = task$warmup,
      seed = task$Effective_Seed,
      refresh = task$refresh
    )
    if (!is.na(task$adapt_delta) || !is.na(task$max_treedepth)) {
      control <- list()
      if (!is.na(task$adapt_delta)) control$adapt_delta <- task$adapt_delta
      if (!is.na(task$max_treedepth)) control$max_treedepth <- task$max_treedepth
      brm_args$control <- control
    }
    fit_prior <- do.call(brms::brm, brm_args)
    simulated <- brms::posterior_predict(fit_prior)

    obs_q <- summarize_quantiles(observed)
    prior_q <- summarize_quantiles(as.vector(simulated))
    share_gt_1 <- mean(abs(simulated) > 1, na.rm = TRUE)
    share_gt_2 <- mean(abs(simulated) > 2, na.rm = TRUE)
    share_gt_5 <- mean(abs(simulated) > 5, na.rm = TRUE)
    gate <- classify_chapter3_prior_predictive(
      share_gt_1 = share_gt_1,
      share_gt_2 = share_gt_2,
      prior_p01 = prior_q$p01,
      prior_p99 = prior_q$p99,
      observed_p01 = obs_q$p01,
      observed_p99 = obs_q$p99
    )

    summary_row <- data.frame(
      task_index = task$task_index,
      model_key = task$model_key,
      Model_ID = task$Model_ID,
      Model_Name = task$Model_Name,
      Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group,
      Heterogeneity_Variant = task$Heterogeneity_Variant,
      N_Obs = length(observed),
      Observed_TA_Min = obs_q$min,
      Observed_TA_P01 = obs_q$p01,
      Observed_TA_Median = obs_q$median,
      Observed_TA_P99 = obs_q$p99,
      Observed_TA_Max = obs_q$max,
      PriorPred_TA_P01 = prior_q$p01,
      PriorPred_TA_Median = prior_q$median,
      PriorPred_TA_P99 = prior_q$p99,
      PriorPred_TA_P01_P99_Range = prior_q$p99 - prior_q$p01,
      Observed_TA_P01_P99_Range = obs_q$p99 - obs_q$p01,
      PriorPred_Range_Ratio_to_Observed = gate$range_ratio,
      PriorPred_Share_Abs_GT_1 = share_gt_1,
      PriorPred_Share_Abs_GT_2 = share_gt_2,
      PriorPred_Share_Abs_GT_5 = share_gt_5,
      Prior_Plausibility_Flag = gate$status,
      Prior_Plausibility_Reason = gate$reason,
      Prior_Set_ID = task$Prior_Set_ID,
      Likelihood_Family = task$Likelihood_Family,
      Model_Structure = task$Model_Structure,
      Output_Root = output_root,
      stringsAsFactors = FALSE
    )

    extreme_row <- data.frame(
      task_index = task$task_index,
      Model_ID = task$Model_ID,
      Model_Name = task$Model_Name,
      Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group,
      Heterogeneity_Variant = task$Heterogeneity_Variant,
      Threshold = c("abs(TA_scaled) > 1", "abs(TA_scaled) > 2", "abs(TA_scaled) > 5"),
      Rate = c(share_gt_1, share_gt_2, share_gt_5),
      Prior_Set_ID = task$Prior_Set_ID,
      Likelihood_Family = task$Likelihood_Family,
      Model_Structure = task$Model_Structure,
      Output_Root = output_root,
      stringsAsFactors = FALSE
    )
    status <- "SUCCESS"
  }, error = function(e) {
    error_message <<- conditionMessage(e)
  })

  ended_at <- as.character(Sys.time())
  writeLines(c(
    paste0("Task: ", task$task_key),
    paste0("Model key: ", task$model_key),
    paste0("Started: ", started_at),
    paste0("Ended: ", ended_at),
    paste0("Status: ", status),
    paste0("Error: ", error_message)
  ), task$log_path, useBytes = TRUE)

  list(
    status = data.frame(
      task_index = task$task_index,
      task_key = task$task_key,
      model_key = task$model_key,
      Model_ID = task$Model_ID,
      Model_Name = task$Model_Name,
      Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group,
      Heterogeneity_Variant = task$Heterogeneity_Variant,
      Required = task$Required,
      status = status,
      error_message = error_message,
      log_path = task$log_path,
      started_at = started_at,
      ended_at = ended_at,
      stringsAsFactors = FALSE
    ),
    summary = summary_row,
    extreme = extreme_row,
    observed = observed,
    simulated = simulated
  )
}

notes <- c(
  "ma06 prior predictive notes",
  sprintf("Mode: %s", mode),
  sprintf("Output root: %s", output_root),
  sprintf("Input winsor root: %s", input_winsor_root),
  sprintf("Prior set: %s", prior_set_id),
  sprintf("Likelihood family: %s", likelihood_family),
  sprintf("Representative configurations checked: %d", nrow(representative_rows)),
  "Priors are formalized in table_prior_specification.csv and table_prior_sets.csv and match the current ma07 defaults.",
  "Main-stack design is unchanged: winsorized samples remain primary, and M08/M10 stay outside the main stacks.",
  "Flags use Chapter 3 thresholds: PASS if share |TA_scaled| > 1 <= 0.05, share |TA_scaled| > 2 <= 0.01, and prior predictive p01-p99 range <= 3 times the observed p01-p99 range.",
  "REVIEW is a derived implementation band used for reporting: share |TA_scaled| > 1 <= 0.15, share |TA_scaled| > 2 <= 0.02, and range ratio <= 5.00; otherwise FAIL.",
  ""
)

task_manifest <- build_prior_task_manifest(representative_rows)
write.csv(task_manifest, prior_task_manifest_path, row.names = FALSE)
task_list <- lapply(seq_len(nrow(task_manifest)), function(i) as.list(task_manifest[i, ]))
task_results <- accrual_run_task_pool(
  tasks = task_list,
  worker_fun = fit_ma06_prior_task_worker,
  parallel_cfg = parallel_cfg,
  export_names = c("summarize_quantiles"),
  packages = c("brms", "dplyr"),
  context = "ma06 prior predictive"
)

status_df <- bind_rows(lapply(task_results, `[[`, "status")) %>% arrange(task_index)
write.csv(status_df, prior_task_status_path, row.names = FALSE)
accrual_task_status_blocker(status_df, required_col = "Required", context = "ma06 prior predictive")

summary_df <- bind_rows(lapply(task_results, `[[`, "summary")) %>%
  arrange(task_index) %>%
  select(-task_index, -model_key)
extreme_df <- bind_rows(lapply(task_results, `[[`, "extreme")) %>%
  arrange(task_index) %>%
  select(-task_index)

for (result in task_results[order(vapply(task_results, function(x) x$status$task_index, numeric(1)))]) {
  row_summary <- result$summary
  write_overlay_plot(
    observed = result$observed,
    simulated = result$simulated,
    figure_path = file.path(output_root, "figures", paste0("fig_prior_predictive_overlay_", row_summary$model_key, ".png")),
    title_text = sprintf("Prior Predictive Overlay: %s", row_summary$model_key)
  )
  notes <- c(
    notes,
    sprintf(
      "%s: flag=%s, observed p99=%.4f, prior p99=%.4f, share|TA|>1=%.4f, share|TA|>2=%.4f, share|TA|>5=%.4f",
      row_summary$model_key,
      row_summary$Prior_Plausibility_Flag,
      row_summary$Observed_TA_P99,
      row_summary$PriorPred_TA_P99,
      row_summary$PriorPred_Share_Abs_GT_1,
      row_summary$PriorPred_Share_Abs_GT_2,
      row_summary$PriorPred_Share_Abs_GT_5
    )
  )
}

write.csv(summary_df, prior_summary_path, row.names = FALSE)
write.csv(extreme_df, prior_extreme_path, row.names = FALSE)

status_line <- if (any(summary_df$Prior_Plausibility_Flag == "FAIL")) {
  "FAIL"
} else if (any(summary_df$Prior_Plausibility_Flag == "REVIEW")) {
  "REVIEW"
} else {
  "PASS"
}

# Compile gate status
config_hash_val <- if (requireNamespace("digest", quietly = TRUE)) {
  digest::digest(list(prior_set_id, likelihood_family, model_structure), algo = "sha256")
} else {
  paste(prior_set_id, likelihood_family, model_structure, sep = "_")
}

gate_df <- data.frame(
  model_id = summary_df$Model_ID,
  prior_set_id = prior_set_id,
  family = likelihood_family,
  status = summary_df$Prior_Plausibility_Flag,
  reason = sprintf("Share abs(TA) > 1: %.4f; Share abs(TA) > 2: %.4f; p01-p99 range ratio: %.4f",
                   summary_df$PriorPred_Share_Abs_GT_1,
                   summary_df$PriorPred_Share_Abs_GT_2,
                   summary_df$PriorPred_Range_Ratio_to_Observed),
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  config_hash = config_hash_val,
  stringsAsFactors = FALSE
)

gate_csv_path <- file.path(output_root, "prior_predictive_gate_status.csv")
gate_rds_path <- file.path(output_root, "prior_predictive_gate_status.rds")
write.csv(gate_df, gate_csv_path, row.names = FALSE)
saveRDS(gate_df, gate_rds_path)
message("Saved prior predictive check gate status to ", gate_csv_path)

notes <- c(
  notes,
  "",
  sprintf("Overall prior predictive status: %s", status_line),
  if (status_line == "FAIL") "Do not proceed to ma07 until priors are revised." else "Prior predictive status does not block manual review.",
  "REVIEW does not block the pipeline but should be acknowledged in the manuscript."
)
writeLines(notes, prior_notes_path)

method_note <- c(
  "Scale-aware Student-t prior note for the corrected winsorized Bayesian accrual pipeline",
  "",
  sprintf("Prior set: %s", prior_set_id),
  sprintf("Likelihood family: %s", likelihood_family),
  "The wide_original Gaussian prior set is retained only as a diagnostic reference because its prior predictive checks imply implausibly wide TA_scaled distributions.",
  "The scale-aware Student-t baseline uses normal(0, 0.10) priors on coefficients and intercepts, exponential(10) priors on residual/group scales, and gamma(2, 0.1) on Student-t nu.",
  "Prior predictive checks follow Chapter 3: PASS requires share |TA_scaled| > 1 <= 0.05, share |TA_scaled| > 2 <= 0.01, and prior predictive p01-p99 range <= 3 times the observed p01-p99 range.",
  "FAIL blocks the baseline pipeline unless overridden with ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE."
)
writeLines(method_note, prior_method_note_path)

if (status_line == "FAIL") {
  allow_fail <- env_flag("ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL", "FALSE")
  if (!allow_fail) {
    stop("[GATEKEEPER STOP] Prior predictive check FAIL detected. Proceeding to fitting is blocked. Set ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE to override.")
  } else {
    warning("[GATEKEEPER OVERRIDE] Prior predictive check FAIL detected, but ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE bypass is enabled.")
  }
}
if (status_line == "REVIEW") {
  warning("[WARNING] Prior predictive checks require review for at least one representative configuration.")
}

cat("\n[SUCCESS] ma06 prior predictive checks completed.\n")
phase_end("ma06", "Prior predictive checks")
