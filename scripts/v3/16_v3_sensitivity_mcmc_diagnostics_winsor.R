# -----------------------------------------------------------------------------
# Script: 16_v3_sensitivity_mcmc_diagnostics_winsor.R
# Purpose: MCMC diagnostics gate for sensitivity refit scenarios.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/v3/00_v3_winsor_helpers.R")
ensure_v3_winsor_dirs()
ensure_v3_sensitivity_dirs()
validate_v3_final_analysis_config("sensitivity MCMC diagnostics", final_mode = TRUE)

dry_run <- env_flag_v3("V3_DRY_RUN", "TRUE")
min_ess <- as.numeric(env_value_v3("V3_MCMC_MIN_ESS", "400"))
strict_ess <- as.numeric(env_value_v3("V3_MCMC_STRICT_ESS", "1000"))
max_rhat_allowed <- as.numeric(env_value_v3("V3_MCMC_MAX_RHAT", "1.01"))
if (any(is.na(c(min_ess, strict_ess, max_rhat_allowed)))) stop("[BLOCKER] Invalid MCMC diagnostic thresholds.")

scenarios <- selected_sensitivity_scenarios_v3()
plan_path <- file.path(v3_sensitivity_root(), "tables", "sensitivity_refit_plan.csv")
if (!file.exists(plan_path)) stop("[BLOCKER] Missing sensitivity refit plan. Run script 15 first.")
plan_df <- read.csv(plan_path, stringsAsFactors = FALSE)

if (!dry_run && (!requireNamespace("posterior", quietly = TRUE) || !requireNamespace("brms", quietly = TRUE))) {
  stop("[BLOCKER] posterior and brms are required for non-dry-run sensitivity diagnostics.")
}

classify_diag <- function(max_rhat, divergences, min_bulk, min_tail, treedepth_warnings, n_transitions) {
  if (!is.finite(max_rhat) || !is.finite(min_bulk) || !is.finite(min_tail)) return(c("FAIL", "non-finite Rhat or ESS"))
  if (divergences > 0) return(c("FAIL", paste0("divergences=", divergences)))
  if (max_rhat > max_rhat_allowed) return(c("FAIL", sprintf("max Rhat %.4f > %.2f", max_rhat, max_rhat_allowed)))
  if (min_bulk < min_ess) return(c("FAIL", sprintf("min bulk ESS %.1f < %.1f", min_bulk, min_ess)))
  if (min_tail < min_ess) return(c("FAIL", sprintf("min tail ESS %.1f < %.1f", min_tail, min_ess)))
  treedepth_rate <- if (n_transitions > 0) treedepth_warnings / n_transitions else 0
  if (treedepth_rate > 0.01) return(c("FAIL", sprintf("treedepth warning rate %.4f > 0.01", treedepth_rate)))
  if (treedepth_warnings > 0 || min_bulk < strict_ess || min_tail < strict_ess) return(c("REVIEW", "passes minimum thresholds but below strict ESS marker or has minor treedepth warnings"))
  c("PASS", "passes all configured thresholds")
}

summary_rows <- list()
detail_rows <- list()

for (sidx in seq_len(nrow(scenarios))) {
  scenario <- scenarios$Scenario[sidx]
  scenario_root <- v3_sensitivity_root(scenario)
  rows <- plan_df[plan_df$scenario == scenario, , drop = FALSE]
  if (nrow(rows) == 0) next

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]
    if (dry_run || !file.exists(row$fit_path)) {
      status <- if (dry_run) "NOT_RUN_DRY_RUN" else "FAIL"
      reason <- if (dry_run) "V3_DRY_RUN=TRUE; fit was not read." else paste("Missing fit file:", row$fit_path)
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        scenario = scenario,
        model_id = row$model_id,
        model_name = if ("Model_Name" %in% names(row)) row$Model_Name else NA_character_,
        target_space = row$target_space,
        sample_group = row$sample_group,
        heterogeneity_variant = row$heterogeneity_variant,
        diagnostics_status = status,
        stacking_allowed = FALSE,
        reason = reason,
        max_rhat = NA_real_,
        min_bulk_ess = NA_real_,
        min_tail_ess = NA_real_,
        divergences = NA_integer_,
        treedepth_warnings = NA_integer_,
        prior_set_id = row$prior_set_id,
        family = row$family,
        model_structure = row$model_structure,
        fit_path = row$fit_path,
        stringsAsFactors = FALSE
      )
      next
    }

    fit <- readRDS(row$fit_path)
    draws <- posterior::as_draws_array(fit)
    draw_summ <- as.data.frame(posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail"), stringsAsFactors = FALSE)
    draw_summ <- draw_summ[!grepl("__$", draw_summ$variable), , drop = FALSE]

    np <- brms::nuts_params(fit)
    divergences <- sum(np$Parameter == "divergent__" & np$Value > 0, na.rm = TRUE)
    treedepths <- np$Value[np$Parameter == "treedepth__"]
    treedepth_limit <- suppressWarnings(max(as.integer(row$max_treedepth), na.rm = TRUE))
    if (!is.finite(treedepth_limit)) treedepth_limit <- 12L
    treedepth_warnings <- sum(treedepths >= treedepth_limit, na.rm = TRUE)

    max_rhat <- max(draw_summ$rhat, na.rm = TRUE)
    min_bulk <- min(draw_summ$ess_bulk, na.rm = TRUE)
    min_tail <- min(draw_summ$ess_tail, na.rm = TRUE)
    diag <- classify_diag(max_rhat, divergences, min_bulk, min_tail, treedepth_warnings, length(treedepths))

    detail_rows[[length(detail_rows) + 1]] <- data.frame(
      scenario = scenario,
      model_id = row$model_id,
      target_space = row$target_space,
      sample_group = row$sample_group,
      heterogeneity_variant = row$heterogeneity_variant,
      parameter = draw_summ$variable,
      rhat = draw_summ$rhat,
      bulk_ess = draw_summ$ess_bulk,
      tail_ess = draw_summ$ess_tail,
      prior_set_id = row$prior_set_id,
      family = row$family,
      model_structure = row$model_structure,
      stringsAsFactors = FALSE
    )

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      scenario = scenario,
      model_id = row$model_id,
      model_name = if ("model_name" %in% names(row)) row$model_name else NA_character_,
      target_space = row$target_space,
      sample_group = row$sample_group,
      heterogeneity_variant = row$heterogeneity_variant,
      diagnostics_status = diag[1],
      stacking_allowed = identical(diag[1], "PASS"),
      reason = diag[2],
      max_rhat = max_rhat,
      min_bulk_ess = min_bulk,
      min_tail_ess = min_tail,
      divergences = divergences,
      treedepth_warnings = treedepth_warnings,
      prior_set_id = row$prior_set_id,
      family = row$family,
      model_structure = row$model_structure,
      fit_path = row$fit_path,
      stringsAsFactors = FALSE
    )
  }
}

summary_df <- bind_rows(summary_rows)
detail_df <- bind_rows(detail_rows)
tables_root <- file.path(v3_sensitivity_root(), "tables")
write.csv(summary_df, file.path(tables_root, "sensitivity_mcmc_diagnostics_summary.csv"), row.names = FALSE)
write.csv(detail_df, file.path(tables_root, "sensitivity_mcmc_diagnostics_detailed.csv"), row.names = FALSE)

for (scenario in unique(summary_df$scenario)) {
  sc_root <- v3_sensitivity_root(scenario)
  write.csv(summary_df[summary_df$scenario == scenario, , drop = FALSE],
            file.path(sc_root, "diagnostics", paste0("table_v3_sensitivity_mcmc_diagnostics_", scenario, ".csv")),
            row.names = FALSE)
}

eligible_counts <- summary_df %>%
  filter(stacking_allowed) %>%
  group_by(scenario, target_space) %>%
  summarise(n_eligible = n(), .groups = "drop")
write.csv(eligible_counts, file.path(tables_root, "sensitivity_stacking_eligibility_counts.csv"), row.names = FALSE)

if (!dry_run) {
  required_spaces <- expand.grid(scenario = scenarios$Scenario, target_space = c("ex_post", "real_time"), stringsAsFactors = FALSE)
  check_counts <- required_spaces %>% left_join(eligible_counts, by = c("scenario", "target_space"))
  check_counts$n_eligible[is.na(check_counts$n_eligible)] <- 0L
  if (any(check_counts$n_eligible < 2)) {
    stop("[BLOCKER] Too few diagnostics-PASS models for stacking in at least one scenario/space. See sensitivity_stacking_eligibility_counts.csv.")
  }
}

writeLines(c(
  "Sensitivity MCMC diagnostics notes",
  sprintf("Dry run: %s", dry_run),
  sprintf("Thresholds: max Rhat <= %.2f; divergences = 0; bulk/tail ESS >= %.0f; strict marker %.0f.", max_rhat_allowed, min_ess, strict_ess),
  "Only diagnostics_status == PASS is eligible for stacking."
), file.path(v3_sensitivity_root(), "logs", "v3_sensitivity_mcmc_diagnostics_notes.txt"))

cat("\n[SUCCESS] Sensitivity MCMC diagnostics completed.\n")
