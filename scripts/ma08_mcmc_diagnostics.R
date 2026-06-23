# -----------------------------------------------------------------------------
# Script: ma08_mcmc_diagnostics.R
# Purpose: Build a transparent MCMC diagnostics report from existing winsorized
#          brms fit files without refitting any models.
# -----------------------------------------------------------------------------

library(dplyr)
library(brms)
library(posterior)

source("scripts/ma00_setup.R")
phase_begin("ma08", "MCMC diagnostics")
ensure_analysis_dirs()

diag_input_path <- file.path(output_root, "tables", "table_brms_diagnostics_winsor.csv")
models_dir <- file.path(output_root, "models")

if (!file.exists(diag_input_path)) stop("[BLOCKER] Missing winsor diagnostics table from ma07.")
if (!dir.exists(models_dir)) stop("[BLOCKER] Missing winsor models directory.")

diag_input <- read.csv(diag_input_path, stringsAsFactors = FALSE)

detail_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_detailed.csv")
summary_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_model_summary.csv")
flag_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_flags.csv")
notes_path <- file.path(output_root, "logs", "phase3c_mcmc_diagnostics_notes.txt")

model_file_for <- function(row) {
  candidates <- c(
    file.path(models_dir, paste0("fit_", model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor"), ".rds")),
    file.path(models_dir, paste0("fit_", model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_winsor"), "_sp.rds"))
  )
  candidates[file.exists(candidates)][1]
}

classify_flag <- function(max_rhat, divergences, min_bulk, min_tail) {
  if (!is.finite(max_rhat) || !is.finite(min_bulk) || !is.finite(min_tail)) return("FAIL")
  if (max_rhat > 1.01 || divergences > 0) return("FAIL")
  if (min_bulk < 400 || min_tail < 400) return("FAIL")
  if (min_bulk < 1000 || min_tail < 1000) return("REVIEW")
  "PASS"
}

write_hist <- function(values, path, title_text, xlab_text) {
  png(filename = path, width = 1200, height = 800, res = 140)
  on.exit(dev.off(), add = TRUE)
  hist(values, breaks = 40, col = "#7EA3CC", border = "white", main = title_text, xlab = xlab_text)
}

detailed_rows <- list()
summary_rows <- list()
flag_rows <- list()

for (i in seq_len(nrow(diag_input))) {
  row <- diag_input[i, ]
  model_file <- model_file_for(row)
  if (is.na(model_file) || !nzchar(model_file)) {
    next
  }
  message(sprintf("[%d/%d] MCMC diagnostics: %s %s %s",
                  i, nrow(diag_input), row$Model_ID, row$Target_Space, row$Heterogeneity_Variant))

  fit <- readRDS(model_file)
  draws <- posterior::as_draws_array(fit)
  draw_summ <- posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail")
  draw_summ <- as.data.frame(draw_summ, stringsAsFactors = FALSE)
  draw_summ <- draw_summ[!grepl("__$", draw_summ$variable), , drop = FALSE]

  np <- nuts_params(fit)
  divergences <- sum(np$Parameter == "divergent__" & np$Value > 0)
  treedepth_vals <- np$Value[np$Parameter == "treedepth__"]
  treedepth_limit <- tryCatch(fit$fit@sim$args[[1]]$control$max_treedepth, error = function(e) NULL)
  if (is.null(treedepth_limit) || !is.finite(treedepth_limit)) treedepth_limit <- 12
  treedepth_warnings <- sum(treedepth_vals >= treedepth_limit, na.rm = TRUE)

  detail_df <- data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    Parameter = draw_summ$variable,
    Rhat = draw_summ$rhat,
    Bulk_ESS = draw_summ$ess_bulk,
    Tail_ESS = draw_summ$ess_tail,
    ESS_Minimum_Acceptable = 400,
    ESS_Strict_Marker = 1000,
    Prior_Set_ID = if ("Prior_Set_ID" %in% names(row)) row$Prior_Set_ID else prior_set_id,
    Likelihood_Family = if ("Likelihood_Family" %in% names(row)) row$Likelihood_Family else likelihood_family,
    Model_Structure = if ("Model_Structure" %in% names(row)) row$Model_Structure else model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
  detailed_rows[[length(detailed_rows) + 1]] <- detail_df

  max_rhat <- max(detail_df$Rhat, na.rm = TRUE)
  min_bulk <- min(detail_df$Bulk_ESS, na.rm = TRUE)
  min_tail <- min(detail_df$Tail_ESS, na.rm = TRUE)
  n_rhat_bad <- sum(detail_df$Rhat > 1.01, na.rm = TRUE)
  n_bulk_bad <- sum(detail_df$Bulk_ESS < 400, na.rm = TRUE)
  n_tail_bad <- sum(detail_df$Tail_ESS < 400, na.rm = TRUE)
  n_bulk_below_strict <- sum(detail_df$Bulk_ESS < 1000, na.rm = TRUE)
  n_tail_below_strict <- sum(detail_df$Tail_ESS < 1000, na.rm = TRUE)
  n_params <- nrow(detail_df)
  flag <- classify_flag(max_rhat, divergences, min_bulk, min_tail)

  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    N_Parameters = n_params,
    Max_Rhat = max_rhat,
    Min_Bulk_ESS = min_bulk,
    Min_Tail_ESS = min_tail,
    Divergent_Transitions = divergences,
    Treedepth_Warnings = treedepth_warnings,
    N_Rhat_GT_1_01 = n_rhat_bad,
    Share_Rhat_GT_1_01 = n_rhat_bad / n_params,
    N_Bulk_ESS_LT_400 = n_bulk_bad,
    Share_Bulk_ESS_LT_400 = n_bulk_bad / n_params,
    N_Tail_ESS_LT_400 = n_tail_bad,
    Share_Tail_ESS_LT_400 = n_tail_bad / n_params,
    N_Bulk_ESS_LT_1000 = n_bulk_below_strict,
    Share_Bulk_ESS_LT_1000 = n_bulk_below_strict / n_params,
    N_Tail_ESS_LT_1000 = n_tail_below_strict,
    Share_Tail_ESS_LT_1000 = n_tail_below_strict / n_params,
    ESS_Strict_Marker_Passed = min_bulk >= 1000 && min_tail >= 1000,
    Convergence_Flag = flag,
    Model_File = model_file,
    Prior_Set_ID = if ("Prior_Set_ID" %in% names(row)) row$Prior_Set_ID else prior_set_id,
    Likelihood_Family = if ("Likelihood_Family" %in% names(row)) row$Likelihood_Family else likelihood_family,
    Model_Structure = if ("Model_Structure" %in% names(row)) row$Model_Structure else model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )

  flag_rows[[length(flag_rows) + 1]] <- data.frame(
    Model_ID = row$Model_ID,
    Model_Name = row$Model_Name,
    Target_Space = row$Target_Space,
    Sample_Group = row$Sample_Group,
    Heterogeneity_Variant = row$Heterogeneity_Variant,
    Convergence_Flag = flag,
    Review_Reason = paste(
      c(
        if (max_rhat > 1.01) sprintf("Rhat max %.4f > 1.01", max_rhat),
        if (divergences > 0) sprintf("divergences=%d", divergences),
        if (min_bulk < 400) sprintf("bulk ESS min %.1f < 400", min_bulk),
        if (min_tail < 400) sprintf("tail ESS min %.1f < 400", min_tail),
        if (min_bulk >= 400 && min_bulk < 1000) sprintf("bulk ESS min %.1f below strict marker 1000", min_bulk),
        if (min_tail >= 400 && min_tail < 1000) sprintf("tail ESS min %.1f below strict marker 1000", min_tail)
      ),
      collapse = "; "
    ),
    Prior_Set_ID = if ("Prior_Set_ID" %in% names(row)) row$Prior_Set_ID else prior_set_id,
    Likelihood_Family = if ("Likelihood_Family" %in% names(row)) row$Likelihood_Family else likelihood_family,
    Model_Structure = if ("Model_Structure" %in% names(row)) row$Model_Structure else model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
}

detailed_df <- bind_rows(detailed_rows)
summary_df <- bind_rows(summary_rows)
flags_df <- bind_rows(flag_rows)

write.csv(detailed_df, detail_path, row.names = FALSE)
write.csv(summary_df, summary_path, row.names = FALSE)
write.csv(flags_df, flag_path, row.names = FALSE)

# Compile and save MCMC diagnostics gate
gate_df <- summary_df %>%
  transmute(
    model_id = Model_ID,
    model_name = Model_Name,
    Target_Space = Target_Space,
    Sample_Group = Sample_Group,
    Heterogeneity_Variant = Heterogeneity_Variant,
    max_rhat = Max_Rhat,
    n_divergent = Divergent_Transitions,
    min_bulk_ess = Min_Bulk_ESS,
    min_tail_ess = Min_Tail_ESS,
    max_treedepth_hits = Treedepth_Warnings,
    diagnostics_status = Convergence_Flag,
    fail_reason = case_when(
      Convergence_Flag == "FAIL" ~ sapply(seq_len(n()), function(i) {
        reasons <- c(
          if (Max_Rhat[i] > 1.01) sprintf("Rhat max %.4f > 1.01", Max_Rhat[i]),
          if (Divergent_Transitions[i] > 0) sprintf("divergences=%d", Divergent_Transitions[i]),
          if (Min_Bulk_ESS[i] < 400) sprintf("bulk ESS min %.1f < 400", Min_Bulk_ESS[i]),
          if (Min_Tail_ESS[i] < 400) sprintf("tail ESS min %.1f < 400", Min_Tail_ESS[i])
        )
        paste(reasons[!is.na(reasons)], collapse = "; ")
      }),
      TRUE ~ ""
    ),
    warning_reason = case_when(
      Convergence_Flag == "REVIEW" ~ sapply(seq_len(n()), function(i) {
        reasons <- c(
          if (Min_Bulk_ESS[i] >= 400 && Min_Bulk_ESS[i] < 1000) sprintf("bulk ESS min %.1f below 1000", Min_Bulk_ESS[i]),
          if (Min_Tail_ESS[i] >= 400 && Min_Tail_ESS[i] < 1000) sprintf("tail ESS min %.1f below 1000", Min_Tail_ESS[i])
        )
        paste(reasons[!is.na(reasons)], collapse = "; ")
      }),
      TRUE ~ ""
    )
  )

gate_path <- file.path(output_root, "tables", "table_mcmc_diagnostics_gate_winsor.csv")
write.csv(gate_df, gate_path, row.names = FALSE)
message("Saved MCMC diagnostics gate status to ", gate_path)

if (nrow(detailed_df) > 0) {
  write_hist(detailed_df$Rhat, file.path(output_root, "figures", "fig_mcmc_rhat_distribution.png"),
             "Winsorized BRMS Rhat Distribution", "Rhat")
  write_hist(detailed_df$Bulk_ESS, file.path(output_root, "figures", "fig_mcmc_bulk_ess_distribution.png"),
             "Winsorized BRMS Bulk ESS Distribution", "Bulk ESS")
  write_hist(detailed_df$Tail_ESS, file.path(output_root, "figures", "fig_mcmc_tail_ess_distribution.png"),
             "Winsorized BRMS Tail ESS Distribution", "Tail ESS")
}

notes <- c(
  "ma08 MCMC diagnostics notes",
  sprintf("Models summarized: %d", nrow(summary_df)),
  sprintf("PASS=%d REVIEW=%d FAIL=%d",
          sum(summary_df$Convergence_Flag == "PASS", na.rm = TRUE),
          sum(summary_df$Convergence_Flag == "REVIEW", na.rm = TRUE),
          sum(summary_df$Convergence_Flag == "FAIL", na.rm = TRUE)),
  sprintf("Output root: %s", output_root),
  sprintf("Prior set: %s; likelihood family: %s; model structure: %s", prior_set_id, likelihood_family, model_structure),
  "Thresholds: Rhat <= 1.01, Bulk_ESS >= 400, Tail_ESS >= 400, divergences = 0. ESS >= 1000 is reported as a stricter marker.",
  "ma08 reads existing fit files only and does not refit any models."
)
writeLines(notes, notes_path)

if (any(summary_df$Convergence_Flag == "FAIL", na.rm = TRUE)) {
  warning("[WARNING] Some winsorized brms fits failed formal convergence thresholds.")
}

cat("\n[SUCCESS] ma08 MCMC diagnostics completed.\n")
phase_end("ma08", "MCMC diagnostics")
