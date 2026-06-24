# -----------------------------------------------------------------------------
# Script: se01_prior_predictive.R
# Purpose: Prior predictive gate for the full-refit sensitivity scenarios.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("se01", "Sensitivity prior predictive")
ensure_analysis_dirs()
ensure_sensitivity_dirs()
write_method_design_files()
write_prior_registry()

dry_run <- env_flag("ACCRUAL_DRY_RUN", "TRUE")
allow_prior_fail <- env_flag("ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL", "FALSE")
prior_cfg <- accrual_sampler_config("prior_predictive")
n_draws <- prior_cfg$iter
options(mc.cores = prior_cfg$cores)
parallel_cfg <- accrual_fit_worker_config("prior_predictive", prior_cfg$cores, "se01 sensitivity prior predictive")

scenarios <- selected_sensitivity_scenarios()
formulas_path <- file.path(input_winsor_root, "tables", "table_named_model_formulas_winsor.csv")
if (!file.exists(formulas_path)) stop("[BLOCKER] Missing winsor formula table: ", formulas_path)
formulas_df <- read.csv(formulas_path, stringsAsFactors = FALSE)

truthy <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  toupper(as.character(x)) %in% c("TRUE", "1", "YES", "Y")
}

eligible_formulas <- formulas_df %>%
  filter(Sample_Group == "main_common") %>%
  filter(vapply(Main_Stack_Inclusion, truthy, logical(1))) %>%
  filter(mapply(function(space, id) id %in% main_model_ids_for_space(space), Target_Space, Model_ID)) %>%
  distinct(Model_ID, Model_Name, Target_Space, Sample_Group, Heterogeneity_Variant, Target_Sample, brms_Formula, .keep_all = TRUE) %>%
  arrange(Target_Space, Model_ID, Heterogeneity_Variant)

if (nrow(eligible_formulas) == 0) stop("[BLOCKER] No eligible main-stack formulas for sensitivity prior predictive checks.")

classify_prior_pp <- function(vals, observed) {
  prior_q <- stats::quantile(vals, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE, type = 7)
  obs_q <- stats::quantile(observed, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE, type = 7)
  out <- classify_chapter3_prior_predictive(
    share_gt_1 = mean(abs(vals) > 1, na.rm = TRUE),
    share_gt_2 = mean(abs(vals) > 2, na.rm = TRUE),
    prior_p01 = prior_q[[1]],
    prior_p99 = prior_q[[2]],
    observed_p01 = obs_q[[1]],
    observed_p99 = obs_q[[2]]
  )
  c(out$status, out$reason, as.character(out$range_ratio))
}

scenario_rows <- list()
gate_rows <- list()

if (!dry_run && !requireNamespace("brms", quietly = TRUE)) {
  stop("[BLOCKER] brms is required for non-dry-run prior predictive sensitivity.")
}

build_se01_task_manifest <- function(scenarios, formulas) {
  rows <- list()
  task_index <- 0L
  for (sidx in seq_len(nrow(scenarios))) {
    sc <- scenarios[sidx, ]
    scenario <- sc$Scenario
    scenario_root <- sensitivity_root(scenario)
    ensure_sensitivity_dirs(scenario)
    for (i in seq_len(nrow(formulas))) {
      row <- formulas[i, ]
      task_index <- task_index + 1L
      model_key <- model_key_sampled(
        row$Model_ID,
        row$Target_Space,
        row$Sample_Group,
        row$Heterogeneity_Variant,
        paste0("_", scenario, "_priorpred")
      )
      rng_context <- paste0("sensitivity_prior_predictive_", scenario, "_", row$Target_Space, "_", row$Model_ID)
      rng_offset <- sidx * 1000L + i
      rng <- accrual_rng_metadata_list(rng_context, offset = rng_offset)
      rows[[length(rows) + 1]] <- data.frame(
        task_index = task_index,
        task_key = stable_task_key("se01_prior_predictive", scenario, model_key, task_index),
        scenario_index = sidx,
        formula_index = i,
        scenario = scenario,
        model_key = model_key,
        Model_ID = row$Model_ID,
        Model_Name = row$Model_Name,
        Target_Space = row$Target_Space,
        Sample_Group = row$Sample_Group,
        Heterogeneity_Variant = row$Heterogeneity_Variant,
        Target_Sample = row$Target_Sample,
        brms_Formula = row$brms_Formula,
        Prior_Set_ID = sc$Prior_Set_ID,
        Likelihood_Family = sc$Likelihood_Family,
        Model_Structure = sc$Model_Structure,
        chains = prior_cfg$chains,
        cores = prior_cfg$cores,
        iter = prior_cfg$iter,
        warmup = prior_cfg$warmup,
        adapt_delta = prior_cfg$adapt_delta,
        max_treedepth = prior_cfg$max_treedepth,
        refresh = prior_cfg$refresh,
        backend = prior_cfg$backend,
        sampler_profile = prior_cfg$sampler_profile,
        n_draws = n_draws,
        dry_run = dry_run,
        Required = TRUE,
        RNG_Context = rng$RNG_Context,
        RNG_Offset = rng$RNG_Offset,
        Canonical_Seed = rng$Canonical_Seed,
        Effective_Seed = rng$Effective_Seed,
        RNG_Source = rng$RNG_Source,
        output_path = file.path(scenario_root, "prior_predictive", paste0("fit_", model_key, ".rds")),
        log_path = file.path(scenario_root, "logs", paste0("prior_predictive_", model_key, ".log")),
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows)
}

fit_se01_prior_task_worker <- function(task) {
  source("scripts/ma00_setup.R")
  started_at <- as.character(Sys.time())
  status <- "FAILED"
  error_message <- ""
  row_out <- NULL
  dir.create(dirname(task$log_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(task$output_path), recursive = TRUE, showWarnings = FALSE)

  error_result <- tryCatch({
    if (isTRUE(task$dry_run) || identical(toupper(as.character(task$dry_run)), "TRUE")) {
      row_out <- data.frame(
        task_index = task$task_index,
        scenario = task$scenario,
        model_id = task$Model_ID,
        model_name = task$Model_Name,
        target_space = task$Target_Space,
        family = task$Likelihood_Family,
        prior_set_id = task$Prior_Set_ID,
        model_structure = task$Model_Structure,
        p_abs_gt_1 = NA_real_,
        p_abs_gt_2 = NA_real_,
        yrep_mean = NA_real_,
        yrep_sd = NA_real_,
        range_ratio_to_observed = NA_real_,
        status = "NOT_RUN_DRY_RUN",
        reason = "ACCRUAL_DRY_RUN=TRUE; no brms prior predictive sampling executed.",
        output_path = task$output_path,
        stringsAsFactors = FALSE
      )
      status <- "NOT_RUN_DRY_RUN"
    } else {
      suppressPackageStartupMessages(library(brms))
      df_scaled <- read_winsor_sample(task$Target_Sample)
      observed <- df_scaled$TA_scaled
      formula_str <- fix_formula(task$brms_Formula)
      prior_list <- default_prior_list(
        task$Heterogeneity_Variant,
        model_structure = task$Model_Structure,
        prior_set_id = task$Prior_Set_ID,
        family = task$Likelihood_Family
      )

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
        family = brms_family(task$Likelihood_Family),
        prior = prior_list,
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
      fit <- do.call(brms::brm, brm_args)
      saveRDS(fit, task$output_path)
      yrep <- brms::posterior_predict(fit, ndraws = task$n_draws)
      vals <- as.numeric(yrep)
      pp <- classify_prior_pp(vals, observed)

      row_out <- data.frame(
        task_index = task$task_index,
        scenario = task$scenario,
        model_id = task$Model_ID,
        model_name = task$Model_Name,
        target_space = task$Target_Space,
        family = task$Likelihood_Family,
        prior_set_id = task$Prior_Set_ID,
        model_structure = task$Model_Structure,
        p_abs_gt_1 = mean(abs(vals) > 1, na.rm = TRUE),
        p_abs_gt_2 = mean(abs(vals) > 2, na.rm = TRUE),
        yrep_mean = mean(vals, na.rm = TRUE),
        yrep_sd = stats::sd(vals, na.rm = TRUE),
        range_ratio_to_observed = as.numeric(pp[3]),
        status = pp[1],
        reason = pp[2],
        output_path = task$output_path,
        stringsAsFactors = FALSE
      )
      status <- "SUCCESS"
    }
    list(error_message = "", row_out = NULL)
  }, error = function(e) {
    msg <- conditionMessage(e)
    list(error_message = msg, row_out = data.frame(
      task_index = task$task_index,
      scenario = task$scenario,
      model_id = task$Model_ID,
      model_name = task$Model_Name,
      target_space = task$Target_Space,
      family = task$Likelihood_Family,
      prior_set_id = task$Prior_Set_ID,
      model_structure = task$Model_Structure,
      p_abs_gt_1 = NA_real_,
      p_abs_gt_2 = NA_real_,
      yrep_mean = NA_real_,
      yrep_sd = NA_real_,
      range_ratio_to_observed = NA_real_,
      status = "FAIL",
      reason = paste("brms prior predictive failed:", msg),
      output_path = task$output_path,
      stringsAsFactors = FALSE
    ))
  })
  error_message <- error_result$error_message
  if (!is.null(error_result$row_out)) row_out <- error_result$row_out

  ended_at <- as.character(Sys.time())
  writeLines(c(
    paste0("Task: ", task$task_key),
    paste0("Scenario: ", task$scenario),
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
      scenario = task$scenario,
      model_key = task$model_key,
      Model_ID = task$Model_ID,
      Target_Space = task$Target_Space,
      Required = task$Required,
      status = status,
      error_message = error_message,
      output_path = task$output_path,
      log_path = task$log_path,
      started_at = started_at,
      ended_at = ended_at,
      stringsAsFactors = FALSE
    ),
    row = row_out
  )
}

for (sidx in seq_len(nrow(scenarios))) {
  sc <- scenarios[sidx, ]
  scenario <- sc$Scenario
  scenario_root <- sensitivity_root(scenario)
  ensure_sensitivity_dirs(scenario)

  model_list <- unique(paste(eligible_formulas$Target_Space, eligible_formulas$Model_ID, sep = ":"))
  write_run_manifest(
    file.path(scenario_root, "manifests", "prior_predictive_manifest.csv"),
    scenario = scenario,
    prior_set_id = sc$Prior_Set_ID,
    family = sc$Likelihood_Family,
    model_structure = sc$Model_Structure,
    model_list = model_list,
    seed = accrual_seed_for(paste0("sensitivity_prior_predictive_manifest_", scenario), offset = sidx),
    sampling_config = sprintf("sample_prior=only; draws=%d; chains=%d; cores=%d; iter=%d; warmup=%d; refresh=%d; dry_run=%s",
                              n_draws, prior_cfg$chains, prior_cfg$cores, prior_cfg$iter,
                              prior_cfg$warmup, prior_cfg$refresh, dry_run),
    status = if (dry_run) "DRY_RUN_PLANNED" else "STARTED",
    notes = "Prior predictive gate for sensitivity full-refit scenarios.",
    input_paths = c(formulas_path),
    rng_context = paste0("sensitivity_prior_predictive_manifest_", scenario),
    rng_offset = sidx
  )
}

sens_tables <- file.path(sensitivity_root(), "tables")
task_manifest <- build_se01_task_manifest(scenarios, eligible_formulas)
write.csv(task_manifest, file.path(sens_tables, "table_se01_prior_predictive_task_manifest.csv"), row.names = FALSE)
task_list <- lapply(seq_len(nrow(task_manifest)), function(i) as.list(task_manifest[i, ]))
task_results <- accrual_run_task_pool(
  tasks = task_list,
  worker_fun = fit_se01_prior_task_worker,
  parallel_cfg = parallel_cfg,
  export_names = c("classify_prior_pp"),
  packages = if (dry_run) character() else "brms",
  context = "se01 sensitivity prior predictive"
)

status_df <- bind_rows(lapply(task_results, `[[`, "status")) %>% arrange(task_index)
write.csv(status_df, file.path(sens_tables, "table_se01_prior_predictive_task_status.csv"), row.names = FALSE)
accrual_task_status_blocker(status_df, required_col = "Required", context = "se01 sensitivity prior predictive")

summary_df <- bind_rows(lapply(task_results, `[[`, "row")) %>%
  arrange(task_index) %>%
  select(-task_index)

for (sidx in seq_len(nrow(scenarios))) {
  sc <- scenarios[sidx, ]
  scenario <- sc$Scenario
  scenario_root <- sensitivity_root(scenario)
  sc_rows <- summary_df %>% filter(scenario == !!scenario)
  write.csv(sc_rows, file.path(scenario_root, "prior_predictive", paste0("table_sensitivity_prior_predictive_", scenario, ".csv")), row.names = FALSE)

  gate_status <- if (dry_run) {
    "DRY_RUN_NOT_EVALUATED"
  } else if (any(sc_rows$status == "FAIL", na.rm = TRUE)) {
    if (allow_prior_fail) "FAIL_OVERRIDDEN" else "BLOCKED_FAIL"
  } else if (any(sc_rows$status == "REVIEW", na.rm = TRUE)) {
    "PASS_WITH_REVIEW"
  } else {
    "PASS"
  }
  gate_rows[[length(gate_rows) + 1]] <- data.frame(
    scenario = scenario,
    prior_set_id = sc$Prior_Set_ID,
    family = sc$Likelihood_Family,
    model_structure = sc$Model_Structure,
    gate_status = gate_status,
    proceed_to_refit = gate_status %in% c("PASS", "PASS_WITH_REVIEW", "FAIL_OVERRIDDEN"),
    allow_prior_predictive_fail = allow_prior_fail,
    dry_run = dry_run,
    stringsAsFactors = FALSE
  )
}

gate_df <- bind_rows(gate_rows)
write.csv(summary_df, file.path(sens_tables, "sensitivity_prior_predictive_summary.csv"), row.names = FALSE)
write.csv(gate_df, file.path(sens_tables, "sensitivity_prior_predictive_gate.csv"), row.names = FALSE)

writeLines(c(
  "Sensitivity prior predictive gate",
  sprintf("Dry run: %s", dry_run),
  sprintf("Scenarios: %s", paste(scenarios$Scenario, collapse = ", ")),
  sprintf("Allow prior predictive fail override: %s", allow_prior_fail),
  "Chapter 3 PASS thresholds: share |TA_scaled| > 1 <= 0.05; share |TA_scaled| > 2 <= 0.01; prior predictive p01-p99 range <= 3 times observed p01-p99 range.",
  "Scenario FAIL blocks full refit unless ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE."
), file.path(sensitivity_root(), "logs", "sensitivity_prior_predictive_notes.txt"))

if (!dry_run && any(gate_df$gate_status == "BLOCKED_FAIL", na.rm = TRUE)) {
  stop("[BLOCKER] One or more sensitivity scenarios failed prior predictive checks. Set ACCRUAL_ALLOW_PRIOR_PREDICTIVE_FAIL=TRUE only for an intentional diagnostic override.")
}

cat("\n[SUCCESS] Sensitivity prior predictive gate completed.\n")
phase_end("se01", "Sensitivity prior predictive")
