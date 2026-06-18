# -----------------------------------------------------------------------------
# Script: 28_row_level_exact_kfold.R
# Purpose: Reviewer-final method matching: exact row-level K-fold refits for the
#          winsorized DA model stack, separated from firm-grouped Step 13 output.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(brms)
})

source("scripts/00_helpers.R")
ensure_analysis_dirs()

script_name <- "scripts/28_row_level_exact_kfold.R"
script_version <- "2026-06-18-row-level-exact-kfold-v2-method-matched-row-target"
script_start_time <- Sys.time()

env_flag <- function(name, default = "FALSE") {
  toupper(Sys.getenv(name, default)) %in% c("TRUE", "1", "YES", "Y")
}

split_env <- function(name) {
  x <- trimws(Sys.getenv(name, ""))
  if (!nzchar(x)) return(character())
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

format_time <- function(x) format(x, "%Y-%m-%d %H:%M:%S %Z")

K <- as.integer(Sys.getenv("ACCRUAL_ROW_KFOLD_K", "5"))
if (is.na(K) || K < 2) stop("[BLOCKER] ACCRUAL_ROW_KFOLD_K must be an integer >= 2.")

seed <- as.integer(Sys.getenv("ACCRUAL_ROW_KFOLD_SEED", "42"))
if (is.na(seed)) stop("[BLOCKER] ACCRUAL_ROW_KFOLD_SEED must be an integer.")

run_mode <- toupper(Sys.getenv("ACCRUAL_ROW_KFOLD_MODE", "FULL_MODE"))
if (!run_mode %in% c("FULL_MODE", "FAST_MODE")) {
  stop("[BLOCKER] ACCRUAL_ROW_KFOLD_MODE must be FULL_MODE or FAST_MODE.")
}

target_space_filter <- split_env("ACCRUAL_ROW_KFOLD_TARGET_SPACE")
model_id_filter <- split_env("ACCRUAL_ROW_KFOLD_MODEL_IDS")
fold_filter_raw <- split_env("ACCRUAL_ROW_KFOLD_FOLDS")
fold_filter <- if (length(fold_filter_raw) > 0) as.integer(fold_filter_raw) else integer()
if (any(is.na(fold_filter))) stop("[BLOCKER] ACCRUAL_ROW_KFOLD_FOLDS must be comma-separated integers.")
if (length(fold_filter) > 0 && any(!fold_filter %in% seq_len(K))) {
  stop("[BLOCKER] ACCRUAL_ROW_KFOLD_FOLDS contains folds outside 1:K.")
}

preflight_only <- env_flag("ACCRUAL_ROW_KFOLD_PREFLIGHT_ONLY")
overwrite_outputs <- env_flag("ACCRUAL_ROW_KFOLD_OVERWRITE")
force_resume <- env_flag("ACCRUAL_ROW_KFOLD_FORCE_RESUME")

if (run_mode == "FAST_MODE") {
  chains <- 2
  iter <- 1000
  warmup <- 500
} else {
  chains <- 4
  iter <- 3000
  warmup <- 1000
}
adapt_delta <- 0.95
max_treedepth <- 12
options(mc.cores = 1)

row_kfold_root <- file.path(output_root, "row_exact_kfold")
tables_dir <- file.path(row_kfold_root, "tables")
logs_dir <- file.path(row_kfold_root, "logs")
models_dir <- file.path(row_kfold_root, "models")
cache_dir <- file.path(row_kfold_root, "cache")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(row_kfold_root, file.path(row_kfold_root, "LATEST_RUN.txt"))

table_path <- function(file_name, prefer_input = FALSE) {
  candidates <- if (prefer_input) {
    c(file.path(input_winsor_root, "tables", file_name), file.path(output_root, "tables", file_name))
  } else {
    c(file.path(output_root, "tables", file_name), file.path(input_winsor_root, "tables", file_name))
  }
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) candidates[1] else hit
}

input_paths <- c(
  ex_post_sample = table_path("final_common_ex_post_sample_winsor.csv", prefer_input = TRUE),
  real_time_sample = table_path("final_common_realtime_sample_winsor.csv", prefer_input = TRUE),
  formulas = table_path("table_named_model_formulas_winsor.csv"),
  firm_kfold_latest = file.path(output_root, "kfold_firm", "LATEST_RUN.txt")
)

required_inputs <- input_paths[c("ex_post_sample", "real_time_sample", "formulas")]
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop("[BLOCKER] Missing required row-level K-fold inputs: ", paste(missing_inputs, collapse = "; "))
}

log_mean_exp <- function(x) {
  m <- max(x)
  m + log(mean(exp(x - m)))
}

softmax <- function(theta) {
  z <- c(theta, 0)
  z <- z - max(z)
  exp(z) / sum(exp(z))
}

optimize_stacking_from_lpd <- function(lpd_matrix) {
  lpd_matrix <- as.matrix(lpd_matrix)
  if (ncol(lpd_matrix) == 1) {
    out <- 1
    names(out) <- colnames(lpd_matrix)
    return(out)
  }
  log_sum_exp <- function(vals) {
    m <- max(vals)
    m + log(sum(exp(vals - m)))
  }
  mixture_objective_value <- function(w) {
    adjusted <- sweep(lpd_matrix, 2, log(pmax(w, .Machine$double.eps)), "+")
    sum(apply(adjusted, 1, log_sum_exp))
  }
  objective <- function(theta) -mixture_objective_value(softmax(theta))
  starts <- list(rep(0, ncol(lpd_matrix) - 1))
  for (j in seq_len(ncol(lpd_matrix))) {
    z <- rep(-8, ncol(lpd_matrix))
    z[j] <- 8
    starts[[length(starts) + 1]] <- z[-ncol(lpd_matrix)]
  }
  fits <- lapply(starts, function(st) {
    tryCatch(optim(st, objective, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12)),
             error = function(e) NULL)
  })
  fits <- Filter(Negate(is.null), fits)
  singleton_elpd <- colSums(lpd_matrix)
  singleton_w <- rep(0, ncol(lpd_matrix))
  singleton_w[which.max(singleton_elpd)] <- 1
  names(singleton_w) <- colnames(lpd_matrix)
  if (length(fits) == 0) return(singleton_w)
  best_fit <- fits[[which.max(vapply(fits, function(f) -f$value, numeric(1)))]]
  w <- softmax(best_fit$par)
  names(w) <- colnames(lpd_matrix)
  if (mixture_objective_value(w) + 1e-6 < mixture_objective_value(singleton_w)) singleton_w else w
}

stable_hash <- function(x) {
  x <- sort(unique(as.character(x)))
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(x, algo = "xxhash64"))
  pasted <- paste(x, collapse = "|")
  paste0(length(x), "_", sum(utf8ToInt(pasted)))
}

standardize_fold_data <- function(train_df, test_df) {
  audit <- data.frame(Variable = character(), Train_Mean = double(), Train_SD = double(),
                      Used_Fallback_Zero = logical(), stringsAsFactors = FALSE)
  for (v in pred_vars) {
    if (v %in% names(train_df)) {
      m <- mean(train_df[[v]], na.rm = TRUE)
      s <- sd(train_df[[v]], na.rm = TRUE)
      fallback <- is.na(s) || s <= 0
      train_df[[paste0(v, "_std")]] <- if (!fallback) (train_df[[v]] - m) / s else 0
      test_df[[paste0(v, "_std")]] <- if (!fallback) (test_df[[v]] - m) / s else 0
      audit <- rbind(audit, data.frame(Variable = v, Train_Mean = m, Train_SD = s,
                                       Used_Fallback_Zero = fallback, stringsAsFactors = FALSE))
    }
  }
  list(train = train_df, test = test_df, audit = audit)
}

prepare_factor_levels <- function(train_df, test_df) {
  if ("industry" %in% names(train_df)) {
    unseen_industry <- setdiff(unique(test_df$industry), unique(train_df$industry))
    if (length(unseen_industry) > 0) {
      return(list(ok = FALSE, note = paste("Unseen industry factor levels:", paste(unseen_industry, collapse = "|"))))
    }
    train_df$industry_f <- factor(train_df$industry)
    test_df$industry_f <- factor(test_df$industry, levels = levels(train_df$industry_f))
  }
  if ("year" %in% names(train_df)) {
    unseen_year <- setdiff(unique(test_df$year), unique(train_df$year))
    if (length(unseen_year) > 0) {
      return(list(ok = FALSE, note = paste("Unseen year factor levels:", paste(unseen_year, collapse = "|"))))
    }
    train_df$year_f <- factor(train_df$year)
    test_df$year_f <- factor(test_df$year, levels = levels(train_df$year_f))
  }
  list(ok = TRUE, train = train_df, test = test_df, note = NA_character_)
}

extract_fit_diagnostics <- function(fit) {
  s <- summary(fit)
  rhats <- if (!is.null(s$fixed) && "Rhat" %in% colnames(s$fixed)) s$fixed[, "Rhat"] else numeric()
  if ("random" %in% names(s) && !is.null(s$random)) {
    for (g in names(s$random)) {
      if ("Rhat" %in% colnames(s$random[[g]])) rhats <- c(rhats, s$random[[g]][, "Rhat"])
    }
  }
  np <- brms::nuts_params(fit)
  treedepths <- subset(np, Parameter == "treedepth__")$Value
  list(
    max_rhat = suppressWarnings(max(rhats, na.rm = TRUE)),
    divergences = sum(subset(np, Parameter == "divergent__")$Value),
    treedepth_warnings = sum(treedepths >= max_treedepth)
  )
}

read_sample <- function(path, target_space) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$row_id <- seq_len(nrow(df))
  df$observation_id <- paste(target_space, df$row_id, df$company, df$year, sep = ":")
  df$Target_Space <- target_space
  df
}

assign_row_folds <- function(df, target_space) {
  # Row-level folds are assigned at the firm-year level, not at the firm level.
  # To preserve the row-level/within-firm target for Firm-RE models, the assignment
  # is company-stratified: firms with at least two observations are spread over at
  # least two folds whenever possible. This prevents a non-singleton firm from being
  # entirely held out in one row-level fold. Singleton firms are unavoidable and are
  # flagged downstream as no_same_firm_history.
  set.seed(seed + match(target_space, c("ex_post", "real_time"), nomatch = 10L))
  df <- df %>% arrange(company, year, row_id)
  fold_vec <- integer(nrow(df))
  for (cc in unique(df$company)) {
    idx <- which(df$company == cc)
    idx <- sample(idx, length(idx))
    if (length(idx) == 1) {
      fold_vec[idx] <- sample(seq_len(K), 1)
    } else {
      local_folds <- rep(seq_len(min(K, length(idx))), length.out = length(idx))
      local_folds <- sample(local_folds, length(local_folds))
      fold_vec[idx] <- local_folds
    }
  }
  df <- df %>% arrange(row_id)
  df$Fold_ID <- fold_vec[df$row_id]
  df
}

df_ep <- read_sample(input_paths[["ex_post_sample"]], "ex_post") %>% assign_row_folds("ex_post")
df_rt <- read_sample(input_paths[["real_time_sample"]], "real_time") %>% assign_row_folds("real_time")
formulas_df <- read.csv(input_paths[["formulas"]], stringsAsFactors = FALSE)

firm_obs_counts <- bind_rows(df_ep, df_rt) %>% count(Target_Space, company, name = "firm_obs_count")

fold_assignment <- bind_rows(df_ep, df_rt) %>%
  left_join(firm_obs_counts, by = c("Target_Space", "company")) %>%
  transmute(
    observation_id = observation_id,
    row_id = row_id,
    company = company,
    year = year,
    industry = if ("industry" %in% names(.)) industry else NA_character_,
    target_space = Target_Space,
    fold = Fold_ID,
    K = K,
    seed = seed,
    fold_assignment_unit = "firm_year",
    fold_assignment_design = "company_stratified_row_level_to_preserve_same_firm_training_history",
    firm_obs_count = firm_obs_count,
    singleton_firm = firm_obs_count == 1
  )

fold_balance <- fold_assignment %>%
  group_by(target_space, fold) %>%
  summarise(
    n_obs = n(),
    n_firms = n_distinct(company),
    n_industries = n_distinct(industry),
    n_years = n_distinct(year),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    .groups = "drop"
  )

ex_post_ids <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07")
real_time_ids <- c("M01", "M02", "M03", "M07", "M09")

eligible <- formulas_df %>%
  filter(Sample_Group == "main_common", Main_Stack_Inclusion == TRUE) %>%
  filter((Target_Space == "ex_post" & Model_ID %in% ex_post_ids) |
           (Target_Space == "real_time" & Model_ID %in% real_time_ids))

if (length(target_space_filter) > 0) eligible <- eligible %>% filter(Target_Space %in% target_space_filter)
if (length(model_id_filter) > 0) eligible <- eligible %>% filter(Model_ID %in% model_id_filter)
if (run_mode == "FAST_MODE" && length(model_id_filter) == 0) {
  eligible <- eligible %>% group_by(Target_Space) %>% arrange(Model_ID, Heterogeneity_Variant) %>% slice_head(n = 2) %>% ungroup()
}
active_folds <- if (length(fold_filter) > 0) fold_filter else seq_len(K)
if (run_mode == "FAST_MODE" && length(fold_filter) == 0) active_folds <- active_folds[1]

if (nrow(eligible) == 0) stop("[BLOCKER] No row-level K-fold eligible models after filters.")

planned_tasks <- do.call(rbind, lapply(seq_len(nrow(eligible)), function(i) {
  row <- eligible[i, ]
  do.call(rbind, lapply(active_folds, function(fold_id) {
    model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group,
                                   row$Heterogeneity_Variant, paste0("_rowkfold_f", fold_id))
    data.frame(
      Target_Space = row$Target_Space,
      Sample_Group = row$Sample_Group,
      Fold_ID = fold_id,
      Model_ID = row$Model_ID,
      Model_Name = row$Model_Name,
      Heterogeneity_Variant = row$Heterogeneity_Variant,
      Target_Sample = row$Target_Sample,
      brms_Formula = row$brms_Formula,
      Model_Key = model_key,
      Fit_Path = file.path(models_dir, paste0("fit_", model_key, ".rds")),
      Score_Cache_Path = file.path(cache_dir, paste0("score_", model_key, ".rds")),
      Status = "PLANNED",
      Completed = FALSE,
      Failure_Reason = NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}))

write.csv(fold_assignment, file.path(tables_dir, "table_winsor_row_exact_kfold_fold_assignment.csv"), row.names = FALSE)
write.csv(fold_balance, file.path(tables_dir, "table_winsor_row_exact_kfold_balance.csv"), row.names = FALSE)
write.csv(planned_tasks, file.path(tables_dir, "table_winsor_row_exact_kfold_planned_tasks.csv"), row.names = FALSE)

write_manifest <- function(status, extra_note = NA_character_) {
  end_time <- Sys.time()
  manifest <- data.frame(
    Script_Name = script_name,
    Script_Version = script_version,
    Start_Time = format_time(script_start_time),
    End_Time = format_time(end_time),
    Runtime_Seconds = as.numeric(difftime(end_time, script_start_time, units = "secs")),
    Status = status,
    Extra_Note = extra_note,
    K = K,
    Seed = seed,
    Run_Mode = run_mode,
    Chains = chains,
    Iter = iter,
    Warmup = warmup,
    Adapt_Delta = adapt_delta,
    Max_Treedepth = max_treedepth,
    Target_Space_Filter = paste(target_space_filter, collapse = ","),
    Model_ID_Filter = paste(model_id_filter, collapse = ","),
    Fold_Filter = paste(fold_filter, collapse = ","),
    Preflight_Only = preflight_only,
    Overwrite = overwrite_outputs,
    Force_Resume = force_resume,
    Exact_Refit = TRUE,
    Validation_Unit = "row_level",
    Comparison_Target = "scripts/13_grouped_kfold_firm.R",
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    Row_KFold_Root = row_kfold_root,
    ExPost_Input = input_paths[["ex_post_sample"]],
    RealTime_Input = input_paths[["real_time_sample"]],
    Formula_Input = input_paths[["formulas"]],
    N_Planned_Tasks = nrow(planned_tasks),
    N_ExPost_Obs = nrow(df_ep),
    N_RealTime_Obs = nrow(df_rt),
    stringsAsFactors = FALSE
  )
  write.csv(manifest, file.path(tables_dir, "row_exact_kfold_run_manifest.csv"), row.names = FALSE)
  write.csv(manifest, file.path(logs_dir, "row_exact_kfold_run_manifest.csv"), row.names = FALSE)
}

write_reviewer_note <- function(status) {
  note <- c(
    "# Row-level exact K-fold reviewer note",
    "",
    paste("- Script:", script_name),
    paste("- Version:", script_version),
    paste("- Status:", status),
    paste("- K:", K),
    paste("- Seed:", seed),
    "",
    "This run uses row-level held-out folds where the validation unit is a firm-year observation.",
    "It is intentionally separate from Step 13, which uses firm-grouped folds to test out-of-firm generalization.",
    "",
    "Scoring uses exact refits on training folds and held-out log predictive density with conditional same-firm history (`re_formula = NULL`) for row-level Firm-RE prediction.",
    "The resulting row-level weights are method-matched to firm-grouped exact K-fold weights because both sides use exact refits; PSIS-LOO remains a secondary diagnostic.",
    "",
    "Preflight mode writes fold assignment, fold balance, and planned task manifests without fitting brms models."
  )
  writeLines(note, file.path(tables_dir, "row_exact_kfold_reviewer_note.md"))
}

if (preflight_only) {
  write_manifest("PREFLIGHT_ONLY_COMPLETED", "No brms refits were run.")
  write_reviewer_note("PREFLIGHT_ONLY_COMPLETED")
  message("Row-level exact K-fold preflight completed: ", row_kfold_root)
  quit(save = "no", status = 0)
}

if (env_flag("ACCRUAL_DRY_RUN", "TRUE")) {
  stop("[BLOCKER] Full row-level exact K-fold refits require ACCRUAL_DRY_RUN=FALSE. Use ACCRUAL_ROW_KFOLD_PREFLIGHT_ONLY=TRUE for fold/task planning.")
}

if (!overwrite_outputs && file.exists(file.path(tables_dir, "table_winsor_row_exact_kfold_observation_scores.csv")) && !force_resume) {
  stop("[BLOCKER] Row-level K-fold observation scores already exist. Set ACCRUAL_ROW_KFOLD_OVERWRITE=TRUE or ACCRUAL_ROW_KFOLD_FORCE_RESUME=TRUE.")
}

score_task <- function(task) {
  sample_df <- if (task$Target_Space == "ex_post") df_ep else df_rt
  train_df <- sample_df %>% filter(Fold_ID != task$Fold_ID)
  test_df <- sample_df %>% filter(Fold_ID == task$Fold_ID)
  task_start <- Sys.time()
  expected_meta <- list(
    script_version = script_version,
    target_space = task$Target_Space,
    model_id = task$Model_ID,
    variant = task$Heterogeneity_Variant,
    fold_id = task$Fold_ID,
    K = K,
    seed = seed,
    formula = task$brms_Formula,
    train_hash = stable_hash(train_df$observation_id),
    test_hash = stable_hash(test_df$observation_id)
  )
  cache_ok <- FALSE
  if (file.exists(task$Score_Cache_Path)) {
    cached <- tryCatch(readRDS(task$Score_Cache_Path), error = function(e) NULL)
    cache_ok <- !is.null(cached) && !is.null(cached$cache_meta) &&
      identical(as.character(cached$cache_meta), as.character(expected_meta))
    if (cache_ok && !force_resume) return(cached)
  }
  base_diag <- data.frame(
    Target_Space = task$Target_Space,
    Sample_Group = task$Sample_Group,
    Fold_ID = task$Fold_ID,
    Model_ID = task$Model_ID,
    Model_Name = task$Model_Name,
    Heterogeneity_Variant = task$Heterogeneity_Variant,
    Model_Key = task$Model_Key,
    N_Train_Obs = nrow(train_df),
    N_Test_Obs = nrow(test_df),
    N_Train_Firms = n_distinct(train_df$company),
    N_Test_Firms = n_distinct(test_df$company),
    Started_At = format_time(task_start),
    Ended_At = NA_character_,
    Runtime_Seconds = NA_real_,
    Completed = FALSE,
    Failure_Reason = NA_character_,
    Max_Rhat = NA_real_,
    Divergences = NA_integer_,
    Treedepth_Warnings = NA_integer_,
    N_Test_Obs_No_Same_Firm_History = NA_integer_,
    Any_New_Company_In_Row_Fold = NA,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    stringsAsFactors = FALSE
  )
  finish <- function(diag, obs = data.frame(), audit = data.frame()) {
    diag$Ended_At <- format_time(Sys.time())
    diag$Runtime_Seconds <- as.numeric(difftime(Sys.time(), task_start, units = "secs"))
    result <- list(cache_meta = expected_meta, fold_diag = diag, obs_scores = obs, standardization_audit = audit)
    saveRDS(result, task$Score_Cache_Path)
    result
  }
  if (nrow(train_df) == 0 || nrow(test_df) == 0) {
    base_diag$Failure_Reason <- "Empty train or test split."
    return(finish(base_diag))
  }
  std <- standardize_fold_data(train_df, test_df)
  train_df <- std$train
  test_df <- std$test
  audit <- std$audit %>%
    mutate(Target_Space = task$Target_Space, Fold_ID = task$Fold_ID, Model_ID = task$Model_ID,
           Heterogeneity_Variant = task$Heterogeneity_Variant)
  factor_prep <- prepare_factor_levels(train_df, test_df)
  if (!factor_prep$ok) {
    base_diag$Failure_Reason <- factor_prep$note
    return(finish(base_diag, audit = audit))
  }
  train_df <- factor_prep$train
  test_df <- factor_prep$test
  formula_str <- fix_formula(task$brms_Formula, prefactor = TRUE)
  fit <- if (file.exists(task$Fit_Path)) tryCatch(readRDS(task$Fit_Path), error = function(e) NULL) else NULL
  if (is.null(fit)) {
    fit <- tryCatch({
      brm(
        formula = bf(as.formula(formula_str)),
        data = train_df,
        family = brms_family(),
        prior = default_prior_list(task$Heterogeneity_Variant, model_structure = model_structure),
        chains = chains,
        iter = iter,
        warmup = warmup,
        control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
        seed = seed + task$Fold_ID,
        refresh = 500,
        save_pars = brms::save_pars(all = TRUE)
      )
    }, error = function(e) e)
    if (inherits(fit, "error")) {
      base_diag$Failure_Reason <- paste("brms fit failed:", fit$message)
      return(finish(base_diag, audit = audit))
    }
    saveRDS(fit, task$Fit_Path)
  }
  fit_diag <- tryCatch(extract_fit_diagnostics(fit), error = function(e) {
    list(max_rhat = NA_real_, divergences = NA_integer_, treedepth_warnings = NA_integer_)
  })
  base_diag$Max_Rhat <- fit_diag$max_rhat
  base_diag$Divergences <- fit_diag$divergences
  base_diag$Treedepth_Warnings <- fit_diag$treedepth_warnings
  base_diag$N_Test_Obs_No_Same_Firm_History <- sum(!same_firm_history_available)
  base_diag$Any_New_Company_In_Row_Fold <- any_new_company_in_row_fold
  same_firm_history_available <- test_df$company %in% train_df$company
  any_new_company_in_row_fold <- any(!same_firm_history_available)

  # Exact row-level validation should use same-firm history for Firm-RE models
  # whenever the held-out firm is present in the training fold. Therefore the
  # primary row-level scoring call keeps group-level effects (re_formula = NULL).
  # If a singleton/new company appears in a row fold, brms is allowed to generate
  # a new level, but those observations are flagged and excluded from primary
  # row-target weight construction below.
  ll_draws <- tryCatch({
    if (any_new_company_in_row_fold) {
      brms::log_lik(
        fit,
        newdata = test_df,
        re_formula = NULL,
        allow_new_levels = TRUE,
        sample_new_levels = "uncertainty"
      )
    } else {
      brms::log_lik(
        fit,
        newdata = test_df,
        re_formula = NULL,
        allow_new_levels = FALSE
      )
    }
  }, error = function(e) e)

  if (inherits(ll_draws, "error")) {
    base_diag$Failure_Reason <- paste("held-out row-level conditional log_lik failed:", ll_draws$message)
    return(finish(base_diag, audit = audit))
  }
  if (ncol(ll_draws) != nrow(test_df)) {
    base_diag$Failure_Reason <- sprintf("held-out log_lik N mismatch: ncol=%d test_n=%d", ncol(ll_draws), nrow(test_df))
    return(finish(base_diag, audit = audit))
  }
  lpd_obs <- apply(ll_draws, 2, log_mean_exp)
  obs <- data.frame(
    target_space = task$Target_Space,
    model_id = task$Model_ID,
    model_name = task$Model_Name,
    heterogeneity_variant = task$Heterogeneity_Variant,
    sample_group = task$Sample_Group,
    fold = task$Fold_ID,
    company = test_df$company,
    year = test_df$year,
    row_id = test_df$row_id,
    observation_id = test_df$observation_id,
    observed_TA_scaled = test_df$TA_scaled,
    log_predictive_density = lpd_obs,
    prediction_rule = ifelse(same_firm_history_available, "heldout_log_lik_re_formula_NULL_same_firm_history", "heldout_log_lik_re_formula_NULL_new_level_uncertainty_fallback"),
    same_firm_history_available = same_firm_history_available,
    new_company_in_row_fold = !same_firm_history_available,
    primary_row_target_inclusion = same_firm_history_available,
    new_firm_prediction_mode = ifelse(same_firm_history_available, "existing_firm_conditional_random_effect", "new_level_uncertainty_fallback_not_primary"),
    refit_type = "exact_refit",
    validation_unit = "row_level",
    prior_set_id = prior_set_id,
    likelihood_family = likelihood_family,
    model_structure = model_structure,
    output_root = output_root,
    stringsAsFactors = FALSE
  )
  base_diag$Completed <- TRUE
  finish(base_diag, obs, audit)
}

message("Row-level exact K-fold tasks: ", nrow(planned_tasks))
results <- vector("list", nrow(planned_tasks))
for (i in seq_len(nrow(planned_tasks))) {
  task <- planned_tasks[i, ]
  message(sprintf("[%d/%d] %s %s %s fold %d", i, nrow(planned_tasks), task$Target_Space,
                  task$Model_ID, task$Heterogeneity_Variant, task$Fold_ID))
  results[[i]] <- score_task(task)
}

fold_diagnostics <- bind_rows(lapply(results, `[[`, "fold_diag"))
standardization_audit <- bind_rows(lapply(results, `[[`, "standardization_audit"))
obs_scores <- bind_rows(lapply(results, `[[`, "obs_scores"))

write.csv(fold_diagnostics, file.path(tables_dir, "table_winsor_row_exact_kfold_refit_diagnostics.csv"), row.names = FALSE)
write.csv(standardization_audit, file.path(tables_dir, "table_winsor_row_exact_kfold_train_standardization_audit.csv"), row.names = FALSE)
write.csv(obs_scores, file.path(tables_dir, "table_winsor_row_exact_kfold_observation_scores.csv"), row.names = FALSE)

model_scores <- if (nrow(obs_scores) > 0) {
  obs_scores %>%
    filter(primary_row_target_inclusion == TRUE) %>%
    group_by(target_space, sample_group, model_id, model_name, heterogeneity_variant) %>%
    summarise(
      n_obs_scored = n(),
      n_obs_excluded_no_same_firm_history = sum(!primary_row_target_inclusion, na.rm = TRUE),
      elpd_exact_row_kfold = sum(log_predictive_density, na.rm = TRUE),
      mean_lpd = mean(log_predictive_density, na.rm = TRUE),
      sd_lpd = sd(log_predictive_density, na.rm = TRUE),
      refit_type = "exact_refit",
      validation_unit = "row_level",
      .groups = "drop"
    ) %>%
    left_join(
      fold_diagnostics %>%
        group_by(Target_Space, Sample_Group, Model_ID, Model_Name, Heterogeneity_Variant) %>%
        summarise(
          n_folds_attempted = n(),
          n_folds_completed = sum(Completed, na.rm = TRUE),
          max_rhat_max = suppressWarnings(max(Max_Rhat, na.rm = TRUE)),
          divergences_total = sum(Divergences, na.rm = TRUE),
          treedepth_warnings_total = sum(Treedepth_Warnings, na.rm = TRUE),
          failure_reason = paste(na.omit(unique(Failure_Reason)), collapse = " | "),
          .groups = "drop"
        ),
      by = c("target_space" = "Target_Space", "sample_group" = "Sample_Group",
             "model_id" = "Model_ID", "model_name" = "Model_Name",
             "heterogeneity_variant" = "Heterogeneity_Variant")
    ) %>%
    mutate(
      max_rhat_max = ifelse(is.infinite(max_rhat_max), NA_real_, max_rhat_max),
      reliability_flag = case_when(
        n_folds_completed == 0 ~ "FAILED",
        length(fold_filter) == 0 & n_folds_completed < K ~ "LOW_RELIABILITY",
        !is.na(max_rhat_max) & max_rhat_max <= 1.01 & divergences_total == 0 ~ "OK",
        !is.na(max_rhat_max) & max_rhat_max <= 1.05 ~ "CAUTION",
        TRUE ~ "LOW_RELIABILITY"
      ),
      included_in_stack = reliability_flag %in% c("OK", "CAUTION") &
        ifelse(length(fold_filter) == 0, n_folds_completed == K, n_folds_completed > 0)
    )
} else {
  data.frame()
}
if (nrow(model_scores) > 0 && nrow(obs_scores) > 0) {
  exclusion_counts <- obs_scores %>%
    group_by(target_space, sample_group, model_id, model_name, heterogeneity_variant) %>%
    summarise(
      n_obs_total_heldout = n(),
      n_obs_excluded_no_same_firm_history = sum(primary_row_target_inclusion == FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  model_scores <- model_scores %>%
    select(-any_of("n_obs_excluded_no_same_firm_history")) %>%
    left_join(exclusion_counts,
              by = c("target_space", "sample_group", "model_id", "model_name", "heterogeneity_variant")) %>%
    relocate(n_obs_total_heldout, n_obs_scored, n_obs_excluded_no_same_firm_history, .after = heterogeneity_variant)
}
write.csv(model_scores, file.path(tables_dir, "table_winsor_row_exact_kfold_model_scores.csv"), row.names = FALSE)

build_row_weights <- function(target_space) {
  included <- model_scores %>%
    filter(target_space == !!target_space, sample_group == "main_common", included_in_stack == TRUE) %>%
    arrange(model_id, heterogeneity_variant)
  if (nrow(included) == 0 || nrow(obs_scores) == 0) return(data.frame())
  expected_n <- obs_scores %>%
    filter(target_space == !!target_space, primary_row_target_inclusion == TRUE) %>%
    distinct(observation_id) %>%
    nrow()
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, ]
    key <- model_key_sampled(row$model_id, row$target_space, row$sample_group,
                             row$heterogeneity_variant, "_row_exact_kfold")
    one <- obs_scores %>%
      filter(target_space == !!target_space,
             model_id == row$model_id,
             heterogeneity_variant == row$heterogeneity_variant,
             primary_row_target_inclusion == TRUE) %>%
      arrange(observation_id)
    if (nrow(one) != expected_n) next
    score_list[[key]] <- one$log_predictive_density
    meta_keys <- c(meta_keys, key)
  }
  if (length(score_list) == 0) return(data.frame())
  lpd_matrix <- do.call(cbind, score_list)
  colnames(lpd_matrix) <- names(score_list)
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  if (abs(sum(weights) - 1) > 1e-5) {
    stop("[BLOCKER] Row-level exact K-fold stacking weights do not sum to 1 for ", target_space)
  }
  meta_idx <- match(names(weights), meta_keys)
  included[meta_idx, ] %>%
    mutate(
      model_key_row_exact_kfold = names(weights),
      weight_row_exact_kfold = as.numeric(weights),
      singleton_elpd = as.numeric(colSums(lpd_matrix)[names(weights)])
    ) %>%
    arrange(desc(weight_row_exact_kfold)) %>%
    mutate(rank_row_exact_kfold = row_number()) %>%
    select(target_space, sample_group, model_id, model_name, heterogeneity_variant,
           model_key_row_exact_kfold, weight_row_exact_kfold, rank_row_exact_kfold,
           elpd_exact_row_kfold, singleton_elpd, mean_lpd, reliability_flag)
}

weights_ep <- build_row_weights("ex_post")
weights_rt <- build_row_weights("real_time")
write.csv(weights_ep, file.path(tables_dir, "table_winsor_row_exact_kfold_weights_ex_post.csv"), row.names = FALSE)
write.csv(weights_rt, file.path(tables_dir, "table_winsor_row_exact_kfold_weights_no_lookahead.csv"), row.names = FALSE)

read_firm_kfold_weight <- function(file_name) {
  latest_path <- input_paths[["firm_kfold_latest"]]
  if (!file.exists(latest_path)) return(data.frame())
  root <- trimws(readLines(latest_path, warn = FALSE)[1])
  path <- file.path(root, "tables", file_name)
  if (!file.exists(path)) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE) %>%
    transmute(
      target_space = Target_Space,
      model_id = Model_ID,
      model_name = Model_Name,
      heterogeneity_variant = Heterogeneity_Variant,
      firm_grouped_kfold_weight = Weight_KFold,
      firm_grouped_rank = Rank_KFold
    )
}

family_indicator <- function(variant) {
  ifelse(grepl("Firm RE|Random Intercept", variant, ignore.case = TRUE), "firm_RE", "pooled_industry_year")
}

row_weights <- bind_rows(weights_ep, weights_rt) %>%
  select(target_space, model_id, model_name, heterogeneity_variant, row_exact_kfold_weight = weight_row_exact_kfold,
         row_exact_rank = rank_row_exact_kfold)
firm_weights <- bind_rows(
  read_firm_kfold_weight("table_winsor_kfold_weights_ex_post.csv"),
  read_firm_kfold_weight("table_winsor_kfold_weights_no_lookahead.csv")
)

weight_comparison <- full_join(row_weights, firm_weights,
                               by = c("target_space", "model_id", "model_name", "heterogeneity_variant")) %>%
  mutate(
    row_exact_kfold_weight = ifelse(is.na(row_exact_kfold_weight), 0, row_exact_kfold_weight),
    firm_grouped_kfold_weight = ifelse(is.na(firm_grouped_kfold_weight), 0, firm_grouped_kfold_weight),
    difference = row_exact_kfold_weight - firm_grouped_kfold_weight,
    firmRE_family_indicator = family_indicator(heterogeneity_variant)
  ) %>%
  arrange(target_space, row_exact_rank, firm_grouped_rank)
write.csv(weight_comparison, file.path(tables_dir, "table_winsor_exact_kfold_weight_comparison_row_vs_firm.csv"), row.names = FALSE)
write.csv(weight_comparison, file.path(tables_dir, "table_winsor_row_vs_firm_kfold_weight_comparison.csv"), row.names = FALSE)

family_comparison <- weight_comparison %>%
  group_by(target_space, firmRE_family_indicator) %>%
  summarise(
    row_exact_kfold_family_weight = sum(row_exact_kfold_weight, na.rm = TRUE),
    firm_grouped_kfold_family_weight = sum(firm_grouped_kfold_weight, na.rm = TRUE),
    difference = row_exact_kfold_family_weight - firm_grouped_kfold_family_weight,
    .groups = "drop"
  ) %>%
  arrange(target_space, firmRE_family_indicator)
write.csv(family_comparison, file.path(tables_dir, "table_winsor_exact_kfold_family_weight_comparison_row_vs_firm.csv"), row.names = FALSE)
write.csv(family_comparison, file.path(tables_dir, "table_winsor_row_vs_firm_kfold_family_comparison.csv"), row.names = FALSE)

write_manifest("COMPLETED", NA_character_)
write_reviewer_note("COMPLETED")
message("Row-level exact K-fold completed: ", row_kfold_root)
