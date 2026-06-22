# -----------------------------------------------------------------------------
# Script: 13_grouped_kfold_firm.R
# Purpose: Reviewer Priority 2b - exact grouped K-fold cross-validation by firm
#          on the winsorized accrual uncertainty pipeline, with cache-safe run roots.
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(brms)
})

source("scripts/ma00_setup.R")
phase_begin("ma12", "Grouped exact firm K-fold")
ensure_analysis_dirs()

script_start_time <- Sys.time()
script_name <- "scripts/ma12_grouped_kfold_firm.R"
script_version <- "2026-06-18-v3-reviewer-final-main-stack-ess-gate"

split_env <- function(name) {
  x <- trimws(Sys.getenv(name, ""))
  if (!nzchar(x)) return(character())
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

run_mode <- toupper(env_value("ACCRUAL_KFOLD_FIRM_MODE", "FULL_MODE"))
if (!run_mode %in% c("FULL_MODE", "FAST_MODE")) {
  stop("[BLOCKER] ACCRUAL_KFOLD_FIRM_MODE must be FULL_MODE or FAST_MODE.")
}
kfold_cfg <- accrual_kfold_config("grouped_firm", run_mode = run_mode)
K <- kfold_cfg$K
chains <- kfold_cfg$chains
iter <- kfold_cfg$iter
warmup <- kfold_cfg$warmup
adapt_delta <- kfold_cfg$adapt_delta
max_treedepth <- kfold_cfg$max_treedepth
kfold_chain_cores <- kfold_cfg$cores
grouped_run_rng_meta <- accrual_rng_metadata_list("grouped_kfold_run_manifest")
options(mc.cores = kfold_chain_cores)

run_id <- trimws(Sys.getenv("ACCRUAL_KFOLD_FIRM_RUN_ID", "default"))
if (!nzchar(run_id)) run_id <- "default"
run_id <- gsub("[^A-Za-z0-9_.-]", "_", run_id)

preflight_only <- env_flag("ACCRUAL_KFOLD_FIRM_PREFLIGHT_ONLY")
overwrite_run <- env_flag("ACCRUAL_KFOLD_FIRM_OVERWRITE")
force_resume <- env_flag("ACCRUAL_KFOLD_FIRM_FORCE_RESUME")
# Stratified grouped K-fold. Default TRUE: firms are dealt out fold by fold WITHIN
# each industry (round-robin), so every industry with at least K firms appears in
# every fold and therefore in every training fold. This is the job of this script:
# build correct folds. It does NOT paper over data problems such as an industry
# that contains fewer than K firms; that is handled by a hard data check below.
kfold_stratified_groups <- env_flag("ACCRUAL_KFOLD_STRATIFIED_GROUPS", "TRUE")
kfold_repeats <- env_int("ACCRUAL_KFOLD_REPEATS", 1L, min = 1L)
if (kfold_repeats > 1) {
  warning("[WARNING] ACCRUAL_KFOLD_REPEATS is recognized for future repeated grouped K-fold runs; this script currently executes one repeat per run_id. Use separate ACCRUAL_KFOLD_FIRM_RUN_ID values for repeated runs.")
}

target_space_filter <- split_env("ACCRUAL_KFOLD_TARGET_SPACE")
model_id_filter <- split_env("ACCRUAL_KFOLD_MODEL_IDS")
fold_filter_raw <- split_env("ACCRUAL_KFOLD_FOLDS")
fold_filter <- if (length(fold_filter_raw) > 0) as.integer(fold_filter_raw) else integer()
if (any(is.na(fold_filter))) stop("[BLOCKER] ACCRUAL_KFOLD_FOLDS must be comma-separated integers.")
kfold_target_mode <- toupper(Sys.getenv("ACCRUAL_KFOLD_TARGET_MODE", "MAIN_STACK_FULL"))
if (!kfold_target_mode %in% c("PARETO_PROBLEM_ONLY", "MAIN_STACK_FULL")) {
  stop("[BLOCKER] ACCRUAL_KFOLD_TARGET_MODE must be PARETO_PROBLEM_ONLY or MAIN_STACK_FULL.")
}

partial_run <- length(target_space_filter) > 0 || length(model_id_filter) > 0 || length(fold_filter) > 0

config_tag <- paste0("K", K, "_", run_mode, "_modelset_primary_v", script_version)
config_tag <- paste0(config_tag, "_", run_id)

kfold_base_root <- file.path(winsor_root, "kfold_firm")
kfold_run_root <- file.path(kfold_base_root, config_tag)
tables_dir <- file.path(kfold_run_root, "tables")
logs_dir <- file.path(kfold_run_root, "logs")
figures_dir <- file.path(kfold_run_root, "figures")
models_dir <- file.path(kfold_run_root, "models")
cache_dir <- file.path(kfold_run_root, "cache")
checkpoints_dir <- file.path(kfold_run_root, "checkpoints")
lock_path <- file.path(kfold_run_root, "RUNNING.lock")
latest_run_path <- file.path(kfold_base_root, "LATEST_RUN.txt")
latest_completed_run_path <- file.path(kfold_base_root, "LATEST_COMPLETED_RUN.txt")
completed_run_pin_eligible <- FALSE
completed_run_pin_updated <- FALSE

dir.create(kfold_base_root, recursive = TRUE, showWarnings = FALSE)

assert_safe_path <- function(path, required_substring, label) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!grepl(required_substring, normalized, fixed = TRUE)) {
    stop("[BLOCKER] Unsafe path for ", label, ": ", path)
  }
}

path_starts_with <- function(path, root) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  r <- normalizePath(root, winslash = "/", mustWork = FALSE)
  startsWith(p, r)
}

file_size_or_na <- function(path) if (file.exists(path)) file.info(path)$size else NA_real_
mtime_or_na <- function(path) if (file.exists(path)) as.character(file.info(path)$mtime) else NA_character_
file_hash_or_na <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
}
nrows_or_na <- function(path) {
  if (!file.exists(path) || !grepl("\\.csv$", path, ignore.case = TRUE)) return(NA_integer_)
  out <- tryCatch(nrow(read.csv(path, stringsAsFactors = FALSE)), error = function(e) NA_integer_)
  out
}
git_commit_or_na <- function() {
  tryCatch(system("git rev-parse HEAD", intern = TRUE)[1], error = function(e) NA_character_)
}

stable_hash <- function(x) {
  x <- sort(unique(as.character(x)))
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(x, algo = "xxhash64"))
  }
  pasted <- paste(x, collapse = "|")
  paste0(length(x), "_", sum(utf8ToInt(pasted)))
}

format_time <- function(x) format(x, "%Y-%m-%d %H:%M:%S %Z")

write_run_manifest <- function(status, end_time = NA, runtime_seconds = NA,
                               exp_n = NA_integer_, exp_firms = NA_integer_,
                               rt_n = NA_integer_, rt_firms = NA_integer_) {
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- data.frame(
    Script_Name = script_name,
    Script_Version = script_version,
    Start_Time = format_time(script_start_time),
    End_Time = if (inherits(end_time, "POSIXt")) format_time(end_time) else NA_character_,
    Runtime_Seconds = runtime_seconds,
    Runtime_Hours = ifelse(is.na(runtime_seconds), NA_real_, runtime_seconds / 3600),
    K = K,
    Run_Mode = run_mode,
    Run_ID = run_id,
    Config_Tag = config_tag,
    Kfold_Run_Root = kfold_run_root,
    Chains = chains,
    Chain_Cores = kfold_chain_cores,
    Iter = iter,
    Warmup = warmup,
    Adapt_Delta = adapt_delta,
    Max_Treedepth = max_treedepth,
    Sampler_Profile = kfold_cfg$sampler_profile,
    Config_Source = kfold_cfg$config_source,
    RNG_Context = grouped_run_rng_meta$RNG_Context,
    RNG_Offset = grouped_run_rng_meta$RNG_Offset,
    Canonical_Seed = grouped_run_rng_meta$Canonical_Seed,
    Effective_Seed = grouped_run_rng_meta$Effective_Seed,
    RNG_Source = grouped_run_rng_meta$RNG_Source,
    ExPost_N_Obs = exp_n,
    ExPost_N_Firms = exp_firms,
    NoLookahead_N_Obs = rt_n,
    NoLookahead_N_Firms = rt_firms,
    ExPost_Model_IDs = paste(main_model_ids_for_space("ex_post"), collapse = ","),
    NoLookahead_Model_IDs = paste(main_model_ids_for_space("real_time"), collapse = ","),
    Target_Space_Filter = paste(target_space_filter, collapse = ","),
    Model_ID_Filter = paste(model_id_filter, collapse = ","),
    KFold_Target_Mode = kfold_target_mode,
    Stratified_Grouped_KFold = kfold_stratified_groups,
    Prior_Set_ID = prior_set_id,
    Likelihood_Family = likelihood_family,
    Model_Structure = model_structure,
    Output_Root = output_root,
    Fold_Filter = paste(fold_filter, collapse = ","),
    Preflight_Only = preflight_only,
    Partial_Run = partial_run,
    Latest_Run_Path = latest_run_path,
    Latest_Completed_Run_Path = latest_completed_run_path,
    Completed_Run_Pin_Eligible = completed_run_pin_eligible,
    Completed_Run_Pin_Updated = completed_run_pin_updated,
    Status = status,
    stringsAsFactors = FALSE
  )
  write.csv(manifest, file.path(logs_dir, "run_config_manifest.csv"), row.names = FALSE)
}

completed_manifest_exists <- function() {
  path <- file.path(logs_dir, "run_config_manifest.csv")
  if (!file.exists(path)) return(FALSE)
  x <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(x) == 0) return(FALSE)
  partial_completed <- "Partial_Run" %in% names(x) && isTRUE(as.logical(x$Partial_Run[1]))
  x$Status[1] %in% c("COMPLETED", "PREFLIGHT_ONLY_COMPLETED") && !partial_completed
}

if (dir.exists(kfold_run_root) && completed_manifest_exists() && !overwrite_run) {
  stop("[BLOCKER] Completed run folder already exists: ", kfold_run_root,
       ". Set ACCRUAL_KFOLD_FIRM_OVERWRITE='TRUE' to overwrite, or choose a new ACCRUAL_KFOLD_FIRM_RUN_ID.")
}

for (d in c(kfold_run_root, tables_dir, logs_dir, figures_dir, models_dir, cache_dir, checkpoints_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
writeLines(kfold_run_root, latest_run_path)

if (file.exists(lock_path) && !force_resume && !overwrite_run) {
  stop("[BLOCKER] RUNNING.lock exists for this run. If the previous run crashed, set ACCRUAL_KFOLD_FIRM_FORCE_RESUME='TRUE'. Path: ", lock_path)
}
writeLines(c(
  paste("Started:", format_time(script_start_time)),
  paste("Config_Tag:", config_tag),
  paste("Run_ID:", run_id)
), lock_path)

write_run_manifest("STARTED")

heartbeat_path <- file.path(logs_dir, "heartbeat.log")
heartbeat <- function(target_space, model_id, variant, fold_id, status, extra = "") {
  elapsed <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
  line <- sprintf("%s | %s | %s | %s | fold %s | %s | elapsed_sec=%.1f%s",
                  format_time(Sys.time()), target_space, model_id, variant, fold_id, status, elapsed,
                  ifelse(nzchar(extra), paste0(" | ", extra), ""))
  cat(line, "\n")
  cat(line, "\n", file = heartbeat_path, append = TRUE)
}

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
  no_lookahead_sample = table_path("final_common_realtime_sample_winsor.csv", prefer_input = TRUE),
  formulas = table_path("table_named_model_formulas_winsor.csv"),
  rowloo_ex_post = file.path(output_root, "tables", "table_stacking_weights_ex_post_winsor_corrected.csv"),
  rowloo_no_lookahead = file.path(output_root, "tables", "table_stacking_weights_no_lookahead_winsor_corrected.csv"),
  lofo_ex_post = file.path(output_root, "lofo", "tables", "table_winsor_lofo_weights_ex_post.csv"),
  lofo_no_lookahead = file.path(output_root, "lofo", "tables", "table_winsor_lofo_weights_no_lookahead.csv"),
  diagnostics = file.path(output_root, "tables", "table_brms_diagnostics_winsor.csv"),
  lofo_diagnostics = file.path(output_root, "lofo", "tables", "table_winsor_lofo_model_diagnostics.csv")
)
optional_inputs <- c(
  rowloo_ex_post = TRUE,
  rowloo_no_lookahead = TRUE,
  lofo_ex_post = TRUE,
  lofo_no_lookahead = TRUE,
  diagnostics = TRUE,
  lofo_diagnostics = TRUE
)

write_input_manifest <- function() {
  man <- data.frame(
    Script_Name = script_name,
    Script_Version = script_version,
    Run_Root = kfold_run_root,
    Input_Name = names(input_paths),
    Path = unname(input_paths),
    Exists = file.exists(input_paths),
    File_Size_Bytes = vapply(input_paths, file_size_or_na, numeric(1)),
    Modified_Time = vapply(input_paths, mtime_or_na, character(1)),
    File_Hash = vapply(input_paths, file_hash_or_na, character(1)),
    N_Rows = vapply(input_paths, nrows_or_na, integer(1)),
    Optional = names(input_paths) %in% names(optional_inputs),
    Primary_Secondary = ifelse(names(input_paths) %in% c("lofo_ex_post", "lofo_no_lookahead", "lofo_diagnostics", "rowloo_ex_post", "rowloo_no_lookahead"),
                               "secondary_comparison_optional", "primary_grouped_kfold_input"),
    Git_Commit = git_commit_or_na(),
    Notes = "",
    stringsAsFactors = FALSE
  )
  write.csv(man, file.path(logs_dir, "input_file_manifest.csv"), row.names = FALSE)
}

write_output_manifest <- function(final_decision = NA_character_) {
  outputs <- c(
    fold_assignment = file.path(tables_dir, "table_winsor_firm_fold_assignment.csv"),
    fold_balance = file.path(tables_dir, "table_winsor_kfold_balance.csv"),
    industry_fold_coverage = file.path(tables_dir, "table_winsor_kfold_industry_fold_coverage.csv"),
    model_fold_manifest = file.path(tables_dir, "table_winsor_kfold_model_fold_manifest.csv"),
    refit_diagnostics = file.path(tables_dir, "table_winsor_kfold_refit_diagnostics.csv"),
    standardization_audit = file.path(tables_dir, "table_winsor_kfold_train_standardization_audit.csv"),
    observation_scores = file.path(tables_dir, "table_winsor_kfold_observation_scores.csv"),
    fold_scores = file.path(tables_dir, "table_winsor_kfold_fold_scores.csv"),
    model_scores = file.path(tables_dir, "table_winsor_kfold_model_scores.csv"),
    weights_ex_post = file.path(tables_dir, "table_winsor_kfold_weights_ex_post.csv"),
    weights_no_lookahead = file.path(tables_dir, "table_winsor_kfold_weights_no_lookahead.csv"),
    model_weight_comparison = file.path(tables_dir, "table_winsor_weight_stability_loo_lofo_kfold.csv"),
    family_weight_comparison = file.path(tables_dir, "table_winsor_family_weight_stability_loo_lofo_kfold.csv"),
    decision = file.path(tables_dir, "table_reviewer_priority2b_exact_kfold_decision.csv"),
    reviewer_notes = file.path(logs_dir, "reviewer_priority2b_exact_kfold_response_notes.txt"),
    technical_log = file.path(logs_dir, "phase4e_exact_grouped_kfold_winsor_notes.txt"),
    run_config_manifest = file.path(logs_dir, "run_config_manifest.csv"),
    input_file_manifest = file.path(logs_dir, "input_file_manifest.csv"),
    output_file_manifest = file.path(logs_dir, "output_file_manifest.csv"),
    heartbeat = file.path(logs_dir, "heartbeat.log")
  )
  man <- data.frame(
    Script_Name = script_name,
    Script_Version = script_version,
    Run_Root = kfold_run_root,
    Output_Name = names(outputs),
    Path = unname(outputs),
    Exists = file.exists(outputs),
    File_Size_Bytes = vapply(outputs, file_size_or_na, numeric(1)),
    Modified_Time = vapply(outputs, mtime_or_na, character(1)),
    File_Hash = vapply(outputs, file_hash_or_na, character(1)),
    N_Rows = vapply(outputs, nrows_or_na, integer(1)),
    Notes = ifelse(names(outputs) == "decision", paste("Final decision:", final_decision), ""),
    Latest_Run_Path = latest_run_path,
    Latest_Completed_Run_Path = latest_completed_run_path,
    Completed_Run_Pin_Eligible = completed_run_pin_eligible,
    Completed_Run_Pin_Updated = completed_run_pin_updated,
    Primary_Inference_Allowed = completed_run_pin_eligible,
    Primary_Secondary = ifelse(names(outputs) %in% c("model_weight_comparison", "family_weight_comparison"),
                               "secondary_comparison", "primary_grouped_kfold_output"),
    Git_Commit = git_commit_or_na(),
    stringsAsFactors = FALSE
  )
  write.csv(man, file.path(logs_dir, "output_file_manifest.csv"), row.names = FALSE)
}

optional_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE)
}

safe_shutdown <- function(status, exp_n = NA_integer_, exp_firms = NA_integer_,
                          rt_n = NA_integer_, rt_firms = NA_integer_,
                          remove_lock = FALSE) {
  end_time <- Sys.time()
  runtime_seconds <- as.numeric(difftime(end_time, script_start_time, units = "secs"))
  write_run_manifest(status, end_time, runtime_seconds, exp_n, exp_firms, rt_n, rt_firms)
  write_output_manifest(status)
  if (remove_lock && file.exists(lock_path)) unlink(lock_path)
}

main <- function() {
  assert_safe_path(kfold_run_root, config_tag, "kfold_run_root")
  assert_safe_path(input_paths[["ex_post_sample"]], "winsor", "ex_post_sample")
  assert_safe_path(input_paths[["ex_post_sample"]], "_winsor", "ex_post_sample")
  assert_safe_path(input_paths[["no_lookahead_sample"]], "winsor", "no_lookahead_sample")
  assert_safe_path(input_paths[["no_lookahead_sample"]], "_winsor", "no_lookahead_sample")

  required_input_names <- setdiff(names(input_paths), names(optional_inputs))
  missing_required <- input_paths[required_input_names][!file.exists(input_paths[required_input_names])]
  write_input_manifest()
  if (length(missing_required) > 0) {
    stop("[BLOCKER] Missing required input(s): ",
         paste(names(missing_required), missing_required, sep = "=", collapse = "; "))
  }

  df_ep <- read.csv(input_paths[["ex_post_sample"]], stringsAsFactors = FALSE)
  df_rt <- read.csv(input_paths[["no_lookahead_sample"]], stringsAsFactors = FALSE)
  formulas_df <- read.csv(input_paths[["formulas"]], stringsAsFactors = FALSE)
  rowloo_ep <- optional_read_csv(input_paths[["rowloo_ex_post"]])
  rowloo_rt <- optional_read_csv(input_paths[["rowloo_no_lookahead"]])
  lofo_ep <- optional_read_csv(input_paths[["lofo_ex_post"]])
  lofo_rt <- optional_read_csv(input_paths[["lofo_no_lookahead"]])

  required_cols <- c("company", "year", "industry", "TA_scaled")
  for (nm in c("df_ep", "df_rt")) {
    df <- get(nm)
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) stop("[BLOCKER] ", nm, " missing columns: ", paste(missing_cols, collapse = ", "))
  }
  if (any(!grepl("_winsor", input_paths[c("ex_post_sample", "no_lookahead_sample")], fixed = TRUE))) {
    stop("[BLOCKER] Non-winsorized sample path detected.")
  }

  df_ep$Obs_ID <- seq_len(nrow(df_ep))
  df_rt$Obs_ID <- seq_len(nrow(df_rt))

  ex_post_ids <- main_model_ids_for_space("ex_post")
  no_lookahead_ids <- main_model_ids_for_space("real_time")

  if (identical(kfold_target_mode, "PARETO_PROBLEM_ONLY") &&
      length(target_space_filter) == 0 && length(model_id_filter) == 0) {
    pareto_sources <- list()
    if (file.exists(input_paths[["diagnostics"]])) {
      d0 <- read.csv(input_paths[["diagnostics"]], stringsAsFactors = FALSE)
      if ("pareto_k_above_07" %in% names(d0)) {
        pareto_sources[[length(pareto_sources) + 1]] <- d0 %>%
          filter(pareto_k_above_07 > 0) %>%
          transmute(Target_Space, Model_ID)
      }
    }
    if (file.exists(input_paths[["lofo_diagnostics"]])) {
      d1 <- read.csv(input_paths[["lofo_diagnostics"]], stringsAsFactors = FALSE)
      if ("pareto_k_gt_0_7" %in% names(d1)) {
        pareto_sources[[length(pareto_sources) + 1]] <- d1 %>%
          filter(pareto_k_gt_0_7 > 0) %>%
          transmute(Target_Space, Model_ID)
      }
    }
    pareto_targets <- if (length(pareto_sources) > 0) bind_rows(pareto_sources) %>% distinct() else data.frame()
    pareto_targets <- pareto_targets %>% filter(Model_ID %in% c(ex_post_ids, no_lookahead_ids))
    if (nrow(pareto_targets) == 0) {
      stop("[BLOCKER] ACCRUAL_KFOLD_TARGET_MODE='PARETO_PROBLEM_ONLY' found no main-stack models with Pareto-k > 0.7. Set ACCRUAL_KFOLD_TARGET_MODE='MAIN_STACK_FULL' or explicit ACCRUAL_KFOLD_MODEL_IDS to run anyway.")
    }
    ex_post_ids <- intersect(ex_post_ids, pareto_targets$Model_ID[pareto_targets$Target_Space == "ex_post"])
    no_lookahead_ids <- intersect(no_lookahead_ids, pareto_targets$Model_ID[pareto_targets$Target_Space == "real_time"])
    target_space_filter <- unique(pareto_targets$Target_Space)
  }

  family_label <- function(model_id) {
    dplyr::case_when(
      model_id %in% c("M01", "M02", "M03") ~ "Jones-family",
      model_id %in% c("M04", "M05", "M06") ~ "Cash-flow/McNichols-family",
      model_id == "M07" ~ "Ball-Shivakumar/asymmetry",
      model_id == "M09" ~ "No-lookahead/real-time",
      model_id == "M08" ~ "Secondary volatility",
      model_id == "M10" ~ "Secondary operating-cycle",
      TRUE ~ "Other"
    )
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
    if (is.null(colnames(lpd_matrix))) {
      colnames(lpd_matrix) <- paste0("model_", seq_len(ncol(lpd_matrix)))
    }

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
      log_w <- log(pmax(w, .Machine$double.eps))
      adjusted <- sweep(lpd_matrix, 2, log_w, "+")
      sum(apply(adjusted, 1, log_sum_exp))
    }

    objective <- function(theta) {
      -mixture_objective_value(softmax(theta))
    }

    singleton_elpd <- colSums(lpd_matrix)
    best_singleton <- which.max(singleton_elpd)

    starts <- list(rep(0, ncol(lpd_matrix) - 1))
    for (j in seq_len(ncol(lpd_matrix))) {
      z <- rep(-8, ncol(lpd_matrix))
      z[j] <- 8
      starts[[length(starts) + 1]] <- z[-ncol(lpd_matrix)]
    }

    fits <- lapply(starts, function(st) {
      tryCatch(
        optim(st, objective, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12)),
        error = function(e) NULL
      )
    })
    fits <- Filter(Negate(is.null), fits)

    singleton_w <- rep(0, ncol(lpd_matrix))
    singleton_w[best_singleton] <- 1
    names(singleton_w) <- colnames(lpd_matrix)

    if (length(fits) == 0) {
      warning("Stacking optimizer failed for all starts; falling back to best singleton elpd model.")
      return(singleton_w)
    }

    vals <- vapply(fits, function(f) -f$value, numeric(1))
    best_fit <- fits[[which.max(vals)]]
    w <- softmax(best_fit$par)
    names(w) <- colnames(lpd_matrix)

    if (mixture_objective_value(w) + 1e-6 < mixture_objective_value(singleton_w)) {
      warning("Stacking optimizer returned a solution worse than the best singleton; falling back to best singleton elpd model.")
      return(singleton_w)
    }

    w
  }

  make_fold_assignment <- function(df_ep, df_rt) {
    combined <- bind_rows(
      df_ep %>% select(company, year, any_of("industry")),
      df_rt %>% select(company, year, any_of("industry"))
    ) %>% distinct(company, year, industry)
    firm_summary <- combined %>%
      group_by(company) %>%
      summarise(
        N_Obs = n(),
        Min_Year = min(year, na.rm = TRUE),
        Max_Year = max(year, na.rm = TRUE),
        Dominant_Industry = if ("industry" %in% names(pick(everything()))) {
          names(sort(table(industry), decreasing = TRUE))[1]
        } else {
          NA_character_
        },
        .groups = "drop"
      )
    set_accrual_seed("grouped_kfold_fold_assignment")
    ordered <- firm_summary %>%
      arrange(company) %>%
      mutate(Random_Order = runif(n()))

    if (kfold_stratified_groups && !all(is.na(ordered$Dominant_Industry))) {
      # True round-robin WITHIN each industry: shuffle firms of an industry, then
      # deal them fold 1,2,...,K,1,2,... so each industry with >= K firms lands in
      # every fold (and thus every training fold).
      assigned <- ordered %>%
        group_by(Dominant_Industry) %>%
        arrange(Random_Order, .by_group = TRUE) %>%
        mutate(Fold_ID = ((row_number() - 1) %% K) + 1) %>%
        ungroup()
    } else {
      assigned <- ordered %>%
        arrange(Random_Order) %>%
        mutate(Fold_ID = rep(seq_len(K), length.out = n()))
    }
    assigned %>%
      arrange(company) %>%
      select(company, Fold_ID, N_Obs, Min_Year, Max_Year, Dominant_Industry)
  }

  fold_assignment <- make_fold_assignment(df_ep, df_rt)
  if (anyDuplicated(fold_assignment$company) > 0) stop("[BLOCKER] Duplicate firm fold assignment.")
  write.csv(fold_assignment, file.path(tables_dir, "table_winsor_firm_fold_assignment.csv"), row.names = FALSE)

  # Industry-by-fold coverage report (diagnostic for the manuscript).
  firms_per_industry <- fold_assignment %>%
    filter(!is.na(Dominant_Industry)) %>%
    distinct(company, Dominant_Industry) %>%
    count(Dominant_Industry, name = "N_Firms")
  folds_covered <- fold_assignment %>%
    filter(!is.na(Dominant_Industry)) %>%
    distinct(Dominant_Industry, Fold_ID) %>%
    count(Dominant_Industry, name = "N_Folds_Present")
  industry_fold_coverage <- firms_per_industry %>%
    left_join(folds_covered, by = "Dominant_Industry") %>%
    mutate(Present_In_All_Folds = N_Folds_Present == K) %>%
    arrange(N_Firms)
  write.csv(industry_fold_coverage, file.path(tables_dir, "table_winsor_kfold_industry_fold_coverage.csv"), row.names = FALSE)

  # DATA CHECK (not a fix): grouped-by-firm K-fold cannot place an industry in every
  # training fold if that industry has fewer than K firms. This is a data-coverage
  # problem to resolve upstream (drop/merge the sparse industry, or lower K), not
  # something this fold builder should silently work around. Stop loudly.
  sparse_industries <- firms_per_industry %>% filter(N_Firms < K)
  if (nrow(sparse_industries) > 0) {
    msg <- paste(sprintf("%s (%d firms)", sparse_industries$Dominant_Industry, sparse_industries$N_Firms),
                 collapse = "; ")
    stop("[BLOCKER] Industries with fewer than K=", K, " firms cannot appear in every training fold: ",
         msg, ". Resolve this at the data level (drop or merge the sparse industry, or lower K). ",
         "See table_winsor_kfold_industry_fold_coverage.csv.")
  }

  attach_folds <- function(df, target_space) {
    out <- df %>% left_join(fold_assignment %>% select(company, Fold_ID), by = "company")
    if (any(is.na(out$Fold_ID))) stop("[BLOCKER] Missing fold assignment in ", target_space)
    split_check <- out %>% distinct(company, Fold_ID) %>% count(company) %>% filter(n > 1)
    if (nrow(split_check) > 0) stop("[BLOCKER] Firm split across folds in ", target_space)
    out
  }

  df_ep <- attach_folds(df_ep, "ex_post")
  df_rt <- attach_folds(df_rt, "real_time")

  fold_balance_one <- function(df, target_space) {
    df %>%
      group_by(Fold_ID, company) %>%
      summarise(
        N_Obs_Firm = n(),
        Min_Year_Firm = min(year),
        Max_Year_Firm = max(year),
        Year_Distribution_Firm = paste(names(table(year)), as.integer(table(year)), sep = ":", collapse = ";"),
        Industry_Firm = if ("industry" %in% names(pick(everything()))) names(sort(table(industry), decreasing = TRUE))[1] else NA_character_,
        .groups = "drop"
      ) %>%
      group_by(Fold_ID) %>%
      summarise(
        Target_Space = target_space,
        N_Firms = n(),
        N_Obs = sum(N_Obs_Firm),
        Min_Obs_Per_Firm = min(N_Obs_Firm),
        Median_Obs_Per_Firm = median(N_Obs_Firm),
        Max_Obs_Per_Firm = max(N_Obs_Firm),
        Min_Year = min(Min_Year_Firm),
        Max_Year = max(Max_Year_Firm),
        Year_Distribution = paste(Year_Distribution_Firm, collapse = "|"),
        Industry_Distribution = paste(names(table(Industry_Firm)), as.integer(table(Industry_Firm)), sep = ":", collapse = ";"),
        Stratified_Grouped_KFold = kfold_stratified_groups,
        Repeated_Grouped_KFold_Repeats = kfold_repeats,
        .groups = "drop"
      ) %>%
      select(Target_Space, Fold_ID, N_Firms, N_Obs, Min_Obs_Per_Firm, Median_Obs_Per_Firm, Max_Obs_Per_Firm, Min_Year, Max_Year,
             Year_Distribution, Industry_Distribution, Stratified_Grouped_KFold, Repeated_Grouped_KFold_Repeats)
  }
  fold_balance <- bind_rows(fold_balance_one(df_ep, "ex_post"), fold_balance_one(df_rt, "real_time"))
  write.csv(fold_balance, file.path(tables_dir, "table_winsor_kfold_balance.csv"), row.names = FALSE)

  get_model_rows <- function(target_space, model_ids) {
    formulas_df %>%
      filter(Target_Space == target_space,
             Model_ID %in% model_ids,
             Sample_Group == "main_common",
             Main_Stack_Inclusion == TRUE) %>%
      distinct(Model_ID, Model_Name, Target_Space, Sample_Group, Heterogeneity_Variant, Target_Sample, brms_Formula) %>%
      arrange(Model_ID, Heterogeneity_Variant)
  }

  rows_ep <- if (length(ex_post_ids) > 0) get_model_rows("ex_post", ex_post_ids) else data.frame()
  rows_rt <- if (length(no_lookahead_ids) > 0) get_model_rows("real_time", no_lookahead_ids) else data.frame()
  if (nrow(rows_ep) == 0 && nrow(rows_rt) == 0) stop("[BLOCKER] Missing model formula rows for selected K-fold target mode.")

  planned_task_parts <- list()
  if (nrow(rows_ep) > 0) planned_task_parts[[length(planned_task_parts) + 1]] <- rows_ep %>% tidyr::crossing(Fold_ID = seq_len(K))
  if (nrow(rows_rt) > 0) planned_task_parts[[length(planned_task_parts) + 1]] <- rows_rt %>% tidyr::crossing(Fold_ID = seq_len(K))
  planned_tasks <- bind_rows(planned_task_parts) %>%
    arrange(Target_Space, Model_ID, Heterogeneity_Variant, Fold_ID)

  if (length(target_space_filter) > 0) planned_tasks <- planned_tasks %>% filter(Target_Space %in% target_space_filter)
  if (length(model_id_filter) > 0) planned_tasks <- planned_tasks %>% filter(Model_ID %in% model_id_filter)
  if (length(fold_filter) > 0) planned_tasks <- planned_tasks %>% filter(Fold_ID %in% fold_filter)
  if (nrow(planned_tasks) == 0) stop("[BLOCKER] Task filters leave zero model-fold tasks.")

  model_key_for_task <- function(model_id, target_space, sample_group, heterogeneity_variant, fold_id) {
    paste0(model_key_sampled(model_id, target_space, sample_group, heterogeneity_variant, "_winsor"), "_fold", fold_id)
  }

  build_task_manifest <- function(tasks) {
    out <- tasks %>%
      mutate(
        Config_Tag = config_tag,
        Run_Mode = run_mode,
        K = K,
        Run_ID = run_id,
        Model_Key = mapply(model_key_for_task, Model_ID, Target_Space, Sample_Group, Heterogeneity_Variant, Fold_ID),
        Fit_Path = file.path(models_dir, paste0("fit_", Model_Key, ".rds")),
        Score_Cache_Path = file.path(cache_dir, paste0(Model_Key, "_scores.rds")),
        M02_Included_In_Main_KFold = "M02" %in% unique(Model_ID),
        Stratified_Grouped_KFold = kfold_stratified_groups,
        Repeated_Grouped_KFold_Repeats = kfold_repeats,
        Prior_Set_ID = prior_set_id,
        Likelihood_Family = likelihood_family,
        Model_Structure = model_structure,
        RNG_Context = paste0("grouped_kfold_refit_", Target_Space, "_", Model_ID, "_", Heterogeneity_Variant),
        RNG_Offset = Fold_ID,
        Canonical_Seed = accrual_base_seed(),
        Effective_Seed = mapply(
          function(target_space, model_id, heterogeneity_variant, fold_id) {
            accrual_seed_for(
              paste0("grouped_kfold_refit_", target_space, "_", model_id, "_", heterogeneity_variant),
              offset = fold_id
            )
          },
          Target_Space, Model_ID, Heterogeneity_Variant, Fold_ID
        ),
        RNG_Source = "scripts/ma00_setup.R",
        Status = "PENDING",
        Started_At = NA_character_,
        Ended_At = NA_character_,
        Runtime_Seconds = NA_real_,
        N_Train_Obs = NA_integer_,
        N_Test_Obs = NA_integer_,
        N_Train_Firms = NA_integer_,
        N_Test_Firms = NA_integer_,
        Completed = FALSE,
        Failure_Reason = NA_character_,
        Max_Rhat = NA_real_,
        Min_ESS_Bulk = NA_real_,
        Min_ESS_Tail = NA_real_,
        ESS_Warning = NA,
        Divergences = NA_integer_,
        Treedepth_Warnings = NA_integer_,
        Prediction_Rule = "grouped_firm_log_lik_re_formula_NA_population_level",
        New_Firm_Predictive_Tail_Verified = FALSE
      ) %>%
      select(Target_Space, Sample_Group, Fold_ID, Model_ID, Model_Name, Heterogeneity_Variant,
             Config_Tag, Run_Mode, K, Run_ID, Model_Key, Fit_Path, Score_Cache_Path,
             Status, Started_At, Ended_At, Runtime_Seconds, N_Train_Obs, N_Test_Obs,
             N_Train_Firms, N_Test_Firms, Completed, Failure_Reason, Max_Rhat,
             Min_ESS_Bulk, Min_ESS_Tail, ESS_Warning, Divergences, Treedepth_Warnings,
             Prediction_Rule, New_Firm_Predictive_Tail_Verified, brms_Formula, Target_Sample)
    out
  }

  manifest_path <- file.path(tables_dir, "table_winsor_kfold_model_fold_manifest.csv")
  task_manifest <- build_task_manifest(planned_tasks)
  if (file.exists(manifest_path) && force_resume) {
    old <- read.csv(manifest_path, stringsAsFactors = FALSE)
    join_cols <- c("Target_Space", "Sample_Group", "Fold_ID", "Model_ID", "Heterogeneity_Variant", "Model_Key")
    task_manifest <- task_manifest %>%
      left_join(old %>% select(any_of(c(join_cols, "Status", "Started_At", "Ended_At", "Runtime_Seconds", "N_Train_Obs",
                                        "N_Test_Obs", "N_Train_Firms", "N_Test_Firms", "Completed", "Failure_Reason",
                                        "Max_Rhat", "Min_ESS_Bulk", "Min_ESS_Tail", "ESS_Warning",
                                        "Divergences", "Treedepth_Warnings"))) %>%
                  rename_with(~ paste0(.x, "_old"), -all_of(join_cols)),
                by = join_cols) %>%
      mutate(
        Status = coalesce(Status_old, Status),
        Started_At = coalesce(Started_At_old, Started_At),
        Ended_At = coalesce(Ended_At_old, Ended_At),
        Runtime_Seconds = coalesce(Runtime_Seconds_old, Runtime_Seconds),
        N_Train_Obs = coalesce(N_Train_Obs_old, N_Train_Obs),
        N_Test_Obs = coalesce(N_Test_Obs_old, N_Test_Obs),
        N_Train_Firms = coalesce(N_Train_Firms_old, N_Train_Firms),
        N_Test_Firms = coalesce(N_Test_Firms_old, N_Test_Firms),
        Completed = coalesce(Completed_old, Completed),
        Failure_Reason = coalesce(Failure_Reason_old, Failure_Reason),
        Max_Rhat = coalesce(Max_Rhat_old, Max_Rhat),
        Min_ESS_Bulk = coalesce(Min_ESS_Bulk_old, Min_ESS_Bulk),
        Min_ESS_Tail = coalesce(Min_ESS_Tail_old, Min_ESS_Tail),
        ESS_Warning = coalesce(ESS_Warning_old, ESS_Warning),
        Divergences = coalesce(Divergences_old, Divergences),
        Treedepth_Warnings = coalesce(Treedepth_Warnings_old, Treedepth_Warnings)
      ) %>%
      select(names(build_task_manifest(planned_tasks)))
  }
  write.csv(task_manifest, manifest_path, row.names = FALSE)

  update_manifest_row <- function(model_key, values) {
    mf <- read.csv(manifest_path, stringsAsFactors = FALSE)
    idx <- which(mf$Model_Key == model_key)
    if (length(idx) == 1) {
      for (nm in names(values)) mf[idx, nm] <- values[[nm]]
      write.csv(mf, manifest_path, row.names = FALSE)
    }
  }

  cache_meta_matches <- function(cache_meta, expected_meta) {
    needed <- names(expected_meta)
    if (is.null(cache_meta) || !all(needed %in% names(cache_meta))) return(FALSE)
    all(vapply(needed, function(nm) identical(as.character(cache_meta[[nm]]), as.character(expected_meta[[nm]])), logical(1)))
  }

  standardize_fold_data <- function(train_df, test_df) {
    audit <- data.frame(Variable = character(), Train_Mean = double(), Train_SD = double(), Used_Fallback_Zero = logical())
    for (v in pred_vars) {
      if (v %in% names(train_df)) {
        m <- mean(train_df[[v]], na.rm = TRUE)
        s <- sd(train_df[[v]], na.rm = TRUE)
        fallback <- is.na(s) || s <= 0
        train_df[[paste0(v, "_std")]] <- if (!fallback) (train_df[[v]] - m) / s else 0
        test_df[[paste0(v, "_std")]] <- if (!fallback) (test_df[[v]] - m) / s else 0
        audit <- rbind(audit, data.frame(Variable = v, Train_Mean = m, Train_SD = s, Used_Fallback_Zero = fallback))
      }
    }
    list(train = train_df, test = test_df, audit = audit)
  }

  prepare_factor_levels <- function(train_df, test_df) {
    unseen_industry <- setdiff(unique(test_df$industry), unique(train_df$industry))
    unseen_year <- setdiff(unique(test_df$year), unique(train_df$year))
    if (length(unseen_industry) > 0 || length(unseen_year) > 0) {
      return(list(ok = FALSE, train = train_df, test = test_df,
                  note = paste0("Unseen test factor levels. industry=", paste(unseen_industry, collapse = "|"),
                                "; year=", paste(unseen_year, collapse = "|"))))
    }
    train_df$industry_f <- factor(train_df$industry)
    test_df$industry_f <- factor(test_df$industry, levels = levels(train_df$industry_f))
    train_df$year_f <- factor(train_df$year)
    test_df$year_f <- factor(test_df$year, levels = levels(train_df$year_f))
    list(ok = TRUE, train = train_df, test = test_df, note = NA_character_)
  }

  fit_prior_list <- function(heterogeneity_variant) {
    default_prior_list(heterogeneity_variant, model_structure = model_structure)
  }

  extract_fit_diagnostics <- function(fit) {
    s <- summary(fit)
    rhats <- s$fixed[, "Rhat"]
    if ("random" %in% names(s) && !is.null(s$random)) {
      for (g in names(s$random)) rhats <- c(rhats, s$random[[g]][, "Rhat"])
    }
    ess_bulk <- numeric()
    ess_tail <- numeric()
    if ("fixed" %in% names(s) && !is.null(s$fixed)) {
      if ("Bulk_ESS" %in% colnames(s$fixed)) ess_bulk <- c(ess_bulk, s$fixed[, "Bulk_ESS"])
      if ("Tail_ESS" %in% colnames(s$fixed)) ess_tail <- c(ess_tail, s$fixed[, "Tail_ESS"])
    }
    if ("random" %in% names(s) && !is.null(s$random)) {
      for (g in names(s$random)) {
        if ("Bulk_ESS" %in% colnames(s$random[[g]])) ess_bulk <- c(ess_bulk, s$random[[g]][, "Bulk_ESS"])
        if ("Tail_ESS" %in% colnames(s$random[[g]])) ess_tail <- c(ess_tail, s$random[[g]][, "Tail_ESS"])
      }
    }
    np <- nuts_params(fit)
    treedepths <- subset(np, Parameter == "treedepth__")$Value
    min_ess_bulk <- if (length(ess_bulk) > 0) suppressWarnings(min(ess_bulk, na.rm = TRUE)) else NA_real_
    min_ess_tail <- if (length(ess_tail) > 0) suppressWarnings(min(ess_tail, na.rm = TRUE)) else NA_real_
    list(
      max_rhat = suppressWarnings(max(rhats, na.rm = TRUE)),
      min_ess_bulk = ifelse(is.infinite(min_ess_bulk), NA_real_, min_ess_bulk),
      min_ess_tail = ifelse(is.infinite(min_ess_tail), NA_real_, min_ess_tail),
      ess_warning = !is.na(min_ess_bulk) && !is.na(min_ess_tail) && (min_ess_bulk < 400 || min_ess_tail < 400),
      divergences = sum(subset(np, Parameter == "divergent__")$Value),
      treedepth_warnings = sum(treedepths >= max_treedepth)
    )
  }

  score_task <- function(task, task_index, total_tasks) {
    sample_df <- if (task$Target_Space == "ex_post") df_ep else df_rt
    fold_id <- task$Fold_ID
    train_df <- sample_df %>% filter(Fold_ID != fold_id)
    test_df <- sample_df %>% filter(Fold_ID == fold_id)
    model_key <- task$Model_Key
    fit_path <- task$Fit_Path
    score_cache_path <- task$Score_Cache_Path

    assert_safe_path(kfold_run_root, config_tag, "kfold_run_root")
    if (!path_starts_with(fit_path, models_dir)) stop("[BLOCKER] Fit path is outside models_dir: ", fit_path)
    if (!path_starts_with(score_cache_path, cache_dir)) stop("[BLOCKER] Cache path is outside cache_dir: ", score_cache_path)
    baseline_models_root <- normalizePath(file.path(baseline_root, "models"), winslash = "/", mustWork = FALSE)
    baseline_draws_root <- normalizePath(file.path(baseline_root, "draws"), winslash = "/", mustWork = FALSE)
    fit_path_norm <- normalizePath(fit_path, winslash = "/", mustWork = FALSE)
    if (startsWith(fit_path_norm, baseline_models_root) || startsWith(fit_path_norm, baseline_draws_root)) {
      stop("[BLOCKER] Non-winsorized model/draw path detected.")
    }

    formula_str <- fix_formula(task$brms_Formula, prefactor = TRUE)
    train_firm_hash <- stable_hash(train_df$company)
    test_firm_hash <- stable_hash(test_df$company)
    expected_meta <- list(
      script_version = script_version,
      config_tag = config_tag,
      run_mode = run_mode,
      run_id = run_id,
      K = K,
      target_space = task$Target_Space,
      sample_group = task$Sample_Group,
      fold_id = fold_id,
      model_id = task$Model_ID,
      model_name = task$Model_Name,
      heterogeneity_variant = task$Heterogeneity_Variant,
      formula = task$brms_Formula,
      train_firm_hash = train_firm_hash,
      test_firm_hash = test_firm_hash,
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = accrual_seed_for(
        paste0("grouped_kfold_expected_meta_", task$Target_Space, "_", task$Model_ID),
        offset = fold_id
      )
    )

    if (file.exists(score_cache_path)) {
      cached <- tryCatch(readRDS(score_cache_path), error = function(e) NULL)
      cached_failed <- !is.null(cached) && !is.null(cached$fold_diag) &&
        isFALSE(as.logical(cached$fold_diag$Completed))
      # On resume, reuse cached COMPLETED folds (fast resume) but RE-RUN cached
      # FAILED folds, since a previous failure may have been transient or caused by
      # an issue fixed since. Completed cache is only reused when metadata matches.
      reuse_cache <- !is.null(cached) && cache_meta_matches(cached$cache_meta, expected_meta) &&
        !(force_resume && cached_failed)
      if (reuse_cache) {
        status_label <- "SKIPPED_CACHE_VALID"
        heartbeat(task$Target_Space, task$Model_ID, task$Heterogeneity_Variant, fold_id, status_label, "cache=HIT")
        d <- cached$fold_diag
        update_manifest_row(model_key, list(
          Status = status_label,
          Started_At = d$Started_At,
          Ended_At = d$Ended_At,
          Runtime_Seconds = d$Runtime_Seconds,
          N_Train_Obs = d$N_Train_Obs,
          N_Test_Obs = d$N_Test_Obs,
          N_Train_Firms = d$N_Train_Firms,
          N_Test_Firms = d$N_Test_Firms,
          Completed = d$Completed,
          Failure_Reason = d$Failure_Reason,
          Max_Rhat = d$Max_Rhat,
          Min_ESS_Bulk = d$Min_ESS_Bulk,
          Min_ESS_Tail = d$Min_ESS_Tail,
          ESS_Warning = d$ESS_Warning,
          Divergences = d$Divergences,
          Treedepth_Warnings = d$Treedepth_Warnings
        ))
        return(cached)
      }
      if (force_resume && cached_failed) {
        heartbeat(task$Target_Space, task$Model_ID, task$Heterogeneity_Variant, fold_id, "RETRY_PREVIOUSLY_FAILED", "cache=STALE_FAILED")
        if (file.exists(fit_path)) tryCatch(unlink(fit_path), error = function(e) NULL)
      } else {
        heartbeat(task$Target_Space, task$Model_ID, task$Heterogeneity_Variant, fold_id, "CACHE_INVALID_METADATA_MISMATCH", "cache=MISS")
      }
    }

    task_start <- Sys.time()
    heartbeat(task$Target_Space, task$Model_ID, task$Heterogeneity_Variant, fold_id, "STARTED", "cache=MISS")
    update_manifest_row(model_key, list(
      Status = "RUNNING",
      Started_At = format_time(task_start),
      N_Train_Obs = nrow(train_df),
      N_Test_Obs = nrow(test_df),
      N_Train_Firms = length(unique(train_df$company)),
      N_Test_Firms = length(unique(test_df$company))
    ))

    base_diag <- data.frame(
      Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group,
      Fold_ID = fold_id,
      Model_ID = task$Model_ID,
      Model_Name = task$Model_Name,
      Heterogeneity_Variant = task$Heterogeneity_Variant,
      Config_Tag = config_tag,
      Run_Mode = run_mode,
      K = K,
      Run_ID = run_id,
      Model_Key = model_key,
      Fit_Path = fit_path,
      Score_Cache_Path = score_cache_path,
      Started_At = format_time(task_start),
      Ended_At = NA_character_,
      Runtime_Seconds = NA_real_,
      N_Train_Obs = nrow(train_df),
      N_Test_Obs = nrow(test_df),
      N_Train_Firms = length(unique(train_df$company)),
      N_Test_Firms = length(unique(test_df$company)),
      Completed = FALSE,
      Failure_Reason = NA_character_,
      Factor_Level_Note = NA_character_,
      Max_Rhat = NA_real_,
      Min_ESS_Bulk = NA_real_,
      Min_ESS_Tail = NA_real_,
      ESS_Warning = NA,
      Divergences = NA_integer_,
      Treedepth_Warnings = NA_integer_,
      Prediction_Rule = "grouped_firm_log_lik_re_formula_NA_population_level",
      New_Firm_Predictive_Tail_Verified = FALSE,
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      Output_Root = output_root,
      stringsAsFactors = FALSE
    )

    finish_failure <- function(reason, audit = data.frame()) {
      end <- Sys.time()
      base_diag$Ended_At <- format_time(end)
      base_diag$Runtime_Seconds <- as.numeric(difftime(end, task_start, units = "secs"))
      base_diag$Failure_Reason <- reason
      result <- list(cache_meta = expected_meta, fold_diag = base_diag, obs_scores = data.frame(), standardization_audit = audit)
      saveRDS(result, score_cache_path)
      update_manifest_row(model_key, list(
        Status = "FAILED",
        Ended_At = base_diag$Ended_At,
        Runtime_Seconds = base_diag$Runtime_Seconds,
        Completed = FALSE,
        Failure_Reason = reason,
        Max_Rhat = base_diag$Max_Rhat,
        Min_ESS_Bulk = base_diag$Min_ESS_Bulk,
        Min_ESS_Tail = base_diag$Min_ESS_Tail,
        ESS_Warning = base_diag$ESS_Warning,
        Divergences = base_diag$Divergences,
        Treedepth_Warnings = base_diag$Treedepth_Warnings
      ))
      heartbeat(task$Target_Space, task$Model_ID, task$Heterogeneity_Variant, fold_id, "FAILED",
                sprintf("runtime_sec=%.1f reason=%s", base_diag$Runtime_Seconds, reason))
      result
    }

    if (nrow(train_df) == 0 || nrow(test_df) == 0) return(finish_failure("Empty train or test split."))

    std <- standardize_fold_data(train_df, test_df)
    train_df <- std$train
    test_df <- std$test
    standardization_audit <- std$audit %>%
      mutate(Target_Space = task$Target_Space, Fold_ID = fold_id, Model_ID = task$Model_ID,
             Heterogeneity_Variant = task$Heterogeneity_Variant, Config_Tag = config_tag,
             Run_Mode = run_mode, K = K, Run_ID = run_id) %>%
      select(Target_Space, Fold_ID, Model_ID, Heterogeneity_Variant, Config_Tag, Run_Mode, K, Run_ID,
             Variable, Train_Mean, Train_SD, Used_Fallback_Zero)

    factor_prep <- prepare_factor_levels(train_df, test_df)
    if (!factor_prep$ok) {
      base_diag$Factor_Level_Note <- factor_prep$note
      return(finish_failure("Unseen test factor levels under training-only factor preparation.", standardization_audit))
    }
    train_df <- factor_prep$train
    test_df <- factor_prep$test

    fit <- NULL
    if (file.exists(fit_path)) {
      fit <- tryCatch(readRDS(fit_path), error = function(e) NULL)
    }
    if (is.null(fit)) {
      message(
        "brms/rstan sampler controls: chains=", chains,
        ", cores=", kfold_chain_cores,
        ", iter=", iter,
        ", warmup=", warmup,
        ", adapt_delta=", adapt_delta,
        ", max_treedepth=", max_treedepth
      )
      fit <- tryCatch({
        brm(
          formula = bf(as.formula(formula_str)),
          data = train_df,
          family = brms_family(),
          prior = fit_prior_list(task$Heterogeneity_Variant),
          chains = chains,
          cores = kfold_chain_cores,
          iter = iter,
          warmup = warmup,
          control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
          seed = accrual_seed_for(
            paste0("grouped_kfold_refit_", task$Target_Space, "_", task$Model_ID, "_", task$Heterogeneity_Variant),
            offset = fold_id
          ),
          refresh = 500
        )
      }, error = function(e) e)
      if (inherits(fit, "error")) return(finish_failure(paste("brms fit failed:", fit$message), standardization_audit))
      saveRDS(fit, fit_path)
    }

    fit_diag <- tryCatch(extract_fit_diagnostics(fit), error = function(e) {
      list(
        max_rhat = NA_real_,
        min_ess_bulk = NA_real_,
        min_ess_tail = NA_real_,
        ess_warning = NA,
        divergences = NA_integer_,
        treedepth_warnings = NA_integer_
      )
    })
    base_diag$Max_Rhat <- fit_diag$max_rhat
    base_diag$Min_ESS_Bulk <- fit_diag$min_ess_bulk
    base_diag$Min_ESS_Tail <- fit_diag$min_ess_tail
    base_diag$ESS_Warning <- fit_diag$ess_warning
    base_diag$Divergences <- fit_diag$divergences
    base_diag$Treedepth_Warnings <- fit_diag$treedepth_warnings

    ll_draws <- tryCatch({
      brms::log_lik(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE)
    }, error = function(e) e)
    if (inherits(ll_draws, "error")) return(finish_failure(paste("held-out log_lik failed:", ll_draws$message), standardization_audit))
    if (ncol(ll_draws) != nrow(test_df)) {
      return(finish_failure(sprintf("held-out log_lik N mismatch: ncol=%d test_n=%d", ncol(ll_draws), nrow(test_df)), standardization_audit))
    }

    epred_draws <- tryCatch(posterior_epred(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE), error = function(e) NULL)
    pred_draws <- tryCatch(posterior_predict(fit, newdata = test_df, re_formula = NA, allow_new_levels = TRUE), error = function(e) NULL)

    lpd_obs <- apply(ll_draws, 2, log_mean_exp)
    pred_mean <- if (!is.null(epred_draws)) colMeans(epred_draws) else rep(NA_real_, nrow(test_df))
    pred_sd <- if (!is.null(pred_draws)) apply(pred_draws, 2, sd) else rep(NA_real_, nrow(test_df))

    obs_scores <- data.frame(
      Target_Space = task$Target_Space,
      Sample_Group = task$Sample_Group,
      Fold_ID = fold_id,
      Obs_ID = test_df$Obs_ID,
      company = test_df$company,
      year = test_df$year,
      Model_ID = task$Model_ID,
      Model_Name = task$Model_Name,
      Heterogeneity_Variant = task$Heterogeneity_Variant,
      Config_Tag = config_tag,
      Run_Mode = run_mode,
      K = K,
      Run_ID = run_id,
      lpd_obs = lpd_obs,
      y_actual = test_df$TA_scaled,
      pred_mean = pred_mean,
      pred_sd = pred_sd,
      abs_error = abs(test_df$TA_scaled - pred_mean),
      squared_error = (test_df$TA_scaled - pred_mean)^2,
      Prediction_Rule = "grouped_firm_log_lik_re_formula_NA_population_level",
      New_Firm_Predictive_Tail_Verified = FALSE,
      Prior_Set_ID = prior_set_id,
      Likelihood_Family = likelihood_family,
      Model_Structure = model_structure,
      Output_Root = output_root,
      stringsAsFactors = FALSE
    )

    task_end <- Sys.time()
    base_diag$Ended_At <- format_time(task_end)
    base_diag$Runtime_Seconds <- as.numeric(difftime(task_end, task_start, units = "secs"))
    base_diag$Completed <- TRUE

    result <- list(cache_meta = expected_meta, fold_diag = base_diag, obs_scores = obs_scores, standardization_audit = standardization_audit)
    saveRDS(result, score_cache_path)
    update_manifest_row(model_key, list(
      Status = "COMPLETED",
      Ended_At = base_diag$Ended_At,
      Runtime_Seconds = base_diag$Runtime_Seconds,
      Completed = TRUE,
      Failure_Reason = NA_character_,
      Max_Rhat = base_diag$Max_Rhat,
      Min_ESS_Bulk = base_diag$Min_ESS_Bulk,
      Min_ESS_Tail = base_diag$Min_ESS_Tail,
      ESS_Warning = base_diag$ESS_Warning,
      Divergences = base_diag$Divergences,
      Treedepth_Warnings = base_diag$Treedepth_Warnings
    ))
    heartbeat(task$Target_Space, task$Model_ID, task$Heterogeneity_Variant, fold_id, "COMPLETED",
              sprintf("runtime_sec=%.1f", base_diag$Runtime_Seconds))

    mf <- read.csv(manifest_path, stringsAsFactors = FALSE)
    completed_now <- sum(mf$Completed, na.rm = TRUE)
    failed_now <- sum(mf$Status == "FAILED", na.rm = TRUE)
    remaining <- nrow(mf) - completed_now - failed_now
    elapsed <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
    avg <- if (completed_now > 0) elapsed / completed_now else NA_real_
    eta <- if (!is.na(avg)) avg * remaining else NA_real_
    cat(sprintf("Progress: %d/%d completed, %d failed, %d remaining, elapsed %.1fs, avg %.1fs/task, ETA %.1fs\n",
                completed_now, nrow(mf), failed_now, remaining, elapsed, avg, eta))
    result
  }

  write_run_manifest("RUNNING", exp_n = nrow(df_ep), exp_firms = length(unique(df_ep$company)),
                     rt_n = nrow(df_rt), rt_firms = length(unique(df_rt$company)))

  expected_refits <- nrow(task_manifest)
  ex_tasks <- sum(task_manifest$Target_Space == "ex_post")
  rt_tasks <- sum(task_manifest$Target_Space == "real_time")

  if (preflight_only) {
    final_decision <- "PREFLIGHT_ONLY_NO_FINAL_DECISION"
    decision <- data.frame(
      Criterion = c("Was exact grouped K-fold by firm implemented?", "Preflight mode completed", "Final decision"),
      Evidence = c("Preflight validated folders, inputs, folds, model formulas, and task manifest without calling brm().",
                   sprintf("Expected refits=%d; ex-post tasks=%d; no-look-ahead tasks=%d.", expected_refits, ex_tasks, rt_tasks),
                   final_decision),
      Decision = c("Preflight only", "Yes", final_decision),
      Severity = c("High", "High", "High"),
      Manuscript_Action = c("Run smoke test and full run before substantive interpretation.", "No manuscript decision from preflight.", "No final manuscript decision.")
    )
    write.csv(decision, file.path(tables_dir, "table_reviewer_priority2b_exact_kfold_decision.csv"), row.names = FALSE)
    writeLines(c(
      "Reviewer Priority 2b exact grouped K-fold response notes",
      "Preflight only: no brms models were fit.",
      paste("Expected output root:", kfold_run_root),
      paste("Expected model-fold refits:", expected_refits)
    ), file.path(logs_dir, "reviewer_priority2b_exact_kfold_response_notes.txt"))
    writeLines(c(
      "ma12 exact grouped K-fold by firm technical log",
      paste("Script:", script_name),
      paste("Start_Time:", format_time(script_start_time)),
      paste("End_Time:", format_time(Sys.time())),
      "Status: PREFLIGHT_ONLY_COMPLETED",
      paste("K:", K),
      paste("Run_Mode:", run_mode),
      paste("Run_ID:", run_id),
      paste("Config_Tag:", config_tag),
      paste("Expected refits:", expected_refits),
      paste("Ex-post tasks:", ex_tasks),
      paste("No-look-ahead tasks:", rt_tasks)
    ), file.path(logs_dir, "phase4e_exact_grouped_kfold_winsor_notes.txt"))
    safe_shutdown("PREFLIGHT_ONLY_COMPLETED", nrow(df_ep), length(unique(df_ep$company)), nrow(df_rt), length(unique(df_rt$company)), remove_lock = TRUE)
    cat("\n===== PREFLIGHT ONLY COMPLETE =====\n")
    cat("Expected output root: ", kfold_run_root, "\n", sep = "")
    cat("Expected model-fold refits: ", expected_refits, "\n", sep = "")
    cat("Ex-post tasks: ", ex_tasks, "\n", sep = "")
    cat("No-look-ahead tasks: ", rt_tasks, "\n", sep = "")
    cat("No brms models were fit.\n\n")
    cat("Smoke test command:\n")
    cat("$env:ACCRUAL_KFOLD_FIRM_MODE='FAST_MODE'\n$env:ACCRUAL_KFOLD_FIRM_K='2'\n$env:ACCRUAL_KFOLD_FIRM_RUN_ID='smoke_k2_fast'\n$env:ACCRUAL_KFOLD_TARGET_SPACE='ex_post'\n$env:ACCRUAL_KFOLD_MODEL_IDS='M01,M07'\n$env:ACCRUAL_KFOLD_FOLDS='1'\n$env:ACCRUAL_KFOLD_FIRM_PREFLIGHT_ONLY='FALSE'\n& 'C:\\Program Files\\R\\R-4.4.3\\bin\\Rscript.exe' scripts\\13_grouped_kfold_firm.R\n\n")
    cat("Main run command:\n")
    cat("Remove-Item Env:\\ACCRUAL_KFOLD_TARGET_SPACE -ErrorAction SilentlyContinue\nRemove-Item Env:\\ACCRUAL_KFOLD_MODEL_IDS -ErrorAction SilentlyContinue\nRemove-Item Env:\\ACCRUAL_KFOLD_FOLDS -ErrorAction SilentlyContinue\n$env:ACCRUAL_KFOLD_FIRM_MODE='FULL_MODE'\n$env:ACCRUAL_KFOLD_FIRM_K='5'\n$env:ACCRUAL_KFOLD_FIRM_RUN_ID='main_k5_full'\n$env:ACCRUAL_KFOLD_FIRM_PREFLIGHT_ONLY='FALSE'\n& 'C:\\Program Files\\R\\R-4.4.3\\bin\\Rscript.exe' scripts\\13_grouped_kfold_firm.R\n\n")
    cat("Resume command:\n")
    cat("$env:ACCRUAL_KFOLD_FIRM_FORCE_RESUME='TRUE'\n$env:ACCRUAL_KFOLD_FIRM_MODE='FULL_MODE'\n$env:ACCRUAL_KFOLD_FIRM_K='5'\n$env:ACCRUAL_KFOLD_FIRM_RUN_ID='main_k5_full'\n& 'C:\\Program Files\\R\\R-4.4.3\\bin\\Rscript.exe' scripts\\13_grouped_kfold_firm.R\n")
    return(invisible(final_decision))
  }

  results <- vector("list", nrow(task_manifest))
  for (i in seq_len(nrow(task_manifest))) {
    results[[i]] <- score_task(task_manifest[i, ], i, nrow(task_manifest))
  }

  fold_diagnostics <- bind_rows(lapply(results, `[[`, "fold_diag"))
  obs_scores <- bind_rows(lapply(results, `[[`, "obs_scores"))
  standardization_audit <- bind_rows(lapply(results, `[[`, "standardization_audit"))

  write.csv(fold_diagnostics, file.path(tables_dir, "table_winsor_kfold_refit_diagnostics.csv"), row.names = FALSE)
  write.csv(standardization_audit, file.path(tables_dir, "table_winsor_kfold_train_standardization_audit.csv"), row.names = FALSE)
  write.csv(obs_scores, file.path(tables_dir, "table_winsor_kfold_observation_scores.csv"), row.names = FALSE)

  fold_scores <- if (nrow(obs_scores) > 0) {
    obs_scores %>%
      group_by(Target_Space, Sample_Group, Fold_ID, Model_ID, Model_Name, Heterogeneity_Variant) %>%
      summarise(
        N_Test_Obs = n(),
        N_Test_Firms = n_distinct(company),
        elpd_fold = sum(lpd_obs, na.rm = TRUE),
        mean_lpd_obs = mean(lpd_obs, na.rm = TRUE),
        RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
        MAE = mean(abs_error, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    data.frame()
  }
  write.csv(fold_scores, file.path(tables_dir, "table_winsor_kfold_fold_scores.csv"), row.names = FALSE)

  model_scores <- fold_diagnostics %>%
    group_by(Target_Space, Sample_Group, Model_ID, Model_Name, Heterogeneity_Variant) %>%
    summarise(
      N_Folds_Attempted = n(),
      N_Folds_Completed = sum(Completed, na.rm = TRUE),
      max_rhat_max = suppressWarnings(max(Max_Rhat, na.rm = TRUE)),
      min_ess_bulk = suppressWarnings(min(Min_ESS_Bulk, na.rm = TRUE)),
      min_ess_tail = suppressWarnings(min(Min_ESS_Tail, na.rm = TRUE)),
      ess_warning_any = any(isTRUE(ESS_Warning) | (is.logical(ESS_Warning) & !is.na(ESS_Warning) & ESS_Warning), na.rm = TRUE),
      divergences_total = sum(Divergences, na.rm = TRUE),
      treedepth_warnings_total = sum(Treedepth_Warnings, na.rm = TRUE),
      Runtime_Seconds = sum(Runtime_Seconds, na.rm = TRUE),
      exclusion_reason = paste(na.omit(unique(Failure_Reason)), collapse = " | "),
      .groups = "drop"
    ) %>%
    left_join(
      fold_scores %>%
        group_by(Target_Space, Sample_Group, Model_ID, Model_Name, Heterogeneity_Variant) %>%
        summarise(
          N_Test_Obs_Total = sum(N_Test_Obs),
          N_Test_Firms_Total = sum(N_Test_Firms),
          elpd_kfold = sum(elpd_fold),
          mean_lpd_obs = weighted.mean(mean_lpd_obs, N_Test_Obs),
          se_elpd_fold = sd(elpd_fold),
          RMSE = sqrt(weighted.mean(RMSE^2, N_Test_Obs)),
          MAE = weighted.mean(MAE, N_Test_Obs),
          .groups = "drop"
        ),
      by = c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant")
    ) %>%
    mutate(
      max_rhat_max = ifelse(is.infinite(max_rhat_max), NA_real_, max_rhat_max),
      min_ess_bulk = ifelse(is.infinite(min_ess_bulk), NA_real_, min_ess_bulk),
      min_ess_tail = ifelse(is.infinite(min_ess_tail), NA_real_, min_ess_tail),
      reliability_flag = case_when(
        N_Folds_Completed == 0 ~ "FAILED",
        !partial_run & N_Folds_Completed < K ~ "LOW_RELIABILITY",
        divergences_total > 0 | treedepth_warnings_total > 0 ~ "LOW_RELIABILITY",
        !is.na(max_rhat_max) & !is.na(min_ess_bulk) & !is.na(min_ess_tail) &
          max_rhat_max <= 1.01 & min_ess_bulk >= 400 & min_ess_tail >= 400 ~ "OK",
        !is.na(max_rhat_max) & !is.na(min_ess_bulk) & !is.na(min_ess_tail) &
          max_rhat_max <= 1.05 & min_ess_bulk >= 100 & min_ess_tail >= 100 ~ "CAUTION",
        TRUE ~ "LOW_RELIABILITY"
      ),
      included_in_stack = reliability_flag %in% c("OK", "CAUTION") &
        ifelse(partial_run, N_Folds_Completed > 0, N_Folds_Completed == K),
      exclusion_reason = ifelse(included_in_stack, NA_character_, exclusion_reason)
    )
  write.csv(model_scores, file.path(tables_dir, "table_winsor_kfold_model_scores.csv"), row.names = FALSE)

  build_kfold_weights <- function(target_space) {
    included <- model_scores %>%
      filter(Target_Space == target_space, Sample_Group == "main_common", included_in_stack == TRUE) %>%
      arrange(Model_ID, Heterogeneity_Variant)

    if (nrow(included) == 0 || nrow(obs_scores) == 0) return(data.frame())

    score_list <- list()
    expected_n <- if (partial_run) {
      obs_scores %>% filter(Target_Space == target_space) %>% distinct(Obs_ID) %>% nrow()
    } else {
      ifelse(target_space == "ex_post", nrow(df_ep), nrow(df_rt))
    }

    for (i in seq_len(nrow(included))) {
      row <- included[i, ]
      key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_kfold")
      one <- obs_scores %>%
        filter(
          Target_Space == target_space,
          Sample_Group == row$Sample_Group,
          Model_ID == row$Model_ID,
          Heterogeneity_Variant == row$Heterogeneity_Variant
        ) %>%
        arrange(Obs_ID)

      if (nrow(one) != expected_n) next
      score_list[[key]] <- one$lpd_obs
    }

    if (length(score_list) == 0) return(data.frame())

    lpd_matrix <- do.call(cbind, score_list)
    colnames(lpd_matrix) <- names(score_list)

    weights <- optimize_stacking_from_lpd(lpd_matrix)
    if (is.null(names(weights))) names(weights) <- colnames(lpd_matrix)
    weights <- weights[colnames(lpd_matrix)]

    if (any(is.na(weights))) {
      stop("[BLOCKER] Exact K-fold stacking weights contain NA after aligning by model key for ", target_space)
    }
    if (abs(sum(weights) - 1) > 1e-5) {
      stop("[BLOCKER] Exact K-fold stacking weights do not sum to 1 for ", target_space)
    }

    # model_key_sampled() is scalar in 00_helpers.R; vectorize it explicitly.
    included_keys <- mapply(
      model_key_sampled,
      included$Model_ID,
      included$Target_Space,
      included$Sample_Group,
      included$Heterogeneity_Variant,
      MoreArgs = list(suffix = ""),
      USE.NAMES = FALSE
    )

    stripped_weight_keys <- sub("_kfold$", "", names(weights))
    meta_idx <- match(stripped_weight_keys, included_keys)
    if (any(is.na(meta_idx))) {
      stop("[BLOCKER] Could not align exact K-fold weights to model metadata for ", target_space,
           ". Missing keys: ", paste(stripped_weight_keys[is.na(meta_idx)], collapse = ", "))
    }

    singleton_elpd <- colSums(lpd_matrix)
    best_elpd_key <- names(singleton_elpd)[which.max(singleton_elpd)]
    top_weight_key <- names(weights)[which.max(weights)]
    top_weight_not_best_singleton <- max(weights) > 0.999 && !identical(top_weight_key, best_elpd_key)

    if (top_weight_not_best_singleton) {
      warning(
        "Top exact K-fold weight is approximately 1 but is not assigned to the best singleton elpd model for ",
        target_space, ". top_weight_key=", top_weight_key,
        "; best_elpd_key=", best_elpd_key,
        ". This is a sanity warning; check optimizer diagnostics before interpretation."
      )
    }

    meta <- included[meta_idx, ]

    meta %>%
      mutate(
        Model_Key_KFold = names(weights),
        Weight_KFold = as.numeric(weights),
        Singleton_ELPD = as.numeric(singleton_elpd[names(weights)]),
        Best_Singleton_ELPD_Key = best_elpd_key,
        Top_Weight_Key = top_weight_key,
        Top_Weight_Not_Best_Singleton = top_weight_not_best_singleton
      ) %>%
      arrange(desc(Weight_KFold)) %>%
      mutate(Rank_KFold = row_number()) %>%
      mutate(M10_Included = FALSE) %>%
      select(Target_Space, Sample_Group, M10_Included, Model_ID, Model_Name, Heterogeneity_Variant,
             Model_Key_KFold, Weight_KFold, Rank_KFold, elpd_kfold, Singleton_ELPD,
             mean_lpd_obs, RMSE, MAE, reliability_flag,
             Best_Singleton_ELPD_Key, Top_Weight_Key, Top_Weight_Not_Best_Singleton)
  }

  kfold_weights_ep <- build_kfold_weights("ex_post")
  kfold_weights_rt <- build_kfold_weights("real_time")
  write.csv(kfold_weights_ep, file.path(tables_dir, "table_winsor_kfold_weights_ex_post.csv"), row.names = FALSE)
  write.csv(kfold_weights_rt, file.path(tables_dir, "table_winsor_kfold_weights_no_lookahead.csv"), row.names = FALSE)

  prepare_rowloo <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(
        Target_Space = character(),
        Model_ID = character(),
        Model_Name = character(),
        Heterogeneity_Variant = character(),
        Weight_RowLOO_Winsor = double(),
        Rank_RowLOO_Winsor = integer(),
        stringsAsFactors = FALSE
      ))
    }
    df %>%
      mutate(Rank_RowLOO_Winsor = rank(-Weight, ties.method = "first")) %>%
      select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant, Weight_RowLOO_Winsor = Weight, Rank_RowLOO_Winsor)
  }
  prepare_lofo <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(
        Target_Space = character(),
        Model_ID = character(),
        Model_Name = character(),
        Heterogeneity_Variant = character(),
        Weight_GroupedPSIS_LOFO_Winsor = double(),
        Rank_GroupedPSIS_LOFO_Winsor = integer(),
        stringsAsFactors = FALSE
      ))
    }
    df %>%
      mutate(Rank_GroupedPSIS_LOFO_Winsor = rank(-Weight_LOFO, ties.method = "first")) %>%
      select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant,
             Weight_GroupedPSIS_LOFO_Winsor = Weight_LOFO, Rank_GroupedPSIS_LOFO_Winsor)
  }
  prepare_kfold <- function(df) {
    if (nrow(df) == 0) {
      return(data.frame(
        Target_Space = character(),
        Model_ID = character(),
        Model_Name = character(),
        Heterogeneity_Variant = character(),
        Weight_ExactKFold_Winsor = double(),
        Rank_ExactKFold = integer(),
        stringsAsFactors = FALSE
      ))
    }
    df %>% select(Target_Space, Model_ID, Model_Name, Heterogeneity_Variant,
                  Weight_ExactKFold_Winsor = Weight_KFold, Rank_ExactKFold = Rank_KFold)
  }

  weight_comparison <- bind_rows(prepare_rowloo(rowloo_ep), prepare_rowloo(rowloo_rt)) %>%
    full_join(bind_rows(prepare_lofo(lofo_ep), prepare_lofo(lofo_rt)),
              by = c("Target_Space", "Model_ID", "Model_Name", "Heterogeneity_Variant")) %>%
    full_join(bind_rows(prepare_kfold(kfold_weights_ep), prepare_kfold(kfold_weights_rt)),
              by = c("Target_Space", "Model_ID", "Model_Name", "Heterogeneity_Variant")) %>%
    mutate(
      Weight_RowLOO_Winsor = ifelse(is.na(Weight_RowLOO_Winsor), 0, Weight_RowLOO_Winsor),
      Weight_GroupedPSIS_LOFO_Winsor = ifelse(is.na(Weight_GroupedPSIS_LOFO_Winsor), 0, Weight_GroupedPSIS_LOFO_Winsor),
      Weight_ExactKFold_Winsor = ifelse(is.na(Weight_ExactKFold_Winsor), 0, Weight_ExactKFold_Winsor),
      Delta_KFold_minus_RowLOO = Weight_ExactKFold_Winsor - Weight_RowLOO_Winsor,
      Delta_KFold_minus_GroupedPSIS_LOFO = Weight_ExactKFold_Winsor - Weight_GroupedPSIS_LOFO_Winsor,
      Family = vapply(Model_ID, family_label, character(1)),
      Interpretation = case_when(
        Family == "Jones-family" & Weight_ExactKFold_Winsor >= 0.50 ~ "Jones-family becomes dominant under exact grouped K-fold.",
        Family == "Jones-family" & Weight_ExactKFold_Winsor > Weight_RowLOO_Winsor ~ "Jones-family gains exact K-fold support but family-level dominance must be checked.",
        Weight_ExactKFold_Winsor > Weight_GroupedPSIS_LOFO_Winsor ~ "Exact grouped K-fold support exceeds grouped PSIS-LOFO support.",
        TRUE ~ "Compare row-level LOO, grouped PSIS-LOFO, and exact grouped K-fold support."
      )
    ) %>%
    arrange(Target_Space, Rank_ExactKFold)
  write.csv(weight_comparison, file.path(tables_dir, "table_winsor_weight_stability_loo_lofo_kfold.csv"), row.names = FALSE)

  family_comparison <- weight_comparison %>%
    group_by(Target_Space, Family) %>%
    summarise(
      Weight_RowLOO_Winsor = sum(Weight_RowLOO_Winsor, na.rm = TRUE),
      Weight_GroupedPSIS_LOFO_Winsor = sum(Weight_GroupedPSIS_LOFO_Winsor, na.rm = TRUE),
      Weight_ExactKFold_Winsor = sum(Weight_ExactKFold_Winsor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Target_Space) %>%
    mutate(
      Family_Rank_RowLOO = rank(-Weight_RowLOO_Winsor, ties.method = "first"),
      Family_Rank_GroupedPSIS_LOFO = rank(-Weight_GroupedPSIS_LOFO_Winsor, ties.method = "first"),
      Family_Rank_ExactKFold = rank(-Weight_ExactKFold_Winsor, ties.method = "first"),
      Interpretation = case_when(
        Family == "Jones-family" & Weight_ExactKFold_Winsor >= 0.50 ~ "Jones-family dominates under exact grouped K-fold.",
        Family == "Jones-family" & Weight_ExactKFold_Winsor < 0.50 ~ "Jones-family remains below dominance threshold under exact grouped K-fold.",
        Family_Rank_ExactKFold == 1 ~ "Dominant exact grouped K-fold family.",
        TRUE ~ "Secondary exact grouped K-fold family."
      )
    ) %>%
    ungroup() %>%
    arrange(Target_Space, Family_Rank_ExactKFold)
  write.csv(family_comparison, file.path(tables_dir, "table_winsor_family_weight_stability_loo_lofo_kfold.csv"), row.names = FALSE)

  safe_family_weight <- function(space, family) {
    x <- family_comparison %>% filter(Target_Space == space, Family == family)
    if (nrow(x) == 0) return(NA_real_)
    x$Weight_ExactKFold_Winsor[1]
  }
  dominant_family <- function(space) {
    family_comparison %>% filter(Target_Space == space) %>% arrange(desc(Weight_ExactKFold_Winsor)) %>% slice(1)
  }

  attempted <- nrow(fold_diagnostics)
  completed <- sum(fold_diagnostics$Completed, na.rm = TRUE)
  failed <- attempted - completed
  failure_share <- ifelse(attempted > 0, failed / attempted, 1)
  low_rel_share <- mean(model_scores$reliability_flag %in% c("FAILED", "LOW_RELIABILITY"), na.rm = TRUE)
  dom_ep <- dominant_family("ex_post")
  dom_rt <- dominant_family("real_time")
  jones_ep <- safe_family_weight("ex_post", "Jones-family")
  jones_rt <- safe_family_weight("real_time", "Jones-family")
  cash_ep <- safe_family_weight("ex_post", "Cash-flow/McNichols-family")
  cash_rt <- safe_family_weight("real_time", "Cash-flow/McNichols-family")
  ball_ep <- safe_family_weight("ex_post", "Ball-Shivakumar/asymmetry")
  ball_rt <- safe_family_weight("real_time", "Ball-Shivakumar/asymmetry")

  ex_survives <- !partial_run && nrow(dom_ep) == 1 && dom_ep$Family %in% c("Cash-flow/McNichols-family", "Ball-Shivakumar/asymmetry") && !is.na(jones_ep) && jones_ep < 0.50
  rt_survives <- !partial_run && nrow(dom_rt) == 1 && dom_rt$Family %in% c("Cash-flow/McNichols-family", "Ball-Shivakumar/asymmetry", "No-lookahead/real-time") && !is.na(jones_rt) && jones_rt < 0.50
  jones_dominant_both <- !is.na(jones_ep) && !is.na(jones_rt) && jones_ep >= 0.50 && jones_rt >= 0.50

  if (partial_run) {
    final_decision <- "PARTIAL_RUN_NO_FINAL_DECISION"
  } else if (attempted == 0 || completed == 0 || failure_share > 0.30 || low_rel_share > 0.50 ||
             nrow(kfold_weights_ep) == 0 || nrow(kfold_weights_rt) == 0) {
    final_decision <- "INCONCLUSIVE_DUE_TO_KFOLD_FAILURES"
  } else if (jones_dominant_both || (!ex_survives && !rt_survives)) {
    final_decision <- "DOES_NOT_SURVIVE_WINSOR_AND_EXACT_GROUPED_KFOLD"
  } else if (ex_survives && rt_survives) {
    final_decision <- "SURVIVES_WINSOR_AND_EXACT_GROUPED_KFOLD"
  } else {
    final_decision <- "PARTIALLY_SURVIVES_WINSOR_AND_EXACT_GROUPED_KFOLD"
  }

  required_completed_weight_files <- c(
    file.path(tables_dir, "table_winsor_kfold_weights_ex_post.csv"),
    file.path(tables_dir, "table_winsor_kfold_weights_no_lookahead.csv")
  )
  completed_run_pin_eligible <<- !preflight_only &&
    !partial_run &&
    identical(run_mode, "FULL_MODE") &&
    K == 5L &&
    attempted > 0 &&
    completed == attempted &&
    final_decision %in% c(
      "SURVIVES_WINSOR_AND_EXACT_GROUPED_KFOLD",
      "PARTIALLY_SURVIVES_WINSOR_AND_EXACT_GROUPED_KFOLD",
      "DOES_NOT_SURVIVE_WINSOR_AND_EXACT_GROUPED_KFOLD"
    ) &&
    all(file.exists(required_completed_weight_files))

  if (completed_run_pin_eligible) {
    writeLines(kfold_run_root, latest_completed_run_path)
    completed_run_pin_updated <<- TRUE
  } else {
    completed_run_pin_updated <<- FALSE
  }

  decision_table <- data.frame(
    Criterion = c(
      "Was exact grouped K-fold by firm implemented?",
      "Were all firm observations assigned to the same fold?",
      "Were predictors standardized using training-fold moments only?",
      "Were Firm-RE held-out predictions evaluated as new-firm/population-level predictions?",
      "Did ex-post family-level headline survive exact grouped K-fold?",
      "Did no-look-ahead family-level headline survive exact grouped K-fold?",
      "Did Jones-family become dominant?",
      "Did cash-flow/McNichols-family remain important?",
      "Did Ball-Shivakumar/asymmetry remain important?",
      "Did exact K-fold resolve the grouped PSIS-LOFO Pareto-k concern?",
      "Overall Priority 2b decision.",
      "Recommended manuscript action."
    ),
    Evidence = c(
      sprintf("K=%d grouped by company; actual refits by fold attempted=%d completed=%d failed=%d.", K, attempted, completed, failed),
      "Firm fold assignment is one row per company and each sample joins folds by company.",
      "Standardization audit records train-fold means and SDs; test folds use those train-fold moments.",
      "Held-out scoring calls log_lik/prediction with re_formula = NA and allow_new_levels = TRUE.",
      ifelse(nrow(dom_ep) == 1, sprintf("Ex-post dominant exact K-fold family: %s (%.4f).", dom_ep$Family, dom_ep$Weight_ExactKFold_Winsor), "Unavailable."),
      ifelse(nrow(dom_rt) == 1, sprintf("No-look-ahead dominant exact K-fold family: %s (%.4f).", dom_rt$Family, dom_rt$Weight_ExactKFold_Winsor), "Unavailable."),
      sprintf("Jones-family exact K-fold weight: ex-post %.4f; no-look-ahead %.4f.", jones_ep, jones_rt),
      sprintf("Cash-flow/McNichols exact K-fold weight: ex-post %.4f; no-look-ahead %.4f.", cash_ep, cash_rt),
      sprintf("Ball-Shivakumar/asymmetry exact K-fold weight: ex-post %.4f; no-look-ahead %.4f.", ball_ep, ball_rt),
      "Exact K-fold uses refitting and therefore avoids PSIS Pareto-k diagnostics for the main result.",
      final_decision,
      "Use conservative wording based on family-level exact grouped K-fold evidence."
    ),
    Decision = c(
      "Yes", "Yes", "Yes", "Yes",
      ifelse(ex_survives, "Yes", "No, partial, or unavailable"),
      ifelse(rt_survives, "Yes", "No, partial, or unavailable"),
      ifelse(!is.na(jones_ep) && !is.na(jones_rt) && (jones_ep >= 0.50 || jones_rt >= 0.50), "Yes", "No"),
      ifelse(max(c(cash_ep, cash_rt), na.rm = TRUE) > 0.20, "Yes", "Weak or unavailable"),
      ifelse(max(c(ball_ep, ball_rt), na.rm = TRUE) > 0.10, "Yes", "Weak or unavailable"),
      ifelse(final_decision %in% c("INCONCLUSIVE_DUE_TO_KFOLD_FAILURES", "PARTIAL_RUN_NO_FINAL_DECISION"), "Not yet", "Yes"),
      final_decision,
      "Conservative revision"
    ),
    Severity = c("High", "High", "High", "High", "High", "High", "High", "Medium", "Medium", "High", "High", "High"),
    Manuscript_Action = c(
      "Describe as exact grouped K-fold by firm, not PSIS-LOFO.",
      "Report fold assignment and fold balance tables.",
      "State no held-out firm moments were used for predictor standardization.",
      "State Firm-RE held-out firms were evaluated as new firms.",
      "Report family-level ex-post exact K-fold weights.",
      "Report family-level no-look-ahead feature-space exact K-fold weights.",
      "Use as key reviewer response criterion.",
      "Report whether McNichols/cash-flow support remains material.",
      "Report whether asymmetry support remains material.",
      "Use exact K-fold as the cleaner panel-aware robustness check.",
      "Use final decision wording in response letter.",
      "Revise claims conservatively and report model-ranking sensitivity."
    ),
    stringsAsFactors = FALSE
  )
  decision_table$Kfold_Run_Root <- kfold_run_root
  decision_table$Completed_Run_Pin_Eligible <- completed_run_pin_eligible
  decision_table$Completed_Run_Pin_Updated <- completed_run_pin_updated
  write.csv(decision_table, file.path(tables_dir, "table_reviewer_priority2b_exact_kfold_decision.csv"), row.names = FALSE)

  manuscript_wording <- switch(
    final_decision,
    SURVIVES_WINSOR_AND_EXACT_GROUPED_KFOLD = "To address the panel dependence concern, we supplemented row-level LOO and grouped PSIS-LOFO diagnostics with exact grouped K-fold cross-validation by firm on the winsorized sample. All observations from the same firm were assigned to the same fold, and firm random-intercept models were evaluated using population-level predictions for held-out firms. The family-level ranking remains non-Jones: cash-flow/McNichols-style and asymmetric accrual models continue to receive the strongest predictive support. This suggests that the main model-uncertainty conclusion is not driven solely by row-level LOO.",
    PARTIALLY_SURVIVES_WINSOR_AND_EXACT_GROUPED_KFOLD = "Exact grouped K-fold cross-validation by firm attenuates some model-level stacking weights but preserves the broad family-level conclusion that traditional Jones-family models do not dominate. We therefore interpret the evidence as supportive but not definitive and report the model rankings as cross-validation-sensitive.",
    DOES_NOT_SURVIVE_WINSOR_AND_EXACT_GROUPED_KFOLD = "Exact grouped K-fold cross-validation by firm materially changes the model ranking. We therefore no longer treat the row-level stacking results as the primary headline and reframe the analysis as diagnostic evidence on model uncertainty rather than a stable model-ranking result.",
    INCONCLUSIVE_DUE_TO_KFOLD_FAILURES = "Exact grouped K-fold refitting encountered substantial convergence or fold-level scoring limitations. We therefore report the analysis as a computational stress test and avoid strong claims based on cross-validation rankings alone.",
    PARTIAL_RUN_NO_FINAL_DECISION = "This was a filtered partial run for operational validation, so it is not used for substantive manuscript conclusions."
  )

  writeLines(c(
    "Reviewer Priority 2b exact grouped K-fold response notes",
    paste("Final decision:", final_decision),
    "Exact grouped K-fold tests the main common sample excluding operating-cycle restrictions.",
    "M10 is not included in the main exact grouped K-fold because it is secondary operating-cycle robustness only.",
    "All observations from the same firm are held out together.",
    "Firm-RE held-out predictions use population-level scoring via re_formula = NA and allow_new_levels = TRUE.",
    "Predictors are standardized using training-fold moments only.",
    "Recommended manuscript wording:",
    manuscript_wording
  ), file.path(logs_dir, "reviewer_priority2b_exact_kfold_response_notes.txt"))

  end_time <- Sys.time()
  runtime_seconds <- as.numeric(difftime(end_time, script_start_time, units = "secs"))
  writeLines(c(
    "ma12 exact grouped K-fold by firm technical log",
    paste("Script:", script_name),
    paste("Script_Version:", script_version),
    paste("Start_Time:", format_time(script_start_time)),
    paste("End_Time:", format_time(end_time)),
    sprintf("Runtime_Seconds: %.2f", runtime_seconds),
    sprintf("Runtime_Hours: %.4f", runtime_seconds / 3600),
    paste("K:", K),
    paste("Run_Mode:", run_mode),
    paste("Run_ID:", run_id),
    paste("Config_Tag:", config_tag),
    paste("Kfold_Run_Root:", kfold_run_root),
    sprintf(
      "Sampling settings: chains=%d iter=%d warmup=%d adapt_delta=%.2f max_treedepth=%d canonical_seed=%d effective_seed=%d",
      chains, iter, warmup, adapt_delta, max_treedepth,
      grouped_run_rng_meta$Canonical_Seed, grouped_run_rng_meta$Effective_Seed
    ),
    sprintf("Stratified_Grouped_KFold: %s (per-industry round-robin; every industry with >= K firms appears in every fold).", kfold_stratified_groups),
    sprintf("Ex-post observations=%d firms=%d.", nrow(df_ep), length(unique(df_ep$company))),
    sprintf("No-look-ahead observations=%d firms=%d.", nrow(df_rt), length(unique(df_rt$company))),
    paste("Ex-post model IDs:", paste(ex_post_ids, collapse = ", ")),
    paste("No-look-ahead model IDs:", paste(no_lookahead_ids, collapse = ", ")),
    "Sample_Group: main_common",
    "M10_Included: FALSE",
    sprintf("Model-fold refits attempted=%d completed=%d failed=%d.", attempted, completed, failed),
    paste("Final decision:", final_decision)
  ), file.path(logs_dir, "phase4e_exact_grouped_kfold_winsor_notes.txt"))

  if (!partial_run && !preflight_only && final_decision != "INCONCLUSIVE_DUE_TO_KFOLD_FAILURES") {
    latest_complete_dir <- file.path(kfold_base_root, "latest_complete")
    dir.create(latest_complete_dir, recursive = TRUE, showWarnings = FALSE)
    files_to_copy <- c(
      file.path(tables_dir, "table_winsor_kfold_weights_ex_post.csv"),
      file.path(tables_dir, "table_winsor_kfold_weights_no_lookahead.csv"),
      file.path(tables_dir, "table_winsor_weight_stability_loo_lofo_kfold.csv"),
      file.path(tables_dir, "table_winsor_family_weight_stability_loo_lofo_kfold.csv"),
      file.path(tables_dir, "table_reviewer_priority2b_exact_kfold_decision.csv"),
      file.path(logs_dir, "reviewer_priority2b_exact_kfold_response_notes.txt"),
      file.path(logs_dir, "phase4e_exact_grouped_kfold_winsor_notes.txt"),
      file.path(logs_dir, "run_config_manifest.csv")
    )
    file.copy(files_to_copy[file.exists(files_to_copy)], latest_complete_dir, overwrite = TRUE)
  }

  status <- ifelse(final_decision == "PARTIAL_RUN_NO_FINAL_DECISION", "COMPLETED", "COMPLETED")
  safe_shutdown(status, nrow(df_ep), length(unique(df_ep$company)), nrow(df_rt), length(unique(df_rt$company)), remove_lock = TRUE)

  cat("\n===== REVIEWER PRIORITY 2b EXACT GROUPED K-FOLD SUMMARY =====\n")
  cat("Start time: ", format_time(script_start_time), "\n", sep = "")
  cat("End time: ", format_time(Sys.time()), "\n", sep = "")
  elapsed <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
  cat(sprintf("Total runtime: %.2f seconds (%.4f hours)\n", elapsed, elapsed / 3600))
  cat("K value: ", K, "\n", sep = "")
  cat("Run root: ", kfold_run_root, "\n", sep = "")
  cat(sprintf("Ex-post N observations: %d; N firms: %d\n", nrow(df_ep), length(unique(df_ep$company))))
  cat(sprintf("No-look-ahead N observations: %d; N firms: %d\n", nrow(df_rt), length(unique(df_rt$company))))
  cat(sprintf("Model-fold refits attempted: %d\n", attempted))
  cat(sprintf("Completed: %d; Failed: %d\n", completed, failed))
  cat("Top exact K-fold ex-post weights:\n")
  print(as.data.frame(head(kfold_weights_ep, 5)), row.names = FALSE)
  cat("Top exact K-fold no-look-ahead weights:\n")
  print(as.data.frame(head(kfold_weights_rt, 5)), row.names = FALSE)
  cat("Family-level exact K-fold weights:\n")
  print(as.data.frame(family_comparison %>% select(Target_Space, Family, Weight_ExactKFold_Winsor, Family_Rank_ExactKFold)), row.names = FALSE)
  cat("Final Priority 2b decision: ", final_decision, "\n", sep = "")
  invisible(final_decision)
}

result <- tryCatch(
  main(),
  error = function(e) {
    message("[FATAL] ", e$message)
    safe_shutdown("FAILED", remove_lock = FALSE)
    stop(e)
  }
)
phase_end("ma12", "Grouped exact firm K-fold")
