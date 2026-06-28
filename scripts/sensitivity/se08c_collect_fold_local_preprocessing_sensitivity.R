# Script: se08c_collect_fold_local_preprocessing_sensitivity.R
# Purpose: Collect fold-local preprocessing sensitivity outputs and decisions.

se08c_top_lock <- local({
  lock_path <- file.path("out", "interim", "winsor", "sensitivity", "fold_local_preprocessing", "logs", "se08c_collect.lock")
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)

  pid_alive <- function(pid) {
    pid <- suppressWarnings(as.integer(pid))
    if (length(pid) != 1L || is.na(pid) || pid <= 0L) return(FALSE)
    if (identical(pid, Sys.getpid())) return(TRUE)
    isTRUE(tryCatch(tools::pskill(pid, signal = 0), error = function(e) FALSE))
  }

  read_lock <- function(path) {
    if (!file.exists(path)) return(list(pid = NA_integer_))
    lines <- readLines(path, warn = FALSE)
    parts <- strsplit(lines, "=", fixed = TRUE)
    out <- list(pid = NA_integer_)
    for (part in parts) {
      if (length(part) >= 2L) out[[tolower(part[[1L]])]] <- paste(part[-1L], collapse = "=")
    }
    out$pid <- suppressWarnings(as.integer(out$pid))
    out
  }

  duplicate_count <- function() {
    if (!requireNamespace("ps", quietly = TRUE)) return(NA_integer_)
    script_name <- "se08c_collect_fold_local_preprocessing_sensitivity.R"
    procs <- tryCatch(ps::ps(), error = function(e) list())
    hits <- vapply(procs, function(proc) {
      cmd <- tryCatch(ps::ps_cmdline(proc), error = function(e) character())
      any(grepl(script_name, cmd, fixed = TRUE))
    }, logical(1))
    sum(hits, na.rm = TRUE)
  }

  n_matches <- duplicate_count()
  if (!is.na(n_matches) && n_matches > 1L) {
    stop("[BLOCKER] duplicate se08c process detected; matches=", n_matches, "; lock=", lock_path)
  }

  if (file.exists(lock_path)) {
    lock <- read_lock(lock_path)
    if (pid_alive(lock$pid)) {
      stop("[BLOCKER] se08c is already running; lock PID=", lock$pid, "; lock=", lock_path)
    }
    unlink(lock_path, force = TRUE)
  }

  lock_lines <- c(
    paste0("PID=", Sys.getpid()),
    paste0("start_time=", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste0("commandArgs=", paste(commandArgs(), collapse = " ")),
    paste0("working_directory=", getwd())
  )
  tmp_lock <- tempfile("se08c_collect_", tmpdir = dirname(lock_path), fileext = ".tmp")
  writeLines(lock_lines, tmp_lock, useBytes = TRUE)
  if (!file.rename(tmp_lock, lock_path)) {
    unlink(tmp_lock, force = TRUE)
    lock <- read_lock(lock_path)
    stop("[BLOCKER] se08c is already running; lock PID=", lock$pid, "; lock=", lock_path)
  }

  list(
    path = lock_path,
    pid = Sys.getpid(),
    release = function() {
      lock <- read_lock(lock_path)
      if (identical(as.integer(lock$pid), as.integer(Sys.getpid()))) {
        unlink(lock_path, force = TRUE)
      }
      invisible(TRUE)
    }
  )
})

tryCatch({

source("scripts/ma00_setup.R")
phase_begin("se08c", "Collect fold-local preprocessing sensitivity")

se08_root <- file.path(output_root, "sensitivity", "fold_local_preprocessing")
tables_dir <- file.path(se08_root, "tables")
logs_dir <- file.path(se08_root, "logs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

se08c_lock_path <- se08c_top_lock$path
logs_dir_lock_path <- se08c_top_lock$path
expected_se08c_lock_path <- file.path("out", "interim", "winsor", "sensitivity", "fold_local_preprocessing", "logs", "se08c_collect.lock")
if (!identical(normalizePath(se08c_lock_path, winslash = "/", mustWork = FALSE),
               normalizePath(expected_se08c_lock_path, winslash = "/", mustWork = FALSE))) {
  stop("[BLOCKER] se08c lock path mismatch after setup; lock=", se08c_lock_path)
}
if (!identical(logs_dir_lock_path, se08c_lock_path)) {
  stop("[BLOCKER] se08c logs_dir lock path mismatch after setup.")
}

se08c_checkpoint <- function(label) {
  message("[se08c][checkpoint] ", label, " | time=", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  invisible(label)
}

stacking_singleton_fallback <- function(lpd_matrix) {
  lpd_matrix <- as.matrix(lpd_matrix)
  singleton_elpd <- colSums(lpd_matrix)
  best <- which.max(singleton_elpd)
  weights <- rep(0, ncol(lpd_matrix))
  weights[best] <- 1
  names(weights) <- colnames(lpd_matrix)
  weights
}

optimize_stacking_guarded <- function(lpd_matrix, context) {
  lpd_matrix <- as.matrix(lpd_matrix)
  if (!nrow(lpd_matrix) || !ncol(lpd_matrix)) {
    stop("[BLOCKER] se08c stacking matrix is empty for ", context)
  }
  if (is.null(colnames(lpd_matrix))) {
    colnames(lpd_matrix) <- paste0("model_", seq_len(ncol(lpd_matrix)))
  }
  if (any(!is.finite(lpd_matrix))) {
    stop("[BLOCKER] se08c stacking matrix has non-finite values for ", context)
  }
  timeout_seconds <- env_int("ACCRUAL_SE08C_STACKING_TIMEOUT_SECONDS", 300L, min = 1L)
  se08c_checkpoint(paste0(context, " optimizer begin"))
  out <- tryCatch({
    setTimeLimit(elapsed = timeout_seconds, transient = TRUE)
    optimize_stacking_from_lpd(lpd_matrix)
  }, error = function(e) {
    warning(
      "[WARNING] se08c stacking optimizer failed or timed out for ", context,
      "; falling back to best singleton ELPD model. Reason: ", conditionMessage(e),
      call. = FALSE
    )
    stacking_singleton_fallback(lpd_matrix)
  }, finally = {
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  se08c_checkpoint(paste0(context, " optimizer end"))
  out
}

bind_rows_base <- function(x) {
  x <- Filter(function(z) is.data.frame(z) && nrow(z) > 0L, x)
  if (!length(x)) return(data.frame())
  all_names <- unique(unlist(lapply(x, names), use.names = FALSE))
  x <- lapply(x, function(df) {
    missing <- setdiff(all_names, names(df))
    for (nm in missing) df[[nm]] <- NA
    df[, all_names, drop = FALSE]
  })
  out <- do.call(rbind, x)
  row.names(out) <- NULL
  out
}

as_bool <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  toupper(trimws(as.character(x))) %in% c("TRUE", "1", "YES", "Y")
}

num <- function(x) suppressWarnings(as.numeric(x))

count_distinct_base <- function(x) length(unique(x[!is.na(x)]))

safe_sum <- function(x) {
  x <- num(x)
  sum(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_sd <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

safe_min <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

safe_max_abs <- function(x) {
  x <- abs(num(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

safe_weighted_mean <- function(x, w) {
  x <- num(x)
  w <- num(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

rank_desc_base <- function(x) rank(-num(x), ties.method = "first")

group_indices <- function(df, keys) {
  if (!nrow(df)) return(list())
  interaction(df[, keys, drop = FALSE], drop = TRUE, lex.order = TRUE)
}

aggregate_by_base <- function(df, keys, fun) {
  if (!nrow(df)) return(data.frame())
  pieces <- split(seq_len(nrow(df)), group_indices(df, keys))
  out <- lapply(pieces, function(idx) fun(df[idx, , drop = FALSE]))
  bind_rows_base(out)
}

order_by_base <- function(df, cols) {
  if (!nrow(df)) return(df)
  cols <- cols[cols %in% names(df)]
  if (!length(cols)) return(df)
  ord <- do.call(order, df[, cols, drop = FALSE])
  df[ord, , drop = FALSE]
}

left_join_base <- function(x, y, by) {
  if (!nrow(x) || !nrow(y)) return(x)
  x$.left_order_internal <- seq_len(nrow(x))
  if (is.null(names(by)) || all(!nzchar(names(by)))) {
    by_x <- by
    by_y <- by
  } else {
    by_x <- names(by)
    by_y <- unname(by)
  }
  out <- merge(x, y, by.x = by_x, by.y = by_y, all.x = TRUE, sort = FALSE)
  out <- out[order(out$.left_order_internal), , drop = FALSE]
  out$.left_order_internal <- NULL
  row.names(out) <- NULL
  out
}

first_existing_col <- function(x, candidates, required = TRUE, context = "table") {
  hit <- candidates[candidates %in% names(x)][1]
  if ((is.na(hit) || !nzchar(hit)) && required) {
    stop("[BLOCKER] ", context, " lacks required column candidate: ", paste(candidates, collapse = ", "))
  }
  hit
}

replace_nonfinite_na <- function(x) {
  x <- num(x)
  x[!is.finite(x)] <- NA_real_
  x
}

reliability_label <- function(n_completed, K, divergences_total, treedepth_warnings_total,
                              max_rhat, min_ess_bulk, min_ess_tail) {
  mapply(
    function(nc, kk, div, td, rhat, ess_b, ess_t) {
      nc <- suppressWarnings(as.integer(nc))
      kk <- suppressWarnings(as.integer(kk))
      div <- safe_sum(div)
      td <- safe_sum(td)
      rhat <- suppressWarnings(as.numeric(rhat))
      ess_b <- suppressWarnings(as.numeric(ess_b))
      ess_t <- suppressWarnings(as.numeric(ess_t))
      if (is.na(nc) || nc == 0L) return("FAILED")
      if (!partial_run && !is.na(kk) && nc < kk) return("LOW_RELIABILITY")
      if (is.finite(div) && div > 0) return("LOW_RELIABILITY")
      if (is.finite(td) && td > 0) return("LOW_RELIABILITY")
      if (is.finite(rhat) && is.finite(ess_b) && is.finite(ess_t) &&
          rhat <= 1.01 && ess_b >= 400 && ess_t >= 400) return("OK")
      if (is.finite(rhat) && is.finite(ess_b) && is.finite(ess_t) &&
          rhat <= 1.05 && ess_b >= 100 && ess_t >= 100) return("CAUTION")
      "LOW_RELIABILITY"
    },
    n_completed, K, divergences_total, treedepth_warnings_total,
    max_rhat, min_ess_bulk, min_ess_tail,
    USE.NAMES = FALSE
  )
}

manifest_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_manifest.csv")
status_path <- file.path(tables_dir, "table_se08_fold_local_preprocessing_task_status.csv")
if (!file.exists(manifest_path) || !file.exists(status_path)) {
  stop("[BLOCKER] se08c requires se08a manifest and se08b task status.")
}
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
status <- read.csv(status_path, stringsAsFactors = FALSE)
accrual_task_status_blocker(status, required_col = "Required", context = "se08 fold-local preprocessing collect")
required_status <- status[as_bool(status$Required), , drop = FALSE]
if (nrow(required_status) && !all(required_status$status == "SUCCESS")) {
  stop("[BLOCKER] se08c requires all required fold-local preprocessing tasks to complete successfully.")
}

results <- lapply(manifest$result_path, function(path) {
  if (!file.exists(path)) stop("[BLOCKER] se08c missing task result: ", path)
  readRDS(path)
})
fold_diagnostics <- bind_rows_base(lapply(results, `[[`, "fold_diag"))
preprocessing_audit <- bind_rows_base(lapply(results, `[[`, "preprocessing_audit"))
obs_all <- lapply(results, `[[`, "obs_scores")
grouped_obs <- bind_rows_base(obs_all[manifest$Validation_Scheme == "grouped_firm_kfold"])
row_obs <- bind_rows_base(obs_all[manifest$Validation_Scheme == "row_exact_kfold"])

write_csv_safely(preprocessing_audit, file.path(tables_dir, "table_se08_fold_local_preprocessing_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")

winsor_rows <- preprocessing_audit[preprocessing_audit$preprocessing_step == "winsorization", , drop = FALSE]
winsor_summary <- aggregate_by_base(
  winsor_rows,
  c("validation_scheme", "target_space", "variable"),
  function(df) data.frame(
    validation_scheme = df$validation_scheme[1],
    target_space = df$target_space[1],
    variable = df$variable[1],
    n_fold_model_tasks = nrow(df),
    mean_train_cutoff_p01 = safe_mean(df$train_cutoff_p01),
    mean_train_cutoff_p99 = safe_mean(df$train_cutoff_p99),
    max_abs_delta_train_vs_global_p01 = safe_max_abs(df$delta_train_vs_global_p01),
    max_abs_delta_train_vs_global_p99 = safe_max_abs(df$delta_train_vs_global_p99),
    max_share_test_capped_low = safe_max(df$share_test_capped_low),
    max_share_test_capped_high = safe_max(df$share_test_capped_high),
    stringsAsFactors = FALSE
  )
)
write_csv_safely(winsor_summary, file.path(tables_dir, "table_se08_fold_local_cutoff_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

std_rows <- preprocessing_audit[preprocessing_audit$preprocessing_step == "standardization", , drop = FALSE]
standardization_summary <- aggregate_by_base(
  std_rows,
  c("validation_scheme", "target_space", "variable"),
  function(df) data.frame(
    validation_scheme = df$validation_scheme[1],
    target_space = df$target_space[1],
    variable = df$variable[1],
    n_fold_model_tasks = nrow(df),
    mean_train_mean = safe_mean(df$train_mean),
    sd_train_mean = safe_sd(df$train_mean),
    mean_train_sd = safe_mean(df$train_sd),
    min_train_sd = safe_min(df$train_sd),
    stringsAsFactors = FALSE
  )
)
write_csv_safely(standardization_summary, file.path(tables_dir, "table_se08_fold_local_standardization_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

write_csv_safely(grouped_obs, file.path(tables_dir, "table_se08_grouped_fold_local_observation_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(row_obs, file.path(tables_dir, "table_se08_row_fold_local_observation_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")

K_by_scheme <- aggregate_by_base(
  manifest,
  "Validation_Scheme",
  function(df) data.frame(
    Validation_Scheme = df$Validation_Scheme[1],
    K = safe_max(as.integer(df$K)),
    stringsAsFactors = FALSE
  )
)
partial_run <- FALSE

grouped_fold_scores <- aggregate_by_base(
  grouped_obs,
  c("Target_Space", "Sample_Group", "Fold_ID", "Model_ID", "Model_Name", "Heterogeneity_Variant"),
  function(df) data.frame(
    Target_Space = df$Target_Space[1],
    Sample_Group = df$Sample_Group[1],
    Fold_ID = df$Fold_ID[1],
    Model_ID = df$Model_ID[1],
    Model_Name = df$Model_Name[1],
    Heterogeneity_Variant = df$Heterogeneity_Variant[1],
    N_Test_Obs = nrow(df),
    N_Test_Firms = count_distinct_base(df$company),
    elpd_fold = safe_sum(df$lpd_obs),
    mean_lpd_obs = safe_mean(df$lpd_obs),
    stringsAsFactors = FALSE
  )
)

grouped_diag <- fold_diagnostics[fold_diagnostics$Validation_Scheme == "grouped_firm_kfold", , drop = FALSE]
grouped_model_scores <- aggregate_by_base(
  grouped_diag,
  c("Validation_Scheme", "Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant"),
  function(df) data.frame(
    Validation_Scheme = df$Validation_Scheme[1],
    Target_Space = df$Target_Space[1],
    Sample_Group = df$Sample_Group[1],
    Model_ID = df$Model_ID[1],
    Model_Name = df$Model_Name[1],
    Heterogeneity_Variant = df$Heterogeneity_Variant[1],
    N_Folds_Attempted = nrow(df),
    N_Folds_Completed = sum(as_bool(df$Completed), na.rm = TRUE),
    max_rhat_max = safe_max(df$Max_Rhat),
    min_ess_bulk = safe_min(df$Min_ESS_Bulk),
    min_ess_tail = safe_min(df$Min_ESS_Tail),
    ess_warning_any = any(as_bool(df$ESS_Warning), na.rm = TRUE),
    divergences_total = safe_sum(df$Divergences),
    treedepth_warnings_total = safe_sum(df$Treedepth_Warnings),
    Runtime_Seconds = safe_sum(df$Runtime_Seconds),
    stringsAsFactors = FALSE
  )
)

grouped_fold_totals <- aggregate_by_base(
  grouped_fold_scores,
  c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant"),
  function(df) data.frame(
    Target_Space = df$Target_Space[1],
    Sample_Group = df$Sample_Group[1],
    Model_ID = df$Model_ID[1],
    Model_Name = df$Model_Name[1],
    Heterogeneity_Variant = df$Heterogeneity_Variant[1],
    N_Test_Obs_Total = safe_sum(df$N_Test_Obs),
    N_Test_Firms_Total = safe_sum(df$N_Test_Firms),
    elpd_kfold = safe_sum(df$elpd_fold),
    mean_lpd_obs = safe_weighted_mean(df$mean_lpd_obs, df$N_Test_Obs),
    stringsAsFactors = FALSE
  )
)
grouped_model_scores <- left_join_base(
  grouped_model_scores,
  grouped_fold_totals,
  by = c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant")
)
grouped_model_scores <- left_join_base(grouped_model_scores, K_by_scheme, by = "Validation_Scheme")
grouped_model_scores$max_rhat_max <- replace_nonfinite_na(grouped_model_scores$max_rhat_max)
grouped_model_scores$min_ess_bulk <- replace_nonfinite_na(grouped_model_scores$min_ess_bulk)
grouped_model_scores$min_ess_tail <- replace_nonfinite_na(grouped_model_scores$min_ess_tail)
grouped_model_scores$reliability_flag <- reliability_label(
  grouped_model_scores$N_Folds_Completed,
  grouped_model_scores$K,
  grouped_model_scores$divergences_total,
  grouped_model_scores$treedepth_warnings_total,
  grouped_model_scores$max_rhat_max,
  grouped_model_scores$min_ess_bulk,
  grouped_model_scores$min_ess_tail
)
grouped_model_scores$included_in_stack <- grouped_model_scores$reliability_flag %in% c("OK", "CAUTION") &
  grouped_model_scores$N_Folds_Completed == grouped_model_scores$K
write_csv_safely(grouped_model_scores, file.path(tables_dir, "table_se08_grouped_fold_local_model_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")

row_included <- row_obs[as_bool(row_obs$primary_row_target_inclusion), , drop = FALSE]
row_model_scores <- aggregate_by_base(
  row_included,
  c("target_space", "sample_group", "model_id", "model_name", "heterogeneity_variant"),
  function(df) data.frame(
    target_space = df$target_space[1],
    sample_group = df$sample_group[1],
    model_id = df$model_id[1],
    model_name = df$model_name[1],
    heterogeneity_variant = df$heterogeneity_variant[1],
    n_obs_scored = nrow(df),
    elpd_exact_row_kfold = safe_sum(df$log_predictive_density),
    mean_lpd = safe_mean(df$log_predictive_density),
    sd_lpd = safe_sd(df$log_predictive_density),
    n_new_company_excluded_from_primary = sum(as_bool(df$new_company_in_row_fold), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
)

row_diag <- fold_diagnostics[fold_diagnostics$Validation_Scheme == "row_exact_kfold", , drop = FALSE]
row_diag_summary <- aggregate_by_base(
  row_diag,
  c("Target_Space", "Sample_Group", "Model_ID", "Model_Name", "Heterogeneity_Variant"),
  function(df) data.frame(
    Target_Space = df$Target_Space[1],
    Sample_Group = df$Sample_Group[1],
    Model_ID = df$Model_ID[1],
    Model_Name = df$Model_Name[1],
    Heterogeneity_Variant = df$Heterogeneity_Variant[1],
    n_folds_attempted = nrow(df),
    n_folds_completed = sum(as_bool(df$Completed), na.rm = TRUE),
    max_rhat_max = safe_max(df$Max_Rhat),
    min_ess_bulk_min = safe_min(df$Min_ESS_Bulk),
    min_ess_tail_min = safe_min(df$Min_ESS_Tail),
    divergences_total = safe_sum(df$Divergences),
    treedepth_warnings_total = safe_sum(df$Treedepth_Warnings),
    stringsAsFactors = FALSE
  )
)
row_model_scores <- left_join_base(
  row_model_scores,
  row_diag_summary,
  by = c(
    target_space = "Target_Space",
    sample_group = "Sample_Group",
    model_id = "Model_ID",
    model_name = "Model_Name",
    heterogeneity_variant = "Heterogeneity_Variant"
  )
)
row_k <- K_by_scheme$K[K_by_scheme$Validation_Scheme == "row_exact_kfold"][1]
row_model_scores$max_rhat_max <- replace_nonfinite_na(row_model_scores$max_rhat_max)
row_model_scores$min_ess_bulk_min <- replace_nonfinite_na(row_model_scores$min_ess_bulk_min)
row_model_scores$min_ess_tail_min <- replace_nonfinite_na(row_model_scores$min_ess_tail_min)
row_model_scores$reliability_flag <- reliability_label(
  row_model_scores$n_folds_completed,
  row_k,
  row_model_scores$divergences_total,
  row_model_scores$treedepth_warnings_total,
  row_model_scores$max_rhat_max,
  row_model_scores$min_ess_bulk_min,
  row_model_scores$min_ess_tail_min
)
row_model_scores$included_in_stack <- row_model_scores$reliability_flag %in% c("OK", "CAUTION") &
  row_model_scores$n_folds_completed == row_k
row_model_scores$refit_type <- "fold_local_exact_refit"
row_model_scores$validation_unit <- "row_level"
row_model_scores$primary_row_target_excludes_new_company_rows <- TRUE
write_csv_safely(row_model_scores, file.path(tables_dir, "table_se08_row_fold_local_model_scores.csv"), row.names = FALSE, fileEncoding = "UTF-8")

build_grouped_weights <- function(target_space) {
  included <- grouped_model_scores[
    grouped_model_scores$Target_Space == target_space &
      grouped_model_scores$Sample_Group == "main_common" &
      as_bool(grouped_model_scores$included_in_stack),
    ,
    drop = FALSE
  ]
  included <- order_by_base(included, c("Model_ID", "Heterogeneity_Variant"))
  if (!nrow(included)) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, , drop = FALSE]
    key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, "_se08_grouped_fold_local")
    one <- grouped_obs[
      grouped_obs$Target_Space == target_space &
        grouped_obs$Sample_Group == row$Sample_Group &
        grouped_obs$Model_ID == row$Model_ID &
        grouped_obs$Heterogeneity_Variant == row$Heterogeneity_Variant,
      ,
      drop = FALSE
    ]
    one <- order_by_base(one, c("company", "year"))
    score_list[[key]] <- one$lpd_obs
    meta_keys <- c(meta_keys, key)
  }
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) {
    stop("[BLOCKER] se08 grouped score vectors have unequal lengths for ", target_space)
  }
  lpd_matrix <- do.call(cbind, score_list)
  weights <- optimize_stacking_guarded(lpd_matrix, paste("grouped", target_space, "stacking"))
  meta_idx <- match(names(weights), meta_keys)
  out <- included[meta_idx, , drop = FALSE]
  out$Model_Key_Fold_Local <- names(weights)
  out$Weight_Fold_Local <- as.numeric(weights)
  out$Singleton_ELPD <- as.numeric(colSums(lpd_matrix)[names(weights)])
  out$Rank_Fold_Local <- rank_desc_base(out$Weight_Fold_Local)
  out[order(out$Rank_Fold_Local), , drop = FALSE]
}

build_row_weights <- function(target_space) {
  included <- row_model_scores[
    row_model_scores$target_space == target_space &
      row_model_scores$sample_group == "main_common" &
      as_bool(row_model_scores$included_in_stack),
    ,
    drop = FALSE
  ]
  included <- order_by_base(included, c("model_id", "heterogeneity_variant"))
  if (!nrow(included)) return(data.frame())
  score_list <- list()
  meta_keys <- character()
  for (i in seq_len(nrow(included))) {
    row <- included[i, , drop = FALSE]
    key <- model_key_sampled(row$model_id, row$target_space, row$sample_group, row$heterogeneity_variant, "_se08_row_fold_local")
    one <- row_included[
      row_included$target_space == target_space &
        row_included$model_id == row$model_id &
        row_included$heterogeneity_variant == row$heterogeneity_variant,
      ,
      drop = FALSE
    ]
    one <- order_by_base(one, "observation_id")
    score_list[[key]] <- one$log_predictive_density
    meta_keys <- c(meta_keys, key)
  }
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) {
    stop("[BLOCKER] se08 row score vectors have unequal lengths for ", target_space)
  }
  lpd_matrix <- do.call(cbind, score_list)
  weights <- optimize_stacking_guarded(lpd_matrix, paste("row", target_space, "stacking"))
  meta_idx <- match(names(weights), meta_keys)
  out <- included[meta_idx, , drop = FALSE]
  out$model_key_fold_local <- names(weights)
  out$weight_fold_local <- as.numeric(weights)
  out$singleton_elpd <- as.numeric(colSums(lpd_matrix)[names(weights)])
  out$rank_fold_local <- rank_desc_base(out$weight_fold_local)
  out[order(out$rank_fold_local), , drop = FALSE]
}

se08c_checkpoint("grouped ex_post stacking begin")
grouped_ep <- build_grouped_weights("ex_post")
se08c_checkpoint("grouped ex_post stacking end")
se08c_checkpoint("grouped real_time stacking begin")
grouped_rt <- build_grouped_weights("real_time")
se08c_checkpoint("grouped real_time stacking end")
se08c_checkpoint("row ex_post stacking begin")
row_ep <- build_row_weights("ex_post")
se08c_checkpoint("row ex_post stacking end")
se08c_checkpoint("row real_time stacking begin")
row_rt <- build_row_weights("real_time")
se08c_checkpoint("row real_time stacking end")
write_csv_safely(grouped_ep, file.path(tables_dir, "table_se08_grouped_fold_local_weights_ex_post.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(grouped_rt, file.path(tables_dir, "table_se08_grouped_fold_local_weights_no_lookahead.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(row_ep, file.path(tables_dir, "table_se08_row_fold_local_weights_ex_post.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_csv_safely(row_rt, file.path(tables_dir, "table_se08_row_fold_local_weights_no_lookahead.csv"), row.names = FALSE, fileEncoding = "UTF-8")

read_pin_root <- function(kind) {
  pin <- file.path(output_root, if (identical(kind, "grouped_firm")) "kfold_firm" else "row_exact_kfold", "LATEST_COMPLETED_RUN.txt")
  if (!file.exists(pin)) return(NA_character_)
  read_single_line_no_bom(pin, paste0("se08c completed ", kind, " exact K-fold run pin"))
}

read_primary_weights <- function(kind, target_space) {
  root <- read_pin_root(kind)
  file_name <- if (identical(kind, "grouped_firm")) {
    if (identical(target_space, "ex_post")) "table_winsor_kfold_weights_ex_post.csv" else "table_winsor_kfold_weights_no_lookahead.csv"
  } else {
    if (identical(target_space, "ex_post")) "table_winsor_row_exact_kfold_weights_ex_post.csv" else "table_winsor_row_exact_kfold_weights_no_lookahead.csv"
  }
  candidates <- c(file.path(root, "tables", file_name), file.path(output_root, "tables", file_name))
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path) || !nzchar(path)) return(data.frame())
  x <- read.csv(path, stringsAsFactors = FALSE)
  x$.source_path <- path
  x
}

standardize_weight_table <- function(x, validation_scheme, target_space, source_type) {
  if (!is.data.frame(x) || !nrow(x)) return(data.frame())
  model_col <- first_existing_col(x, c("Model_ID", "model_id"), context = "weight table")
  name_col <- first_existing_col(x, c("Model_Name", "model_name"), required = FALSE, context = "weight table")
  variant_col <- first_existing_col(x, c("Heterogeneity_Variant", "heterogeneity_variant"), context = "weight table")
  weight_col <- first_existing_col(x, c("Weight_KFold", "weight_row_exact_kfold", "Weight_Fold_Local", "weight_fold_local"), context = "weight table")
  rank_col <- first_existing_col(x, c("Rank_KFold", "rank_row_exact_kfold", "Rank_Fold_Local", "rank_fold_local"), required = FALSE, context = "weight table")
  weight <- num(x[[weight_col]])
  data.frame(
    validation_scheme = validation_scheme,
    target_space = target_space,
    source_type = source_type,
    model_id = x[[model_col]],
    model_name = if (!is.na(name_col)) x[[name_col]] else x[[model_col]],
    heterogeneity_variant = x[[variant_col]],
    weight = weight,
    rank = if (!is.na(rank_col)) suppressWarnings(as.integer(x[[rank_col]])) else rank_desc_base(weight),
    source_path = if (".source_path" %in% names(x)) x$.source_path else se08_root,
    stringsAsFactors = FALSE
  )
}

weight_long <- bind_rows_base(list(
  standardize_weight_table(read_primary_weights("grouped_firm", "ex_post"), "grouped_firm_kfold", "ex_post", "global"),
  standardize_weight_table(read_primary_weights("grouped_firm", "real_time"), "grouped_firm_kfold", "real_time", "global"),
  standardize_weight_table(read_primary_weights("row", "ex_post"), "row_exact_kfold", "ex_post", "global"),
  standardize_weight_table(read_primary_weights("row", "real_time"), "row_exact_kfold", "real_time", "global"),
  standardize_weight_table(grouped_ep, "grouped_firm_kfold", "ex_post", "fold_local"),
  standardize_weight_table(grouped_rt, "grouped_firm_kfold", "real_time", "fold_local"),
  standardize_weight_table(row_ep, "row_exact_kfold", "ex_post", "fold_local"),
  standardize_weight_table(row_rt, "row_exact_kfold", "real_time", "fold_local")
))

weight_comparison <- aggregate_by_base(
  weight_long,
  c("validation_scheme", "target_space", "model_id", "model_name", "heterogeneity_variant"),
  function(df) {
    global <- df$weight[df$source_type == "global"][1]
    fold_local <- df$weight[df$source_type == "fold_local"][1]
    if (length(global) == 0L) global <- NA_real_
    if (length(fold_local) == 0L) fold_local <- NA_real_
    absolute_difference <- fold_local - global
    data.frame(
      validation_scheme = df$validation_scheme[1],
      target_space = df$target_space[1],
      model_id = df$model_id[1],
      model_name = df$model_name[1],
      heterogeneity_variant = df$heterogeneity_variant[1],
      global = global,
      fold_local = fold_local,
      absolute_difference = absolute_difference,
      relative_difference = absolute_difference / pmax(abs(global), .Machine$double.eps),
      stringsAsFactors = FALSE
    )
  }
)
write_csv_safely(weight_comparison, file.path(tables_dir, "table_se08_fold_local_vs_global_weight_comparison.csv"), row.names = FALSE, fileEncoding = "UTF-8")

firmre_long <- aggregate_by_base(
  weight_long,
  c("target_space", "validation_scheme", "source_type"),
  function(df) {
    is_firm_re <- grepl("Firm RE|Random Intercept", df$heterogeneity_variant, ignore.case = TRUE)
    data.frame(
      target_space = df$target_space[1],
      validation_scheme = df$validation_scheme[1],
      source_type = df$source_type[1],
      firmre_weight = safe_sum(df$weight[is_firm_re]),
      stringsAsFactors = FALSE
    )
  }
)

firmre_summary <- aggregate_by_base(
  firmre_long,
  "target_space",
  function(df) {
    value <- function(scheme, source) {
      hit <- df$firmre_weight[df$validation_scheme == scheme & df$source_type == source][1]
      if (length(hit) == 0L) NA_real_ else hit
    }
    grouped_global <- value("grouped_firm_kfold", "global")
    grouped_fold <- value("grouped_firm_kfold", "fold_local")
    row_global <- value("row_exact_kfold", "global")
    row_fold <- value("row_exact_kfold", "fold_local")
    shift_global <- row_global - grouped_global
    shift_fold <- row_fold - grouped_fold
    data.frame(
      target_space = df$target_space[1],
      grouped_firm_kfold_global = grouped_global,
      grouped_firm_kfold_fold_local = grouped_fold,
      row_exact_kfold_global = row_global,
      row_exact_kfold_fold_local = row_fold,
      row_minus_grouped_firmre_shift_global = shift_global,
      row_minus_grouped_firmre_shift_fold_local = shift_fold,
      row_over_grouped_firmre_ratio_global = row_global / pmax(grouped_global, .Machine$double.eps),
      row_over_grouped_firmre_ratio_fold_local = row_fold / pmax(grouped_fold, .Machine$double.eps),
      shift_absolute_difference = shift_fold - shift_global,
      shift_relative_to_global = shift_fold / pmax(abs(shift_global), .Machine$double.eps),
      stringsAsFactors = FALSE
    )
  }
)
write_csv_safely(firmre_summary, file.path(tables_dir, "table_se08_fold_local_vs_global_firmre_shift_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

top_model_long <- aggregate_by_base(
  weight_long,
  c("target_space", "validation_scheme", "source_type"),
  function(df) {
    df <- df[order(num(df$rank), -num(df$weight)), , drop = FALSE]
    top <- df[1, , drop = FALSE]
    data.frame(
      target_space = top$target_space,
      validation_scheme = top$validation_scheme,
      source_type = top$source_type,
      top_model_key = paste(top$model_id, top$heterogeneity_variant, sep = " | "),
      top_heterogeneity_axis = ifelse(
        grepl("Firm RE|Random Intercept", top$heterogeneity_variant, ignore.case = TRUE),
        "firm_re",
        "pooled_or_cross_sectional"
      ),
      weight = top$weight,
      stringsAsFactors = FALSE
    )
  }
)

top_model_comparison <- aggregate_by_base(
  top_model_long,
  c("target_space", "validation_scheme"),
  function(df) {
    get_value <- function(source, col) {
      hit <- df[df$source_type == source, col, drop = TRUE][1]
      if (length(hit) == 0L) NA else hit
    }
    top_global <- get_value("global", "top_model_key")
    top_fold <- get_value("fold_local", "top_model_key")
    axis_global <- get_value("global", "top_heterogeneity_axis")
    axis_fold <- get_value("fold_local", "top_heterogeneity_axis")
    weight_global <- suppressWarnings(as.numeric(get_value("global", "weight")))
    weight_fold <- suppressWarnings(as.numeric(get_value("fold_local", "weight")))
    data.frame(
      target_space = df$target_space[1],
      validation_scheme = df$validation_scheme[1],
      top_model_key_global = top_global,
      top_model_key_fold_local = top_fold,
      top_heterogeneity_axis_global = axis_global,
      top_heterogeneity_axis_fold_local = axis_fold,
      weight_global = weight_global,
      weight_fold_local = weight_fold,
      top_model_same = identical(as.character(top_global), as.character(top_fold)),
      heterogeneity_axis_same = identical(as.character(axis_global), as.character(axis_fold)),
      stringsAsFactors = FALSE
    )
  }
)
write_csv_safely(top_model_comparison, file.path(tables_dir, "table_se08_fold_local_vs_global_top_model_comparison.csv"), row.names = FALSE, fileEncoding = "UTF-8")

decision_rows <- lapply(seq_len(nrow(firmre_summary)), function(i) {
  row <- firmre_summary[i, , drop = FALSE]
  target_space <- row$target_space
  global_shift <- row$row_minus_grouped_firmre_shift_global
  fold_shift <- row$row_minus_grouped_firmre_shift_fold_local
  rel <- fold_shift / pmax(abs(global_shift), .Machine$double.eps)
  top_target <- top_model_comparison[top_model_comparison$target_space == target_space, , drop = FALSE]
  grouped_axis <- top_target[top_target$validation_scheme == "grouped_firm_kfold", , drop = FALSE]
  row_axis <- top_target[top_target$validation_scheme == "row_exact_kfold", , drop = FALSE]
  shift_decision <- if (is.na(fold_shift) || is.na(global_shift)) {
    "FAIL"
  } else if (fold_shift <= 0 || abs(fold_shift) < 1e-8) {
    "FAIL"
  } else if (rel < 0.70) {
    "WARN"
  } else {
    "PASS"
  }
  axis_decision <- if (nrow(grouped_axis) && nrow(row_axis) &&
                       grouped_axis$top_heterogeneity_axis_fold_local %in% c("pooled_or_cross_sectional") &&
                       row_axis$top_heterogeneity_axis_fold_local %in% c("firm_re")) {
    "PASS"
  } else if (nrow(grouped_axis) && nrow(row_axis) &&
             grouped_axis$top_heterogeneity_axis_fold_local == grouped_axis$top_heterogeneity_axis_global &&
             row_axis$top_heterogeneity_axis_fold_local == row_axis$top_heterogeneity_axis_global) {
    "PASS"
  } else {
    "WARN"
  }
  data.frame(
    decision_id = c(paste0(target_space, "_firmre_shift"), paste0(target_space, "_top_model_axis")),
    target_space = target_space,
    metric = c("row_minus_grouped_Firm_RE_shift", "top_model_heterogeneity_axis"),
    global_value = c(global_shift, paste(top_target$top_heterogeneity_axis_global, collapse = ";")),
    fold_local_value = c(fold_shift, paste(top_target$top_heterogeneity_axis_fold_local, collapse = ";")),
    absolute_difference = c(fold_shift - global_shift, NA_real_),
    relative_difference = c(rel, NA_real_),
    decision = c(shift_decision, axis_decision),
    interpretation = c(
      "PASS if row-minus-grouped Firm-RE shift remains positive and at least 70% of the global-preprocessing shift.",
      "PASS if the grouped-vs-row pooling/firm-specificity conclusion remains substantively unchanged."
    ),
    stringsAsFactors = FALSE
  )
})
decision <- bind_rows_base(decision_rows)
write_csv_safely(decision, file.path(tables_dir, "table_se08_fold_local_sensitivity_decision.csv"), row.names = FALSE, fileEncoding = "UTF-8")

manifest_row <- data.frame(
  Script_Name = "scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R",
  Sensitivity_Root = se08_root,
  N_Tasks = nrow(manifest),
  N_Completed = sum(status$status == "SUCCESS"),
  Decision_Overall = if (any(decision$decision == "FAIL")) "FAIL" else if (any(decision$decision == "WARN")) "WARN" else "PASS",
  Output_Tables_Dir = tables_dir,
  stringsAsFactors = FALSE
)
write_csv_safely(manifest_row, file.path(logs_dir, "se08_fold_local_preprocessing_collect_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")

message("se08c collected fold-local preprocessing sensitivity outputs.")
phase_end("se08c", "Collect fold-local preprocessing sensitivity")
}, finally = {
  se08c_top_lock$release()
})
