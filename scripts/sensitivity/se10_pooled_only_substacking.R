# Script: se10_pooled_only_substacking.R
# Purpose: Pooled-only row-vs-grouped K-fold sub-stacking sensitivity.

suppressPackageStartupMessages({
  library(dplyr)
})

source("scripts/ma00_setup.R")
phase_begin("se10", "Pooled-only sub-stacking sensitivity")

script_name <- "scripts/sensitivity/se10_pooled_only_substacking.R"
script_start_time <- Sys.time()

se10_root <- file.path(output_root, "sensitivity", "pooled_only_substacking")
tables_dir <- file.path(se10_root, "tables")
logs_dir <- file.path(se10_root, "logs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

material_l1_shift <- env_num("ACCRUAL_SE10_MATERIAL_L1_SHIFT", 0.20, min = 0)
material_max_family_shift <- env_num("ACCRUAL_SE10_MATERIAL_MAX_FAMILY_SHIFT", 0.10, min = 0)

nonempty <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
}

read_pin <- function(path, context) {
  if (!file.exists(path)) stop("[BLOCKER] SE10 missing required ", context, " completed-run pin: ", path)
  x <- trimws(readLines(path, warn = FALSE))
  x <- x[nzchar(x)]
  if (!length(x)) stop("[BLOCKER] SE10 empty completed-run pin: ", path)
  normalizePath(x[[1]], winslash = "/", mustWork = FALSE)
}

resolve_run_root <- function(env_name, pin_path, context) {
  explicit <- trimws(env_value(env_name, ""))
  if (nonempty(explicit)) return(normalizePath(explicit, winslash = "/", mustWork = FALSE))
  read_pin(pin_path, context)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

first_existing_col <- function(x, candidates, context, required = TRUE) {
  hit <- candidates[candidates %in% names(x)][1]
  if ((is.na(hit) || !nzchar(hit)) && isTRUE(required)) {
    stop("[BLOCKER] ", context, " missing required column; expected one of: ", paste(candidates, collapse = ", "))
  }
  if (is.na(hit)) NA_character_ else hit
}

as_bool <- function(x) {
  if (is.logical(x)) return(x)
  as.character(x) %in% c("TRUE", "true", "True", "1", "yes", "YES")
}

is_pooled_variant <- function(variant, model_name = "") {
  grepl("Pooled", as.character(variant), ignore.case = TRUE) &
    !grepl("Firm RE|Random Intercept|firm random effect", paste(variant, model_name), ignore.case = TRUE)
}

accounting_family_from_model <- function(model_id, model_name) {
  family <- ifelse(!is.na(model_name) & nzchar(model_name), extract_base_model_name(model_name), NA_character_)
  family <- sub("\\s*\\((Pooled|Firm RE|Random Intercept).*$", "", family, ignore.case = TRUE)
  family <- trimws(family)
  ifelse(!is.na(family) & nzchar(family), family, as.character(model_id))
}

find_required_source_tables <- function(grouped_root, row_root) {
  grouped_obs <- file.path(grouped_root, "tables", "table_winsor_kfold_observation_scores.csv")
  grouped_model <- file.path(grouped_root, "tables", "table_winsor_kfold_model_scores.csv")
  row_obs_candidates <- file.path(row_root, "tables", c(
    "table_winsor_row_exact_kfold_observation_scores.csv",
    "table_row_exact_kfold_observation_scores.csv"
  ))
  row_model_candidates <- file.path(row_root, "tables", c(
    "table_winsor_row_exact_kfold_model_scores.csv",
    "table_row_exact_kfold_model_scores.csv"
  ))
  row_obs <- row_obs_candidates[file.exists(row_obs_candidates)][1]
  row_model <- row_model_candidates[file.exists(row_model_candidates)][1]
  missing <- c(grouped_obs, grouped_model, row_obs, row_model)
  missing <- missing[is.na(missing) | !nzchar(missing) | !file.exists(missing)]
  if (length(missing)) {
    write_blocked_decision("BLOCKED_MISSING_SOURCE_SCORES", "SE10 missing required row/grouped K-fold score tables.", grouped_root, row_root)
    stop("[BLOCKER] SE10 missing required row/grouped K-fold score tables.")
  }
  list(
    grouped_obs = grouped_obs,
    grouped_model = grouped_model,
    row_obs = row_obs,
    row_model = row_model
  )
}

write_blocked_decision <- function(decision, interpretation, grouped_root, row_root) {
  out <- data.frame(
    Target_Space = c("ex_post", "real_time"),
    Top_Pooled_Model_Row = NA_character_,
    Top_Pooled_Model_Grouped = NA_character_,
    Top_Pooled_Family_Row = NA_character_,
    Top_Pooled_Family_Grouped = NA_character_,
    Top_Pooled_Model_Changed = NA,
    Top_Pooled_Family_Changed = NA,
    Pooled_Family_Weight_Shift_L1 = NA_real_,
    Pooled_Family_Weight_Shift_Max = NA_real_,
    Decision = decision,
    Interpretation = interpretation,
    Source_Row_KFold_Run_Root = row_root,
    Source_Grouped_KFold_Run_Root = grouped_root,
    stringsAsFactors = FALSE
  )
  write_csv_safely(out, file.path(tables_dir, "table_se10_pooled_only_decision.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out)
}

standardize_model_scores <- function(x, source_label, source_root) {
  target_col <- first_existing_col(x, c("Target_Space", "target_space"), source_label)
  sample_col <- first_existing_col(x, c("Sample_Group", "sample_group"), source_label)
  id_col <- first_existing_col(x, c("Model_ID", "model_id"), source_label)
  name_col <- first_existing_col(x, c("Model_Name", "model_name"), source_label)
  variant_col <- first_existing_col(x, c("Heterogeneity_Variant", "heterogeneity_variant"), source_label)
  reliability_col <- first_existing_col(x, c("reliability_flag", "Reliability_Flag"), source_label, required = FALSE)
  included_col <- first_existing_col(x, c(
    "included_in_stack", "Included_In_Stack", "MA12C_Included_In_Stack",
    "Source_Included_In_Stack", "MA12D_Primary_Stack_Eligible"
  ), source_label, required = FALSE)
  elpd_col <- first_existing_col(x, c("elpd_kfold", "elpd_exact_row_kfold", "ELPD", "Singleton_ELPD"), source_label, required = FALSE)
  out <- data.frame(
    Target_Space = as.character(x[[target_col]]),
    Sample_Group = as.character(x[[sample_col]]),
    Model_ID = as.character(x[[id_col]]),
    Model_Name = as.character(x[[name_col]]),
    Heterogeneity_Variant = as.character(x[[variant_col]]),
    Reliability_Flag = if (!is.na(reliability_col)) as.character(x[[reliability_col]]) else NA_character_,
    Included_In_Stack = NA,
    ELPD = if (!is.na(elpd_col)) suppressWarnings(as.numeric(x[[elpd_col]])) else NA_real_,
    Source_Run_Root = source_root,
    stringsAsFactors = FALSE
  )
  if (!is.na(included_col)) {
    out$Included_In_Stack <- as_bool(x[[included_col]])
  } else {
    out$Included_In_Stack <- toupper(out$Reliability_Flag) %in% c("OK", "CAUTION")
  }
  out$Accounting_Family <- accounting_family_from_model(out$Model_ID, out$Model_Name)
  out$Pooled_Only_Candidate <- is_pooled_variant(out$Heterogeneity_Variant, out$Model_Name)
  out
}

standardize_observation_scores <- function(x, source_label, source_root) {
  target_col <- first_existing_col(x, c("Target_Space", "target_space"), source_label)
  sample_col <- first_existing_col(x, c("Sample_Group", "sample_group"), source_label, required = FALSE)
  id_col <- first_existing_col(x, c("Model_ID", "model_id"), source_label)
  name_col <- first_existing_col(x, c("Model_Name", "model_name"), source_label)
  variant_col <- first_existing_col(x, c("Heterogeneity_Variant", "heterogeneity_variant"), source_label)
  obs_col <- first_existing_col(x, c("Obs_ID", "observation_id", "Observation_ID"), source_label)
  lpd_col <- first_existing_col(x, c("lpd_obs", "log_predictive_density", "Log_Predictive_Density"), source_label)
  primary_col <- first_existing_col(x, c("primary_row_target_inclusion", "Primary_Row_Target_Inclusion"), source_label, required = FALSE)
  out <- data.frame(
    Target_Space = as.character(x[[target_col]]),
    Sample_Group = if (!is.na(sample_col)) as.character(x[[sample_col]]) else "main_common",
    Model_ID = as.character(x[[id_col]]),
    Model_Name = as.character(x[[name_col]]),
    Heterogeneity_Variant = as.character(x[[variant_col]]),
    Observation_ID = as.character(x[[obs_col]]),
    LPD = suppressWarnings(as.numeric(x[[lpd_col]])),
    Primary_Row_Target_Inclusion = if (!is.na(primary_col)) as_bool(x[[primary_col]]) else TRUE,
    Source_Run_Root = source_root,
    stringsAsFactors = FALSE
  )
  out$Accounting_Family <- accounting_family_from_model(out$Model_ID, out$Model_Name)
  out
}

candidate_key <- function(x) {
  paste(x$Target_Space, x$Sample_Group, x$Model_ID, x$Heterogeneity_Variant, sep = "\r")
}

build_pooled_stack <- function(target, target_space, obs, models, source_root) {
  candidates <- models %>%
    filter(.data$Target_Space == target_space,
           .data$Sample_Group == "main_common",
           .data$Pooled_Only_Candidate == TRUE,
           .data$Included_In_Stack == TRUE) %>%
    arrange(.data$Model_ID, .data$Heterogeneity_Variant)
  if (nrow(candidates) < 2L) {
    return(list(weights = data.frame(), n_candidates = nrow(candidates), n_excluded_by_gate = NA_integer_))
  }
  eligible_keys <- candidate_key(candidates)
  obs_use <- obs %>%
    filter(.data$Target_Space == target_space,
           .data$Sample_Group == "main_common",
           .data$Primary_Row_Target_Inclusion == TRUE)
  obs_use$.Candidate_Key <- candidate_key(obs_use)
  score_list <- list()
  meta_keys <- character()
  reference_obs_ids <- NULL
  for (i in seq_len(nrow(candidates))) {
    row <- candidates[i, ]
    ckey <- eligible_keys[[i]]
    one <- obs_use[obs_use$.Candidate_Key == ckey, , drop = FALSE]
    one <- one[order(one$Observation_ID), , drop = FALSE]
    if (!nrow(one)) next
    if (any(!is.finite(one$LPD))) next
    if (is.null(reference_obs_ids)) {
      reference_obs_ids <- one$Observation_ID
    } else if (!identical(reference_obs_ids, one$Observation_ID)) {
      stop("[BLOCKER] SE10 pooled-only score vectors have non-identical observation IDs for ", target, " / ", target_space)
    }
    model_key <- model_key_sampled(row$Model_ID, row$Target_Space, row$Sample_Group, row$Heterogeneity_Variant, paste0("_se10_", target))
    score_list[[model_key]] <- one$LPD
    meta_keys <- c(meta_keys, model_key)
  }
  if (length(score_list) < 2L) {
    return(list(weights = data.frame(), n_candidates = length(score_list), n_excluded_by_gate = NA_integer_))
  }
  expected_n <- length(score_list[[1]])
  if (any(vapply(score_list, length, integer(1)) != expected_n)) {
    stop("[BLOCKER] SE10 pooled-only score vectors have unequal lengths for ", target, " / ", target_space)
  }
  lpd_matrix <- do.call(cbind, score_list)
  colnames(lpd_matrix) <- names(score_list)
  weights <- optimize_stacking_from_lpd(lpd_matrix)
  singleton_elpd <- colSums(lpd_matrix)
  meta_idx <- match(names(weights), meta_keys)
  out <- candidates[meta_idx, , drop = FALSE] %>%
    mutate(
      Target = target,
      Model_Key = names(weights),
      Weight = as.numeric(weights),
      ELPD = as.numeric(singleton_elpd[names(weights)])
    ) %>%
    arrange(desc(.data$Weight)) %>%
    select(Target, Target_Space, Model_Key, Model_ID, Model_Name, Accounting_Family,
           Heterogeneity_Variant, Weight, ELPD, Reliability_Flag, Included_In_Stack,
           Source_Run_Root)
  list(weights = out, n_candidates = nrow(candidates), n_excluded_by_gate = NA_integer_)
}

write_weight_table <- function(x, file_name) {
  write_csv_safely(x, file.path(tables_dir, file_name), row.names = FALSE, fileEncoding = "UTF-8")
}

family_weights <- function(weights, prefix) {
  if (!nrow(weights)) {
    return(data.frame(Target_Space = character(), Accounting_Family = character(), Weight = numeric()))
  }
  weights %>%
    group_by(.data$Target_Space, .data$Accounting_Family) %>%
    summarise(Weight = sum(.data$Weight, na.rm = TRUE), .groups = "drop") %>%
    rename(!!prefix := .data$Weight)
}

make_decision <- function(family_shift, row_weights, grouped_weights, row_root, grouped_root) {
  spaces <- sort(unique(c(row_weights$Target_Space, grouped_weights$Target_Space)))
  bind_rows(lapply(spaces, function(space) {
    fs <- family_shift[family_shift$Target_Space == space, , drop = FALSE]
    rw <- row_weights[row_weights$Target_Space == space, , drop = FALSE]
    gw <- grouped_weights[grouped_weights$Target_Space == space, , drop = FALSE]
    if (nrow(rw) < 2L || nrow(gw) < 2L) {
      decision <- "BLOCKED_NO_POOLED_CANDIDATES"
      interpretation <- "SE10 could not compare pooled-only stacks because one validation target has fewer than two source-gated pooled candidates."
    } else {
      top_row <- rw[which.max(rw$Weight), , drop = FALSE]
      top_grouped <- gw[which.max(gw$Weight), , drop = FALSE]
      l1 <- sum(abs(fs$Weight_Difference), na.rm = TRUE)
      max_shift <- if (nrow(fs)) max(fs$Abs_Weight_Difference, na.rm = TRUE) else NA_real_
      family_changed <- !identical(top_row$Accounting_Family[[1]], top_grouped$Accounting_Family[[1]])
      if (family_changed || isTRUE(l1 >= material_l1_shift) || isTRUE(max_shift >= material_max_family_shift)) {
        decision <- "ACCOUNTING_FAMILY_SHIFT_REMAINS_WITHOUT_FIRMRE"
        interpretation <- "Within pooled-only candidates, row-level exact K-fold and grouped-firm K-fold still assign materially different accounting-family weights. This indicates that the validation-target effect is not solely driven by Firm-RE availability."
      } else if (is.finite(l1) && is.finite(max_shift)) {
        decision <- "JOINT_SHIFT_DOMINATED_BY_HETEROGENEITY"
        interpretation <- "Within pooled-only candidates, row-level exact K-fold and grouped-firm K-fold produce similar accounting-family weights. This suggests that the full row-versus-grouped shift is primarily driven by heterogeneity structure rather than accounting-family substitution among pooled models."
      } else {
        decision <- "INCONCLUSIVE"
        interpretation <- "The pooled-only comparison does not provide a stable directional conclusion under the available source gates and candidate set."
      }
    }
    top_row <- if (nrow(rw)) rw[which.max(rw$Weight), , drop = FALSE] else data.frame()
    top_grouped <- if (nrow(gw)) gw[which.max(gw$Weight), , drop = FALSE] else data.frame()
    data.frame(
      Target_Space = space,
      Top_Pooled_Model_Row = if (nrow(top_row)) top_row$Model_Key[[1]] else NA_character_,
      Top_Pooled_Model_Grouped = if (nrow(top_grouped)) top_grouped$Model_Key[[1]] else NA_character_,
      Top_Pooled_Family_Row = if (nrow(top_row)) top_row$Accounting_Family[[1]] else NA_character_,
      Top_Pooled_Family_Grouped = if (nrow(top_grouped)) top_grouped$Accounting_Family[[1]] else NA_character_,
      Top_Pooled_Model_Changed = if (nrow(top_row) && nrow(top_grouped)) top_row$Model_Key[[1]] != top_grouped$Model_Key[[1]] else NA,
      Top_Pooled_Family_Changed = if (nrow(top_row) && nrow(top_grouped)) top_row$Accounting_Family[[1]] != top_grouped$Accounting_Family[[1]] else NA,
      Pooled_Family_Weight_Shift_L1 = if (nrow(fs)) sum(abs(fs$Weight_Difference), na.rm = TRUE) else NA_real_,
      Pooled_Family_Weight_Shift_Max = if (nrow(fs)) max(fs$Abs_Weight_Difference, na.rm = TRUE) else NA_real_,
      Decision = decision,
      Interpretation = interpretation,
      Source_Row_KFold_Run_Root = row_root,
      Source_Grouped_KFold_Run_Root = grouped_root,
      stringsAsFactors = FALSE
    )
  }))
}

grouped_root <- resolve_run_root(
  "ACCRUAL_SE10_SOURCE_GROUPED_KFOLD_RUN_ROOT",
  file.path(output_root, "kfold_firm", "LATEST_COMPLETED_RUN.txt"),
  "grouped K-fold"
)
row_root <- resolve_run_root(
  "ACCRUAL_SE10_SOURCE_ROW_KFOLD_RUN_ROOT",
  file.path(output_root, "row_exact_kfold", "LATEST_COMPLETED_RUN.txt"),
  "row exact K-fold"
)

source_tables <- find_required_source_tables(grouped_root, row_root)
grouped_obs <- standardize_observation_scores(safe_read_csv(source_tables$grouped_obs), "SE10 grouped observation scores", grouped_root)
grouped_models <- standardize_model_scores(safe_read_csv(source_tables$grouped_model), "SE10 grouped model scores", grouped_root)
row_obs <- standardize_observation_scores(safe_read_csv(source_tables$row_obs), "SE10 row observation scores", row_root)
row_models <- standardize_model_scores(safe_read_csv(source_tables$row_model), "SE10 row model scores", row_root)

pooled_counts <- data.frame(
  Target = c("row", "grouped"),
  Pooled_Candidates_Before_Gate = c(sum(row_models$Pooled_Only_Candidate), sum(grouped_models$Pooled_Only_Candidate)),
  Pooled_Candidates_After_Gate = c(sum(row_models$Pooled_Only_Candidate & row_models$Included_In_Stack),
                                  sum(grouped_models$Pooled_Only_Candidate & grouped_models$Included_In_Stack)),
  Pooled_Candidates_Excluded_By_Gate = c(sum(row_models$Pooled_Only_Candidate & !row_models$Included_In_Stack),
                                        sum(grouped_models$Pooled_Only_Candidate & !grouped_models$Included_In_Stack)),
  stringsAsFactors = FALSE
)
write_csv_safely(pooled_counts, file.path(logs_dir, "pooled_candidate_counts.csv"), row.names = FALSE, fileEncoding = "UTF-8")

if (any(pooled_counts$Pooled_Candidates_After_Gate < 2L)) {
  write_weight_table(data.frame(), "table_se10_pooled_only_weights_grouped_ex_post.csv")
  write_weight_table(data.frame(), "table_se10_pooled_only_weights_grouped_no_lookahead.csv")
  write_weight_table(data.frame(), "table_se10_pooled_only_weights_row_ex_post.csv")
  write_weight_table(data.frame(), "table_se10_pooled_only_weights_row_no_lookahead.csv")
  write_csv_safely(data.frame(), file.path(tables_dir, "table_se10_pooled_only_row_vs_grouped_family_shift.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_blocked_decision("BLOCKED_NO_POOLED_CANDIDATES", "SE10 found fewer than two source-gated pooled candidates for row or grouped K-fold.", grouped_root, row_root)
  completed <- FALSE
} else {
  grouped_ep <- build_pooled_stack("grouped_firm_kfold", "ex_post", grouped_obs, grouped_models, grouped_root)$weights
  grouped_rt <- build_pooled_stack("grouped_firm_kfold", "real_time", grouped_obs, grouped_models, grouped_root)$weights
  row_ep <- build_pooled_stack("row_exact_kfold", "ex_post", row_obs, row_models, row_root)$weights
  row_rt <- build_pooled_stack("row_exact_kfold", "real_time", row_obs, row_models, row_root)$weights

  write_weight_table(grouped_ep, "table_se10_pooled_only_weights_grouped_ex_post.csv")
  write_weight_table(grouped_rt, "table_se10_pooled_only_weights_grouped_no_lookahead.csv")
  write_weight_table(row_ep, "table_se10_pooled_only_weights_row_ex_post.csv")
  write_weight_table(row_rt, "table_se10_pooled_only_weights_row_no_lookahead.csv")

  row_all <- bind_rows(row_ep, row_rt)
  grouped_all <- bind_rows(grouped_ep, grouped_rt)
  row_fam <- family_weights(row_all, "Row_Pooled_Only_Weight")
  grouped_fam <- family_weights(grouped_all, "Grouped_Pooled_Only_Weight")
  family_shift <- full_join(row_fam, grouped_fam, by = c("Target_Space", "Accounting_Family")) %>%
    mutate(
      Row_Pooled_Only_Weight = ifelse(is.na(.data$Row_Pooled_Only_Weight), 0, .data$Row_Pooled_Only_Weight),
      Grouped_Pooled_Only_Weight = ifelse(is.na(.data$Grouped_Pooled_Only_Weight), 0, .data$Grouped_Pooled_Only_Weight),
      Weight_Difference = .data$Grouped_Pooled_Only_Weight - .data$Row_Pooled_Only_Weight,
      Abs_Weight_Difference = abs(.data$Weight_Difference)
    ) %>%
    arrange(.data$Target_Space, desc(.data$Abs_Weight_Difference), .data$Accounting_Family)
  write_csv_safely(family_shift, file.path(tables_dir, "table_se10_pooled_only_row_vs_grouped_family_shift.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  decision <- make_decision(family_shift, row_all, grouped_all, row_root, grouped_root)
  write_csv_safely(decision, file.path(tables_dir, "table_se10_pooled_only_decision.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  completed <- all(file.exists(file.path(tables_dir, c(
    "table_se10_pooled_only_weights_grouped_ex_post.csv",
    "table_se10_pooled_only_weights_grouped_no_lookahead.csv",
    "table_se10_pooled_only_weights_row_ex_post.csv",
    "table_se10_pooled_only_weights_row_no_lookahead.csv",
    "table_se10_pooled_only_row_vs_grouped_family_shift.csv",
    "table_se10_pooled_only_decision.csv"
  ))))
}

manifest <- data.frame(
  Timestamp = as.character(Sys.time()),
  Working_Directory = getwd(),
  output_root = output_root,
  Source_Row_KFold_Run_Root = row_root,
  Source_Grouped_KFold_Run_Root = grouped_root,
  Row_Observation_Score_Table = source_tables$row_obs,
  Row_Model_Score_Table = source_tables$row_model,
  Grouped_Observation_Score_Table = source_tables$grouped_obs,
  Grouped_Model_Score_Table = source_tables$grouped_model,
  Pooled_Filter_Rule = "grepl('Pooled', Heterogeneity_Variant, ignore.case=TRUE) and exclude Firm RE / Random Intercept / firm random effect",
  Reliability_Gate_Rule = "Prefer included_in_stack/Included_In_Stack/MA12C_Included_In_Stack; otherwise reliability_flag in OK/CAUTION; never derive OK from completion status.",
  Material_L1_Shift = material_l1_shift,
  Material_Max_Family_Shift = material_max_family_shift,
  Completed = completed,
  stringsAsFactors = FALSE
)
write_csv_safely(manifest, file.path(logs_dir, "run_config_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (isTRUE(completed)) writeLines(se10_root, file.path(se10_root, "LATEST_COMPLETED_RUN.txt"))

cat("\n[SUCCESS] SE10 pooled-only sub-stacking sensitivity completed.\n")
cat("Output root:", se10_root, "\n")
cat("Completed:", completed, "\n")
phase_end("se10", "Pooled-only sub-stacking sensitivity")
