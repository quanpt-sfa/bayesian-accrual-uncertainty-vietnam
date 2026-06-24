txt <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

with_temp_env <- function(values, code) {
  names_to_manage <- names(values)
  old <- Sys.getenv(names_to_manage, unset = NA_character_)
  on.exit({
    for (nm in names(old)) {
      if (is.na(old[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, as.list(stats::setNames(old[[nm]], nm)))
    }
  }, add = TRUE)
  for (nm in names(values)) {
    if (is.na(values[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, as.list(stats::setNames(values[[nm]], nm)))
  }
  force(code)
}

kfold_env_names <- c(
  "ACCRUAL_OUTPUT_ROOT",
  "ACCRUAL_INPUT_WINSOR_ROOT",
  "ACCRUAL_LOG_ROOT",
  "ACCRUAL_KFOLD_FIRM_MODE",
  "ACCRUAL_KFOLD_FIRM_CHAINS",
  "ACCRUAL_KFOLD_FIRM_CORES",
  "ACCRUAL_KFOLD_FIRM_ITER",
  "ACCRUAL_KFOLD_FIRM_WARMUP",
  "ACCRUAL_KFOLD_FIRM_ADAPT_DELTA",
  "ACCRUAL_KFOLD_FIRM_MAX_TREEDEPTH",
  "ACCRUAL_KFOLD_FIRM_REFRESH",
  "ACCRUAL_ROW_KFOLD_MODE",
  "ACCRUAL_ROW_KFOLD_CHAINS",
  "ACCRUAL_ROW_KFOLD_CORES",
  "ACCRUAL_ROW_KFOLD_ITER",
  "ACCRUAL_ROW_KFOLD_WARMUP",
  "ACCRUAL_ROW_KFOLD_ADAPT_DELTA",
  "ACCRUAL_ROW_KFOLD_MAX_TREEDEPTH",
  "ACCRUAL_ROW_KFOLD_REFRESH"
)

with_temp_env(stats::setNames(as.list(rep(NA_character_, length(kfold_env_names))), kfold_env_names), {
  source("scripts/ma00_setup.R")
  grouped_cfg <- accrual_kfold_config("grouped_firm")
  row_cfg <- accrual_kfold_config("row")
  for (cfg_name in c("grouped_cfg", "row_cfg")) {
    cfg <- get(cfg_name)
    if (!identical(cfg$chains, 4L) ||
        !identical(cfg$cores, 4L) ||
        !identical(cfg$iter, 12000L) ||
        !identical(cfg$warmup, 4000L) ||
        !isTRUE(all.equal(cfg$adapt_delta, 0.99, tolerance = 1e-12)) ||
        !identical(cfg$max_treedepth, 15L) ||
        !identical(cfg$refresh, 500L)) {
      stop(cfg_name, " FULL_MODE default must be production K-fold config 4/4/12000/4000/0.99/15/500.")
    }
  }

  temp_root <- tempfile("kfold_runtime_config_")
  out_root <- file.path(temp_root, "out")
  input_root <- file.path(temp_root, "winsor")
  dir.create(file.path(input_root, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_root, "tables"), recursive = TRUE, showWarnings = FALSE)
  formulas <- data.frame(
    Model_ID = "M01",
    Model_Name = "M01 test",
    Target_Space = "ex_post",
    Sample_Group = "main_common",
    Heterogeneity_Variant = "Pooled (Industry + Year FE)",
    Target_Sample = "sample_ex_post.csv",
    brms_Formula = "TA_scaled ~ 1 + delta_revenue + ppe + factor(industry) + factor(year)",
    Main_Stack_Inclusion = TRUE,
    Secondary_Robustness = FALSE,
    Reason = "test fixture",
    stringsAsFactors = FALSE
  )
  sample_df <- data.frame(
    company = paste0("F", 1:6),
    year = rep(2018:2020, length.out = 6),
    industry = rep(c("A", "B"), length.out = 6),
    TA_scaled = seq(-0.3, 0.2, length.out = 6),
    delta_revenue = seq(0.1, 0.6, length.out = 6),
    ppe = seq(1.1, 1.6, length.out = 6),
    stringsAsFactors = FALSE
  )
  write.csv(formulas, file.path(out_root, "tables", "table_named_model_formulas_winsor.csv"), row.names = FALSE)
  write.csv(sample_df, file.path(input_root, "tables", "sample_ex_post.csv"), row.names = FALSE)

  with_temp_env(list(
    ACCRUAL_OUTPUT_ROOT = out_root,
    ACCRUAL_INPUT_WINSOR_ROOT = input_root,
    ACCRUAL_LOG_ROOT = file.path(temp_root, "logs")
  ), {
    source("scripts/ma12a_plan_grouped_kfold_firm.R")
    source("scripts/ma13a_plan_row_level_exact_kfold.R")
  })

  grouped_manifest <- read.csv(file.path(out_root, "tables", "table_ma12_grouped_kfold_task_manifest.csv"), stringsAsFactors = FALSE)
  row_manifest <- read.csv(file.path(out_root, "tables", "table_ma13_row_kfold_task_manifest.csv"), stringsAsFactors = FALSE)
  required_cols <- c("sampler_profile", "run_mode", "config_source", "chains", "cores", "iter",
                     "warmup", "adapt_delta", "max_treedepth", "refresh", "backend")
  for (manifest_name in c("grouped_manifest", "row_manifest")) {
    manifest <- get(manifest_name)
    missing_cols <- setdiff(required_cols, names(manifest))
    if (length(missing_cols)) stop(manifest_name, " missing sampler provenance columns: ", paste(missing_cols, collapse = ", "))
    if (any(manifest$chains != 4L) ||
        any(manifest$cores != 4L) ||
        any(manifest$iter != 12000L) ||
        any(manifest$warmup != 4000L) ||
        any(abs(manifest$adapt_delta - 0.99) > 1e-12) ||
        any(manifest$max_treedepth != 15L) ||
        any(manifest$refresh != 500L) ||
        any(manifest$run_mode != "FULL_MODE")) {
      stop(manifest_name, " did not write production exact K-fold sampler defaults.")
    }
  }
})

ma00 <- txt("scripts/ma00_setup.R")
for (fragment in c("accrual_production_sampler_defaults <- function", "accrual_assert_kfold_manifest_matches_config <- function", "accrual_assert_reusable_fit_metadata <- function", "ACCRUAL_FORCE_REFIT")) {
  if (!grepl(fragment, ma00, fixed = TRUE)) stop("ma00 missing centralized K-fold runtime guard fragment: ", fragment)
}
for (path in c("scripts/ma12b_fit_grouped_kfold_firm_workers.R", "scripts/ma13b_fit_row_level_exact_kfold_workers.R")) {
  body <- txt(path)
  for (fragment in c("accrual_assert_kfold_manifest_matches_config", "accrual_assert_reusable_fit_metadata")) {
    if (!grepl(fragment, body, fixed = TRUE)) stop(path, " missing stale manifest/fit guard fragment: ", fragment)
  }
}

cat("test_kfold_runtime_config_provenance_static.R passed\n")
