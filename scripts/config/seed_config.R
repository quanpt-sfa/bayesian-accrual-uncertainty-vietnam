# -----------------------------------------------------------------------------
# Canonical RNG seed helpers
# Sourced by scripts/ma00_setup.R compatibility facade.
# -----------------------------------------------------------------------------

accrual_base_seed <- function() {
  env_int("ACCRUAL_SEED", 42L, min = 0L)
}

accrual_seed <- function(kind = c("baseline", "grouped_kfold", "row_kfold", "sensitivity", "simulation"), default = NULL) {
  kind <- match.arg(kind)
  base <- accrual_base_seed()
  legacy_env <- switch(
    kind,
    baseline = "ACCRUAL_BASELINE_SEED",
    grouped_kfold = "ACCRUAL_KFOLD_FIRM_SEED",
    row_kfold = "ACCRUAL_ROW_KFOLD_SEED",
    sensitivity = "ACCRUAL_SENS_SEED",
    simulation = "ACCRUAL_SIM_SEED"
  )
  if (!is.null(default)) {
    default_value <- suppressWarnings(as.integer(default))
    if (is.na(default_value)) {
      stop("[BLOCKER] Invalid deprecated default override for accrual_seed(", kind, "): ", default)
    }
    if (!identical(default_value, base)) {
      stop("[BLOCKER] Deprecated default override for accrual_seed(", kind, ")=", default_value,
           " differs from canonical ACCRUAL_SEED=", base, ".")
    }
  }

  legacy_raw <- Sys.getenv(legacy_env, unset = "")
  if (nzchar(legacy_raw)) {
    legacy_value <- suppressWarnings(as.integer(legacy_raw))
    if (is.na(legacy_value)) {
      stop("[BLOCKER] Invalid integer seed in deprecated ", legacy_env, ": ", legacy_raw)
    }
    if (!identical(legacy_value, base)) {
      stop("[BLOCKER] Branch-specific seed ", legacy_env, "=", legacy_value,
           " differs from canonical ACCRUAL_SEED=", base,
           ". Use one common seed to avoid branch-specific tuning/cherry-picking risk.")
    }
    warning("[WARNING] ", legacy_env, " is deprecated. Use ACCRUAL_SEED only.", call. = FALSE)
  }

  base
}

normalize_accrual_seed_offset <- function(offset = 0L, context = "unknown") {
  offset_value <- suppressWarnings(as.integer(offset))
  if (length(offset_value) != 1 || is.na(offset_value)) {
    stop("[BLOCKER] Seed offset for ", context, " must be one integer value. Got: ", paste(offset, collapse = ", "))
  }
  offset_value
}

accrual_seed_for <- function(context, offset = 0L) {
  if (missing(context) || !nzchar(trimws(as.character(context)))) {
    stop("[BLOCKER] accrual_seed_for() requires a non-empty context label.")
  }
  accrual_base_seed() + normalize_accrual_seed_offset(offset, context)
}

set_accrual_seed <- function(context, offset = 0L) {
  seed_value <- accrual_seed_for(context, offset = offset)
  base::set.seed(seed_value)
  invisible(seed_value)
}

set_accrual_effective_seed <- function(seed, context = "unknown") {
  seed_value <- suppressWarnings(as.integer(seed))
  if (length(seed_value) != 1L || is.na(seed_value)) {
    stop("[BLOCKER] Effective seed for ", context, " must be one integer value. Got: ",
         paste(seed, collapse = ", "))
  }
  base::set.seed(seed_value)
  invisible(seed_value)
}

accrual_rng_metadata_list <- function(context = "global", offset = 0L) {
  list(
    RNG_Context = context,
    RNG_Offset = normalize_accrual_seed_offset(offset, context),
    Canonical_Seed = accrual_base_seed(),
    Effective_Seed = accrual_seed_for(context, offset),
    RNG_Source = "scripts/ma00_setup.R"
  )
}

accrual_rng_metadata <- function(context = "global", offset = 0L) {
  as.data.frame(accrual_rng_metadata_list(context, offset), stringsAsFactors = FALSE)
}

