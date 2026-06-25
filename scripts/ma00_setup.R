# -----------------------------------------------------------------------------
# Script: ma00_setup.R
# Purpose: Compatibility facade for shared accrual pipeline setup.
#
# Public helpers live in focused modules under scripts/config, scripts/runtime,
# and scripts/utils. Existing pipeline scripts should continue to source this file.
# The source guard keeps repeated sourcing safe for PSOCK workers and test files.
# -----------------------------------------------------------------------------

.ma00_source_modules <- function(paths, envir = parent.frame()) {
  for (path in paths) {
    sys.source(path, envir = envir)
  }
  invisible(paths)
}

.ma00_modules <- c(
  file.path("scripts", "config", "env_helpers.R"),
  file.path("scripts", "config", "path_config.R"),
  file.path("scripts", "config", "seed_config.R"),
  file.path("scripts", "config", "method_registries.R"),
  file.path("scripts", "config", "sampler_config.R"),
  file.path("scripts", "config", "run_profile_registry.R"),
  file.path("scripts", "utils", "io_helpers.R"),
  file.path("scripts", "utils", "baseline_marker.R"),
  file.path("scripts", "runtime", "worker_pool.R"),
  file.path("scripts", "utils", "analysis_helpers.R")
)

invisible(.ma00_source_modules(.ma00_modules))
