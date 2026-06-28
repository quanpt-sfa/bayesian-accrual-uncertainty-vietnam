# SE08C Base-R Refactor Report

## Root Cause

`scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R` loaded `dplyr` before sourcing `scripts/ma00_setup.R` and used tidyverse verbs throughout the collector. A clean R environment without `dplyr` therefore failed before shared setup and reproducibility checks could run.

## Files Changed

- `scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R`
- `tests/test_se08c_base_r_static.R`
- `tests/test_se08c_base_r_behavioral.R`

## Refactor Summary

SE08C now sources `scripts/ma00_setup.R` first and uses only base R for collection, grouping, joins, reshaping, reliability labels, stacking-weight construction, fold-local-vs-global comparisons, Firm-RE shift summaries, top-model comparisons, and decision tables.

The refactor replaces tidyverse operations with local base-R helpers:

- `bind_rows_base()`
- `aggregate_by_base()`
- `left_join_base()`
- `safe_min()`, `safe_max()`, `safe_mean()`, `safe_sd()`, `safe_sum()`
- narrow base-R reshaping logic for the SE08C comparison tables

Completed-run pin reading now reuses `read_single_line_no_bom()` so UTF-8 BOM and mojibake BOM markers do not corrupt pinned run paths.

## Self-Invocation Investigation

A repository search for `system`, `system2`, `shell`, `cmd.exe`, `Rscript`, `callr`, `processx`, and direct SE08C source/self-invocation found no SE08C code path that relaunches `scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R`.

`scripts/ma00_setup.R` only sources focused helper modules. The sourced helper modules do not call `Rscript` or `cmd.exe` for SE08C. The only startup-level process-launch candidate found in the repository is the project `.Rprofile`, which sources `renv/activate.R`; `renv/activate.R` contains bootstrap-time `system2()` logic used by renv itself. That is outside SE08C collector logic.

The initial lock implementation lived after `source("scripts/ma00_setup.R")`, which was too late to guard against any startup/setup relaunch behavior. The second patch moves the lock guard to the first non-comment executable expression in SE08C, before shared setup and before `phase_begin()`.

To make duplicate execution harmless and explicit, SE08C now creates a deterministic pre-setup lock independent of `output_root`:

```text
out/interim/winsor/sensitivity/fold_local_preprocessing/logs/se08c_collect.lock
```

The lock records the current PID, start time, `commandArgs()`, and working directory. If the lock exists, SE08C stops before sourcing setup with:

```text
[BLOCKER] se08c lock exists; refusing to start another collector. Remove the lock manually only after verifying no SE08C process is running. lock PID=<pid>; lock=<path>
```

The lock is removed in a `finally` block on normal or error exit.

The lock acquisition writes a temporary lock file and renames it into place, so concurrent collectors cannot silently overwrite each other's lock file. After setup is sourced, SE08C confirms that it is still using the deterministic top-level lock and does not create a second independent `logs_dir` lock.

SE08C also performs a best-effort duplicate-process self-check before setup. If the optional `ps` package is available, it counts running command lines matching `se08c_collect_fold_local_preprocessing_sensitivity.R` and stops with:

```text
[BLOCKER] duplicate se08c process detected
```

No shell command is used for this check.

## Stacking Guard And Fast Optimizer

SE08C now emits checkpoint logs around all four stacking blocks:

- grouped ex-post
- grouped real-time
- row ex-post
- row real-time

The previous guard still called the legacy `optimize_stacking_from_lpd()` path, which uses multi-start BFGS with finite-difference gradients and can become idle on Windows before writing the fold-local weight files.

The collector now defaults to:

```text
ACCRUAL_SE08C_STACKING_METHOD=fast_exact
```

`fast_exact` uses `optimize_stacking_from_lpd_fast()`, a softmax-parameterized optimizer with a numerically stable stacking objective and an analytic gradient. It uses at most two starts and `stats::optim(..., method = "BFGS", maxit = 500, reltol = 1e-8)`.

Allowed methods are:

- `fast_exact`: default analytic-gradient optimizer.
- `singleton`: deterministic best-singleton fallback.
- `pseudo_bma`: deterministic softmax over singleton ELPD values.
- `exact_legacy`: the old multi-start optimizer, only when explicitly requested.

For every grouped and row fold-local weight table, SE08C now writes stacking metadata:

- `Stacking_Method_Fold_Local`
- `Stacking_Fallback_Used`
- `Stacking_Convergence_Code`
- `Stacking_Objective`
- `Singleton_Objective`
- `Stacking_Context`

The optimizer compares the fitted mixture objective against the best-singleton ELPD. If the optimized mixture is worse than the best singleton within tolerance, SE08C falls back to the singleton solution and records this in the metadata.

## Output Compatibility

The collector preserves the existing output file names:

- `table_se08_fold_local_preprocessing_audit.csv`
- `table_se08_fold_local_cutoff_summary.csv`
- `table_se08_fold_local_standardization_summary.csv`
- `table_se08_grouped_fold_local_observation_scores.csv`
- `table_se08_row_fold_local_observation_scores.csv`
- `table_se08_grouped_fold_local_model_scores.csv`
- `table_se08_row_fold_local_model_scores.csv`
- `table_se08_grouped_fold_local_weights_ex_post.csv`
- `table_se08_grouped_fold_local_weights_no_lookahead.csv`
- `table_se08_row_fold_local_weights_ex_post.csv`
- `table_se08_row_fold_local_weights_no_lookahead.csv`
- `table_se08_fold_local_vs_global_weight_comparison.csv`
- `table_se08_fold_local_vs_global_firmre_shift_summary.csv`
- `table_se08_fold_local_vs_global_top_model_comparison.csv`
- `table_se08_fold_local_sensitivity_decision.csv`
- `logs/se08_fold_local_preprocessing_collect_manifest.csv`

No model fitting is triggered. SE08C remains a collector/post-processing stage and still blocks if required SE08B task results are incomplete.

## Validation Status

Static validation confirms SE08C contains no `library(dplyr)`, `dplyr::`, `%>%`, `tidyr::`, or `pivot_wider` usage.

Static validation also confirms SE08C contains no `system`, `system2`, `shell`, `cmd.exe`, `Rscript`, `callr`, `processx`, `.rs.restartR`, or `rstudioapi` self-invocation fragments.

A lightweight behavioral smoke test creates synthetic SE08A/SE08B-style manifest, status, RDS outputs, and primary global weight pins under a temporary output root, then runs SE08C end-to-end. The smoke test verifies all expected SE08C outputs and the collect manifest are written without installing tidyverse packages or refitting models.

A separate lock behavioral test writes `se08c_collect.lock` and verifies SE08C blocks immediately before setup with `[BLOCKER] se08c lock exists; refusing to start another collector`.

The second lock patch changed SE08C to fail closed on Windows: if `se08c_collect.lock` exists, SE08C refuses to start another collector and does not use `tools::pskill(signal = 0)` to decide whether the lock is stale. Stale-lock removal now requires:

```text
ACCRUAL_SE08C_CLEAR_STALE_LOCK=TRUE
```

The lock behavioral test verifies that an existing lock is not removed automatically and that explicit clear mode reaches the normal setup blocker.

`tests/test_se08c_fast_stacking_static_behavioral.R` verifies the new fast optimizer returns finite nonnegative weights summing to one, does not return an objective below the best singleton, and that SE08C calls the legacy optimizer only inside the explicit `exact_legacy` branch.

## Remaining Runtime Requirement

The production command:

```powershell
Rscript --vanilla scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R
```

will exit successfully only after SE08B has completed all required tasks with `SUCCESS` status and all required result RDS files exist. If SE08B outputs are incomplete, SE08C intentionally stops with a clear `[BLOCKER]`.
