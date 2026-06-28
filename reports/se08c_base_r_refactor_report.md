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

The lock records the current PID, start time, `commandArgs()`, and working directory. If the lock exists and the recorded PID is alive, SE08C stops before sourcing setup with:

```text
[BLOCKER] se08c is already running; lock PID=<pid>; lock=<path>
```

The lock is removed in a `finally` block on normal or error exit.

The lock acquisition writes a temporary lock file and renames it into place, so concurrent collectors cannot silently overwrite each other's lock file. After setup is sourced, SE08C confirms that it is still using the deterministic top-level lock and does not create a second independent `logs_dir` lock.

SE08C also performs a best-effort duplicate-process self-check before setup. If the optional `ps` package is available, it counts running command lines matching `se08c_collect_fold_local_preprocessing_sensitivity.R` and stops with:

```text
[BLOCKER] duplicate se08c process detected
```

No shell command is used for this check.

## Stacking Guard

SE08C now emits checkpoint logs around all four stacking blocks:

- grouped ex-post
- grouped real-time
- row ex-post
- row real-time

The stacking optimizer is wrapped by `optimize_stacking_guarded()`, which applies `ACCRUAL_SE08C_STACKING_TIMEOUT_SECONDS` and falls back to the best singleton ELPD model if optimization fails or times out. This prevents the collector from waiting indefinitely inside a stacking optimization call.

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

A separate lock behavioral test writes `se08c_collect.lock` with the current live PID and verifies SE08C blocks immediately before setup with `[BLOCKER] se08c is already running`.

## Remaining Runtime Requirement

The production command:

```powershell
Rscript --vanilla scripts/sensitivity/se08c_collect_fold_local_preprocessing_sensitivity.R
```

will exit successfully only after SE08B has completed all required tasks with `SUCCESS` status and all required result RDS files exist. If SE08B outputs are incomplete, SE08C intentionally stops with a clear `[BLOCKER]`.
