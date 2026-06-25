# Replication instructions

## Baseline DA

1. Place the workbook at `data/raw/data.xlsx` or set `ACCRUAL_DATA_PATH`.
2. Run `Rscript run.R --dry-run` to inspect the main Chapter 3 plan.
3. Run `Rscript run.R` for the main Chapter 3 pipeline, and set `ACCRUAL_RUN_HEAVY=TRUE` when you are ready to fit models and produce full baseline outputs.
4. `ma10` constructs PSIS/LOO DA as secondary. `ma14` constructs primary exact-KFoldW DA from explicit exact K-fold run roots or completed grouped and row exact K-fold pins, and `ma15` audits finite DA outputs before RQ2/export.
5. `ACCRUAL_ACCRUALS_ROOT` defaults to `accruals`, so the baseline DA dependency is `accruals/baseline/final_uncertainty_adjusted_accruals_winsor.csv` unless overridden.
6. Execution configuration is centralized in `scripts/ma00_setup.R`: use `accrual_base_seed()`, `accrual_seed()`, `accrual_seed_for()`, `set_accrual_seed()`, `accrual_sampler_config()`, `accrual_kfold_config()`, and `main_model_ids_for_space()` as the source of truth. The pipeline uses one canonical seed, `ACCRUAL_SEED`, with default `42`, and helper-owned seed setting is the only accepted RNG entrypoint.
7. Grouped exact K-fold does not have a separate seed, row exact K-fold does not have a separate seed, and sensitivity/simulation branches derive any deterministic internal offsets from the same canonical base seed.
8. Branch-specific seed env vars (`ACCRUAL_BASELINE_SEED`, `ACCRUAL_KFOLD_FIRM_SEED`, `ACCRUAL_ROW_KFOLD_SEED`, `ACCRUAL_SENS_SEED`, `ACCRUAL_SIM_SEED`) are deprecated and blocked if they differ from `ACCRUAL_SEED`, to avoid branch-specific tuning or cherry-picking concerns.
9. Split collectors `ma12c` and `ma13c` own `LATEST_COMPLETED_RUN.txt` pins after primary-eligible completed exact refit runs. Row exact K-fold may update the pin for either a full unfiltered run or an explicit full primary no-lookahead run covering `real_time` models M01, M02, M03, M07, M09 and all K=5 folds.
10. `LATEST_RUN.txt` is operational only; primary downstream steps use explicit root environment variables or `LATEST_COMPLETED_RUN.txt`.
11. Split exact K-fold collectors write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.
12. `ma14` writes exact-KFold DA source, draw-hash, IO, and model-inclusion manifests under `out/interim/winsor/tables`. It refuses old or stale exact K-fold run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. `table_model_primary_inclusion_gate.csv` controls whether MCMC `REVIEW`/`CAUTION` models are retained with `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS` or excluded.
13. Primary model helpers return M01-M07 for ex-post and M01, M02, M03, M07, M09 for real-time/no-lookahead. M08/M10 remain secondary/robustness unless a documented secondary flow includes them; M11/M12 remain excluded from active primary helpers.
14. Final baseline DA outputs are written to `accruals/baseline`; exact-KFold DA provenance outputs are written under `out/interim/winsor/tables`. The centralized execution registry is written to `out/manifests/method_design/execution_config_registry.csv`.

## Heavy brms worker architecture

All heavy independent brms fit workloads use the shared worker architecture from `scripts/ma00_setup.R`. Worker stages use `accrual_fit_worker_config()` and `accrual_run_task_pool()` and may write only task-local fit, prediction, metadata, result, and log artifacts. Collectors own shared tables, validation scores, stacking weights, completed-run pins, reports, and manuscript-facing outputs.

The split stages are:

- `ma09a`/`ma09b`/`ma09c` for secondary PSIS/LOO save-pars refits and stacking collection.
- `ma12a`/`ma12b`/`ma12c` for primary exact grouped-firm K-fold.
- `ma13a`/`ma13b`/`ma13c` for primary exact row-level K-fold.
- `se02a`/`se02b`/`se02c` for sensitivity prior-scenario refits.
- `si03a`/`si03b`/`si03c` and `si04a`/`si04b`/`si04c` for brms simulation branches.
- `di08a`/`di08b`/`di08c` for diagnostic-only sampler calibration.

The production worker policy comes from `accrual_run_profile_registry()`. The current workflow is split into numbered 10w4c profiles: run `run_01_main_production_10w4c.ps1` first, then run downstream sensitivity, diagnostics, or simulation profiles only after `BASELINE_MA17_COMPLETE.txt` and the latest-main pointer exist. No validation-target semantics, formulas, priors, likelihoods, simulation DGPs, or parameter recovery estimands are changed by the split.

## Sensitivity analysis

1. Complete the baseline setup and winsorized inputs first.
2. Run `Rscript run.R sensitivity --dry-run` to inspect the sensitivity sequence.
3. For full scenario refits, enable heavy execution and optionally set `ACCRUAL_SENS_SCENARIO` to restrict to one scenario.
4. Final scenario outputs are written to `accruals/sensitivity/baseline`, `accruals/sensitivity/tight`, and `accruals/sensitivity/wide`.

## Validation reports

1. Primary validation is produced by `ma16` from the exact row-KFold DA file `out/interim/winsor/tables/final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv`, after the finite DA and model-inclusion gates have run. PSIS/LOO DA from `ma10` is secondary validation only.
2. Sensitivity validation is produced by `se06` and summarized by `se07`.
3. Manuscript export requires `table_DA_finite_gate_decision.csv`, `table_model_primary_inclusion_gate.csv`, and `table_new_firm_predictive_integration_decision.csv`; failed gates are not represented as primary RQ2 evidence.
4. If the new-firm audit returns `PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES`, export stops unless `ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS=TRUE`; with the override, affected tail quantities remain suppressed/non-primary.
5. Final narrative report artifacts are written under `reports/`.
