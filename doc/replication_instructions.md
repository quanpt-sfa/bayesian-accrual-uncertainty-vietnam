# Replication instructions

## Baseline DA

1. Place the workbook at `data/raw/data.xlsx` or set `ACCRUAL_DATA_PATH`.
2. Run `Rscript run.R --dry-run` to inspect the main Chapter 3 plan.
3. Run `Rscript run.R` for the main Chapter 3 pipeline, and set `ACCRUAL_RUN_HEAVY=TRUE` when you are ready to fit models and produce full baseline outputs.
4. Script `10` constructs PSIS/LOO DA as secondary. Script `31` constructs primary exact-KFoldW DA from explicit exact K-fold run roots or completed grouped and row exact K-fold pins, and script `32` audits finite DA outputs before RQ2/export.
5. `ACCRUAL_ACCRUALS_ROOT` defaults to `accruals`, so the baseline DA dependency is `accruals/baseline/final_uncertainty_adjusted_accruals_winsor.csv` unless overridden.
6. Scripts `13` and `28` write `LATEST_COMPLETED_RUN.txt` only after primary-eligible completed exact refit runs. Script `28` may update the pin for either a full unfiltered row exact K-fold or an explicit full primary no-lookahead run covering `real_time` models M01, M02, M03, M07, M09 and all K=5 folds.
7. `LATEST_RUN.txt` is operational only; primary downstream steps use explicit root environment variables or `LATEST_COMPLETED_RUN.txt`.
8. Scripts `13` and `28` write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.
9. Script `31` writes exact-KFold DA source, draw-hash, IO, and model-inclusion manifests under `out/interim/winsor/tables`. It refuses old or stale exact K-fold run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. `table_model_primary_inclusion_gate.csv` controls whether MCMC `REVIEW`/`CAUTION` models are retained with `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS` or excluded.
10. Final baseline DA outputs are written to `accruals/baseline`; exact-KFold DA provenance outputs are written under `out/interim/winsor/tables`.

## Sensitivity analysis

1. Complete the baseline setup and winsorized inputs first.
2. Run `Rscript run.R sensitivity` to exercise the sensitivity sequence in dry-run mode.
3. For full scenario refits, enable heavy execution and optionally set `ACCRUAL_SENS_SCENARIO` to restrict to one scenario.
4. Final scenario outputs are written to `accruals/sensitivity/baseline`, `accruals/sensitivity/tight`, and `accruals/sensitivity/wide`.

## Validation reports

1. Primary validation is produced by script `21` from the exact row-KFold DA file `out/interim/winsor/tables/final_uncertainty_adjusted_accruals_exact_kfold_row_winsor.csv`, after the finite DA and model-inclusion gates have run. PSIS/LOO DA from script `10` is secondary validation only.
2. Sensitivity validation is produced by script `19` and summarized by script `20`.
3. Manuscript export requires `table_DA_finite_gate_decision.csv`, `table_model_primary_inclusion_gate.csv`, and `table_new_firm_predictive_integration_decision.csv`; failed gates are not represented as primary RQ2 evidence.
4. If the new-firm audit returns `PRIMARY_SUPPRESSION_REQUIRED_FOR_UNVERIFIED_FIRMRE_OUT_OF_FIRM_QUANTITIES`, export stops unless `ACCRUAL_ALLOW_NEW_FIRM_SUPPRESSED_TAIL_FLAGS=TRUE`; with the override, affected tail quantities remain suppressed/non-primary.
5. Final narrative report artifacts are written under `reports/`.
