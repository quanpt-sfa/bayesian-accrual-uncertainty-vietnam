# Replication instructions

## Baseline DA

1. Place the workbook at `data/raw/data.xlsx` or set `ACCRUAL_DATA_PATH`.
2. Run `Rscript run.R --dry-run` to inspect the main Chapter 3 plan.
3. Run `Rscript run.R` for the main Chapter 3 pipeline, and set `ACCRUAL_RUN_HEAVY=TRUE` when you are ready to fit models and produce full baseline outputs.
4. Script `10` constructs PSIS/LOO DA as secondary. Script `31` constructs primary exact-KFoldW DA from completed grouped and row exact K-fold pins, and script `32` audits finite DA outputs before RQ2/export.
5. `ACCRUAL_ACCRUALS_ROOT` defaults to `accruals`, so the baseline DA dependency is `accruals/baseline/final_uncertainty_adjusted_accruals_winsor.csv` unless overridden.
6. Scripts `13` and `28` write `LATEST_COMPLETED_RUN.txt` only after primary-eligible completed exact refit runs. Script `28` may update the pin for either a full unfiltered row exact K-fold or an explicit full primary no-lookahead run covering `real_time` models M01, M02, M03, M07, M09 and all K=5 folds.
7. Final baseline DA outputs are written to `accruals/baseline`; exact-KFold DA provenance outputs are written under `out/interim/winsor/tables`.

## Sensitivity analysis

1. Complete the baseline setup and winsorized inputs first.
2. Run `Rscript run.R sensitivity` to exercise the sensitivity sequence in dry-run mode.
3. For full scenario refits, enable heavy execution and optionally set `ACCRUAL_SENS_SCENARIO` to restrict to one scenario.
4. Final scenario outputs are written to `accruals/sensitivity/baseline`, `accruals/sensitivity/tight`, and `accruals/sensitivity/wide`.

## Validation reports

1. Baseline validation is produced by script `21` after DA construction and after the finite DA gate has run in the main pipeline.
2. Sensitivity validation is produced by script `19` and summarized by script `20`.
3. Manuscript export requires both `table_DA_finite_gate_decision.csv` and `table_new_firm_predictive_integration_decision.csv`; failed gates are not represented as primary RQ2 evidence.
4. Final narrative report artifacts are written under `reports/`.
