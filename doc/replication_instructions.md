# Replication instructions

## Baseline DA

1. Place the workbook at `data/raw/data.xlsx` or set `V3_DATA_PATH`.
2. Run `Rscript run.R baseline` for a dry-run oriented baseline pass.
3. Set `V3_DRY_RUN=FALSE` and `V3_RUN_HEAVY=TRUE` when you are ready to fit models and produce full baseline outputs.
4. Final baseline DA outputs are written to `accruals/baseline`.

## Sensitivity analysis

1. Complete the baseline setup and winsorized inputs first.
2. Run `Rscript run.R sensitivity` to exercise the sensitivity sequence in dry-run mode.
3. For full scenario refits, enable heavy execution and optionally set `V3_SENS_SCENARIO` to restrict to one scenario.
4. Final scenario outputs are written to `accruals/sensitivity/baseline`, `accruals/sensitivity/tight`, and `accruals/sensitivity/wide`.

## Validation reports

1. Baseline validation is produced by script `21` after DA construction.
2. Sensitivity validation is produced by script `19` and summarized by script `20`.
3. Final narrative report artifacts are written under `reports/`.
