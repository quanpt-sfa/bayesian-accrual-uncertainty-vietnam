# Raw data

The canonical local workbook path for this repository is `data/raw/data.xlsx`.

This file is treated as private by default and is ignored by Git. If you intend to publish the workbook, remove the ignore rule only after confirming that the data can be made public.

Expected workbook layout:

- `Sheet1`: firm-year financial observations.
- `Sheet2`: metadata and industry mapping.

To use a workbook stored elsewhere, set `ACCRUAL_DATA_PATH` before running the pipeline.
