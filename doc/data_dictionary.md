# Data dictionary

## Canonical workbook

The repository expects the raw workbook at `data/raw/data.xlsx` unless `V3_DATA_PATH` is set.

## Sheets

- `Sheet1`: firm-year financial data used to build common analysis samples.
- `Sheet2`: metadata used for joins such as industry or firm descriptors.

## Required `Sheet1` columns

- `company`
- `year`
- `A`
- `NI`
- `REV`
- `CFO`
- `REC`
- `PPE`
- `ROA`
- `COGS`
- `INV`

## Metadata expectations

`Sheet2` should contain company-level metadata, including industry mapping. The source scripts join metadata using company identifiers, so the metadata sheet should preserve the original firm code column used in the source workbook.
