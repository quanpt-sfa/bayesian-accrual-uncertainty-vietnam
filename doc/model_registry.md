# Model registry

The active methodology uses a ten-model space, `M01` through `M10`.

## Core models

- `M01`: Jones model.
- `M02`: Modified Jones model.
- `M03`: Performance-adjusted Modified Jones / Kothari-style model.
- `M04`: Dechow-Dichev style total-accruals mapping.
- `M05`: McNichols integrated cash-flow and Jones specification.
- `M06`: McNichols plus performance control.
- `M07`: Ball-Shivakumar style asymmetry model.

## Extended models

- `M08`: extended performance and volatility model. Treated as secondary robustness because it requires rolling volatility variables.
- `M09`: no-lookahead real-time specification.
- `M10`: operating-cycle robustness model. It is secondary because it depends on operating-cycle availability and is not part of the main stacking space.

## Robustness status

`M08` and `M10` are available as robustness checks rather than core stacking models. Grouped K-fold manifest lists in script `13_grouped_kfold_firm.R` include `M02` in both ex-post and no-lookahead model sets.
