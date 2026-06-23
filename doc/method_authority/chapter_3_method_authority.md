# Chapter 3 Method Authority

This file records the tracked method authority used by static alignment tests.
It is intentionally stored under `doc/` rather than `reports/` so it remains
available in clean checkouts where generated or draft report files are ignored.

## Prior Predictive Gates

The Chapter 3 prior predictive rule is that mass outside (|TA|>2) should not exceed 1%.

For the implementation in `scripts/ma00_setup.R`, the PASS thresholds are:

- |TA| > 1 PASS threshold = 0.05
- |TA| > 2 PASS threshold = 0.01
- range-ratio PASS threshold = 3.00
