# rstan cache metadata follow-up

This follow-up records sampler provenance for cached brms refits without changing model formulas, priors, seeds, likelihoods, diagnostics gates, or output paths.

## Changes

- `scripts/ma12_grouped_kfold_firm.R`
  - Grouped K-fold run and task manifests now record `Chains`, `Cores`, `Iter`, `Warmup`, `Adapt_Delta`, `Max_Treedepth`, and `Backend = "rstan"`.
  - Score cache results now include a separate `sampler_provenance` object with `chains`, `cores`, `iter`, `warmup`, `adapt_delta`, `max_treedepth`, and `backend`.

- `scripts/ma13_row_level_exact_kfold.R`
  - Row-level K-fold planned-task and run manifests now record `Chains`, `Cores`, `Iter`, `Warmup`, `Adapt_Delta`, `Max_Treedepth`, and `Backend = "rstan"`.
  - Score cache results now include a separate `sampler_provenance` object with `chains`, `cores`, `iter`, `warmup`, `adapt_delta`, `max_treedepth`, and `backend`.

- `scripts/sensitivity/se02_refit_prior_scenarios.R`
  - Sensitivity refit metadata now records `backend = "rstan"` in addition to the existing sampler controls.

## Cache invalidation choice

For K-fold caches, `sampler_provenance` is intentionally stored separately from the existing `cache_meta` matching key. This means changing `cores` alone does not force a K-fold refit. The reason is computational: under the current rstan backend, `cores` controls between-chain worker allocation but does not alter formulas, priors, seeds, likelihoods, folds, or requested sampler draws.

For sensitivity refits, the existing design already treats sampler metadata in the per-fit metadata CSV as cache-matching information. The follow-up therefore records `backend = "rstan"` in that metadata alongside the existing sampler fields.
