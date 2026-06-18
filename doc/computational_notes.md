# Computational notes

- Full-sample baseline `brms` fits in script `07` use 4 chains, 4000 iterations, and 1000 warmup iterations. This 4000/1000 baseline protocol is intentional, not an error.
- Exact K-fold refits in scripts `13` and `28` use 4 chains, 3000 iterations, and 1000 warmup iterations because they are repeated across validation folds and are used for method-matched validation comparisons.
- FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference. FAST_MODE is not valid for primary RQ1/RQ2 inference.
- Full Bayesian refits can be expensive because scripts `07` and `15` fit multiple `brms` models.
- Exact grouped K-fold in script `13` and exact row-level K-fold in script `28` are computationally expensive and should be treated as intentional heavy steps.
- Run manifests should record the actual sampler settings used by each branch.
- Posterior draws, fitted objects, and diagnostic tables can become large.
- Heavy outputs should not be committed. Keep them under `out/` and rely on the ignore rules.
- The former top-level `R` helper folder was removed from the active repository after dependency checks found no active sourcing or call sites.
