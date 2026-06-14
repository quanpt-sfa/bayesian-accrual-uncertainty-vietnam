# Computational notes

- Full Bayesian refits can be expensive because scripts `07` and `15` fit multiple `brms` models.
- Exact grouped K-fold in script `13` is computationally expensive and should be treated as an intentional heavy step.
- Posterior draws, fitted objects, and diagnostic tables can become large.
- Heavy outputs should not be committed. Keep them under `out/` and rely on the ignore rules.
- No `renv.lock` was available from the source workspace, so package installation is documented rather than pinned.
