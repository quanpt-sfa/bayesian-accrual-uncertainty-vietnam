# Computational notes

- Full-sample baseline `brms` fits in script `07` use 4 chains, 4000 iterations, and 1000 warmup iterations. This 4000/1000 baseline protocol is intentional, not an error.
- Exact K-fold refits in scripts `13` and `28` use 4 chains, 3000 iterations, and 1000 warmup iterations because they are repeated across validation folds and are used for method-matched validation comparisons.
- FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference. FAST_MODE is not valid for primary RQ1/RQ2 inference.
- Full Bayesian refits can be expensive because scripts `07` and `15` fit multiple `brms` models.
- Exact grouped K-fold in script `13` and exact row-level K-fold in script `28` are computationally expensive and should be treated as intentional heavy steps.
- Run manifests should record the actual sampler settings used by each branch.
- `Rscript run.R` runs the main Chapter 3 pipeline by default. Use `Rscript run.R --dry-run` to print the ordered plan without running scripts.
- The main target includes adjacent exact K-fold arms: `scripts/13_grouped_kfold_firm.R` followed immediately by `scripts/28_row_level_exact_kfold.R`.
- LOFO is an opt-in robustness target, sensitivity and simulation are opt-in branches, and PSIS reliability is a secondary diagnostics target.
- `scripts/30_new_firm_predictive_integration_audit.R` is a main reporting gate for Firm-RE out-of-firm posterior predictive tail flags and also remains callable through `Rscript run.R diagnostics`.
- Chapter 3 manuscript export uses `scripts/temp/22_chapter3_methods_tables.R`.
- Posterior draws, fitted objects, and diagnostic tables can become large.
- Heavy outputs should not be committed. Keep them under `out/` and rely on the ignore rules. Heavy steps may be skipped only with explicit logged warnings.
- The former top-level `R` helper folder was removed from the active repository after dependency checks found no active sourcing or call sites.
