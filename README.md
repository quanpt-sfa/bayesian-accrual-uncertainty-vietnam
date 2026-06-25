# Bayesian Accrual Uncertainty Vietnam

Bayesian pipeline for uncertainty-adjusted discretionary accruals using Vietnamese firm-year data. The repository is organized as a reproducible Chapter 3 replication package with explicit run targets, provenance manifests, gate tables, and split fit/collect stages for heavy `brms` workloads.

## Quickstart

1. Restore the R environment:

   ```r
   install.packages("renv")
   renv::restore()
   ```

2. Put the raw workbook at `data/raw/data.xlsx`, or set a different path:

   ```powershell
   $env:ACCRUAL_DATA_PATH = "D:\path\to\data.xlsx"
   ```

3. Inspect the pipeline without running any scripts:

   ```powershell
   Rscript run.R all --dry-run
   ```

4. Run the main Chapter 3 pipeline. Heavy `brms` stages run only when explicitly enabled:

   ```powershell
   $env:ACCRUAL_RUN_HEAVY = "TRUE"
   Rscript run.R main
   ```

5. For the production 10-worker x 4-core workflow, run the numbered profiles. Downstream profiles require the main profile to complete through `ma17` and write `BASELINE_MA17_COMPLETE.txt` first:

   ```powershell
   .\run_profiles\run_01_main_production_10w4c.ps1
   .\run_profiles\run_02_sensitivity_after_main_10w4c.ps1
   .\run_profiles\run_03_diagnostics_after_main_10w4c.ps1
   .\run_profiles\run_04_simulation_after_main_10w4c.ps1
   ```

Principal outputs are under `out/interim/winsor/tables/`, `out/interim/winsor/diagnostics/`, `accruals/`, and `reports/chapter3_methods_tables/`.

## Repository Structure

- `run.R`: root orchestrator for `main`, `diagnostics`, `robustness`, `sensitivity`, `simulation`, `reviewer`, and `all`.
- `scripts/`: active pipeline scripts using the `maNN`, `diNN`, `roNN`, `seNN`, and `siNN` naming scheme.
- `scripts/ma00_setup.R`: shared configuration, sampler profiles, seed helpers, worker-pool helpers, path helpers, behavioral helpers, and method registries.
- `data/raw/data.xlsx`: default local raw workbook path; local raw data are not committed.
- `out/`: generated intermediate tables, fits, diagnostics, logs, and manifests; not committed.
- `accruals/`: generated final DA/NDA output tables; not committed.
- `reports/`: tracked audit notes plus generated report outputs where applicable.
- `doc/`: method authority, pipeline index, replication notes, and audit documentation intended for version control.
- `tests/`: lightweight static and behavioral checks that do not refit heavy Bayesian models.
- `run_profiles/`: reproducible PowerShell run profiles for production-style execution.

## Data Contract

The default data path is `data/raw/data.xlsx`. The pipeline reads `Sheet1` for firm-year observations and `Sheet2` for metadata. The metadata loader auto-detects common company-code column variants such as `company`, `ticker`, `code`, `Ma`, `Mã`, `Mã CK`, and `StockCode`.

Raw data are treated as read-only. `run.R` snapshots `data/raw/` before and after execution and warns if raw inputs change during a run.

## Run Targets

`Rscript run.R` defaults to `main`. All targets support `--dry-run`.

- `Rscript run.R main`: primary Chapter 3 pipeline through manuscript table export.
- `Rscript run.R diagnostics`: standalone diagnostics, including `di01`, `di02`, `di03`, and split diagnostic calibration `di08a/b/c`.
- `Rscript run.R robustness`: grouped PSIS-LOFO robustness evidence.
- `Rscript run.R sensitivity`: sensitivity prior predictive checks, scenario refits, diagnostics, stacking, DA reconstruction, validation, and report.
- `Rscript run.R simulation`: LMER and BRMS leakage/parameter-recovery simulation targets.
- `Rscript run.R reviewer`: reviewer-facing supplementary diagnostics and evidence package.
- `Rscript run.R all`: `main`, selected non-duplicated diagnostics, robustness, sensitivity, simulation, and reviewer targets.

In `all`, `di02` is not repeated after `main`; `di08a/b/c` are included as diagnostic-only heavy calibration stages.

## Main Pipeline

The current `main` target is split-stage where fitting is heavy and collectors own shared outputs:

1. `ma00` shared setup and registries.
2. `ma01` setup and ten-model registry.
3. `ma02` common sample construction.
4. `ma03` data integrity audit.
5. `ma04` named model formulas.
6. `ma05` winsorized common samples.
7. `ma06` prior predictive gate.
8. `ma07a` baseline `brms` fit worker stage.
9. `ma07b` baseline fit collector for diagnostics, coefficients, draws, and audit tables.
10. `ma08` MCMC diagnostics.
11. `ma09a` secondary PSIS/LOO save-pars refit planning.
12. `ma09b` secondary PSIS/LOO save-pars refit workers.
13. `ma09c` secondary PSIS/LOO collector and stacking.
14. `ma10` secondary PSIS/LOO DA construction.
15. `ma11` posterior predictive checks for secondary PSIS/LOO DA.
16. `ma12a` grouped-firm exact K-fold planning.
17. `ma12b` grouped-firm exact K-fold fit workers.
18. `ma12c` grouped-firm exact K-fold score/weight collector.
19. `ma13a` row-level exact K-fold planning.
20. `ma13b` row-level exact K-fold fit workers.
21. `ma13c` row-level exact K-fold score/weight collector.
22. `ma14` primary exact-KFoldW DA construction.
23. `ma15` finite-output gate.
24. `ma16` primary validation on exact row-KFold DA.
25. `di02` new-firm predictive integration reporting gate.
26. `di03` exact K-fold reclassification/Jaccard audit.
27. `ma17` Chapter 3 manuscript table export.

`ma09` remains secondary PSIS/LOO evidence. Primary RQ1 comparison is the exact grouped-firm versus exact row-level K-fold evidence from `ma12a/b/c` and `ma13a/b/c`. Primary RQ2 DA construction is `ma14`, gated by `ma15`, `di02`, and model-inclusion provenance.

## Worker Architecture

Heavy independent `brms` workloads use the shared worker architecture in `scripts/ma00_setup.R`:

- `accrual_fit_worker_config()`
- `accrual_run_task_pool()`
- `accrual_task_status_blocker()`

Worker stages write only task-local artifacts such as fit RDS, result RDS, metadata CSV, and logs. Collectors write shared CSVs, diagnostics, stacking weights, completed-run pins, reports, and manuscript-facing outputs.

Current split heavy stages:

- `ma07a` / `ma07b`: baseline model fits and collection.
- `ma09a` / `ma09b` / `ma09c`: secondary PSIS/LOO save-pars refits and stacking collection.
- `ma12a` / `ma12b` / `ma12c`: primary grouped-firm exact K-fold.
- `ma13a` / `ma13b` / `ma13c`: primary row-level exact K-fold.
- `se02a` / `se02b` / `se02c`: sensitivity prior-scenario refits.
- `si03a` / `si03b` / `si03c`: BRMS leakage confirmation simulation.
- `si04a` / `si04b` / `si04c`: BRMS parameter recovery simulation.
- `di08a` / `di08b` / `di08c`: diagnostic-only MCMC sampler calibration.

The default behavior is sequential. Model-level parallelism is opt-in:

```powershell
$env:ACCRUAL_ENABLE_MODEL_PARALLEL = "TRUE"
$env:ACCRUAL_MODEL_PARALLEL_WORKERS = "10"
$env:ACCRUAL_TOTAL_CORE_BUDGET = "40"
$env:ACCRUAL_ALLOW_NESTED_RSTAN_CORES = "TRUE"
$env:ACCRUAL_BASELINE_CORES = "4"
$env:ACCRUAL_RUN_HEAVY = "TRUE"
Rscript run.R main
```

The active core budget is:

```text
model-level workers x rstan cores per fit
```

On Windows, nested PSOCK workers with `rstan` chain-level cores greater than 1 require explicit `ACCRUAL_ALLOW_NESTED_RSTAN_CORES=TRUE`.

## Exact K-Fold Design

`ma12a` and `ma13a` own fold planning. They write fixed fold-assignment artifacts and task manifests before any model fit runs. Worker stages `ma12b` and `ma13b` read those planned fold assignments; they do not create or randomize folds inside workers. This preserves model-comparison semantics: models within the same target space are scored on the same held-out partitions.

`ma12c` and `ma13c` own shared score tables, stacking weights, and completed-run outputs. `ma14` consumes the completed exact K-fold outputs and refuses stale or non-primary-eligible manifests.

## Configuration and Seeds

Runtime and sampler configuration is centralized in `scripts/ma00_setup.R`.

- Canonical seed: `ACCRUAL_SEED`, default `42`.
- Context-specific seeds: `accrual_seed_for()`.
- Sampler profiles: `accrual_sampler_config()`.
- K-fold profiles: `accrual_kfold_config()`.
- Runtime profiles: `accrual_runtime_config()`, `accrual_loo_config()`, `accrual_simulation_runtime_config()`.
- Production profile values: `accrual_run_profile_registry()` and `accrual_run_profile_config("run_01_main_production_10w4c")`.

Branch-specific seed variables are deprecated and blocked if they disagree with `ACCRUAL_SEED`.

Chapter 3 default full sampler policy is 4 chains, 3000 iterations, 1000 warmup iterations, `adapt_delta = 0.95`, and `max_treedepth = 12`, unless an explicit profile or environment override is used and recorded in manifests. FAST/smoke modes are not valid for primary inference.

## Gates and Evidence Roles

- `ma06`: prior predictive gate using the Chapter 3 method authority in `doc/method_authority/chapter_3_method_authority.md`.
- `ma08`: baseline MCMC diagnostics.
- `ma09`: PSIS/LOO secondary evidence only.
- `ma12`/`ma13`: primary exact K-fold evidence.
- `ma14`: primary exact-KFoldW DA construction.
- `ma15`: hard finite-output gate for exact-KFold DA.
- `di02`: hard new-firm predictive reporting gate for Firm-RE out-of-firm posterior predictive tail quantities.
- `di03`: exact row-vs-grouped K-fold reclassification/Jaccard diagnostics.
- `di08`: diagnostic-only sampler calibration; not primary inference.

Tail flags and posterior-predictive tail quantities remain supplementary/non-primary when the new-firm predictive audit requires suppression.

## Sensitivity, Simulation, and Reviewer Targets

Sensitivity:

1. `se01` prior predictive sensitivity gate.
2. `se02a/b/c` scenario refit planning, workers, and collection.
3. `se03` diagnostics.
4. `se04` stacking.
5. `se05` DA reconstruction.
6. `se06` validation.
7. `se07` report.

Simulation:

- `si01`/`si02`: LMER leakage pilot and report.
- `si03a/b/c`: BRMS leakage confirmation, split plan/fit/collect.
- `si04a/b/c`: BRMS parameter recovery, split plan/fit/collect.

Reviewer:

- `di04`: full versus strict model-space stacking diagnostic.
- `di05`: denominator diagnostics for estimation-scaled DA.
- `di06`: supplementary top-5 economic-validity validation.
- `si05`/`si06`: temporal-dependence simulation and report.
- `di07`: Section 4.7 reviewer evidence package.

## Outputs

Generated outputs are intentionally ignored by Git unless explicitly documented as tracked source material.

- `out/interim/winsor/tables/`: main tables, task manifests/statuses, gates, exact K-fold scores, weights, and DA files.
- `out/interim/winsor/diagnostics/`: diagnostic tables, reclassification/Jaccard evidence, sampler calibration outputs.
- `out/interim/winsor/models/`, `draws/`, `kfold_firm/`, `row_exact_kfold/`: heavy fit and task artifacts.
- `accruals/baseline/` and `accruals/sensitivity/`: final DA/NDA output copies.
- `reports/chapter3_methods_tables/`: manuscript-ready tables exported by `ma17`.
- `doc/`: tracked method authority and replication documentation.

Do not commit generated CSV/RDS/log/model artifacts from `out/`, `accruals/`, benchmark folders, or local scratch scripts.

## Common Checks

Light checks that do not refit heavy models:

```powershell
Rscript run.R all --dry-run
Rscript tests/test_run_dry_plan_split_stages_static.R
Rscript tests/test_split_fit_collect_contract_static.R
Rscript tests/test_heavy_stage_worker_coverage_static.R
Rscript tests/test_centralized_runtime_config_static.R
Rscript tests/test_behavioral_core_helpers.R
Rscript tests/test_kfold_factor_level_coverage.R
```

The static worker tests guard against production placeholders, worker-owned shared outputs, missing split stages, and K-fold workers that attempt to randomize folds inside the fit stage.

## Windows Toolchain

This project uses `brms`/Stan through `rstan`. On Windows, install the matching Rtools toolchain. For the current lockfile, use the Rtools version compatible with the recorded R version.

Check the toolchain:

```powershell
where make
where g++
Rscript -e "Sys.which('make'); Sys.which('g++')"
Rscript -e "pkgbuild::check_build_tools(debug = TRUE)"
```

If `make` or `g++` is missing, Stan model compilation will fail.

## AI Assistance Disclosure

See `AI_USE.md` for a concise disclosure of AI-assisted code review/refactoring. Research design, data interpretation, and final responsibility remain with the researcher/project maintainer.
