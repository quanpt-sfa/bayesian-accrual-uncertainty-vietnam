# accrual uncertainty pipeline index

Active scripts use the ma/ro/se/si/di reorg prefixes. The execution order is defined by `run.R`.

| Order | Script | Role |
|---|---|---|
| ma00 | `scripts/ma00_setup.R` | Shared helpers and registries |
| ma01 | `scripts/ma01_setup_and_registry.R` | Setup and model registry |
| ma02 | `scripts/ma02_build_common_sample.R` | Build common samples |
| ma03 | `scripts/ma03_audit_data_integrity.R` | COGS/INV audit |
| ma04 | `scripts/ma04_define_named_models.R` | Define model formulas |
| ma05 | `scripts/ma05_winsorize_common_samples.R` | Winsorize common samples |
| ma06 | `scripts/ma06_prior_predictive_checks.R` | Baseline prior predictive checks |
| ma07 | `scripts/ma07_fit_brms_named_models.R` | Baseline brms fits |
| ma08 | `scripts/ma08_mcmc_diagnostics.R` | Baseline MCMC diagnostics |
| ma09 | `scripts/ma09_loo_stacking.R` | Baseline LOO stacking |
| ma10 | `scripts/ma10_construct_psis_loo_DA.R` | Secondary PSIS/LOO uncertainty-adjusted DA |
| ma11 | `scripts/ma11_posterior_predictive_checks.R` | Posterior predictive checks for secondary PSIS/LOO DA |
| ma12 | `scripts/ma12_grouped_kfold_firm.R` | Baseline exact grouped K-fold |
| ma13 | `scripts/ma13_row_level_exact_kfold.R` | Main row-level exact K-fold method-matching arm |
| ma14 | `scripts/ma14_construct_exact_kfold_DA.R` | Primary exact-KFoldW DA construction from completed-run pins |
| ma15 | `scripts/ma15_audit_DA_finite_outputs.R` | Hard finite-output gate for exact-KFold DA |
| ma16 | `scripts/ma16_validate_outcomes.R` | Primary validation on exact row-KFold DA |
| di02 | `scripts/diagnostics/di02_new_firm_predictive_integration_audit.R` | Main new-firm predictive integration reporting gate |
| ma17 | `scripts/ma17_export_tables_figures.R` | Chapter 3 manuscript table export |
| ro01 | `scripts/robustness/ro01_lofo_stacking.R` | Optional grouped PSIS-LOFO robustness |
| se01 | `scripts/sensitivity/se01_prior_predictive.R` | Sensitivity prior predictive gate |
| se02 | `scripts/sensitivity/se02_refit_prior_scenarios.R` | Sensitivity full refits by prior scenario |
| se03 | `scripts/sensitivity/se03_mcmc_diagnostics.R` | Sensitivity MCMC diagnostics gate |
| se04 | `scripts/sensitivity/se04_stacking.R` | Sensitivity LOO/stacking by scenario |
| se05 | `scripts/sensitivity/se05_construct_DA.R` | Sensitivity DA reconstruction |
| se06 | `scripts/sensitivity/se06_validation.R` | Sensitivity validation/outcome tests |
| se07 | `scripts/sensitivity/se07_report.R` | Sensitivity report |
| si00 | `scripts/simulation/si00_helpers.R` | Simulation helper functions for leakage pilot scripts |
| si01 | `scripts/simulation/si01_lmer_pilot_run.R` | LMER leakage pilot simulation run |
| si02 | `scripts/simulation/si02_lmer_pilot_report.R` | LMER leakage pilot simulation report |
| si03 | `scripts/simulation/si03_brms_leakage_confirmation.R` | BRMS leakage confirmation simulation |
| si04 | `scripts/simulation/si04_brms_parameter_recovery.R` | BRMS parameter recovery simulation |
| di01 | `scripts/diagnostics/di01_psis_reliability_gate.R` | Optional secondary PSIS reliability diagnostics |

Sensitivity scripts se01-se07 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `ACCRUAL_DRY_RUN=FALSE` and the relevant script is launched intentionally.

Sampler protocol: full-sample baseline `brms` fits use 4 chains, 4000 iterations, and 1000 warmup iterations; exact K-fold refits use 4 chains, 3000 iterations, and 1000 warmup iterations because they are repeated across validation folds and are used for method-matched validation comparisons; FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference. The baseline 4000/1000 setting is intentional, while 3000/1000 is the primary validation-refit protocol. Manifests should record actual sampler settings.

Execution configuration is centralized in `scripts/ma00_setup.R`: `accrual_base_seed()` and `accrual_seed()` enforce one canonical seed (`ACCRUAL_SEED`, default 42) across baseline, grouped exact K-fold, row exact K-fold, sensitivity, and simulation branches; `accrual_seed_for()` derives deterministic context-specific offsets from that same canonical seed; `set_accrual_seed()` is the only helper that calls base `set.seed()`; `accrual_sampler_config()` supplies sampler settings; `accrual_kfold_config()` supplies exact K-fold K/seed/sampler settings; and `main_model_ids_for_space()` supplies primary model IDs. Branch-specific seed env vars (`ACCRUAL_BASELINE_SEED`, `ACCRUAL_KFOLD_FIRM_SEED`, `ACCRUAL_ROW_KFOLD_SEED`, `ACCRUAL_SENS_SEED`, `ACCRUAL_SIM_SEED`) are deprecated and blocked if they differ from `ACCRUAL_SEED`. The helper writes `out/manifests/method_design/execution_config_registry.csv`.

Primary model helpers return M01-M07 for ex-post and M01, M02, M03, M07, M09 for real-time/no-lookahead. M08/M10 remain secondary/robustness unless explicitly included through documented secondary flows, and M11/M12 remain excluded from active primary helpers.

`Rscript run.R` runs the `main` target by default. The main target includes grouped exact firm K-fold (`scripts/ma12_grouped_kfold_firm.R`) and row-level exact K-fold (`scripts/ma13_row_level_exact_kfold.R`) as adjacent primary RQ1 evidence steps, then constructs primary exact-KFoldW DA (`scripts/ma14_construct_exact_kfold_DA.R`), applies the finite-output gate (`scripts/ma15_audit_DA_finite_outputs.R`), runs validation on the primary exact row-KFold DA, the new-firm predictive integration reporting gate, and the corrected Chapter 3 manuscript export path `scripts/ma17_export_tables_figures.R`.

`scripts/ma10_construct_psis_loo_DA.R` remains the PSIS/LOO secondary DA constructor, including secondary validation panels only. Scripts `ma12` and `ma13` write `LATEST_COMPLETED_RUN.txt` only for completed primary-eligible exact-refit runs, and script `ma14` uses those pins or explicit run-root environment variables instead of moving `LATEST_RUN.txt` for primary inference. `LATEST_RUN.txt` is operational only and should not be used as primary provenance. Scripts `ma12` and `ma13` write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.

`scripts/ma14_construct_exact_kfold_DA.R` refuses completed-run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. It writes file-size/mtime/hash source manifests, draw-file hash manifests, and `table_model_primary_inclusion_gate.csv`. MCMC `FAIL`/`LOW_RELIABILITY` models are excluded from primary exact-KFold DA; `REVIEW`/`CAUTION` models can be retained only with `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS`.

`scripts/ma15_audit_DA_finite_outputs.R` writes `table_DA_finite_gate_decision.csv` and is a hard RQ2/export gate. Script `di02` is a hard new-firm tail-suppression gate; if unverified Firm-RE out-of-firm posterior predictive tail quantities require suppression, export stops unless the explicit suppression override is set and the outputs are labelled non-primary. `Rscript run.R all --dry-run` de-duplicates `scripts/diagnostics/di02_new_firm_predictive_integration_audit.R` so the new-firm audit appears once.

LOFO (`scripts/robustness/ro01_lofo_stacking.R`) is an opt-in robustness branch, not a default main step. Sensitivity scripts se01-se07 and simulation scripts si00-si04 are opt-in branches. PSIS reliability (`scripts/diagnostics/di01_psis_reliability_gate.R`) is secondary diagnostics, not the primary RQ1 comparison.

The machine-readable pipeline index is written to `out/manifests/method_design/pipeline_index.csv`.
