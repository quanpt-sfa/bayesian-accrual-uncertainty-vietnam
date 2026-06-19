# accrual uncertainty pipeline index

Active scripts use numeric prefixes only. No letter suffixes are used in script numbers.

| Order | Script | Role |
|---|---|---|
| 00 | `00_helpers.R` | Shared helpers and registries |
| 01 | `01_setup_and_registry.R` | Setup and model registry |
| 02 | `02_build_common_sample.R` | Build common samples |
| 03 | `03_audit_cogs_inv_operating_cycle.R` | COGS/INV audit |
| 04 | `04_define_named_models.R` | Define model formulas |
| 05 | `05_winsorize_common_samples.R` | Winsorize common samples |
| 06 | `06_prior_predictive_checks.R` | Baseline prior predictive checks |
| 07 | `07_fit_brms_named_models.R` | Baseline brms fits |
| 08 | `08_mcmc_diagnostics.R` | Baseline MCMC diagnostics |
| 09 | `09_loo_stacking.R` | Baseline LOO stacking |
| 10 | `10_construct_uncertainty_adjusted_DA.R` | Secondary PSIS/LOO uncertainty-adjusted DA |
| 11 | `11_posterior_predictive_checks.R` | Posterior predictive checks for secondary PSIS/LOO DA |
| 13 | `13_grouped_kfold_firm.R` | Baseline exact grouped K-fold |
| 28 | `28_row_level_exact_kfold.R` | Main row-level exact K-fold method-matching arm |
| 31 | `31_construct_exact_kfold_DA.R` | Primary exact-KFoldW DA construction from completed-run pins |
| 32 | `32_audit_DA_finite_outputs.R` | Hard finite-output gate for exact-KFold DA |
| 21 | `21_validation_on_scaleaware_student_DA.R` | Primary validation on exact row-KFold DA |
| 30 | `30_new_firm_predictive_integration_audit.R` | Main new-firm predictive integration reporting gate |
| C3 | `temp/22_chapter3_methods_tables.R` | Chapter 3 manuscript table export |
| 12 | `12_lofo_stacking.R` | Optional grouped PSIS-LOFO robustness |
| 14 | `14_sensitivity_prior_predictive.R` | Sensitivity prior predictive gate |
| 15 | `15_sensitivity_refit_prior_scenarios.R` | Sensitivity full refits by prior scenario |
| 16 | `16_sensitivity_mcmc_diagnostics.R` | Sensitivity MCMC diagnostics gate |
| 17 | `17_sensitivity_stacking.R` | Sensitivity LOO/stacking by scenario |
| 18 | `18_sensitivity_construct_DA.R` | Sensitivity DA reconstruction |
| 19 | `19_sensitivity_validation.R` | Sensitivity validation/outcome tests |
| 20 | `20_sensitivity_report.R` | Sensitivity report |
| 22 | `22_reset_and_rerun_after_cogs_inv_fix.R` | Reset/orchestrator |
| 23 | `23_sim_lmer_leakage_pilot_helpers.R` | Simulation helper functions for leakage pilot scripts |
| 24 | `24_sim_lmer_leakage_pilot_run.R` | LMER leakage pilot simulation run |
| 25 | `25_sim_lmer_leakage_pilot_report.R` | LMER leakage pilot simulation report |
| 26 | `26_sim_brms_leakage_confirmation.R` | BRMS leakage confirmation simulation |
| 27 | `27_sim_brms_parameter_recovery.R` | BRMS parameter recovery simulation |
| 29 | `29_psis_reliability_gate.R` | Optional secondary PSIS reliability diagnostics |

Sensitivity phases 14-20 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `ACCRUAL_DRY_RUN=FALSE` and the relevant phase is launched intentionally.

Sampler protocol: full-sample baseline `brms` fits use 4 chains, 4000 iterations, and 1000 warmup iterations; exact K-fold refits use 4 chains, 3000 iterations, and 1000 warmup iterations because they are repeated across validation folds and are used for method-matched validation comparisons; FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference. The baseline 4000/1000 setting is intentional, while 3000/1000 is the primary validation-refit protocol. Manifests should record actual sampler settings.

Execution configuration is centralized in `scripts/00_helpers.R`: `accrual_base_seed()` and `accrual_seed()` enforce one canonical seed (`ACCRUAL_SEED`, default `42`) across baseline, grouped exact K-fold, row exact K-fold, sensitivity, and simulation branches; `accrual_seed_for()` derives deterministic context offsets from that same base seed; `set_accrual_seed()` is the only helper that calls base `set.seed()`; `accrual_sampler_config()` supplies sampler settings; `accrual_kfold_config()` supplies exact K-fold K/seed/sampler settings; and `main_model_ids_for_space()` supplies primary model IDs. Branch-specific seed env vars (`ACCRUAL_BASELINE_SEED`, `ACCRUAL_KFOLD_FIRM_SEED`, `ACCRUAL_ROW_KFOLD_SEED`, `ACCRUAL_SENS_SEED`, `ACCRUAL_SIM_SEED`) are deprecated and blocked if they differ from `ACCRUAL_SEED`, to avoid branch-specific tuning or cherry-picking concerns. The helper writes `out/manifests/method_design/execution_config_registry.csv`.

Primary model helpers return M01-M07 for ex-post and M01, M02, M03, M07, M09 for real-time/no-lookahead. M08/M10 remain secondary/robustness unless explicitly included through documented secondary flows, and M11/M12 remain excluded from active primary helpers.

`Rscript run.R` runs the `main` target by default. The main target includes grouped exact firm K-fold (`scripts/13_grouped_kfold_firm.R`) and row-level exact K-fold (`scripts/28_row_level_exact_kfold.R`) as adjacent primary RQ1 evidence steps, then constructs primary exact-KFoldW DA (`scripts/31_construct_exact_kfold_DA.R`), applies the finite-output gate (`scripts/32_audit_DA_finite_outputs.R`), runs validation on the primary exact row-KFold DA, the new-firm predictive integration reporting gate, and the corrected Chapter 3 manuscript export path `scripts/temp/22_chapter3_methods_tables.R`.

`scripts/10_construct_uncertainty_adjusted_DA.R` remains the PSIS/LOO secondary DA constructor, including secondary validation panels only. Scripts `13` and `28` write `LATEST_COMPLETED_RUN.txt` only for completed primary-eligible exact-refit runs, and script `31` uses those pins or explicit run-root environment variables instead of moving `LATEST_RUN.txt` for primary inference. `LATEST_RUN.txt` is operational only and should not be used as primary provenance. Scripts `13` and `28` write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.

`scripts/31_construct_exact_kfold_DA.R` refuses completed-run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. It writes file-size/mtime/hash source manifests, draw-file hash manifests, and `table_model_primary_inclusion_gate.csv`. MCMC `FAIL`/`LOW_RELIABILITY` models are excluded from primary exact-KFold DA; `REVIEW`/`CAUTION` models can be retained only with `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS`.

`scripts/32_audit_DA_finite_outputs.R` writes `table_DA_finite_gate_decision.csv` and is a hard RQ2/export gate. Script `30` is a hard new-firm tail-suppression gate; if unverified Firm-RE out-of-firm posterior predictive tail quantities require suppression, export stops unless the explicit suppression override is set and the outputs are labelled non-primary. `Rscript run.R all --dry-run` de-duplicates script `30_new_firm_predictive_integration_audit.R` so the new-firm audit appears once.

LOFO (`scripts/12_lofo_stacking.R`) is an opt-in robustness branch, not a default main step. Sensitivity scripts 14-20 and simulation scripts 23-27 are opt-in branches. PSIS reliability (`scripts/29_psis_reliability_gate.R`) is secondary diagnostics, not the primary RQ1 comparison.

The machine-readable pipeline index is written to `out/manifests/method_design/pipeline_index.csv`.
