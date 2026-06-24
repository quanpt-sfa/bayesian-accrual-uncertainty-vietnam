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
| ma07a | `scripts/ma07a_fit_brms_named_models.R` | Baseline brms fit worker stage |
| ma07b | `scripts/ma07b_extract_brms_fit_outputs_workers.R` | Extract baseline brms fit outputs with workers |
| ma07c | `scripts/ma07c_collect_brms_fit_outputs.R` | Collect extracted baseline brms fit outputs |
| ma08 | `scripts/ma08_mcmc_diagnostics.R` | Baseline MCMC diagnostics |
| ma09a | `scripts/ma09a_plan_loo_savepars_refits.R` | Plan secondary PSIS/LOO save_pars refits |
| ma09b | `scripts/ma09b_fit_loo_savepars_refits.R` | Worker fit stage for secondary PSIS/LOO save_pars refits |
| ma09c | `scripts/ma09c_collect_loo_stacking.R` | Collector for secondary PSIS/LOO stacking outputs |
| ma10 | `scripts/ma10_construct_psis_loo_DA.R` | Secondary PSIS/LOO uncertainty-adjusted DA |
| ma11 | `scripts/ma11_posterior_predictive_checks.R` | Posterior predictive checks for secondary PSIS/LOO DA |
| ma12a | `scripts/ma12a_plan_grouped_kfold_firm.R` | Plan primary exact grouped-firm K-fold |
| ma12b | `scripts/ma12b_fit_grouped_kfold_firm_workers.R` | Worker fit stage for primary exact grouped-firm K-fold |
| ma12c | `scripts/ma12c_collect_grouped_kfold_firm_scores.R` | Collector for primary exact grouped-firm K-fold scores, weights, and pins |
| ma13a | `scripts/ma13a_plan_row_level_exact_kfold.R` | Plan primary exact row-level K-fold |
| ma13b | `scripts/ma13b_fit_row_level_exact_kfold_workers.R` | Worker fit stage for primary exact row-level K-fold |
| ma13c | `scripts/ma13c_collect_row_level_exact_kfold_scores.R` | Collector for primary exact row-level K-fold scores, weights, and pins |
| ma14 | `scripts/ma14_construct_exact_kfold_DA.R` | Primary exact-KFoldW DA construction from completed-run pins |
| ma15 | `scripts/ma15_audit_DA_finite_outputs.R` | Hard finite-output gate for exact-KFold DA |
| ma16 | `scripts/ma16_validate_outcomes.R` | Primary validation on exact row-KFold DA |
| di02 | `scripts/diagnostics/di02_new_firm_predictive_integration_audit.R` | Main new-firm predictive integration reporting gate |
| ma17 | `scripts/ma17_export_tables_figures.R` | Chapter 3 manuscript table export |
| ro01 | `scripts/robustness/ro01_lofo_stacking.R` | Optional grouped PSIS-LOFO robustness |
| se01 | `scripts/sensitivity/se01_prior_predictive.R` | Sensitivity prior predictive gate |
| se02a | `scripts/sensitivity/se02a_plan_prior_scenario_refits.R` | Plan sensitivity full refits by prior scenario |
| se02b | `scripts/sensitivity/se02b_fit_prior_scenario_workers.R` | Worker fit stage for sensitivity full refits |
| se02c | `scripts/sensitivity/se02c_collect_prior_scenario_outputs.R` | Collector for sensitivity refit outputs |
| se03 | `scripts/sensitivity/se03_mcmc_diagnostics.R` | Sensitivity MCMC diagnostics gate |
| se04 | `scripts/sensitivity/se04_stacking.R` | Sensitivity LOO/stacking by scenario |
| se05 | `scripts/sensitivity/se05_construct_DA.R` | Sensitivity DA reconstruction |
| se06 | `scripts/sensitivity/se06_validation.R` | Sensitivity validation/outcome tests |
| se07 | `scripts/sensitivity/se07_report.R` | Sensitivity report |
| si00 | `scripts/simulation/si00_helpers.R` | Simulation helper functions for leakage pilot scripts |
| si01 | `scripts/simulation/si01_lmer_pilot_run.R` | LMER leakage pilot simulation run |
| si02 | `scripts/simulation/si02_lmer_pilot_report.R` | LMER leakage pilot simulation report |
| si03a | `scripts/simulation/si03a_plan_brms_leakage_confirmation.R` | Plan BRMS leakage confirmation simulation |
| si03b | `scripts/simulation/si03b_fit_brms_leakage_confirmation_workers.R` | Worker fit stage for BRMS leakage confirmation simulation |
| si03c | `scripts/simulation/si03c_collect_brms_leakage_confirmation.R` | Collector for BRMS leakage confirmation simulation |
| si04a | `scripts/simulation/si04a_plan_brms_parameter_recovery.R` | Plan BRMS parameter recovery simulation |
| si04b | `scripts/simulation/si04b_fit_brms_parameter_recovery_workers.R` | Worker fit stage for BRMS parameter recovery simulation |
| si04c | `scripts/simulation/si04c_collect_brms_parameter_recovery.R` | Collector for BRMS parameter recovery simulation |
| di01 | `scripts/diagnostics/di01_psis_reliability_gate.R` | Optional secondary PSIS reliability diagnostics |

Sensitivity scripts se01-se07 are prepared for full MCMC refits by prior scenario. Heavy MCMC is not run unless `ACCRUAL_DRY_RUN=FALSE` and the relevant script is launched intentionally.

Sampler protocol: Chapter 3 specifies 4 chains, 3000 iterations, 1000 warmup iterations, fixed seed 42, adapt_delta = 0.95, and max_treedepth = 12 for brms/Stan estimation. Baseline full-sample fits, exact K-fold refits, and sensitivity refits use those defaults unless explicitly overridden and recorded in manifests. FAST_MODE/smoke runs use 2 chains, 1000 iterations, and 500 warmup iterations and are excluded from primary inference.

Execution configuration is centralized in `scripts/ma00_setup.R`: `accrual_base_seed()` and `accrual_seed()` enforce one canonical seed (`ACCRUAL_SEED`, default 42) across baseline, grouped exact K-fold, row exact K-fold, sensitivity, and simulation branches; `accrual_seed_for()` derives deterministic context-specific offsets from that same canonical seed; `set_accrual_seed()` is the only helper that calls base `set.seed()`; `accrual_sampler_config()` supplies sampler settings; `accrual_kfold_config()` supplies exact K-fold K/seed/sampler settings; and `main_model_ids_for_space()` supplies primary model IDs. Branch-specific seed env vars (`ACCRUAL_BASELINE_SEED`, `ACCRUAL_KFOLD_FIRM_SEED`, `ACCRUAL_ROW_KFOLD_SEED`, `ACCRUAL_SENS_SEED`, `ACCRUAL_SIM_SEED`) are deprecated and blocked if they differ from `ACCRUAL_SEED`. The helper writes `out/manifests/method_design/execution_config_registry.csv`.

Production exact K-fold defaults are 4 chains, 4 rstan cores, 12000 iterations, 4000 warmup iterations, `adapt_delta = 0.99`, and `max_treedepth = 15` for both grouped-firm and row-level exact K-fold. Lower settings are light/test modes only and must be explicit in the K-fold run mode and task manifest sampler provenance.

Primary model helpers return M01-M07 for ex-post and M01, M02, M03, M07, M09 for real-time/no-lookahead. M08/M10 remain secondary/robustness unless explicitly included through documented secondary flows, and M11/M12 remain excluded from active primary helpers.

`Rscript run.R` runs the `main` target by default. The main target includes split grouped exact firm K-fold (`ma12a`/`ma12b`/`ma12c`) and split row-level exact K-fold (`ma13a`/`ma13b`/`ma13c`) as adjacent primary RQ1 evidence steps, then constructs primary exact-KFoldW DA (`scripts/ma14_construct_exact_kfold_DA.R`), applies the finite-output gate (`scripts/ma15_audit_DA_finite_outputs.R`), runs validation on the primary exact row-KFold DA, the new-firm predictive integration reporting gate, and the corrected Chapter 3 manuscript export path `scripts/ma17_export_tables_figures.R`.

`scripts/ma10_construct_psis_loo_DA.R` remains the PSIS/LOO secondary DA constructor, including secondary validation panels only. Scripts `ma12` and `ma13` write `LATEST_COMPLETED_RUN.txt` only for completed primary-eligible exact-refit runs, and script `ma14` uses those pins or explicit run-root environment variables instead of moving `LATEST_RUN.txt` for primary inference. `LATEST_RUN.txt` is operational only and should not be used as primary provenance. Scripts `ma12` and `ma13` write reviewer-grade input/output manifests with file size, mtime, MD5 hash, row counts where applicable, run-root fields, and completed-pin fields.

`scripts/ma14_construct_exact_kfold_DA.R` refuses completed-run manifests that lack explicit `Completed_Run_Pin_Eligible = TRUE`. It writes file-size/mtime/hash source manifests, draw-file hash manifests, and `table_model_primary_inclusion_gate.csv`. MCMC `FAIL`/`LOW_RELIABILITY` models are excluded from primary exact-KFold DA; `REVIEW`/`CAUTION` models can be retained only with `MCMC_REVIEW_INCLUDED_WITH_EXACT_REFIT_PASS`.

`scripts/ma15_audit_DA_finite_outputs.R` writes `table_DA_finite_gate_decision.csv` and is a hard RQ2/export gate. Script `di02` is a hard new-firm tail-suppression gate; if unverified Firm-RE out-of-firm posterior predictive tail quantities require suppression, export stops unless the explicit suppression override is set and the outputs are labelled non-primary. `Rscript run.R all --dry-run` de-duplicates `scripts/diagnostics/di02_new_firm_predictive_integration_audit.R` so the new-firm audit appears once.

All heavy independent brms fit workloads use the shared worker architecture from `scripts/ma00_setup.R`. Worker stages write only task-local artifacts; collectors own shared outputs, score tables, stacking weights, completed-run pins, reports, and manuscript-facing tables. The production worker policy comes from `accrual_run_profile_config("full_clean_production_5w4c")`.

LOFO (`scripts/robustness/ro01_lofo_stacking.R`) is an opt-in robustness branch, not a default main step. Sensitivity scripts se01-se07 and simulation scripts si00-si04 are opt-in branches. PSIS reliability (`scripts/diagnostics/di01_psis_reliability_gate.R`) is secondary diagnostics, not the primary RQ1 comparison.

The machine-readable pipeline index is written to `out/manifests/method_design/pipeline_index.csv`.
