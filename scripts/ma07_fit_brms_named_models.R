# -----------------------------------------------------------------------------
# Script: ma07_fit_brms_named_models.R
# Purpose: Compatibility wrapper for the split ma07 fit/collect workflow.
#
# New automated entrypoints:
#   scripts/ma07a_fit_brms_named_models.R
#   scripts/ma07b_collect_brms_fit_outputs.R
# -----------------------------------------------------------------------------

message("[ma07 wrapper] Running ma07a fit stage followed by ma07b collection stage.")
sys.source("scripts/ma07a_fit_brms_named_models.R", envir = globalenv())
sys.source("scripts/ma07b_collect_brms_fit_outputs.R", envir = globalenv())
