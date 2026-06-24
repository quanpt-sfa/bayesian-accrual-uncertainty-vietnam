# -----------------------------------------------------------------------------
# Script: ma07b_collect_brms_fit_outputs.R
# Purpose: Compatibility wrapper for the split ma07b extract + ma07c collect
#          workflow. New pipeline entrypoints should call ma07b_extract... then
#          ma07c_collect... directly.
# -----------------------------------------------------------------------------

message("[ma07b compatibility wrapper] Running ma07b extraction workers followed by ma07c final collection.")
sys.source("scripts/ma07b_extract_brms_fit_outputs_workers.R", envir = globalenv())
sys.source("scripts/ma07c_collect_brms_fit_outputs.R", envir = globalenv())
