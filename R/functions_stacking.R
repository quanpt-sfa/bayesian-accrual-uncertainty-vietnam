source("scripts/v3/00_v3_winsor_helpers.R")

v3_row_loo_weights_path <- function(target_space = c("ex_post", "real_time")) {
  target_space <- match.arg(target_space)
  if (target_space == "ex_post") {
    file.path(v3_output_root, "tables", "table_v3_stacking_weights_ex_post_winsor_corrected.csv")
  } else {
    file.path(v3_output_root, "tables", "table_v3_stacking_weights_no_lookahead_winsor_corrected.csv")
  }
}

v3_lofo_weights_path <- function(target_space = c("ex_post", "real_time")) {
  target_space <- match.arg(target_space)
  if (target_space == "ex_post") {
    file.path(v3_output_root, "lofo", "tables", "table_v3_winsor_lofo_weights_ex_post.csv")
  } else {
    file.path(v3_output_root, "lofo", "tables", "table_v3_winsor_lofo_weights_no_lookahead.csv")
  }
}

v3_kfold_root <- function() {
  file.path(v3_output_root, "kfold_firm")
}
