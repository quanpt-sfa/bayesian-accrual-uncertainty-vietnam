source("scripts/00_helpers.R")

row_loo_weights_path <- function(target_space = c("ex_post", "real_time")) {
  target_space <- match.arg(target_space)
  if (target_space == "ex_post") {
    file.path(output_root, "tables", "table_stacking_weights_ex_post_winsor_corrected.csv")
  } else {
    file.path(output_root, "tables", "table_stacking_weights_no_lookahead_winsor_corrected.csv")
  }
}

lofo_weights_path <- function(target_space = c("ex_post", "real_time")) {
  target_space <- match.arg(target_space)
  if (target_space == "ex_post") {
    file.path(output_root, "lofo", "tables", "table_winsor_lofo_weights_ex_post.csv")
  } else {
    file.path(output_root, "lofo", "tables", "table_winsor_lofo_weights_no_lookahead.csv")
  }
}

kfold_root <- function() {
  file.path(output_root, "kfold_firm")
}
