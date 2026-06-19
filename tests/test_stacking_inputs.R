script_17 <- readLines("scripts/sensitivity/se04_stacking.R", warn = FALSE)
required_tokens <- c("row_loo", "firm_lofo", "grouped_kfold")
missing_tokens <- required_tokens[!vapply(required_tokens, function(token) any(grepl(token, script_17, fixed = TRUE)), logical(1))]
if (length(missing_tokens) > 0) {
  stop("Missing validation-engine branches in script 17: ", paste(missing_tokens, collapse = ", "))
}

if (any(grepl("only row_loo supported", script_17, fixed = TRUE))) {
  stop("Script 17 still contains the deprecated 'only row_loo supported' branch.")
}

cat("test_stacking_inputs.R passed\n")
