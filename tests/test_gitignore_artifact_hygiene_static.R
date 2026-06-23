repo_root <- normalizePath(".", mustWork = TRUE)
gitignore_path <- file.path(repo_root, ".gitignore")

if (!file.exists(gitignore_path)) {
  stop(".gitignore is missing.")
}

lines <- readLines(gitignore_path, warn = FALSE)
rules <- trimws(lines)
rules <- rules[nzchar(rules) & !startsWith(rules, "#")]

require_rule <- function(rule) {
  if (!rule %in% rules) {
    stop("Missing required .gitignore rule: ", rule)
  }
}

for (rule in c(
  "/accruals/",
  "/out/runs/",
  "/brms_rstan_benchmark_results/",
  "/audit_mcmc_gate.R",
  "/benchmark.R",
  "/check_m05_diag.R",
  "/si03_sigma0_console_log.txt",
  "*_console_log.txt",
  "*.out",
  "*.err",
  "/tmp/",
  "/temp/",
  "/scratch/"
)) {
  require_rule(rule)
}

dangerous_rules <- c(
  "scripts/",
  "/scripts/",
  "tests/",
  "/tests/",
  "*.R",
  "*.md",
  "README.md",
  "/README.md",
  "doc/",
  "/doc/"
)

present_dangerous <- intersect(dangerous_rules, rules)
if (length(present_dangerous)) {
  stop("Dangerous broad .gitignore rules found: ", paste(present_dangerous, collapse = ", "))
}

cat("test_gitignore_artifact_hygiene_static.R passed\n")
