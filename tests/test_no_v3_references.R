targets <- c(
  "README.md",
  "run.R",
  "scripts",
  "R",
  "doc",
  "tests",
  file.path("data", "raw", "README.md")
)

patterns <- c("scripts/v3", "v3_", "_v3", "V3_", "v3 pipeline")

exceptions <- data.frame(
  File = file.path("tests", paste0("test_no", "_", "v3", "_references.R")),
  Reason = "The test contains the forbidden patterns as scan fixtures.",
  stringsAsFactors = FALSE
)

collect_files <- function(path) {
  if (dir.exists(path)) {
    return(list.files(path, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE))
  }
  path
}

files <- unique(unlist(lapply(targets, collect_files), use.names = FALSE))
files <- files[file.exists(files) & !dir.exists(files)]
files <- files[!files %in% exceptions$File]

matches <- list()

for (file in files) {
  lines <- readLines(file, warn = FALSE)
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    hit_patterns <- patterns[vapply(patterns, function(pattern) grepl(pattern, line, fixed = TRUE), logical(1))]
    if (length(hit_patterns) == 0) next
    matches[[length(matches) + 1]] <- data.frame(
      File = file,
      Line = i,
      Pattern = paste(hit_patterns, collapse = ", "),
      Text = line,
      stringsAsFactors = FALSE
    )
  }
}

if (length(matches) > 0) {
  matches_df <- do.call(rbind, matches)
  details <- paste0(matches_df$File, ":", matches_df$Line, " [", matches_df$Pattern, "] ", matches_df$Text)
  stop(
    "Found forbidden standalone-naming regressions:\n",
    paste(details, collapse = "\n")
  )
}

cat("no-v3 reference scan passed\n")