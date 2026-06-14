targets <- c(
  "README.md",
  "run.R",
  "scripts",
  "R",
  "doc",
  "tests",
  file.path("data", "raw", "README.md")
)

make_pattern <- function(...) {
  paste0(...)
}

from_code_points <- function(...) {
  intToUtf8(c(...))
}

patterns <- c(
  make_pattern("M", from_code_points(0x00C3)),
  from_code_points(0x00C3, 0x0192),
  from_code_points(0x00C2),
  from_code_points(0x00E1, 0x00BA),
  from_code_points(0x00E1, 0x00BB),
  from_code_points(0x00C4, 0x2018),
  from_code_points(0x00C3, 0x00A1),
  from_code_points(0x00C3, 0x00A2),
  from_code_points(0x00C3, 0x00AA),
  from_code_points(0x00C3, 0x00B4),
  from_code_points(0x00C3, 0x00B5),
  from_code_points(0x00C3, 0x00A8),
  from_code_points(0x00C3, 0x00A9),
  from_code_points(0x00C3, 0x00B9),
  from_code_points(0x00C3, 0x00BA),
  from_code_points(0x00C3, 0x00BD)
)

collect_files <- function(path) {
  if (dir.exists(path)) {
    return(list.files(path, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE))
  }
  path
}

files <- unique(unlist(lapply(targets, collect_files), use.names = FALSE))
files <- files[file.exists(files) & !dir.exists(files)]

matches <- list()

for (file in files) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
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
    "Found mojibake regressions in active source or documentation:\n",
    paste(details, collapse = "\n")
  )
}

cat("no-mojibake reference scan passed\n")