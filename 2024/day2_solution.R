library(tidyverse)

# Read the input file
read_list <- function(path, sep = "", type = identity) {
  lines <- readLines(path)
  res <- strsplit(lines, sep)
  res <- lapply(res, type)
  res
}

input <- read_list("2024/day2_input.txt", " ", as.integer)



# Part 1
safe <- function(x) {
  diffs <- diff(x)
  cond1 <- all(diffs > 0) || all(diffs < 0)
  cond2 <- all(abs(diffs) >= 1) && all(abs(diffs) <= 3)
  cond1 && cond2
}

vapply(input, safe, logical(1)) |>
  sum()

# Part 2
skip_safe <- function(x) {
  is_safe <- FALSE
  for (i in seq_along(x)) {
    if (safe(x[-i])) {
      is_safe <- TRUE
      break
    }
  }
  is_safe
}

vapply(input, skip_safe, logical(1)) |>
  sum()

