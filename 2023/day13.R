library(tidyverse)
source("scripts/utils.R")

paragraphs <- read_paragraphs("2023/day13_input.txt")

parse_pattern <- function(lines) {
  matrix(unlist(strsplit(lines, "")), nrow = length(lines), byrow = TRUE)
}

find_reflection <- function(mat, required_diff = 0) {
  nr <- nrow(mat); nc <- ncol(mat)

  # Check horizontal reflection after row r
  for (r in seq_len(nr - 1)) {
    top    <- mat[r:1, , drop = FALSE]
    bottom <- mat[(r+1):min(nr, 2*r), , drop = FALSE]
    rows_to_check <- min(nrow(top), nrow(bottom))
    diffs <- sum(top[seq_len(rows_to_check),] != bottom[seq_len(rows_to_check),])
    if (diffs == required_diff) return(100L * r)
  }

  # Check vertical reflection after col c
  for (c in seq_len(nc - 1)) {
    left  <- mat[, c:1, drop = FALSE]
    right <- mat[, (c+1):min(nc, 2*c), drop = FALSE]
    cols_to_check <- min(ncol(left), ncol(right))
    diffs <- sum(left[, seq_len(cols_to_check)] != right[, seq_len(cols_to_check)])
    if (diffs == required_diff) return(c)
  }
  0L
}

patterns <- map(paragraphs, parse_pattern)

result1 <- sum(map_int(patterns, ~ find_reflection(.x, required_diff = 0)))
cat("Part 1:", result1, "\n")

result2 <- sum(map_int(patterns, ~ find_reflection(.x, required_diff = 1)))
cat("Part 2:", result2, "\n")
