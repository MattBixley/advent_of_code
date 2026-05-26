library(tidyverse)

# -- Input --------------------------------------------------------------------
input <- read_lines("2024/day25_input.txt")

# Sample (expected Part 1: 3)
# Two locks and three keys as 7-row grids separated by blank lines

# -- Parse --------------------------------------------------------------------
# Split into 7-row blocks
blocks <- list()
current <- character(0)
for (line in input) {
  if (line == "") {
    if (length(current) == 7) blocks <- c(blocks, list(current))
    current <- character(0)
  } else {
    current <- c(current, line)
  }
}
if (length(current) == 7) blocks <- c(blocks, list(current))

parse_schematic <- function(block) {
  mat <- do.call(rbind, strsplit(block, ""))
  is_lock <- all(mat[1, ] == "#")  # lock: top row filled
  # Heights = number of # minus 1 (ignoring the all-# row)
  heights <- map_int(1:5, ~ sum(mat[, .x] == "#") - 1L)
  list(is_lock = is_lock, heights = heights)
}

schematics <- map(blocks, parse_schematic)
locks <- keep(schematics, ~ .x$is_lock)
keys  <- keep(schematics, ~ !.x$is_lock)

# -- Part 1 -------------------------------------------------------------------
# Count lock/key pairs where no column overlap (sum of heights <= 5)

result1 <- sum(map_int(locks, function(lk) {
  sum(map_lgl(keys, ~ all(lk$heights + .x$heights <= 5L)))
}))

cat("Part 1:", result1, "\n")
# Part 2: No Part 2 for Day 25 (requires 49 stars — just press a button)
