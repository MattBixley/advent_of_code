library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day11_input.txt")

grid <- parse_grid(input)
galaxies <- grid |> filter(value == "#")

all_rows <- seq_len(max(grid$y))
all_cols <- seq_len(max(grid$x))
empty_rows <- setdiff(all_rows, unique(galaxies$y))
empty_cols <- setdiff(all_cols, unique(galaxies$x))

pair_distance <- function(factor) {
  gal <- galaxies |> select(x, y)
  n <- nrow(gal)
  total <- 0
  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      r1 <- min(gal$y[i], gal$y[j]); r2 <- max(gal$y[i], gal$y[j])
      c1 <- min(gal$x[i], gal$x[j]); c2 <- max(gal$x[i], gal$x[j])
      er <- sum(empty_rows > r1 & empty_rows < r2)
      ec <- sum(empty_cols > c1 & empty_cols < c2)
      dist <- (r2 - r1) + (c2 - c1) + (factor - 1) * (er + ec)
      total <- total + dist
    }
  }
  total
}

result1 <- pair_distance(2)
cat("Part 1:", result1, "\n")

result2 <- pair_distance(1000000)
cat("Part 2:", format(result2, scientific = FALSE), "\n")
