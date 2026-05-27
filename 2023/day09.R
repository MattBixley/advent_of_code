library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day09_input.txt")

sequences <- map(input, ~ as.numeric(str_split(.x, " ")[[1]]))

extrapolate_next <- function(seq) {
  layers <- list(seq)
  while (!all(layers[[length(layers)]] == 0)) {
    last <- layers[[length(layers)]]
    layers[[length(layers) + 1]] <- diff(last)
  }
  sum(map_dbl(layers, ~ tail(.x, 1)))
}

extrapolate_prev <- function(seq) {
  layers <- list(seq)
  while (!all(layers[[length(layers)]] == 0)) {
    last <- layers[[length(layers)]]
    layers[[length(layers) + 1]] <- diff(last)
  }
  firsts <- map_dbl(layers, ~ .x[1])
  Reduce(function(acc, x) x - acc, rev(firsts), 0)
}

result1 <- sum(map_dbl(sequences, extrapolate_next))
cat("Part 1:", result1, "\n")

result2 <- sum(map_dbl(sequences, extrapolate_prev))
cat("Part 2:", result2, "\n")
