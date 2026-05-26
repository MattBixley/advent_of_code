library(tidyverse)

# Day 19: Linen Layout
# Part 1: How many designs are possible?
# Part 2: How many total ways to make all possible designs?

# Sample:
# r, wr, b, g, bwu, rb, gb, br
#
# brwrr  -> possible (6 possible, 2 impossible)
# Expected Part 1: 6
# Expected Part 2: 16

# -- Input --------------------------------------------------------------------
input <- readLines("2024/day19_input.txt")

blank <- which(input == "")
patterns <- strsplit(input[1], ", ")[[1]]
designs  <- input[(blank + 1):length(input)]
designs  <- designs[nchar(designs) > 0]

# -- Solve using dynamic programming ------------------------------------------
# count_ways(design, patterns) returns number of ways design can be assembled
# Uses memoization via environment
count_ways <- function(design, patterns, memo = new.env(hash = TRUE, parent = emptyenv())) {
  n <- nchar(design)
  if (n == 0L) return(1L)

  key <- design
  if (exists(key, envir = memo)) return(get(key, envir = memo))

  total <- 0L
  for (p in patterns) {
    lp <- nchar(p)
    if (lp <= n && substr(design, 1L, lp) == p) {
      total <- total + count_ways(substr(design, lp + 1L, n), patterns, memo)
    }
  }
  assign(key, total, envir = memo)
  total
}

# Shared memo across all designs for efficiency
memo <- new.env(hash = TRUE, parent = emptyenv())

ways <- sapply(designs, function(d) count_ways(d, patterns, memo))

# Part 1
part1 <- sum(ways > 0)
cat("Part 1:", part1, "\n")

# Part 2
part2 <- sum(ways)
cat("Part 2:", part2, "\n")
