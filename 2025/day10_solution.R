library(tidyverse)

# Day 10: Factory
# Part 1: Each machine has indicator lights (binary); buttons toggle sets of lights.
#         Find minimum number of buttons to press (0 or 1 each) to reach target.
#         Uses GF(2) system: try all 2^n subsets.
# Part 2: Each button increases certain counters by 1 per press (can press multiple times).
#         Find minimum total presses to reach target joltage levels.
#         This is an ILP: minimize sum(x) s.t. Ax = b, x >= 0 integers.
#         Solved using lpSolve.
#
# Sample (P1 expected: 7, P2 expected: 33):
# [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}          -> P1: 2, P2: 10
# [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}  -> P1: 3, P2: 12
# [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5} -> P1: 2, P2: 11

.libPaths(c("/tmp/Rlibs", .libPaths()))

input <- read_lines("/mnt/c/Users/MattBixley/Code/advent_of_code/2025/day10_input.txt")

# --- Part 1: GF(2) toggle puzzle ---
parse_p1 <- function(line) {
  diagram <- regmatches(line, regexpr("\\[[.#]+\\]", line))
  target <- as.integer(strsplit(gsub("\\[|\\]", "", diagram), "")[[1]] == "#")
  buttons_str <- regmatches(line, gregexpr("\\([^)]+\\)", line))[[1]]
  buttons <- lapply(buttons_str, function(b) as.integer(strsplit(gsub("[()]", "", b), ",")[[1]]))
  list(target = target, buttons = buttons, n_lights = length(target))
}

min_presses_p1 <- function(machine) {
  target <- machine$target; buttons <- machine$buttons
  n <- length(buttons); n_lights <- machine$n_lights
  best <- Inf
  for (mask in 0:(2^n - 1)) {
    count <- sum(as.logical(bitwAnd(mask, 2^(0:(n-1)))))
    if (count >= best) next
    state <- integer(n_lights)
    for (i in 1:n) {
      if (bitwAnd(mask, 2^(i-1)) > 0) {
        for (light in buttons[[i]]) state[light + 1] <- (state[light + 1] + 1L) %% 2L
      }
    }
    if (all(state == target)) best <- count
  }
  best
}

part1 <- sum(sapply(input, function(line) min_presses_p1(parse_p1(line))))
cat("Part 1:", part1, "\n")  # 547

# --- Part 2: ILP with lpSolve ---
library(lpSolve)

parse_p2 <- function(line) {
  buttons_str <- regmatches(line, gregexpr("\\([^)]+\\)", line))[[1]]
  b <- as.integer(strsplit(gsub("[{}]", "", regmatches(line, regexpr("\\{[^}]+\\}", line))), ",")[[1]])
  n_buttons <- length(buttons_str); n_counters <- length(b)
  A <- matrix(0L, nrow = n_counters, ncol = n_buttons)
  for (j in 1:n_buttons) {
    for (idx in as.integer(strsplit(gsub("[()]", "", buttons_str[j]), ",")[[1]])) A[idx + 1, j] <- 1L
  }
  list(A = A, b = b)
}

min_presses_p2 <- function(machine) {
  result <- lp("min", rep(1, ncol(machine$A)), machine$A, rep("=", nrow(machine$A)),
               machine$b, all.int = TRUE)
  if (result$status != 0) return(NA)
  result$objval
}

part2 <- sum(sapply(input, function(line) min_presses_p2(parse_p2(line))), na.rm = TRUE)
cat("Part 2:", part2, "\n")  # 21111
