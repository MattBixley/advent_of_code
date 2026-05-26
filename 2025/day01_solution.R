library(tidyverse)

# -- Input --------------------------------------------------------------------
input <- read_lines("2025/day01_input.txt")

# sample <- c("L68","L30","R48","L5","R60","L55","L1","L99","R14","L82")
# Part 1 expected: 3  |  Part 2 expected: 6

moves <- tibble(raw = input) |>
  mutate(
    dir  = str_sub(raw, 1, 1),
    dist = as.integer(str_sub(raw, 2))
  )

# -- Part 1 -------------------------------------------------------------------
# Count rotations where the final position is 0.
# Dial: 0-99, starts at 50. L = subtract, R = add, wraps via %% 100.

positions <- accumulate(
  seq_len(nrow(moves)),
  \(pos, i) {
    d <- moves$dist[i]
    if (moves$dir[i] == "L") (pos - d) %% 100L else (pos + d) %% 100L
  },
  .init = 50L
)[-1]

part1 <- sum(positions == 0L)
cat("Part 1:", part1, "\n")

# -- Part 2 -------------------------------------------------------------------
# Count every click that points at 0, including mid-rotation passes.
#
# Steps to first 0 from pos:
#   L: pos clicks (or 100 if pos == 0)
#   R: (100 - pos) clicks (or 100 if pos == 0)
# Each subsequent pass is 100 clicks later.

count_zeros <- function(pos, dir, dist) {
  first <- if (dir == "L") {
    if (pos == 0L) 100L else pos
  } else {
    if (pos == 0L) 100L else 100L - pos
  }
  if (dist < first) 0L else (dist - first) %/% 100L + 1L
}

step <- function(state, i) {
  pos  <- state$pos
  dir  <- moves$dir[i]
  dist <- moves$dist[i]
  list(
    pos   = if (dir == "L") (pos - dist) %% 100L else (pos + dist) %% 100L,
    zeros = count_zeros(pos, dir, dist)
  )
}

states <- accumulate(seq_len(nrow(moves)), step, .init = list(pos = 50L, zeros = 0L))
part2 <- states[-1] |> map_int("zeros") |> sum()
cat("Part 2:", part2, "\n")
