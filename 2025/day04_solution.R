library(tidyverse)

# -- Input --------------------------------------------------------------------
input <- read_lines("2025/day04_input.txt")

# sample <- c(
#   "..@@.@@@@.",
#   "@@@.@.@.@@",
#   "@@@@@.@.@@",
#   "@.@@@@..@.",
#   "@@.@@@@.@@",
#   ".@@@@@@@.@",
#   ".@.@.@.@@@",
#   "@.@@@.@@@@",
#   ".@@@@@@@@.",
#   "@.@.@@@.@."
# )
# (expected Part 1: 13)

# -- Helpers ------------------------------------------------------------------
parse_grid <- function(lines) {
  do.call(rbind, strsplit(lines, ""))
}

# For each '@', count adjacent '@' in 8 directions.
# Accessible if neighbor count < 4.
count_accessible <- function(grid) {
  nrow_g <- nrow(grid)
  ncol_g <- ncol(grid)

  # Vectorised neighbor count using matrix shifts
  dirs <- list(c(-1,-1), c(-1,0), c(-1,1),
               c(0,-1),           c(0,1),
               c(1,-1),  c(1,0),  c(1,1))

  rolls <- (grid == "@") * 1L
  neighbor_count <- matrix(0L, nrow = nrow_g, ncol = ncol_g)

  for (d in dirs) {
    dr <- d[1]; dc <- d[2]
    # Shift the rolls matrix by (dr, dc)
    shifted <- matrix(0L, nrow = nrow_g, ncol = ncol_g)
    r_from <- max(1, 1 - dr):min(nrow_g, nrow_g - dr)
    r_to   <- r_from + dr
    c_from <- max(1, 1 - dc):min(ncol_g, ncol_g - dc)
    c_to   <- c_from + dc
    shifted[r_to, c_to] <- rolls[r_from, c_from]
    neighbor_count <- neighbor_count + shifted
  }

  sum(rolls == 1L & neighbor_count < 4L)
}

# -- Part 1 -------------------------------------------------------------------
grid <- parse_grid(input)
result1 <- count_accessible(grid)
cat("Part 1:", result1, "\n")

# -- Part 2 -------------------------------------------------------------------
# Iteratively remove accessible rolls until none remain; count total removed.

grid2 <- grid
total_removed <- 0L
repeat {
  rolls2 <- (grid2 == "@") * 1L
  cnt2 <- neighbor_count(rolls2)
  accessible2 <- rolls2 == 1L & cnt2 < 4L
  n_acc <- sum(accessible2)
  if (n_acc == 0L) break
  grid2[accessible2] <- "."
  total_removed <- total_removed + n_acc
}
cat("Part 2:", total_removed, "\n")
