library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2024/day10_input.txt")

# Parse grid into tidy tibble via shared utility
grid_tbl <- parse_grid(input) |>
  mutate(height = as.integer(value))

# Also build a matrix for O(1) neighbour lookups in BFS/DFS
nrow_g <- max(grid_tbl$y)
ncol_g <- max(grid_tbl$x)

grid_mat <- matrix(NA_integer_, nrow = nrow_g, ncol = ncol_g)
grid_mat[cbind(grid_tbl$y, grid_tbl$x)] <- grid_tbl$height

# 4-directional in-bounds neighbours (row = y, col = x)
neighbours <- function(r, c) {
  dirs <- list(c(-1L, 0L), c(1L, 0L), c(0L, -1L), c(0L, 1L))
  keep(
    map(dirs, ~ c(r + .x[1], c + .x[2])),
    ~ .x[1] >= 1L & .x[1] <= nrow_g & .x[2] >= 1L & .x[2] <= ncol_g
  )
}

# Find trailheads (height == 0) from the tidy tibble
trailheads <- grid_tbl |> filter(height == 0L)

# Part 1: BFS from each trailhead, count distinct height-9 cells reachable
count_reachable_nines <- function(start_r, start_c) {
  visited <- matrix(FALSE, nrow = nrow_g, ncol = ncol_g)
  queue   <- list(c(start_r, start_c))
  visited[start_r, start_c] <- TRUE
  nines <- 0L

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue   <- queue[-1]
    r <- current[1]; c <- current[2]
    h <- grid_mat[r, c]

    if (h == 9L) { nines <- nines + 1L; next }

    for (nb in neighbours(r, c)) {
      nr <- nb[1]; nc <- nb[2]
      if (!visited[nr, nc] && grid_mat[nr, nc] == h + 1L) {
        visited[nr, nc] <- TRUE
        queue <- c(queue, list(c(nr, nc)))
      }
    }
  }
  nines
}

result1 <- map_int(seq_len(nrow(trailheads)), function(i) {
  count_reachable_nines(trailheads$y[i], trailheads$x[i])
}) |> sum()

cat("Part 1:", result1, "\n")

# Part 2: DFS from each trailhead, count all distinct paths to any height-9 cell
count_paths <- function(r, c) {
  h <- grid_mat[r, c]
  if (h == 9L) return(1L)

  map_int(neighbours(r, c), function(nb) {
    nr <- nb[1]; nc <- nb[2]
    if (grid_mat[nr, nc] == h + 1L) count_paths(nr, nc) else 0L
  }) |> sum()
}

result2 <- map_int(seq_len(nrow(trailheads)), function(i) {
  count_paths(trailheads$y[i], trailheads$x[i])
}) |> sum()

cat("Part 2:", result2, "\n")
