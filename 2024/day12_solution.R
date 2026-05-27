library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2024/day12_input.txt")

# Parse grid into tibble(x, y, value) — x = col, y = row (top = 1)
grid_tbl <- parse_grid(input)

# Also keep a matrix for O(1) neighbour lookups during BFS
nrow_g <- length(input)
ncol_g <- nchar(input[1])
grid_mat <- matrix(grid_tbl$value, nrow = nrow_g, ncol = ncol_g, byrow = TRUE)

# BFS flood-fill. Returns a list of tibbles, each tibble has columns x (col) and y (row).
find_regions <- function(grid) {
  nr <- nrow(grid); nc <- ncol(grid)
  visited <- matrix(FALSE, nr, nc)
  regions <- list()

  for (r in seq_len(nr)) {
    for (c in seq_len(nc)) {
      if (!visited[r, c]) {
        letter  <- grid[r, c]
        queue   <- list(c(r, c))
        cells_r <- integer(0)
        cells_c <- integer(0)
        visited[r, c] <- TRUE

        while (length(queue) > 0) {
          cell  <- queue[[1]]; queue <- queue[-1]
          cr <- cell[1]; cc <- cell[2]
          cells_r <- c(cells_r, cr)
          cells_c <- c(cells_c, cc)

          for (nb in list(c(cr - 1, cc), c(cr + 1, cc), c(cr, cc - 1), c(cr, cc + 1))) {
            nr2 <- nb[1]; nc2 <- nb[2]
            if (nr2 >= 1 && nr2 <= nr && nc2 >= 1 && nc2 <= nc &&
                !visited[nr2, nc2] && grid[nr2, nc2] == letter) {
              visited[nr2, nc2] <- TRUE
              queue <- c(queue, list(c(nr2, nc2)))
            }
          }
        }
        # x = col, y = row — consistent with parse_grid convention
        regions <- c(regions, list(tibble(x = cells_c, y = cells_r)))
      }
    }
  }
  regions
}

regions <- find_regions(grid_mat)

# Part 1: area * perimeter --------------------------------------------------
# Perimeter = boundary edges = neighbours not in the region.

calc_perimeter <- function(region) {
  deltas <- tibble(dx = c(0L, 0L, -1L, 1L), dy = c(-1L, 1L, 0L, 0L))

  region |>
    cross_join(deltas) |>
    mutate(nx = x + dx, ny = y + dy) |>
    anti_join(region, by = c("nx" = "x", "ny" = "y")) |>
    nrow()
}

result1 <- map_int(regions, ~ nrow(.x) * calc_perimeter(.x)) |> sum()
cat("Part 1:", result1, "\n")

# Part 2: area * sides -------------------------------------------------------
# A "side" is a maximal contiguous run of boundary edges in the same direction.
#   N/S edges: group by (direction, y), count contiguous runs of x
#   E/W edges: group by (direction, x), count contiguous runs of y

# Count contiguous runs in a sorted integer vector.
runs_in_sorted <- function(vals) {
  if (length(vals) == 0L) return(0L)
  1L + sum(diff(sort(vals)) > 1L)
}

count_sides <- function(region) {
  deltas <- tibble(
    dx        = c( 0L,  0L, -1L,  1L),
    dy        = c(-1L,  1L,  0L,  0L),
    direction = c("N", "S", "W", "E")
  )

  boundary <- region |>
    cross_join(deltas) |>
    mutate(nx = x + dx, ny = y + dy) |>
    anti_join(region, by = c("nx" = "x", "ny" = "y"))

  # N/S: for each (direction, row), count contiguous col runs
  ns <- boundary |>
    filter(direction %in% c("N", "S")) |>
    group_by(direction, y) |>
    summarise(runs = runs_in_sorted(x), .groups = "drop") |>
    pull(runs) |>
    sum()

  # E/W: for each (direction, col), count contiguous row runs
  ew <- boundary |>
    filter(direction %in% c("E", "W")) |>
    group_by(direction, x) |>
    summarise(runs = runs_in_sorted(y), .groups = "drop") |>
    pull(runs) |>
    sum()

  ns + ew
}

result2 <- map_int(regions, ~ nrow(.x) * count_sides(.x)) |> sum()
cat("Part 2:", result2, "\n")
