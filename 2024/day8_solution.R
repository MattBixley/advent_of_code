library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2024/day8_input.txt")

grid <- parse_grid(input)
max_x <- max(grid$x)
max_y <- max(grid$y)

in_bounds <- function(x, y) x >= 1 & x <= max_x & y >= 1 & y <= max_y

antennas <- grid |> filter(value != ".")

# --- Part 1: two antinodes per pair (one grid-step beyond each antenna) -------

antinodes_p1 <- antennas |>
  group_by(value) |>
  filter(n() >= 2) |>
  group_modify(~ {
    pos <- .x
    idx <- combn(nrow(pos), 2)
    map_dfr(seq_len(ncol(idx)), function(k) {
      i <- idx[1, k]; j <- idx[2, k]
      ax <- pos$x[i]; ay <- pos$y[i]
      bx <- pos$x[j]; by <- pos$y[j]
      dx <- ax - bx;  dy <- ay - by
      bind_rows(
        tibble(x = ax + dx, y = ay + dy),
        tibble(x = bx - dx, y = by - dy)
      ) |>
        filter(in_bounds(x, y))
    })
  }) |>
  ungroup()

result1 <- antinodes_p1 |> distinct(x, y) |> nrow()
cat("Part 1:", result1, "\n")

# --- Part 2: all collinear points in bounds (resonant harmonics) --------------

antinodes_p2 <- antennas |>
  group_by(value) |>
  filter(n() >= 2) |>
  group_modify(~ {
    pos <- .x
    idx <- combn(nrow(pos), 2)
    map_dfr(seq_len(ncol(idx)), function(k) {
      i <- idx[1, k]; j <- idx[2, k]
      ax <- pos$x[i]; ay <- pos$y[i]
      bx <- pos$x[j]; by <- pos$y[j]
      dx <- ax - bx;  dy <- ay - by

      # Step from A outward in +d direction until out of bounds
      fwd <- tibble(step = 0L:max(max_x, max_y)) |>
        mutate(x = ax + step * dx, y = ay + step * dy) |>
        filter(in_bounds(x, y)) |>
        select(x, y)

      # Step from A outward in -d direction until out of bounds
      bwd <- tibble(step = 1L:max(max_x, max_y)) |>
        mutate(x = ax - step * dx, y = ay - step * dy) |>
        filter(in_bounds(x, y)) |>
        select(x, y)

      bind_rows(fwd, bwd)
    })
  }) |>
  ungroup()

result2 <- antinodes_p2 |> distinct(x, y) |> nrow()
cat("Part 2:", result2, "\n")
