library(tidyverse)
source("scripts/utils.R")

# Grid dimensions
WIDTH  <- 101L
HEIGHT <- 103L

# Parse robots: "p=px,py v=vx,vy"
# str_match captures all four values in one pass
robots <- tibble(line = read_lines("2024/day14_input.txt")) |>
  mutate(
    m  = str_match(line, "p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)"),
    px = as.integer(m[, 2]),
    py = as.integer(m[, 3]),
    vx = as.integer(m[, 4]),
    vy = as.integer(m[, 5])
  ) |>
  select(px, py, vx, vy)

# Simulate positions after t seconds with torus wrapping
positions_at <- function(robots, t) {
  robots |>
    mutate(
      x = ((px + t * vx) %% WIDTH  + WIDTH)  %% WIDTH,
      y = ((py + t * vy) %% HEIGHT + HEIGHT) %% HEIGHT
    ) |>
    select(x, y)
}

# Part 1: safety factor after 100 seconds
mid_x <- (WIDTH  - 1L) %/% 2L   # = 50
mid_y <- (HEIGHT - 1L) %/% 2L   # = 51

pos100 <- positions_at(robots, 100L)

q1 <- pos100 |> filter(x <  mid_x, y <  mid_y) |> nrow()
q2 <- pos100 |> filter(x >  mid_x, y <  mid_y) |> nrow()
q3 <- pos100 |> filter(x <  mid_x, y >  mid_y) |> nrow()
q4 <- pos100 |> filter(x >  mid_x, y >  mid_y) |> nrow()

result1 <- q1 * q2 * q3 * q4
cat("Part 1:", result1, "\n")

# Part 2: find t where robots form a Christmas tree pattern
# Heuristic: minimise var(x) + var(y) — robots cluster spatially
max_t <- 10000L

var_sum <- map_dbl(seq_len(max_t), function(t) {
  pos <- positions_at(robots, t)
  var(pos$x) + var(pos$y)
})

result2 <- which.min(var_sum)
cat("Part 2:", result2, "\n")
