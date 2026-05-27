library(tidyverse)
source("scripts/utils.R")

# Parse all machines into a single tibble (one row per machine)
machines <- read_paragraphs("2024/day13_input.txt") |>
  map_dfr(function(block) {
    tibble(
      ax = as.numeric(str_extract(block[1], "(?<=X\\+)\\d+")),
      ay = as.numeric(str_extract(block[1], "(?<=Y\\+)\\d+")),
      bx = as.numeric(str_extract(block[2], "(?<=X\\+)\\d+")),
      by = as.numeric(str_extract(block[2], "(?<=Y\\+)\\d+")),
      px = as.numeric(str_extract(block[3], "(?<=X=)\\d+")),
      py = as.numeric(str_extract(block[3], "(?<=Y=)\\d+"))
    )
  })

# Solve via Cramer's rule (vectorised over tibble rows):
#   a*ax + b*bx = px
#   a*ay + b*by = py
# det = ax*by - ay*bx
# a   = (px*by - py*bx) / det
# b   = (ax*py - ay*px) / det
# Cost = 3a + b; accepted only if a and b are non-negative integers (±1e-3 tolerance)
cramer_costs <- function(df) {
  df |>
    mutate(
      det   = ax * by - ay * bx,
      a_raw = (px * by - py * bx) / det,
      b_raw = (ax * py - ay * px) / det,
      a_val = round(a_raw),
      b_val = round(b_raw),
      valid = det != 0 &
              a_raw >= -1e-6 & b_raw >= -1e-6 &
              abs(a_val - a_raw) < 1e-3 & abs(b_val - b_raw) < 1e-3,
      cost  = if_else(valid, 3 * a_val + b_val, NA_real_)
    )
}

# Part 1: press counts must be <= 100
result1 <- machines |>
  cramer_costs() |>
  filter(!is.na(cost), a_val <= 100, b_val <= 100) |>
  summarise(total = sum(cost)) |>
  pull(total)

cat("Part 1:", result1, "\n")

# Part 2: add large offset to prize coordinates, no press-count limit
OFFSET <- 10000000000000

result2 <- machines |>
  mutate(px = px + OFFSET, py = py + OFFSET) |>
  cramer_costs() |>
  filter(!is.na(cost)) |>
  summarise(total = sum(cost)) |>
  pull(total)

cat("Part 2:", format(result2, scientific = FALSE), "\n")
