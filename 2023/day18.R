library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day18_input.txt")

plan <- tibble(line = input) |>
  mutate(
    dir   = str_extract(line, "^[RDLU]"),
    steps = as.integer(str_extract(line, "\\d+")),
    hex   = str_extract(line, "(?<=#)[0-9a-f]+")
  )

solve <- function(dirs, steps_vec) {
  n      <- length(dirs)
  x      <- numeric(n + 1)
  y      <- numeric(n + 1)
  x[1]   <- 0
  y[1]   <- 0
  boundary <- 0

  for (i in seq_len(n)) {
    d <- dirs[i]
    s <- steps_vec[i]
    boundary <- boundary + s
    dx <- switch(d, R = 1L, L = -1L, U = 0L, D = 0L)
    dy <- switch(d, R = 0L, L = 0L,  U = 1L, D = -1L)
    x[i + 1] <- x[i] + dx * s
    y[i + 1] <- y[i] + dy * s
  }

  # Shoelace formula for polygon area
  x1   <- x[1:n]
  y1   <- y[1:n]
  x2   <- x[2:(n + 1)]
  y2   <- y[2:(n + 1)]
  area <- abs(sum(x1 * y2 - x2 * y1)) / 2

  # Pick's theorem: total = interior + boundary
  # interior = area - boundary/2 + 1
  interior <- area - boundary / 2 + 1
  interior + boundary
}

result1 <- solve(plan$dir, plan$steps)
cat("Part 1:", format(result1, scientific = FALSE), "\n")

# Part 2: decode hex — first 5 chars = distance, 6th char = direction
dir_map <- c("0" = "R", "1" = "D", "2" = "L", "3" = "U")

plan_p2 <- plan |>
  mutate(
    steps2 = strtoi(str_sub(hex, 1, 5), base = 16L),
    dir2   = dir_map[str_sub(hex, 6, 6)]
  )

result2 <- solve(plan_p2$dir2, plan_p2$steps2)
cat("Part 2:", format(result2, scientific = FALSE), "\n")
