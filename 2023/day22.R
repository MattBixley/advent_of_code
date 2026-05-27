library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day22_input.txt")

# Parse bricks: x1,y1,z1~x2,y2,z2
bricks <- tibble(line = input) |>
  mutate(
    x1 = as.integer(str_extract(line, "^\\d+")),
    y1 = as.integer(str_extract(line, "(?<=,)\\d+(?=,)")),
    z1 = as.integer(str_extract(line, "(?<=,)\\d+(?=~)")),
    x2 = as.integer(str_extract(line, "(?<=~)\\d+")),
    y2 = as.integer(str_extract(line, "(?<=~\\d{1,3},)\\d+")),
    z2 = as.integer(str_extract(line, "\\d+$"))
  ) |>
  arrange(pmin(z1, z2)) |>
  mutate(id = row_number()) |>
  select(-line)

# Check if two bricks overlap in x,y footprint
xy_overlap <- function(ax1, ax2, ay1, ay2, bx1, bx2, by1, by2) {
  max(ax1, ax2) >= min(bx1, bx2) &&
  min(ax1, ax2) <= max(bx1, bx2) &&
  max(ay1, ay2) >= min(by1, by2) &&
  min(ay1, ay2) <= max(by1, by2)
}

# Settle bricks: drop each one as far as possible
n <- nrow(bricks)
bz1 <- bricks$z1; bz2 <- bricks$z2
bx1 <- bricks$x1; bx2 <- bricks$x2
by1 <- bricks$y1; by2 <- bricks$y2

for (i in seq_len(n)) {
  zmin <- min(bz1[i], bz2[i])
  top_z <- 0L
  for (j in seq_len(i - 1L)) {
    top_j <- max(bz1[j], bz2[j])
    if (top_j < zmin &&
        xy_overlap(bx1[i], bx2[i], by1[i], by2[i],
                   bx1[j], bx2[j], by1[j], by2[j])) {
      top_z <- max(top_z, top_j)
    }
  }
  drop <- zmin - top_z - 1L
  bz1[i] <- bz1[i] - drop
  bz2[i] <- bz2[i] - drop
}

# Build support graph
# supports[[i]]      = indices of bricks resting on top of i
# supported_by[[i]]  = indices of bricks directly below i
supports     <- vector("list", n)
supported_by <- vector("list", n)

for (i in seq_len(n)) {
  top_i <- max(bz1[i], bz2[i])
  for (j in seq_len(n)) {
    if (i == j) next
    bot_j <- min(bz1[j], bz2[j])
    if (bot_j == top_i + 1L &&
        xy_overlap(bx1[i], bx2[i], by1[i], by2[i],
                   bx1[j], bx2[j], by1[j], by2[j])) {
      supports[[i]]     <- c(supports[[i]], j)
      supported_by[[j]] <- c(supported_by[[j]], i)
    }
  }
}

# Part 1: brick i is safe to disintegrate if every brick it supports
# has at least 2 supporters
safe <- sapply(seq_len(n), function(i) {
  all(sapply(supports[[i]], function(j) length(supported_by[[j]]) >= 2L))
})
result1 <- sum(safe)
cat("Part 1:", result1, "\n")

# Part 2: chain-reaction count when each brick is removed
count_falls <- function(remove_id) {
  fallen <- c(remove_id)
  queue  <- as.list(supports[[remove_id]])
  while (length(queue) > 0) {
    j <- queue[[1]]; queue <- queue[-1]
    if (j %in% fallen) next
    if (all(supported_by[[j]] %in% fallen)) {
      fallen <- c(fallen, j)
      queue  <- c(queue, as.list(supports[[j]]))
    }
  }
  length(fallen) - 1L  # exclude the removed brick itself
}

result2 <- sum(map_int(seq_len(n), count_falls))
cat("Part 2:", result2, "\n")
