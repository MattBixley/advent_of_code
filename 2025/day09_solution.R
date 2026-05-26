library(tidyverse)

# Day 9: Movie Theater
# Part 1: Find the largest rectangle where two opposite corners are red tiles.
#         Area = (|x2-x1|+1) * (|y2-y1|+1). All pairs of red tiles are candidates.
# Part 2: Rectangle must use only red/green tiles.
#         Green tiles = edges + interior of rectilinear polygon formed by red tiles.
#         Rectangle is valid if it is fully inside the polygon.
#
# Sample (P1 expected: 50, P2 expected: 24):
# 7,1  11,1  11,7  9,7  9,5  2,5  2,3  7,3

input <- read_lines("/mnt/c/Users/MattBixley/Code/advent_of_code/2025/day09_input.txt")

# --- Part 1 ---
solve_part1 <- function(input) {
  coords <- strsplit(input, ",") |> do.call(what = rbind) |> apply(2, as.numeric)
  x <- coords[,1]; y <- coords[,2]
  n <- length(x)
  idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  dx <- abs(x[idx[,1]] - x[idx[,2]]) + 1
  dy <- abs(y[idx[,1]] - y[idx[,2]]) + 1
  max(dx * dy)
}

cat("Part 1:", solve_part1(input), "\n")  # 4761736832

# --- Part 2 ---
# Point-in-polygon test (ray casting)
pip <- function(px, py, poly_x, poly_y) {
  n <- length(poly_x)
  inside <- FALSE; j <- n
  for (i in 1:n) {
    xi <- poly_x[i]; yi <- poly_y[i]; xj <- poly_x[j]; yj <- poly_y[j]
    if (((yi > py) != (yj > py)) && (px < (xj - xi) * (py - yi) / (yj - yi) + xi))
      inside <- !inside
    j <- i
  }
  inside
}

on_boundary <- function(px, py, poly_x, poly_y) {
  n <- length(poly_x)
  for (i in 1:n) {
    j <- if (i == n) 1 else i + 1
    xi <- poly_x[i]; yi <- poly_y[i]; xj <- poly_x[j]; yj <- poly_y[j]
    if (yi == yj && py == yi && px >= min(xi, xj) && px <= max(xi, xj)) return(TRUE)
    if (xi == xj && px == xi && py >= min(yi, yj) && py <= max(yi, yj)) return(TRUE)
  }
  FALSE
}

in_or_on <- function(px, py, poly_x, poly_y) pip(px, py, poly_x, poly_y) || on_boundary(px, py, poly_x, poly_y)

solve_part2 <- function(input) {
  coords <- strsplit(input, ",") |> do.call(what = rbind) |> apply(2, as.numeric)
  red_x <- coords[,1]; red_y <- coords[,2]; n <- nrow(coords)
  poly_x <- red_x; poly_y <- red_y

  # Pre-separate polygon edges
  h_edges <- list(y = numeric(0), x_lo = numeric(0), x_hi = numeric(0))
  v_edges <- list(x = numeric(0), y_lo = numeric(0), y_hi = numeric(0))
  for (k in 1:n) {
    l <- if (k == n) 1 else k + 1
    xk <- poly_x[k]; yk <- poly_y[k]; xl <- poly_x[l]; yl <- poly_y[l]
    if (yk == yl) {
      h_edges$y <- c(h_edges$y, yk)
      h_edges$x_lo <- c(h_edges$x_lo, min(xk, xl))
      h_edges$x_hi <- c(h_edges$x_hi, max(xk, xl))
    } else {
      v_edges$x <- c(v_edges$x, xk)
      v_edges$y_lo <- c(v_edges$y_lo, min(yk, yl))
      v_edges$y_hi <- c(v_edges$y_hi, max(yk, yl))
    }
  }

  max_area <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      x1 <- min(red_x[i], red_x[j]); x2 <- max(red_x[i], red_x[j])
      y1 <- min(red_y[i], red_y[j]); y2 <- max(red_y[i], red_y[j])
      area <- (x2 - x1 + 1) * (y2 - y1 + 1)
      if (area <= max_area) next

      # All 4 corners + center must be inside/on polygon
      if (!in_or_on(x1, y1, poly_x, poly_y)) next
      if (!in_or_on(x2, y1, poly_x, poly_y)) next
      if (!in_or_on(x1, y2, poly_x, poly_y)) next
      if (!in_or_on(x2, y2, poly_x, poly_y)) next
      if (!in_or_on((x1 + x2) / 2, (y1 + y2) / 2, poly_x, poly_y)) next

      # No polygon edge may cut through the rectangle interior
      if (any(h_edges$y > y1 & h_edges$y < y2 & h_edges$x_lo < x2 & h_edges$x_hi > x1)) next
      if (any(v_edges$x > x1 & v_edges$x < x2 & v_edges$y_lo < y2 & v_edges$y_hi > y1)) next

      max_area <- area
    }
  }
  max_area
}

cat("Part 2:", solve_part2(input), "\n")  # 1452422268
