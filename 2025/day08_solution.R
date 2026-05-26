library(tidyverse)

# Day 8: Playground
# Part 1: Given 3D coordinates of junction boxes, connect the 1000 closest pairs.
#         Multiply sizes of the three largest connected components.
# Part 2: Keep connecting closest pairs until all form one circuit.
#         Multiply X coordinates of the last pair connected.
#
# Sample (P1 expected: 40, P2 expected: 25272)

input <- read_lines("/mnt/c/Users/MattBixley/Code/advent_of_code/2025/day08_input.txt")

parse_coords <- function(input) {
  coords <- strsplit(input, ",") |> do.call(what = rbind)
  apply(coords, 2, as.numeric)
}

build_sorted_pairs <- function(coords) {
  n <- nrow(coords)
  x <- coords[,1]; y <- coords[,2]; z <- coords[,3]
  idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  i_idx <- idx[,1]; j_idx <- idx[,2]
  dx <- x[i_idx] - x[j_idx]
  dy <- y[i_idx] - y[j_idx]
  dz <- z[i_idx] - z[j_idx]
  dist2 <- dx^2 + dy^2 + dz^2
  order_idx <- order(dist2)
  list(i = i_idx[order_idx], j = j_idx[order_idx])
}

find_root <- function(parent, x) {
  while (parent[x] != x) x <- parent[x]
  x
}

# --- Part 1 ---
solve_part1 <- function(input, n_connections = 1000) {
  coords <- parse_coords(input)
  n <- nrow(coords)
  pairs <- build_sorted_pairs(coords)

  parent <- 1:n
  for (k in 1:n_connections) {
    px <- find_root(parent, pairs$i[k])
    py <- find_root(parent, pairs$j[k])
    if (px != py) parent[px] <- py
  }

  roots <- sapply(1:n, find_root, parent = parent)
  sizes <- sort(table(roots), decreasing = TRUE)
  prod(as.integer(sizes[1:3]))
}

cat("Part 1:", solve_part1(input), "\n")  # 123420

# --- Part 2 ---
solve_part2 <- function(input) {
  coords <- parse_coords(input)
  n <- nrow(coords)
  pairs <- build_sorted_pairs(coords)
  x_coords <- coords[,1]

  parent <- 1:n
  components <- n

  for (k in seq_along(pairs$i)) {
    ii <- pairs$i[k]; jj <- pairs$j[k]
    px <- find_root(parent, ii); py <- find_root(parent, jj)
    if (px != py) {
      parent[px] <- py
      components <- components - 1
      if (components == 1) return(x_coords[ii] * x_coords[jj])
    }
  }
}

cat("Part 2:", solve_part2(input), "\n")  # 673096646
