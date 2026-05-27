library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day21_input.txt")
mat <- do.call(rbind, strsplit(input, ""))
nr <- nrow(mat); nc <- ncol(mat)

start <- which(mat == "S", arr.ind = TRUE)
sr <- start[1]; sc <- start[2]
mat[sr, sc] <- "."  # treat S as garden plot

# BFS on the finite grid, return count of positions reachable at exactly `steps`
count_reachable <- function(steps) {
  dist <- matrix(NA_integer_, nr, nc)
  dist[sr, sc] <- 0L
  queue <- list(c(sr, sc))

  while (length(queue) > 0) {
    cur <- queue[[1]]; queue <- queue[-1]
    r <- cur[1]; c <- cur[2]; d <- dist[r, c]
    if (d >= steps) next
    for (nb in list(c(r-1L, c), c(r+1L, c), c(r, c-1L), c(r, c+1L))) {
      r2 <- nb[1]; c2 <- nb[2]
      if (r2 >= 1L && r2 <= nr && c2 >= 1L && c2 <= nc &&
          mat[r2, c2] != "#" && is.na(dist[r2, c2])) {
        dist[r2, c2] <- d + 1L
        queue <- c(queue, list(c(r2, c2)))
      }
    }
  }
  sum(!is.na(dist) & dist %% 2L == steps %% 2L)
}

result1 <- count_reachable(64L)
cat("Part 1:", result1, "\n")

# Part 2: infinite tiling with quadratic extrapolation
# 26501365 = 65 + 202300 * 131 (half-width = 65, grid = 131x131)
# BFS on an expanded tiled grid of size (2*expand+1) x (2*expand+1) tiles
bfs_expanded <- function(steps) {
  expand <- ceiling(steps / nr) + 1L
  start_r <- sr + expand * nr
  start_c <- sc + expand * nc
  gr <- nr * (2L * expand + 1L)
  gc <- nc * (2L * expand + 1L)

  dist <- matrix(Inf, gr, gc)
  dist[start_r, start_c] <- 0
  queue <- list(c(start_r, start_c))

  while (length(queue) > 0) {
    cur <- queue[[1]]; queue <- queue[-1]
    r <- cur[1]; c <- cur[2]; d <- dist[r, c]
    if (d >= steps) next
    for (nb in list(c(r-1L, c), c(r+1L, c), c(r, c-1L), c(r, c+1L))) {
      r2 <- nb[1]; c2 <- nb[2]
      if (r2 >= 1L && r2 <= gr && c2 >= 1L && c2 <= gc) {
        mr <- ((r2 - 1L) %% nr) + 1L
        mc <- ((c2 - 1L) %% nc) + 1L
        if (mat[mr, mc] != "#" && is.infinite(dist[r2, c2])) {
          dist[r2, c2] <- d + 1L
          queue <- c(queue, list(c(r2, c2)))
        }
      }
    }
  }
  sum(is.finite(dist) & dist %% 2 == steps %% 2)
}

target <- 26501365L
half   <- nr %/% 2L  # = 65

y0 <- bfs_expanded(half)
y1 <- bfs_expanded(half + nr)
y2 <- bfs_expanded(half + 2L * nr)

# Fit quadratic f(n) = a*n^2 + b*n + c where n = (steps - half) / nr
# n=0 -> y0, n=1 -> y1, n=2 -> y2
c_c <- y0
a_c <- (y2 - 2 * y1 + y0) / 2
b_c <- y1 - y0 - a_c
n   <- (target - half) / nr

result2 <- a_c * n^2 + b_c * n + c_c
cat("Part 2:", format(result2, scientific = FALSE), "\n")
