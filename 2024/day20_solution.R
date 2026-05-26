library(tidyverse)

# Day 20: Race Condition
# Part 1: How many cheats (max 2 steps through walls) save >= 100 picoseconds?
# Part 2: How many cheats (max 20 steps through walls) save >= 100 picoseconds?

# -- Input --------------------------------------------------------------------
input <- readLines("2024/day20_input.txt")

grid <- do.call(rbind, strsplit(input, ""))
nrow_g <- nrow(grid)
ncol_g <- ncol(grid)

s_pos <- which(grid == "S", arr.ind = TRUE)
e_pos <- which(grid == "E", arr.ind = TRUE)

# BFS from a start position to get distances to all reachable track cells
bfs <- function(start, grid) {
  nrow_g <- nrow(grid); ncol_g <- ncol(grid)
  dist <- matrix(Inf, nrow = nrow_g, ncol = ncol_g)
  dist[start[1], start[2]] <- 0
  queue <- list(start)
  dirs <- list(c(-1L, 0L), c(1L, 0L), c(0L, -1L), c(0L, 1L))
  head_idx <- 1L
  while (head_idx <= length(queue)) {
    pos <- queue[[head_idx]]; head_idx <- head_idx + 1L
    d <- dist[pos[1], pos[2]]
    for (dir in dirs) {
      nr <- pos[1] + dir[1]; nc <- pos[2] + dir[2]
      if (nr >= 1L && nr <= nrow_g && nc >= 1L && nc <= ncol_g &&
          grid[nr, nc] != "#" && dist[nr, nc] == Inf) {
        dist[nr, nc] <- d + 1L
        queue <- c(queue, list(c(nr, nc)))
      }
    }
  }
  dist
}

dist_start <- bfs(s_pos, grid)
dist_end   <- bfs(e_pos, grid)
normal_time <- dist_start[e_pos[1], e_pos[2]]

track_cells <- which(grid != "#", arr.ind = TRUE)
rs <- track_cells[, 1]; cs <- track_cells[, 2]

# Distances for each track cell
ds <- dist_start[cbind(rs, cs)]
de <- dist_end[cbind(rs, cs)]

# Count cheats: for each start cell, check all end cells within max manhattan dist
count_cheats_vec <- function(max_cheat, threshold = 100L) {
  count <- 0L
  valid_s <- is.finite(ds)
  ri <- rs[valid_s]; ci <- cs[valid_s]; dsi <- ds[valid_s]
  valid_e <- is.finite(de)
  re <- rs[valid_e]; ce <- cs[valid_e]; dee <- de[valid_e]

  for (i in seq_along(ri)) {
    mhat <- abs(re - ri[i]) + abs(ce - ci[i])
    ok <- mhat >= 2L & mhat <= max_cheat
    if (!any(ok)) next
    cheat_time <- dsi[i] + mhat[ok] + dee[ok]
    count <- count + sum(cheat_time <= normal_time - threshold)
  }
  count
}

# Part 1: cheat up to 2 steps
part1 <- count_cheats_vec(2L, 100L)
cat("Part 1:", part1, "\n")

# Part 2: cheat up to 20 steps
part2 <- count_cheats_vec(20L, 100L)
cat("Part 2:", part2, "\n")
