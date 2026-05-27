library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2024/day6_input.txt")

# Parse via shared utility, then convert to character matrix for simulation
grid_tbl <- parse_grid(input)
nrows <- max(grid_tbl$y)
ncols <- max(grid_tbl$x)
grid <- matrix(grid_tbl$value, nrow = nrows, ncol = ncols, byrow = TRUE)

start_pos <- which(grid == "^", arr.ind = TRUE)
start_r <- start_pos[1, 1]
start_c <- start_pos[1, 2]

# Directions: up, right, down, left (row_delta, col_delta)
dirs <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))

simulate <- function(g, r, c, dir_idx) {
  visited <- matrix(FALSE, nrows, ncols)
  states <- list()

  repeat {
    visited[r, c] <- TRUE
    state <- (r - 1L) * ncols * 4L + (c - 1L) * 4L + dir_idx
    if (state %in% states) return(list(loop = TRUE, visited = visited))
    states[[length(states) + 1L]] <- state

    d <- dirs[[dir_idx]]
    nr <- r + d[1]
    nc <- c + d[2]

    if (nr < 1 || nr > nrows || nc < 1 || nc > ncols) break

    if (g[nr, nc] == "#") {
      dir_idx <- (dir_idx %% 4L) + 1L
    } else {
      r <- nr
      c <- nc
    }
  }
  list(loop = FALSE, visited = visited)
}

# Part 1
result1_sim <- simulate(grid, start_r, start_c, 1L)
p1_visited <- result1_sim$visited
result1 <- sum(p1_visited)

cat("Part 1:", result1, "\n")

# Part 2 - try placing obstacle at each cell visited in Part 1 (except start)
candidate_cells <- which(p1_visited, arr.ind = TRUE)
candidate_cells <- candidate_cells[
  !(candidate_cells[, 1] == start_r & candidate_cells[, 2] == start_c), ]

# Use integer state encoding for fast loop detection
simulate_loop <- function(g, r0, c0) {
  r <- r0; c <- c0; dir_idx <- 1L
  seen <- logical(nrows * ncols * 4L)

  repeat {
    state <- (r - 1L) * ncols * 4L + (c - 1L) * 4L + dir_idx
    if (seen[state]) return(TRUE)
    seen[state] <- TRUE

    d <- dirs[[dir_idx]]
    nr <- r + d[1]
    nc <- c + d[2]

    if (nr < 1 || nr > nrows || nc < 1 || nc > ncols) return(FALSE)

    if (g[nr, nc] == "#") {
      dir_idx <- (dir_idx %% 4L) + 1L
    } else {
      r <- nr
      c <- nc
    }
  }
}

loop_count <- 0L
for (i in seq_len(nrow(candidate_cells))) {
  or <- candidate_cells[i, 1]
  oc <- candidate_cells[i, 2]
  grid[or, oc] <- "#"
  if (simulate_loop(grid, start_r, start_c)) loop_count <- loop_count + 1L
  grid[or, oc] <- "."
}

result2 <- loop_count
cat("Part 2:", result2, "\n")
