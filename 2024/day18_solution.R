library(tidyverse)

input <- read_lines("2024/day18_input.txt")

# ---- Parse input ----
coords <- strsplit(input, ",") |>
  lapply(as.integer) |>
  do.call(rbind, args = _)
# coords[i,] = c(x, y), 0-indexed; grid is 71x71 (0..70)

GRID_SIZE <- 71L  # 0..70

# ---- BFS from (0,0) to (70,70) ----
bfs_steps <- function(blocked_set) {
  # blocked_set: set of encoded positions (r*GRID_SIZE + c) that are blocked
  start <- 0L * GRID_SIZE + 0L
  goal  <- 70L * GRID_SIZE + 70L

  if (start %in% blocked_set) return(NA_integer_)

  visited <- logical(GRID_SIZE * GRID_SIZE)
  visited[start + 1L] <- TRUE

  queue <- start
  steps <- 0L

  while (length(queue) > 0) {
    next_queue <- integer(0)
    for (pos in queue) {
      if (pos == goal) return(steps)
      r <- pos %/% GRID_SIZE
      c <- pos %%  GRID_SIZE

      # Neighbours: up, down, left, right
      nbrs <- c(
        if (r > 0L)             (r - 1L) * GRID_SIZE + c  else NA_integer_,
        if (r < GRID_SIZE - 1L) (r + 1L) * GRID_SIZE + c  else NA_integer_,
        if (c > 0L)              r * GRID_SIZE + (c - 1L) else NA_integer_,
        if (c < GRID_SIZE - 1L)  r * GRID_SIZE + (c + 1L) else NA_integer_
      )
      nbrs <- nbrs[!is.na(nbrs)]
      nbrs <- nbrs[!visited[nbrs + 1L] & !(nbrs %in% blocked_set)]
      visited[nbrs + 1L] <- TRUE
      next_queue <- c(next_queue, nbrs)
    }
    queue <- next_queue
    steps <- steps + 1L
  }
  NA_integer_
}

# Encode each coordinate pair as row * GRID_SIZE + col
# Input is (x=col, y=row)
encode_coord <- function(i) {
  x <- coords[i, 1]
  y <- coords[i, 2]
  y * GRID_SIZE + x
}

all_encoded <- sapply(seq_len(nrow(coords)), encode_coord)

# Part 1: first 1024 bytes fallen
N_PART1 <- 1024L
blocked1 <- all_encoded[seq_len(N_PART1)]
result1  <- bfs_steps(blocked1)
cat("Part 1:", result1, "\n")

# ---- Part 2: binary search for first byte that blocks the path ----
# We know at 1024 it's still open; find the first i where path is blocked.
lo <- N_PART1 + 1L
hi <- nrow(coords)

while (lo < hi) {
  mid      <- (lo + hi) %/% 2L
  blocked  <- all_encoded[seq_len(mid)]
  reachable <- !is.na(bfs_steps(blocked))
  if (reachable) {
    lo <- mid + 1L
  } else {
    hi <- mid
  }
}

# lo is the index of the first blocking byte
blocking_byte <- coords[lo, ]
result2 <- paste(blocking_byte[1], blocking_byte[2], sep = ",")
cat("Part 2:", result2, "\n")
