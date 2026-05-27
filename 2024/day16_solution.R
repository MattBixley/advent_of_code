library(tidyverse)

input <- read_lines("2024/day16_input.txt")

# ---- Parse grid ----
grid <- do.call(rbind, strsplit(input, ""))
nrow_g <- nrow(grid)
ncol_g <- ncol(grid)

start_pos <- which(grid == "S", arr.ind = TRUE)[1, ]
end_pos   <- which(grid == "E", arr.ind = TRUE)[1, ]

# Directions: 1=East, 2=South, 3=West, 4=North
# dr/dc for each direction
DR <- c( 0,  1,  0, -1)
DC <- c( 1,  0, -1,  0)
# direction index: East=1, South=2, West=3, North=4

# State = (row, col, dir) encoded as integer for fast lookup
# Use a named list / environment as a distance table
# States: row * ncol_g * 4 + col * 4 + dir  (0-indexed internally)
encode <- function(r, c, d) (r - 1L) * ncol_g * 4L + (c - 1L) * 4L + (d - 1L)
n_states <- nrow_g * ncol_g * 4L

# ---- Dijkstra ----
# Returns vector of distances indexed by encoded state
dijkstra <- function(start_r, start_c, start_d, reverse = FALSE) {
  dist <- rep(Inf, n_states)
  s0   <- encode(start_r, start_c, start_d) + 1L
  dist[s0] <- 0

  # Priority queue: data frame sorted by cost (simple heap via ordered insertions)
  # Use a list-based min-heap: (cost, r, c, d)
  # For R, we'll use a simple approach: store as matrix and extract min each step.
  # For large grids this is O(n^2) but workable for AoC maze sizes (~141x141 * 4 states).

  # Use a named environment as a lightweight heap via sorted vector trick
  # Actually use a simple unsorted list and track visited.
  visited <- rep(FALSE, n_states)

  # Priority queue: matrix with columns [cost, r, c, d]
  pq <- matrix(c(0L, start_r, start_c, start_d), nrow = 1)

  while (nrow(pq) > 0) {
    # Extract minimum cost entry
    min_idx <- which.min(pq[, 1])
    cur <- pq[min_idx, ]
    pq  <- pq[-min_idx, , drop = FALSE]

    cost <- cur[1]
    r    <- cur[2]
    c    <- cur[3]
    d    <- cur[4]

    sid <- encode(r, c, d) + 1L
    if (visited[sid]) next
    visited[sid] <- TRUE

    if (!reverse) {
      # Forward edges: move forward, turn left, turn right
      # 1. Move forward
      nr <- r + DR[d]
      nc <- c + DC[d]
      if (nr >= 1 && nr <= nrow_g && nc >= 1 && nc <= ncol_g && grid[nr, nc] != "#") {
        new_cost <- cost + 1L
        tid <- encode(nr, nc, d) + 1L
        if (new_cost < dist[tid]) {
          dist[tid] <- new_cost
          pq <- rbind(pq, c(new_cost, nr, nc, d))
        }
      }
      # 2. Turn left (counter-clockwise: d -> d-1 mod 4, 1-indexed)
      d_left <- if (d == 1L) 4L else d - 1L
      tid <- encode(r, c, d_left) + 1L
      if (cost + 1000L < dist[tid]) {
        dist[tid] <- cost + 1000L
        pq <- rbind(pq, c(cost + 1000L, r, c, d_left))
      }
      # 3. Turn right
      d_right <- if (d == 4L) 1L else d + 1L
      tid <- encode(r, c, d_right) + 1L
      if (cost + 1000L < dist[tid]) {
        dist[tid] <- cost + 1000L
        pq <- rbind(pq, c(cost + 1000L, r, c, d_right))
      }
    } else {
      # Reverse edges: move backward (someone moved forward to reach us),
      # and turns are symmetric (turning costs the same both ways).
      # Reverse of "move forward in direction d to (r,c)" is "move backward from (r,c)"
      # i.e., the predecessor was at (r - DR[d], c - DC[d]) facing d.
      pr <- r - DR[d]
      pc <- c - DC[d]
      if (pr >= 1 && pr <= nrow_g && pc >= 1 && pc <= ncol_g && grid[pr, pc] != "#") {
        new_cost <- cost + 1L
        tid <- encode(pr, pc, d) + 1L
        if (new_cost < dist[tid]) {
          dist[tid] <- new_cost
          pq <- rbind(pq, c(new_cost, pr, pc, d))
        }
      }
      # Reverse of "turn left to get dir d" means predecessor had direction d+1
      d_from_left <- if (d == 4L) 1L else d + 1L
      tid <- encode(r, c, d_from_left) + 1L
      if (cost + 1000L < dist[tid]) {
        dist[tid] <- cost + 1000L
        pq <- rbind(pq, c(cost + 1000L, r, c, d_from_left))
      }
      # Reverse of "turn right to get dir d" means predecessor had direction d-1
      d_from_right <- if (d == 1L) 4L else d - 1L
      tid <- encode(r, c, d_from_right) + 1L
      if (cost + 1000L < dist[tid]) {
        dist[tid] <- cost + 1000L
        pq <- rbind(pq, c(cost + 1000L, r, c, d_from_right))
      }
    }
  }
  dist
}

# Forward Dijkstra from S facing East (direction 1)
dist_fwd <- dijkstra(start_pos[1], start_pos[2], 1L, reverse = FALSE)

# Part 1: min cost to reach E in any direction
end_costs <- sapply(1:4, function(d) dist_fwd[encode(end_pos[1], end_pos[2], d) + 1L])
result1   <- min(end_costs)
cat("Part 1:", result1, "\n")

# ---- Part 2 ----
# Backward Dijkstra: start from E in all 4 directions with cost 0
# We'll do 4 separate runs and take min, or inject all 4 into one run.
dist_bwd <- rep(Inf, n_states)
pq_init  <- matrix(nrow = 0, ncol = 4)
for (d in 1:4) {
  sid <- encode(end_pos[1], end_pos[2], d) + 1L
  dist_bwd[sid] <- 0
  pq_init <- rbind(pq_init, c(0L, end_pos[1], end_pos[2], d))
}

# Run backward Dijkstra from E (multi-source)
dist_bwd_full <- (function() {
  dist  <- dist_bwd
  pq    <- pq_init
  visited <- rep(FALSE, n_states)

  while (nrow(pq) > 0) {
    min_idx <- which.min(pq[, 1])
    cur <- pq[min_idx, ]
    pq  <- pq[-min_idx, , drop = FALSE]

    cost <- cur[1]
    r    <- cur[2]
    c    <- cur[3]
    d    <- cur[4]

    sid <- encode(r, c, d) + 1L
    if (visited[sid]) next
    visited[sid] <- TRUE

    # Reverse move forward
    pr <- r - DR[d]
    pc <- c - DC[d]
    if (pr >= 1 && pr <= nrow_g && pc >= 1 && pc <= ncol_g && grid[pr, pc] != "#") {
      new_cost <- cost + 1L
      tid <- encode(pr, pc, d) + 1L
      if (new_cost < dist[tid]) {
        dist[tid] <- new_cost
        pq <- rbind(pq, c(new_cost, pr, pc, d))
      }
    }
    # Reverse turns
    d_from_left <- if (d == 4L) 1L else d + 1L
    tid <- encode(r, c, d_from_left) + 1L
    if (cost + 1000L < dist[tid]) {
      dist[tid] <- cost + 1000L
      pq <- rbind(pq, c(cost + 1000L, r, c, d_from_left))
    }
    d_from_right <- if (d == 1L) 4L else d - 1L
    tid <- encode(r, c, d_from_right) + 1L
    if (cost + 1000L < dist[tid]) {
      dist[tid] <- cost + 1000L
      pq <- rbind(pq, c(cost + 1000L, r, c, d_from_right))
    }
  }
  dist
})()

# A cell (r,c) is on a best path if:
# exists direction d: dist_fwd[(r,c,d)] + dist_bwd_full[(r,c,d)] == result1
on_best <- rep(FALSE, nrow_g * ncol_g)
for (r in seq_len(nrow_g)) {
  for (c in seq_len(ncol_g)) {
    if (grid[r, c] == "#") next
    for (d in 1:4) {
      fwd <- dist_fwd[encode(r, c, d) + 1L]
      bwd <- dist_bwd_full[encode(r, c, d) + 1L]
      if (is.finite(fwd) && is.finite(bwd) && fwd + bwd == result1) {
        on_best[(r - 1L) * ncol_g + c] <- TRUE
        break
      }
    }
  }
}

result2 <- sum(on_best)
cat("Part 2:", result2, "\n")
