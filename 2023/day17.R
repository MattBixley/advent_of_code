library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day17_input.txt")
input <- input[nchar(input) > 0]
mat <- do.call(rbind, lapply(strsplit(input, ""), as.integer))
nr <- nrow(mat)
nc <- ncol(mat)

# Directions: 1=R, 2=D, 3=L, 4=U
DR <- c(0L, 1L, 0L, -1L)
DC <- c(1L, 0L, -1L, 0L)
TURN <- list(c(2L,4L), c(1L,3L), c(2L,4L), c(1L,3L))

dijkstra <- function(min_s, max_s) {
  INF <- .Machine$integer.max
  dist_arr <- array(INF, c(nr, nc, 4L, max_s + 1L))

  MAX_BUCKET <- 5000L
  buckets <- vector("list", MAX_BUCKET + 10L)

  add_state <- function(cost, row, col, dir, steps) {
    bi <- cost + 1L
    if (is.na(bi) || bi < 1L || bi > length(buckets)) return(invisible(NULL))
    buckets[[bi]] <<- base::c(buckets[[bi]], list(base::c(row, col, dir, steps)))
  }

  add_state(0L, 1L, 1L, 1L, 0L)
  add_state(0L, 1L, 1L, 2L, 0L)

  for (bi in seq_len(MAX_BUCKET + 10L)) {
    bucket <- buckets[[bi]]
    if (length(bucket) == 0L) next
    buckets[[bi]] <- list()
    cost <- bi - 1L

    for (state in bucket) {
      row <- state[1]; col <- state[2]; dir <- state[3]; steps <- state[4]

      if (row == nr && col == nc && steps >= min_s) return(cost)

      if (steps > 0L && cost > dist_arr[row, col, dir, steps]) next

      # Move straight
      if (steps < max_s) {
        r2 <- row + DR[dir]; c2 <- col + DC[dir]
        if (r2 >= 1L && r2 <= nr && c2 >= 1L && c2 <= nc) {
          ns <- steps + 1L; nc2 <- cost + mat[r2, c2]
          if (nc2 < dist_arr[r2, c2, dir, ns]) {
            dist_arr[r2, c2, dir, ns] <- nc2
            add_state(nc2, r2, c2, dir, ns)
          }
        }
      }

      # Turn
      if (steps >= min_s) {
        for (nd in TURN[[dir]]) {
          r2 <- row + DR[nd]; c2 <- col + DC[nd]
          if (r2 >= 1L && r2 <= nr && c2 >= 1L && c2 <= nc) {
            nc2 <- cost + mat[r2, c2]
            if (nc2 < dist_arr[r2, c2, nd, 1L]) {
              dist_arr[r2, c2, nd, 1L] <- nc2
              add_state(nc2, r2, c2, nd, 1L)
            }
          }
        }
      }
    }
  }

  Inf
}

result1 <- dijkstra(1L, 3L)
cat("Part 1:", result1, "\n")

result2 <- dijkstra(4L, 10L)
cat("Part 2:", result2, "\n")
