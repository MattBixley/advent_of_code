library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day16_input.txt")
mat <- do.call(rbind, strsplit(input, ""))
nr <- nrow(mat)
nc <- ncol(mat)

# Directions: 1=R, 2=D, 3=L, 4=U
dr <- c(0L, 1L, 0L, -1L)
dc <- c(1L, 0L, -1L, 0L)

# Mirror direction maps (indexed by incoming direction 1..4)
# "/" : R(1)->U(4), D(2)->L(3), L(3)->D(2), U(4)->R(1)
slash_map <- c(4L, 3L, 2L, 1L)
# "\" : R(1)->D(2), D(2)->R(1), L(3)->U(4), U(4)->L(3)
bslash_map <- c(2L, 1L, 4L, 3L)

count_energized <- function(start_r, start_c, start_dir) {
  visited <- array(FALSE, c(nr, nc, 4L))
  queue <- list(c(start_r, start_c, start_dir))

  while (length(queue) > 0) {
    cur <- queue[[1]]
    queue <- queue[-1]
    r <- cur[1]; c <- cur[2]; d <- cur[3]

    if (r < 1L || r > nr || c < 1L || c > nc) next
    if (visited[r, c, d]) next
    visited[r, c, d] <- TRUE

    ch <- mat[r, c]
    new_dirs <- switch(ch,
      "."  = d,
      "/"  = slash_map[d],
      "\\" = bslash_map[d],
      "|"  = if (d == 1L || d == 3L) c(2L, 4L) else d,
      "-"  = if (d == 2L || d == 4L) c(1L, 3L) else d
    )

    for (nd in new_dirs) {
      nr2 <- r + dr[nd]
      nc2 <- c + dc[nd]
      if (nr2 >= 1L && nr2 <= nr && nc2 >= 1L && nc2 <= nc && !visited[nr2, nc2, nd]) {
        queue <- c(queue, list(c(nr2, nc2, nd)))
      }
    }
  }

  sum(apply(visited, c(1, 2), any))
}

result1 <- count_energized(1L, 1L, 1L)
cat("Part 1:", result1, "\n")

# Part 2: try all edge starting positions
starts <- bind_rows(
  tibble(r = 1L,             c = seq_len(nc), d = 2L),  # top row, going down
  tibble(r = nr,             c = seq_len(nc), d = 4L),  # bottom row, going up
  tibble(r = seq_len(nr),    c = 1L,          d = 1L),  # left col, going right
  tibble(r = seq_len(nr),    c = nc,          d = 3L)   # right col, going left
)

result2 <- max(pmap_int(starts, ~ count_energized(..1, ..2, ..3)))
cat("Part 2:", result2, "\n")
