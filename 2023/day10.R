library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day10_input.txt")

grid <- parse_grid(input)
nrows <- max(grid$y)
ncols <- max(grid$x)

# Convert to matrix for fast lookup
mat <- matrix(".", nrows, ncols)
for (i in seq_len(nrow(grid))) mat[grid$y[i], grid$x[i]] <- grid$value[i]

# Find S position
s_pos <- grid |> filter(value == "S")
sr <- s_pos$y[1]; sc <- s_pos$x[1]

# Pipe connections: which directions each pipe connects
connects <- list(
  "|" = c("N","S"), "-" = c("E","W"), "L" = c("N","E"),
  "J" = c("N","W"), "7" = c("S","W"), "F" = c("S","E"), "." = c()
)
opposite <- c(N="S", S="N", E="W", W="E")
delta    <- list(N=c(-1,0), S=c(1,0), E=c(0,1), W=c(0,-1))

# Determine what S actually is
s_dirs <- c()
for (d in c("N","S","E","W")) {
  dr <- delta[[d]]
  nr <- sr + dr[1]; nc <- sc + dr[2]
  if (nr >= 1 && nr <= nrows && nc >= 1 && nc <= ncols) {
    nbpipe <- mat[nr, nc]
    if (nbpipe %in% names(connects) && opposite[d] %in% connects[[nbpipe]])
      s_dirs <- c(s_dirs, d)
  }
}
s_char <- names(which(sapply(connects, function(v) setequal(v, s_dirs))))
mat[sr, sc] <- s_char[1]

# BFS to find loop
dist <- matrix(NA_integer_, nrows, ncols)
dist[sr, sc] <- 0L
queue <- list(c(sr, sc))
while (length(queue) > 0) {
  cur <- queue[[1]]; queue <- queue[-1]
  r <- cur[1]; c <- cur[2]
  pipe <- mat[r, c]
  for (d in connects[[pipe]]) {
    dr <- delta[[d]]
    nr <- r + dr[1]; nc <- c + dr[2]
    if (nr >= 1 && nr <= nrows && nc >= 1 && nc <= ncols && is.na(dist[nr, nc])) {
      dist[nr, nc] <- dist[r, c] + 1L
      queue <- c(queue, list(c(nr, nc)))
    }
  }
}

result1 <- max(dist, na.rm = TRUE)
cat("Part 1:", result1, "\n")

# Part 2: ray casting per row
on_loop <- !is.na(dist)
count_enclosed <- 0L
for (r in seq_len(nrows)) {
  inside <- FALSE
  last_bend <- ""
  for (c in seq_len(ncols)) {
    if (!on_loop[r, c]) {
      if (inside) count_enclosed <- count_enclosed + 1L
    } else {
      p <- mat[r, c]
      if (p == "|") {
        inside <- !inside
      } else if (p %in% c("L", "F")) {
        last_bend <- p
      } else if (p == "J") {
        if (last_bend == "F") inside <- !inside
        last_bend <- ""
      } else if (p == "7") {
        if (last_bend == "L") inside <- !inside
        last_bend <- ""
      }
    }
  }
}
result2 <- count_enclosed
cat("Part 2:", result2, "\n")
