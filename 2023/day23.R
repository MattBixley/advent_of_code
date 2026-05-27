library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day23_input.txt")
mat <- do.call(rbind, strsplit(input, ""))
nr <- nrow(mat); nc <- ncol(mat)

start_c <- which(mat[1L, ] == ".")
end_c   <- which(mat[nr, ] == ".")
start   <- c(1L, start_c)
end_pos <- c(nr, end_c)

all_dirs   <- list(c(-1L, 0L), c(1L, 0L), c(0L, -1L), c(0L, 1L))
slope_dirs <- list(">" = c(0L, 1L), "v" = c(1L, 0L), "<" = c(0L, -1L), "^" = c(-1L, 0L))

# Part 1: iterative DFS with slope constraints (avoids R recursion limit)
{
  visited <- matrix(FALSE, nr, nc)
  visited[start[1], start[2]] <- TRUE
  max_depth <- nr * nc
  sr  <- integer(max_depth); sc  <- integer(max_depth)
  sst <- integer(max_depth); snd <- integer(max_depth)
  sp  <- 1L
  sr[sp] <- start[1]; sc[sp] <- start[2]; sst[sp] <- 0L; snd[sp] <- 1L
  best <- -1L

  while (sp > 0L) {
    r <- sr[sp]; col <- sc[sp]; steps <- sst[sp]; nd <- snd[sp]
    if (r == end_pos[1] && col == end_pos[2]) {
      if (steps > best) best <- steps
      visited[r, col] <- FALSE; sp <- sp - 1L; next
    }
    ch   <- mat[r, col]
    dirs <- if (ch %in% names(slope_dirs)) list(slope_dirs[[ch]]) else all_dirs
    nd_max <- length(dirs)
    found <- FALSE
    if (nd <= nd_max) {
      for (di in nd:nd_max) {
        d <- dirs[[di]]
        r2 <- r + d[1]; c2 <- col + d[2]
        if (r2 >= 1L && r2 <= nr && c2 >= 1L && c2 <= nc &&
            mat[r2, c2] != "#" && !visited[r2, c2]) {
          snd[sp] <- di + 1L
          sp <- sp + 1L
          sr[sp] <- r2; sc[sp] <- c2; sst[sp] <- steps + 1L; snd[sp] <- 1L
          visited[r2, c2] <- TRUE
          found <- TRUE; break
        }
      }
    }
    if (!found) { visited[r, col] <- FALSE; sp <- sp - 1L }
  }
  result1 <- best
}
cat("Part 1:", result1, "\n")

# Part 2: slopes ignored — compress maze to junction graph, then DFS
# Junctions = non-wall cells with >=3 passable neighbours, plus start and end
is_passable <- function(r, c) {
  r >= 1L && r <= nr && c >= 1L && c <= nc && mat[r, c] != "#"
}

passable_neighbour_count <- function(r, c) {
  sum(sapply(all_dirs, function(d) is_passable(r + d[1], c + d[2])))
}

junctions <- list(start, end_pos)
for (r in seq_len(nr)) {
  for (c in seq_len(nc)) {
    if (mat[r, c] != "#" && passable_neighbour_count(r, c) >= 3L) {
      junctions <- c(junctions, list(c(r, c)))
    }
  }
}

# Deduplicate and key junctions
junc_keys <- map_chr(junctions, ~ paste(.x, collapse = ","))
junctions  <- junctions[!duplicated(junc_keys)]
junc_keys  <- junc_keys[!duplicated(junc_keys)]

# BFS from each junction along corridors to find distances to adjacent junctions
adj <- vector("list", length(junctions))
names(adj) <- junc_keys

for (ji in seq_along(junctions)) {
  src_key <- junc_keys[ji]
  sj <- junctions[[ji]]
  vis2 <- matrix(FALSE, nr, nc)
  vis2[sj[1], sj[2]] <- TRUE
  queue <- list(list(pos = sj, dist = 0L))

  while (length(queue) > 0) {
    cur <- queue[[1]]; queue <- queue[-1]
    r <- cur$pos[1]; c <- cur$pos[2]; d <- cur$dist
    k <- paste(c(r, c), collapse = ",")

    # Arrived at a different junction — record edge, do not expand further
    if (k != src_key && k %in% junc_keys) {
      adj[[src_key]] <- c(adj[[src_key]], list(list(key = k, dist = d)))
      next
    }
    for (dir in all_dirs) {
      r2 <- r + dir[1]; c2 <- c + dir[2]
      if (is_passable(r2, c2) && !vis2[r2, c2]) {
        vis2[r2, c2] <- TRUE
        queue <- c(queue, list(list(pos = c(r2, c2), dist = d + 1L)))
      }
    }
  }
}

start_key <- paste(start,   collapse = ",")
end_key   <- paste(end_pos, collapse = ",")

junc_index <- setNames(seq_along(junc_keys), junc_keys)
start_idx  <- junc_index[[start_key]]
end_idx    <- junc_index[[end_key]]
n_junc     <- length(junctions)

best <- 0L
init_vis <- logical(n_junc); init_vis[start_idx] <- TRUE
stack <- list(list(idx = start_idx, vis = init_vis, dist = 0L))

while (length(stack) > 0) {
  frame <- stack[[length(stack)]]
  stack <- stack[-length(stack)]

  if (frame$idx == end_idx) {
    if (frame$dist > best) best <- frame$dist
    next
  }

  key <- junc_keys[frame$idx]
  for (nb in adj[[key]]) {
    nidx <- junc_index[[nb$key]]
    if (!frame$vis[nidx]) {
      new_vis <- frame$vis; new_vis[nidx] <- TRUE
      stack <- c(stack, list(list(idx = nidx, vis = new_vis, dist = frame$dist + nb$dist)))
    }
  }
}

result2 <- best
cat("Part 2:", result2, "\n")
