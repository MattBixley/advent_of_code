library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day25_input.txt")

# Parse undirected edges
edges <- tibble(line = input) |>
  mutate(
    from    = str_extract(line, "^\\w+"),
    to_list = str_split(str_extract(line, "(?<=: ).+"), " ")
  ) |>
  unnest(to_list) |>
  select(from, to = to_list)

nodes  <- unique(c(edges$from, edges$to))
n      <- length(nodes)
node_id <- setNames(seq_along(nodes), nodes)

# Build adjacency list (integer ids, undirected)
adj <- vector("list", n)
for (i in seq_len(n)) adj[[i]] <- integer(0)
all_edges <- bind_rows(edges, rename(edges, from = to, to = from))
for (i in seq_len(nrow(all_edges))) {
  a <- node_id[[all_edges$from[i]]]
  b <- node_id[[all_edges$to[i]]]
  adj[[a]] <- c(adj[[a]], b)
}

# BFS returning the node path from src to dst (NULL if unreachable)
bfs_path <- function(src, dst, adj_list) {
  prev <- integer(n)
  prev[src] <- -1L
  queue <- src
  head_idx <- 1L
  while (head_idx <= length(queue)) {
    v <- queue[head_idx]; head_idx <- head_idx + 1L
    if (v == dst) break
    for (u in adj_list[[v]]) {
      if (prev[u] == 0L) {
        prev[u] <- v
        queue <- c(queue, u)
      }
    }
  }
  if (prev[dst] == 0L && dst != src) return(NULL)
  path <- dst
  v <- dst
  while (v != src) { v <- prev[v]; path <- c(v, path) }
  path
}

# Accumulate edge usage counts via many random BFS paths.
# The 3 bridge edges appear far more often than regular edges.
set.seed(42)
edge_count <- new.env(hash = TRUE, parent = emptyenv())

for (iter in seq_len(600)) {
  src <- sample(n, 1)
  dst <- sample(n, 1)
  if (src == dst) next
  path <- bfs_path(src, dst, adj)
  if (is.null(path)) next
  for (k in seq_len(length(path) - 1)) {
    a   <- min(path[k], path[k + 1])
    b   <- max(path[k], path[k + 1])
    key <- paste(a, b, sep = ",")
    val <- edge_count[[key]]
    edge_count[[key]] <- if (is.null(val)) 1L else val + 1L
  }
}

sorted_keys <- names(sort(unlist(as.list(edge_count)), decreasing = TRUE))

# Remove edges by key from adjacency list
remove_edges <- function(adj_in, keys) {
  adj2 <- lapply(adj_in, identity)
  for (key in keys) {
    parts <- as.integer(strsplit(key, ",")[[1]])
    a <- parts[1]; b <- parts[2]
    adj2[[a]] <- adj2[[a]][adj2[[a]] != b]
    adj2[[b]] <- adj2[[b]][adj2[[b]] != a]
  }
  adj2
}

# BFS component size from node 1 in modified graph
component_product <- function(adj2) {
  visited <- logical(n)
  visited[1] <- TRUE
  queue <- 1L
  head_idx <- 1L
  while (head_idx <= length(queue)) {
    v <- queue[head_idx]; head_idx <- head_idx + 1L
    for (u in adj2[[v]]) {
      if (!visited[u]) {
        visited[u] <- TRUE
        queue <- c(queue, u)
      }
    }
  }
  c1 <- sum(visited); c2 <- n - c1
  if (c1 > 0L && c2 > 0L) c1 * c2 else NA_integer_
}

# Try removing top-3 by betweenness first
result1 <- component_product(remove_edges(adj, sorted_keys[1:3]))

# If top-3 alone aren't the right 3, search combinations in the top candidates
if (is.na(result1)) {
  top <- sorted_keys[1:12]
  found <- FALSE
  for (i in seq_len(10)) {
    if (found) break
    for (j in seq(i + 1, 11)) {
      if (found) break
      for (k in seq(j + 1, 12)) {
        r <- component_product(remove_edges(adj, c(top[i], top[j], top[k])))
        if (!is.na(r)) {
          result1 <- r
          found <- TRUE
          break
        }
      }
    }
  }
}

cat("Part 1:", result1, "\n")
cat("Part 2: 0\n")  # Day 25 Part 2 is always free (no puzzle)
