library(tidyverse)

# -- Input --------------------------------------------------------------------
input <- read_lines("2024/day23_input.txt")

# Sample (expected Part 1: 7, Part 2: co,de,ka,ta)
# sample <- c(
#   "kh-tc","qp-kh","de-cg","ka-co","yn-aq","qp-ub","cg-tb","vc-aq","tb-ka",
#   "wh-tc","yn-cg","kh-ub","ta-co","de-co","tc-td","tb-wq","wh-td","ta-ka",
#   "td-qp","aq-cg","wq-ub","ub-vc","de-ta","wq-aq","wq-vc","wh-yn","ka-de",
#   "kh-ta","co-tc","wh-qp","tb-vc","td-yn"
# )
# input <- sample

# -- Parse --------------------------------------------------------------------
edges <- input |>
  str_split_fixed("-", 2) |>
  as_tibble(.name_repair = ~ c("a", "b"))

# Build adjacency set for fast lookup
adj_set <- new.env(hash = TRUE, parent = emptyenv())
for (i in seq_len(nrow(edges))) {
  a <- edges$a[i]; b <- edges$b[i]
  key1 <- paste(sort(c(a, b)), collapse = "-")
  adj_set[[key1]] <- TRUE
}

connected <- function(x, y) {
  !is.null(adj_set[[paste(sort(c(x, y)), collapse = "-")]])
}

# All unique nodes
nodes <- unique(c(edges$a, edges$b))

# Build adjacency list
adj_list <- list()
for (nd in nodes) adj_list[[nd]] <- character(0)
for (i in seq_len(nrow(edges))) {
  a <- edges$a[i]; b <- edges$b[i]
  adj_list[[a]] <- c(adj_list[[a]], b)
  adj_list[[b]] <- c(adj_list[[b]], a)
}

# -- Part 1 -------------------------------------------------------------------
# Find all triangles with at least one node starting with 't'

triangles <- list()
for (i in seq_along(nodes)) {
  a <- nodes[i]
  nbrs_a <- adj_list[[a]]
  for (b in nbrs_a) {
    if (b <= a) next  # canonical ordering to avoid duplicates
    nbrs_b <- adj_list[[b]]
    common <- intersect(nbrs_a, nbrs_b)
    for (cc in common) {
      if (cc <= b) next
      triple <- sort(c(a, b, cc))
      if (any(startsWith(triple, "t"))) {
        triangles <- c(triangles, list(triple))
      }
    }
  }
}

result1 <- length(triangles)
cat("Part 1:", result1, "\n")

# -- Part 2 -------------------------------------------------------------------
# Find the largest clique (LAN party password = alphabetically sorted, comma-separated)

# Bron-Kerbosch with pivoting for maximum clique
max_clique <- character(0)

bron_kerbosch <- function(R, P, X) {
  if (length(P) == 0 && length(X) == 0) {
    if (length(R) > length(max_clique)) {
      max_clique <<- R
    }
    return(invisible(NULL))
  }
  if (length(P) == 0) return(invisible(NULL))

  # Choose pivot with most connections to P
  candidates <- union(P, X)
  pivot_connections <- sapply(candidates, function(u) length(intersect(adj_list[[u]], P)))
  pivot <- candidates[which.max(pivot_connections)]

  pivot_nbrs <- adj_list[[pivot]]
  for (v in setdiff(P, pivot_nbrs)) {
    nbrs_v <- adj_list[[v]]
    bron_kerbosch(
      R = c(R, v),
      P = intersect(P, nbrs_v),
      X = intersect(X, nbrs_v)
    )
    P <- setdiff(P, v)
    X <- union(X, v)
  }
}

bron_kerbosch(character(0), nodes, character(0))

result2 <- paste(sort(max_clique), collapse = ",")
cat("Part 2:", result2, "\n")
