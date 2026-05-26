# Day 11: Reactor
# Part 1: Count all distinct paths from "you" to "out" in a directed acyclic graph.
# Part 2: Count all paths from "svr" to "out" that visit BOTH "dac" AND "fft".
#
# Sample Part 1 (expected 5):
# aaa: you hhh
# you: bbb ccc
# bbb: ddd eee
# ccc: ddd eee fff
# ddd: ggg
# eee: out
# fff: out
# ggg: out
# hhh: ccc fff iii
# iii: out

# -- Input --------------------------------------------------------------------
input <- readLines("2025/day11_input.txt")

# -- Parse --------------------------------------------------------------------
graph <- list()
for (line in input) {
  parts <- strsplit(line, ": ", fixed = TRUE)[[1]]
  node  <- parts[1]
  dests <- strsplit(parts[2], " ", fixed = TRUE)[[1]]
  graph[[node]] <- dests
}

# -- Helper -------------------------------------------------------------------
# Memoized DFS: count all paths from `from` to `to` in `graph`
count_paths <- function(from, to = "out", graph) {
  memo <- new.env(hash = TRUE, parent = emptyenv())
  f <- function(node) {
    if (node == to) return(1)
    if (!is.null(memo[[node]])) return(memo[[node]])
    neighbors <- graph[[node]]
    if (is.null(neighbors) || length(neighbors) == 0L) {
      memo[[node]] <- 0; return(0)
    }
    total <- sum(vapply(neighbors, f, FUN.VALUE = numeric(1)))
    memo[[node]] <- total
    total
  }
  f(from)
}

# -- Part 1 -------------------------------------------------------------------
# How many different paths lead from "you" to "out"?
result1 <- count_paths("you", "out", graph)
cat("Part 1:", result1, "\n")  # 590

# -- Part 2 -------------------------------------------------------------------
# How many paths from "svr" to "out" visit BOTH "dac" AND "fft"?
#
# Since dac cannot reach fft in this graph, all valid paths must visit
# fft BEFORE dac.  Paths through both = count(svr->fft) * count(fft->dac) * count(dac->out)
# (the segment path counts are all below 2^53 so doubles remain exact).

via_fft_dac <- count_paths("svr", "fft",  graph) *
               count_paths("fft", "dac",  graph) *
               count_paths("dac", "out",  graph)

result2 <- via_fft_dac
cat("Part 2:", format(result2, scientific = FALSE), "\n")  # 319473830844560
