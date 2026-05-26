library(tidyverse)

# Day 21: Keypad Conundrum
# Part 1: Sum of complexities with 2 directional robots + 1 numeric robot
# Part 2: Sum of complexities with 25 directional robots + 1 numeric robot
#
# Sample codes: 029A, 980A, 179A, 456A, 379A -> Expected Part 1: 126384

# Numeric keypad layout (row, col), gap at (3,0):
# 7 8 9
# 4 5 6
# 1 2 3
# . 0 A
numpad_pos <- list(
  "7" = c(0L, 0L), "8" = c(0L, 1L), "9" = c(0L, 2L),
  "4" = c(1L, 0L), "5" = c(1L, 1L), "6" = c(1L, 2L),
  "1" = c(2L, 0L), "2" = c(2L, 1L), "3" = c(2L, 2L),
  "0" = c(3L, 1L), "A" = c(3L, 2L)
)
numpad_gap <- c(3L, 0L)

# Directional keypad layout (row, col), gap at (0,0):
# . ^ A
# < v >
dirpad_pos <- list(
  "^" = c(0L, 1L), "A" = c(0L, 2L),
  "<" = c(1L, 0L), "v" = c(1L, 1L), ">" = c(1L, 2L)
)
dirpad_gap <- c(0L, 0L)

# Generate candidate move sequences from p1 to p2 avoiding the gap
gen_moves <- function(p1, p2, gap) {
  dr <- p2[1] - p1[1]; dc <- p2[2] - p1[2]
  vert  <- if (dr > 0) paste(rep("v", dr), collapse = "") else if (dr < 0) paste(rep("^", -dr), collapse = "") else ""
  horiz <- if (dc > 0) paste(rep(">", dc), collapse = "") else if (dc < 0) paste(rep("<", -dc), collapse = "") else ""
  if (dr == 0 && dc == 0) return(list("A"))
  moves <- list()
  # vert then horiz: safe if not passing through gap column while in gap row
  vert_first_ok  <- !(p1[2] == gap[2] && p2[1] == gap[1])
  horiz_first_ok <- !(p1[1] == gap[1] && p2[2] == gap[2])
  if (nchar(vert) > 0 && nchar(horiz) > 0 && vert_first_ok)  moves <- c(moves, list(paste0(vert, horiz, "A")))
  else if (nchar(vert) > 0 && nchar(horiz) == 0) moves <- c(moves, list(paste0(vert, "A")))
  if (nchar(horiz) > 0 && nchar(vert) > 0 && horiz_first_ok) moves <- c(moves, list(paste0(horiz, vert, "A")))
  else if (nchar(horiz) > 0 && nchar(vert) == 0) moves <- c(moves, list(paste0(horiz, "A")))
  if (length(moves) == 0) moves <- list(paste0(vert, horiz, "A"))
  unique(moves)
}

# Precompute all moves for both keypads
all_num_keys <- names(numpad_pos)
all_dir_keys <- names(dirpad_pos)

num_moves_map <- list()
for (k1 in all_num_keys) for (k2 in all_num_keys)
  num_moves_map[[paste0(k1, "->", k2)]] <- gen_moves(numpad_pos[[k1]], numpad_pos[[k2]], numpad_gap)

dir_moves_map <- list()
for (k1 in all_dir_keys) for (k2 in all_dir_keys)
  dir_moves_map[[paste0(k1, "->", k2)]] <- gen_moves(dirpad_pos[[k1]], dirpad_pos[[k2]], dirpad_gap)

# Global memoization for min_cost_dir
MEMO <- new.env(hash = TRUE, parent = emptyenv())

# min_cost_dir(from, to, depth): minimum keypresses at human level to press 'to'
# starting from 'from' on directional keypad. depth=0 means human presses directly.
min_cost_dir <- function(from_key, to_key, depth) {
  if (depth == 0L) return(1L)
  key <- paste0(from_key, "->", to_key, "@", depth)
  cached <- MEMO[[key]]
  if (!is.null(cached)) return(cached)
  seqs <- dir_moves_map[[paste0(from_key, "->", to_key)]]
  best <- Inf
  for (seq in seqs) {
    chars <- strsplit(seq, "")[[1]]
    cur <- "A"; total <- 0
    for (ch in chars) { total <- total + min_cost_dir(cur, ch, depth - 1L); cur <- ch }
    if (total < best) best <- total
  }
  MEMO[[key]] <- best
  best
}

# Total cost to type a code through num_robots directional robots
code_cost <- function(code, num_robots) {
  chars <- strsplit(code, "")[[1]]
  cur_num <- "A"; total <- 0
  for (ch in chars) {
    seqs <- num_moves_map[[paste0(cur_num, "->", ch)]]
    best <- Inf
    for (seq in seqs) {
      dir_chars <- strsplit(seq, "")[[1]]
      cur_dir <- "A"; cost <- 0
      for (dc in dir_chars) { cost <- cost + min_cost_dir(cur_dir, dc, num_robots); cur_dir <- dc }
      if (cost < best) best <- cost
    }
    total <- total + best
    cur_num <- ch
  }
  total
}

# -- Input --------------------------------------------------------------------
codes <- readLines("2024/day21_input.txt")
codes <- codes[nchar(codes) > 0]

# Part 1: 2 directional robots
part1 <- sum(sapply(codes, function(code) {
  as.numeric(gsub("[^0-9]", "", code)) * code_cost(code, 2L)
}))
cat("Part 1:", part1, "\n")

# Part 2: 25 directional robots
part2 <- sum(sapply(codes, function(code) {
  as.numeric(gsub("[^0-9]", "", code)) * code_cost(code, 25L)
}))
cat("Part 2:", format(part2, scientific = FALSE), "\n")
