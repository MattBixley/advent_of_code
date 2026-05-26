library(tidyverse)

# Day 7: Laboratories
# Part 1: Tachyon beams go downward from S. When hitting ^ (splitter), beam
#         stops and two new downward beams start left and right of splitter.
#         Each splitter activates at most once. Count total activations.
# Part 2: Many-worlds: each splitter creates two timelines.
#         Count total distinct timelines (recursive with memoization).
#
# Sample (expected P1: 21, P2: 40):
# .......S.......
# ...............
# .......^.......
# ...............
# ......^.^......
# ...............
# .....^.^.^.....
# ...............
# ....^.^...^....
# ...............
# ...^.^...^.^...
# ...............
# ..^...^.....^..
# ...............
# .^.^.^.^.^...^.
# ...............

input <- read_lines("/mnt/c/Users/MattBixley/Code/advent_of_code/2025/day07_input.txt")

# --- Part 1 ---
# BFS: each splitter activates at most once; count activations
solve_part1 <- function(input) {
  max_y <- length(input)
  max_x <- nchar(input[1])

  # Build splitter set
  splitter_set <- character(0)
  for (y in seq_along(input)) {
    chars <- strsplit(input[y], "")[[1]]
    for (x in seq_along(chars)) {
      if (chars[x] == "^") splitter_set <- c(splitter_set, paste(x, y, sep = ","))
    }
  }

  # Find S
  for (y in seq_along(input)) {
    x <- regexpr("S", input[y])[1]
    if (x > 0) { start_x <- x; start_y <- y; break }
  }

  splits <- 0L
  queue <- list(c(start_x, start_y + 1))
  visited_starts <- character(0)
  activated_splitters <- character(0)

  while (length(queue) > 0) {
    pos <- queue[[1]]; queue <- queue[-1]
    x <- pos[1]; y <- pos[2]
    key <- paste(x, y, sep = ",")
    if (key %in% visited_starts) next
    visited_starts <- c(visited_starts, key)

    cur_y <- y
    while (cur_y <= max_y && x >= 1 && x <= max_x) {
      cell_key <- paste(x, cur_y, sep = ",")
      if (cell_key %in% splitter_set) {
        if (!(cell_key %in% activated_splitters)) {
          splits <- splits + 1L
          activated_splitters <- c(activated_splitters, cell_key)
          if (x - 1 >= 1) {
            nk <- paste(x - 1, cur_y, sep = ",")
            if (!(nk %in% visited_starts)) queue <- c(queue, list(c(x - 1, cur_y)))
          }
          if (x + 1 <= max_x) {
            nk <- paste(x + 1, cur_y, sep = ",")
            if (!(nk %in% visited_starts)) queue <- c(queue, list(c(x + 1, cur_y)))
          }
        }
        break
      }
      cur_y <- cur_y + 1
    }
  }
  splits
}

cat("Part 1:", solve_part1(input), "\n")  # 1598

# --- Part 2 ---
# f(x, y) = timelines for beam starting at (x, y) going down
# f(x, y) = f(x-1, sy) + f(x+1, sy) where sy is first splitter in col x at row >= y
# f(x, y) = 1 if no splitter below (beam exits)
solve_part2 <- function(input) {
  max_y <- length(input)
  max_x <- nchar(input[1])

  # Build per-column sorted splitter y positions
  splitter_cols <- vector("list", max_x)
  for (x in 1:max_x) splitter_cols[[x]] <- integer(0)
  for (y in seq_along(input)) {
    chars <- strsplit(input[y], "")[[1]]
    for (x in seq_along(chars)) {
      if (chars[x] == "^") splitter_cols[[x]] <- c(splitter_cols[[x]], y)
    }
  }
  for (x in 1:max_x) splitter_cols[[x]] <- sort(splitter_cols[[x]])

  # Find S
  for (y in seq_along(input)) {
    x <- regexpr("S", input[y])[1]
    if (x > 0) { start_x <- x; start_y <- y; break }
  }

  memo <- list()
  f <- function(x, y) {
    if (x < 1 || x > max_x) return(0)
    key <- paste(x, y, sep = ",")
    if (!is.null(memo[[key]])) return(memo[[key]])
    hits <- splitter_cols[[x]][splitter_cols[[x]] >= y]
    if (length(hits) == 0) { memo[[key]] <<- 1; return(1) }
    sy <- hits[1]
    result <- f(x - 1, sy) + f(x + 1, sy)
    memo[[key]] <<- result
    result
  }

  f(start_x, start_y + 1)
}

cat("Part 2:", format(solve_part2(input), scientific = FALSE), "\n")  # 4509723641302
