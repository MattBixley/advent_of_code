library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day14_input.txt")

parse_mat <- function(lines) {
  do.call(rbind, strsplit(lines, ""))
}

tilt_north <- function(mat) {
  nr <- nrow(mat); nc <- ncol(mat)
  for (c in seq_len(nc)) {
    empty <- 0L
    for (r in seq_len(nr)) {
      ch <- mat[r, c]
      if (ch == ".") {
        if (empty == 0L) empty <- r
      } else if (ch == "O") {
        if (empty > 0L) {
          mat[empty, c] <- "O"; mat[r, c] <- "."
          empty <- empty + 1L
        }
      } else {  # '#'
        empty <- 0L
      }
    }
  }
  mat
}

rotate_cw <- function(mat) t(mat)[, nrow(mat):1]

spin_cycle <- function(mat) {
  mat <- tilt_north(mat)              # N
  mat <- tilt_north(rotate_cw(mat))  # W (after CW rot = north)
  mat <- tilt_north(rotate_cw(mat))  # S
  mat <- tilt_north(rotate_cw(mat))  # E
  rotate_cw(mat)                     # restore orientation
}

calc_load <- function(mat) {
  nr <- nrow(mat)
  sum((nr - (row(mat) - 1)) * (mat == "O"))
}

mat <- parse_mat(input)

# Part 1
result1 <- calc_load(tilt_north(mat))
cat("Part 1:", result1, "\n")

# Part 2: cycle detection
mat <- parse_mat(input)
seen <- list()
target <- 1000000000L

for (i in seq_len(target)) {
  mat <- spin_cycle(mat)
  key <- paste(mat, collapse = "")
  if (!is.null(seen[[key]])) {
    cycle_start  <- seen[[key]]
    cycle_length <- i - cycle_start
    remaining    <- (target - i) %% cycle_length
    for (j in seq_len(remaining)) mat <- spin_cycle(mat)
    break
  }
  seen[[key]] <- i
}

result2 <- calc_load(mat)
cat("Part 2:", result2, "\n")
