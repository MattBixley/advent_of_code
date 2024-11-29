library(tidyverse)

# Read the input file
input <- as.numeric(read_lines('2015/day24_input.txt'))

# Part 1
total <- sum(input) / 3

split_vec <- function(input, total) {
  res <- list()
  for (i in seq_along(input)) {
    combs <- combn(input, i)
    colsum_combs <- colSums(combs)
    if (any(colsum_combs == total)) {
      res <- c(res, list(combs[, which(colsum_combs == total)]))
      break
    }
  }
  res
}

first_group <- split_vec(input, total)

split_vec(setdiff(input, first_group[[1]][, 1]), total)

order(apply(first_group[[1]], 2, prod))

prod(first_group[[1]][, 1])


# Part 2
total <- sum(input) / 4

split_vec <- function(input, total) {
  res <- list()
  for (i in seq_along(input)) {
    combs <- combn(input, i)
    colsum_combs <- colSums(combs)
    if (any(colsum_combs == total)) {
      res <- c(res, list(combs[, which(colsum_combs == total)]))
      break
    }
  }
  res
}

first_group <- split_vec(input, total)

split_vec(setdiff(input, first_group[[1]][, 1]), total)

order(apply(first_group[[1]], 2, prod))

prod(first_group[[1]][, 1])

