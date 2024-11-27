library(tidyverse)

# Read the input file
input <- read_lines('2015/day17_input.txt')
input <- as.numeric(input)

# Part 1
all_sets <- expand.grid(purrr::map(seq_along(input), ~c(F, T)))

matrix_vals <- t(t(as.matrix(all_sets)) * input)

sum(rowSums(matrix_vals) == 150)

# Part 2

num_containers <- rowSums(matrix_vals[rowSums(matrix_vals) == 150, ] > 0)

table(num_containers)[1]

