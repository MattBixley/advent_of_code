library(tidyverse)

# Read the input file
input <- read_lines('2024/day1_input.txt')
head(input)

# Part 1
input <- str_split(input, '   ') |>
  unlist() |>
  as.numeric()

left <- input[seq(1, length(input), by = 2)] |>
  sort()

right <- input[seq(2, length(input), by = 2)] |>
  sort()

diff <- abs(right - left)
sum(diff)

# Part 2
# Your solution code for Part 2 here

