# Load necessary libraries
library(tidyverse)

# Read input data
input <- readLines("2015/input03.txt")

# Part 1
chars <- strsplit(input, "")[[1]]

x_key <- c("^" = 0, "v" = 0, ">" = 1, "<" = -1)
y_key <- c("^" = 1, "v" = -1, ">" = 0, "<" = 0)

path <- data.frame(
  x = cumsum(x_key[chars]),
  y = cumsum(y_key[chars])
)

nrow(unique(path))

# Part 2
chars <- strsplit(input, "")[[1]]

path_santa <- data.frame(
  x = cumsum(x_key[chars][seq_along(chars) %% 2 == 1]),
  y = cumsum(y_key[chars][seq_along(chars) %% 2 == 1])
)

path_robosanta <- data.frame(
  x = cumsum(x_key[chars][seq_along(chars) %% 2 == 0]),
  y = cumsum(y_key[chars][seq_along(chars) %% 2 == 0])
)

nrow(unique(rbind(path_santa, path_robosanta)))
