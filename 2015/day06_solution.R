# Load necessary libraries
library(tidyverse)

# Read input data
input <- read_lines("2015/input06.txt")

# Part 1
mat <- matrix(FALSE, 1000, 1000)

for (direction in input) {
  coord <- stringr::str_extract_all(direction, "[0-9]+")[[1]]
  coord <- as.numeric(coord) + 1
  
  command <- stringr::str_extract(direction, "(turn on|toggle|turn off)")
  
  x_range <- coord[1]:coord[3]
  y_range <- coord[2]:coord[4]
  
  if (command == "toggle") {
    mat[x_range, y_range] <-!mat[x_range, y_range]
  } else if (command == "turn on") {
    mat[x_range, y_range] <- TRUE
  } else if (command == "turn off") {
    mat[x_range, y_range] <- FALSE
  }
}

sum(mat)

# Part 2

mat <- matrix(0, 1000, 1000)

for (direction in input) {
  coord <- stringr::str_extract_all(direction, "[0-9]+")[[1]]
  coord <- as.numeric(coord) + 1
  
  command <- stringr::str_extract(direction, "(turn on|toggle|turn off)")
  
  x_range <- coord[1]:coord[3]
  y_range <- coord[2]:coord[4]
  
  if (command == "toggle") {
    mat[x_range, y_range] <- mat[x_range, y_range] + 2
  } else if (command == "turn on") {
    mat[x_range, y_range] <- mat[x_range, y_range] + 1
  } else if (command == "turn off") {
    mat[x_range, y_range] <- pmax(mat[x_range, y_range] - 1, 0)
  }
}

sum(mat)

