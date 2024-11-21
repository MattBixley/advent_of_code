# Load necessary libraries
setwd("C:/Users/mattb/Code/advent_of_code/2015")
library(tidyverse)

# Read input data
input <- read_lines("input02.txt")

# Your code here
# Part 1
chars <- strsplit(input, "x")

package_surface <- function(x) {
  x <- as.numeric(x)
  side <- x[1] * x[2]
  front <- x[1] * x[3]
  top <- x[2] * x[3]
  
  sum(2 * c(side, front, top), min(side, front, top))
}

sum(vapply(chars, package_surface, numeric(1)))

# Part 2
chars <- strsplit(input, "x")

ribbon_length <- function(x) {
  x <- as.numeric(x)
  short_sides <- sort(x)[1:2]
  sum(short_sides) * 2 + prod(x)
}

sum(vapply(chars, ribbon_length, numeric(1)))

