# Load necessary libraries
library(tidyverse)

# Read input data
# input <- read_lines("input.txt")

# Part 1
input <- "bgvyzdsv"
number <- seq_len(1000000)

md5 <- digest::getVDigest()

hash <- vapply(
  paste0(input, number),
  md5,
  FUN.VALUE = character(1),
  serialize = FALSE
)

which(substr(hash, 1, 5) == "00000")

# Part 2
input <- "bgvyzdsv"
number <- seq(2000000, 4000000)
number <- seq_len(10000000)

md5 <- digest::getVDigest()

hash <- vapply(
  paste0(input, number),
  md5,
  FUN.VALUE = character(1),
  serialize = FALSE
)

which(substr(hash, 1, 6) == "000000")



