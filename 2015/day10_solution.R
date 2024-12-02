# Load necessary libraries
library(tidyverse)

# Read input data
input <- "1113122113"

look_and_say <- function(x) {
  x <- strsplit(x, "")[[1]]
  x <- rle(x)
  x <- unlist(x)
  x <- matrix(x, nrow = 2, byrow = TRUE)
  x <- as.numeric(x)
  paste0(x, collapse = "")
}

for (i in seq_len(40)) {
  input <- look_and_say(input)
}

nchar(input)

# Part 2

input <- "1113122113"

for (i in seq_len(50)) {
  input <- look_and_say(input)
}
nchar(input)

