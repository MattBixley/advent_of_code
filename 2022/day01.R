library(tidyverse)
library(plyr)

input <- readLines('2022/day01_input.txt')

seps <- which(input == "")

start <- c(1, seps[-length(seps)] + 1)
end <- seps

inputs <- purrr::map2(start, end, ~as.numeric(input[.x:(.y - 1)]))

# Part 1 
all_sums <- sapply(inputs, sum)
out1 <- max(all_sums)

# Part 2 
out2 <- sort(all_sums, decreasing = TRUE)[1:3] |> 
  sum()

# Results
# part 1
cat("Puzzle 1 solution:", out1, fill = TRUE)

# part 2
cat("Puzzle 2 solution:", out2, fill = TRUE)
