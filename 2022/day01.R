library(tidyverse)

input <- read_lines("2022/day01_input.txt")

w <- c(0,which(input==""))
elves <- list()

for(i in seq_along(w)){
  z <- ifelse(i == length(w), length(input), w[i+1]-1)
  elves[[i]] <- as.integer(input[(w[i]+1):z])
}

#206152

# Part 1 
calories <- sapply(elves,sum)
out1 <- max(calories)

# Part 2 
out2 <- sort(calories, decreasing = TRUE)[1:3] |> 
  sum()

# Results
# part 1
cat("Puzzle 1 solution:", out1, fill = TRUE)

# part 2
cat("Puzzle 2 solution:", out2, fill = TRUE)



