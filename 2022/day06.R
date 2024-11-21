library(tidyverse)
input <- read_lines("2022/day06_input.txt") |>
  str_split("") |>
  unlist()

#part 1
i <- x <- 5 
while(length(x) < 4){
  x <- unique(input[(i-4):(i-1)])
  length(x)

  i = i + 1
}
print(i)

#part 2
i <- x <- 15 
while(length(x) < 14){
  x <- unique(input[(i-14):(i-1)])
  length(x)
  i = i + 1
}
print(i)
