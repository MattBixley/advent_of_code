library(tidyverse)

input <- data.frame(read_table("day01_input.txt", col_names = F))

# Part 1
# 1st last digits
# sum
# 

input |> 
  mutate(num = parse_number(X1))
  
