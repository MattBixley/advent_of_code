library(tidyverse)
library(adventdrob)

#usethis::edit_r_environ("project")

#input <- data.frame(read_table("day01_input.txt", col_names = F))
input <- advent_input(1, 2023)
input

# Part 1
# 1st last digits
# sum

input |> 
  extract(x, c("first"), "(\\d)", remove = F) |>
  extract(x, c("last"), ".*(\\d)", remove = F) |>
  mutate(n = as.numeric(paste0(first,last))) |>
  summarise(sum(n, na.rm=T))

# Part 2
# 1st last digits and words
# sum
num <- c("one", "two", "three", "four", "five", 
         "six", "seven", "eight", "nine")

input |> 
  extract(x, "first", 
          "(\\d|one|two|three|four|five|six|seven|eight|nine)", 
          remove = F) |>
  extract(x, 
          "last", 
          ".*(\\d|one|two|three|four|five|six|seven|eight|nine)") |>
  mutate(input = input$x,
         first = coalesce(as.numeric(first), match(first, num)),
         last = coalesce(as.numeric(last), match(last, num))) |>
  mutate(n = as.numeric(paste0(first,last))) |>  
  summarise(sum(n, na.rm=T))


