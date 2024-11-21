library(tidyverse)
library(adventdrob)

#usethis::edit_r_environ("project")

input <- advent_input(6, 2023)
input

time <- as.numeric(str_extract_all(input$x, "\\d+")[[1]])
distance <- as.numeric(str_extract_all(input$x, "\\d+")[[2]])

n_combos <- function(length_time, record){
  charge <- seq(0, length_time)
  distances <- (length_time - charge) * charge
  sum(distances > record)
}

# part 1
prod(map2_dbl(time, distance, n_combos))

# part 2
combine_numbers <- function(x){as.numeric(paste0(x, collapse =""))}
n_combos(combine_numbers(time),combine_numbers(distance))
