library(tidyverse)

# Read the input file
input <- read_lines('2024/day1_input.txt')
head(input)

# Part 1
input <- str_split(input, '   ') |>
  unlist() |>
  as.numeric()

left <- input[seq(1, length(input), by = 2)] |>
  sort()

right <- input[seq(2, length(input), by = 2)] |>
  sort()

diff <- abs(right - left)
sum(diff)

# Part 2
input <- readr::read_delim(
  "2024/day1_input.txt", 
  delim = "   ", 
  col_names = FALSE,
  show_col_types = FALSE
)

counts <- table(input$X2)[as.character(input$X1)]

sum(abs(input$X1 * counts), na.rm = TRUE)

