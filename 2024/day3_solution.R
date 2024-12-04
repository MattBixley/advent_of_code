library(tidyverse)

# Read the input file
input <- read_lines('2024/day3_input.txt')

# Part 1

# Extract numeric pairs
numeric_pairs <- str_extract_all(input, "mul\\((\\d+),(\\d+)\\)|mul\\[(\\d+),(\\d+)\\]|mul\\((\\d+),(\\d+)\\)|mul\\[(\\d+),(\\d+)\\]") %>%
  unlist() %>%
  str_extract_all("\\d+") %>%
  map(~ as.numeric(.x))

# Multiply each pair and sum the results
result <- numeric_pairs %>%
  map_dbl(~ .x[1] * .x[2]) %>%
  sum()

print(result)

#Part 2
# Sample string
input <- read_lines('2024/day3_input.txt') |>
  paste0(collapse = "")

mul <- function(x, y) x * y

input |>
  stringr::str_remove_all("don't\\(\\).*?do\\(\\)") |>
  stringr::str_extract_all("mul\\(\\d+,\\d+\\)") |>
  unlist() |>
  purrr::map_int(\(x) eval(parse(text = x))) |>
  sum()
