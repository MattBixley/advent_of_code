library(tidyverse)
library(adventdrob)

#usethis::edit_r_environ("project")

input <- advent_input(3, 2023)
input

# part 1
grid <- input |>
  grid_tidy(x) |>
  mutate(is_digit = str_detect(value, "\\d")) |>
  group_by(row) |>
  mutate(number_id = paste0(row, ".", 
        cumsum(is_digit != lag(is_digit, default = F)))) |>
  group_by(number_id) |>
  mutate(part_number = as.numeric(paste0(value, collapse = ""))) |>
  ungroup()

grid |>
  filter(!is.na(part_number)) |>
  adjacent_join(grid, diagonal = T) |>
  filter(value2 != ".", !is_digit2) |>
  arrange(row, col) |>
  distinct(number_id, .keep_all = T) |>
  summarise(sum(part_number))
  
  
# part 2
grid |>
  filter(value == "*") |>
  adjacent_join(grid, diagonal = T) |>
  filter(!is.na(part_number2)) |>
  distinct(row,col, number_id2, .keep_all = T) |>
  group_by(row,col) |>
  summarise(n_adjacent_numbers = n(),
            gear_ratio = prod(part_number2),
            .groups = "drop") |>
  filter(n_adjacent_numbers == 2) |>
  summarise(sum(gear_ratio))
  

