library(tidyverse)
library(adventdrob)

#usethis::edit_r_environ("project")

input <- advent_input(2, 2023)
input

game_cubes <- input |>
  mutate(game = row_number()) |>
  mutate(cubes = str_extract_all(x, "\\d+ [a-z]+")) |>
  unnest(cubes) |>
  separate(cubes, c("number", "colour"), sep = " ", convert = T)

# part 1

maxima <- c(red = 12, green = 13, blue = 14)

game_cubes |>
  mutate(maximum = maxima[colour]) |>
  group_by(game) |>
  summarise(playable = !any(number > maximum)) |>
  filter(playable) |>
  summarise(sum(game))

# part 2
game_cubes |>
  group_by(game, colour) |>
  summarise(number = max(number)) |>
  summarise(power = prod(number)) |>
  summarise(sum(power))
