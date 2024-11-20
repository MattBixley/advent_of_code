library(tidyverse)
library(adventdrob)

#usethis::edit_r_environ("project")

input <- advent_input(7, 2023)
input

hands <- input |>
  separate(x, c("hand", "bid"), convert = T)

# part 1
ranks <- str_split("23456789TJKA", "")[[1]]

hand_types <- hands |>
  mutate(card = str_split(hand, "")) |>
  mutate(rank_str = map_chr(card, 
                            ~ paste0(LETTERS[match(.,ranks)], 
                                   collapse=""))) |>
  unnest() |>
  count(hand, bid, rank_str, card, sort = T) |>
  group_by(hand, bid, rank_str) |>
  summarize(distribution = paste0(n, collapse = ""),.groups = "drop") |>
  mutate(type = match(distribution, 
             c("11111", "2111", "221", "311", "32", "41", "5"))) |>
  arrange(desc(type)) |>
  distinct(hand, .keep_all = T)

hand_types |>
  arrange(type, rank_str) |>
  summarise(sum(bid * row_number()))
  

# part 2
ranks <- str_split("J23456789TKA", "")[[1]]

hand_types <- hands |>
  mutate(card = str_split(hand, "")) |>
  mutate(rank_str = map_chr(card, 
                            ~ paste0(LETTERS[match(.,ranks)], 
                                     collapse=""))) |>
  unnest(cols = c(card)) |>
  # insert part 2 bits here
  mutate(i=row_number()) |>
  crossing(replace_joker_with = ranks) |>
  mutate(card = ifelse(card == "J", replace_joker_with, card)) |>
  count(hand, bid, rank_str, replace_joker_with, card) |>
  group_by(hand, bid, rank_str, replace_joker_with) |>
  # end part 2
  summarize(distribution = paste0(n, collapse = ""), 
            .groups ="drop") |>
  mutate(type = match(distribution, 
                      c("11111", "2111", "221", "311", "32", "41", "5"))) |>
  arrange(desc(type)) |>
  distinct(hand, .keep_all = T)

hand_types |>
  arrange(type, rank_str) |>
  summarise(sum(bid * row_number()))

