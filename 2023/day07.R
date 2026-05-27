library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day07_input.txt")

hands <- tibble(line = input) |>
  separate(line, c("hand", "bid"), sep = " ", convert = TRUE)

hand_type <- function(cards) {
  counts <- sort(table(cards), decreasing = TRUE)
  sig <- paste(as.integer(counts), collapse = "")
  match(sig, c("11111", "2111", "221", "311", "32", "41", "5"))
}

# Part 1
card_rank_p1 <- setNames(1:13, str_split("23456789TJQKA", "")[[1]])

result1 <- hands |>
  mutate(
    cards = str_split(hand, ""),
    type  = map_int(cards, hand_type),
    score = map_chr(cards, ~ paste(sprintf("%02d", card_rank_p1[.x]), collapse = ""))
  ) |>
  arrange(type, score) |>
  mutate(rank = row_number()) |>
  summarise(sum(rank * bid)) |>
  pull(1)

cat("Part 1:", result1, "\n")

# Part 2: J is joker
hand_type_joker <- function(cards) {
  jokers <- sum(cards == "J")
  if (jokers == 5) return(7L)
  non_j <- cards[cards != "J"]
  counts <- sort(table(non_j), decreasing = TRUE)
  counts[1] <- counts[1] + jokers
  sig <- paste(as.integer(counts), collapse = "")
  match(sig, c("11111", "2111", "221", "311", "32", "41", "5"))
}

card_rank_p2 <- setNames(1:13, str_split("J23456789TQKA", "")[[1]])

result2 <- hands |>
  mutate(
    cards = str_split(hand, ""),
    type  = map_int(cards, hand_type_joker),
    score = map_chr(cards, ~ paste(sprintf("%02d", card_rank_p2[.x]), collapse = ""))
  ) |>
  arrange(type, score) |>
  mutate(rank = row_number()) |>
  summarise(sum(rank * bid)) |>
  pull(1)

cat("Part 2:", result2, "\n")
