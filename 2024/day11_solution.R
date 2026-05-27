library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2024/day11_input.txt")[1]

stones_init <- str_split(str_trim(input), "\\s+")[[1]] |> as.numeric()

# Key insight: track counts by value, not individual stones.
# Represent state as a tibble with columns: stone (character), n (numeric).

init_counts <- function(stones) {
  tibble(stone = sprintf("%.0f", stones)) |>
    count(stone, name = "n")
}

# Apply one blink to the counts tibble.
# Rules:
#   0           -> "1"
#   even digits -> split in half (two new stones)
#   else        -> multiply by 2024
blink <- function(tbl) {
  tbl <- tbl |>
    mutate(
      nd   = nchar(stone),
      rule = case_when(
        stone == "0"  ~ "zero",
        nd %% 2 == 0  ~ "split",
        TRUE          ~ "multiply"
      )
    )

  zeros <- tbl |>
    filter(rule == "zero") |>
    transmute(stone = "1", n)

  splits <- tbl |>
    filter(rule == "split") |>
    mutate(
      half  = nd %/% 2L,
      left  = sub("^0+", "", substr(stone, 1L, half)),
      right = sub("^0+", "", substr(stone, half + 1L, nd)),
      left  = if_else(nchar(left)  == 0L, "0", left),
      right = if_else(nchar(right) == 0L, "0", right)
    ) |>
    select(n, left, right) |>
    pivot_longer(c(left, right), values_to = "stone", names_to = NULL) |>
    select(stone, n)

  multiplied <- tbl |>
    filter(rule == "multiply") |>
    transmute(stone = sprintf("%.0f", as.numeric(stone) * 2024), n)

  bind_rows(zeros, splits, multiplied) |>
    group_by(stone) |>
    summarise(n = sum(n), .groups = "drop")
}

run_blinks <- function(stones, n_blinks) {
  counts <- init_counts(stones)
  for (i in seq_len(n_blinks)) counts <- blink(counts)
  sum(counts$n)
}

result1 <- run_blinks(stones_init, 25)
cat("Part 1:", format(result1, scientific = FALSE), "\n")

result2 <- run_blinks(stones_init, 75)
cat("Part 2:", format(result2, scientific = FALSE), "\n")
