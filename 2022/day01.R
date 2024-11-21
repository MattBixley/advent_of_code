library(tidyverse)

input <- tibble(read_lines("2022/day02_input.txt") |>
                  as.numeric())
names(input) <- "x"

calories <- input |>
  mutate(group = cumsum(is.na(x))) |>
  count(group, wt = x)

# Part 1 
out1 <- max(calories)

cat("Puzzle 1 solution:", out1, fill = TRUE)

# Part 2 
out2 <- calories |>
  arrange(desc(n)) |>
  head(3) |>
  summarize(sum(n))

cat("Puzzle 2 solution:", out2[[1]], fill = TRUE)

