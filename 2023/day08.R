library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day08_input.txt")

directions <- str_split(input[1], "")[[1]]

network <- tibble(line = input[-(1:2)]) |>
  mutate(
    node  = str_extract(line, "^\\w+"),
    left  = str_extract(line, "(?<=\\()\\w+"),
    right = str_extract(line, "(?<=, )\\w+")
  ) |>
  select(node, left, right)

# Build lookup as named list for O(1) access
net <- list()
for (i in seq_len(nrow(network))) {
  net[[network$node[i]]] <- c(network$left[i], network$right[i])
}

steps_to_z <- function(start, end_fn) {
  node <- start
  n <- length(directions)
  steps <- 0L
  repeat {
    d <- directions[(steps %% n) + 1L]
    node <- if (d == "L") net[[node]][1] else net[[node]][2]
    steps <- steps + 1L
    if (end_fn(node)) return(steps)
  }
}

# Part 1
result1 <- steps_to_z("AAA", function(n) n == "ZZZ")
cat("Part 1:", result1, "\n")

# Part 2 - LCM of cycle lengths
starts <- network$node[str_ends(network$node, "A")]
cycle_lengths <- map_dbl(starts, ~ steps_to_z(.x, function(n) str_ends(n, "Z")))

lcm2 <- function(a, b) a / gcd(a, b) * b
gcd <- function(a, b) if (b == 0) a else gcd(b, a %% b)

result2 <- format(Reduce(lcm2, cycle_lengths), scientific = FALSE)
cat("Part 2:", result2, "\n")
