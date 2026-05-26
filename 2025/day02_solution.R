library(tidyverse)

# -- Input --------------------------------------------------------------------
# Single line: comma-separated ranges lo-hi
input <- read_lines("2025/day02_input.txt")

# sample (expected sum = 1227775554):
# input <- "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

ranges <- str_split(input, ",")[[1]] |>
  str_trim() |>
  keep(~ .x != "") |>
  str_split("-") |>
  map_dfr(~ tibble(lo = as.numeric(.x[1]), hi = as.numeric(.x[2])))

# -- Part 1 -------------------------------------------------------------------
# "Invalid" IDs: numbers where digits are some sequence repeated exactly twice.
# e.g. 11 = "1"+"1", 6464 = "64"+"64", 123123 = "123"+"123"
# No leading zeros: base must start >= 10^(n-1).
#
# A 2n-digit doubled number with n-digit base d = d * (10^n + 1).
# Generate all such numbers up to max(hi), check each against ranges.

max_val <- max(ranges$hi)

doubled <- map(1:5, function(n) {
  bases <- seq(10^(n - 1), 10^n - 1)  # 1-digit: 1..9, 2-digit: 10..99, etc.
  bases * (10^n + 1)
}) |>
  unlist() |>
  keep(~ .x <= max_val)

in_any_range <- function(x) any(ranges$lo <= x & x <= ranges$hi)

invalid <- doubled[map_lgl(doubled, in_any_range)]

part1 <- sum(invalid)
cat("Part 1:", format(part1, scientific = FALSE), "\n")

# -- Part 2 -------------------------------------------------------------------
# Now invalid = base repeated AT LEAST twice (k >= 2).
# Factor for n-digit base repeated k times: (10^(n*k) - 1) / (10^n - 1)
# Deduplicate (1111 = "1"x4 AND "11"x2).

invalid_set2 <- map(1:5, function(n) {
  map(2:floor(12 / n), function(k) {
    factor <- round((10^(n * k) - 1) / (10^n - 1))
    bases  <- seq(10^(n - 1), 10^n - 1)
    vals   <- bases * factor
    vals[vals <= max_val]
  }) |> unlist()
}) |>
  unlist() |>
  unique()

part2 <- sum(invalid_set2[map_lgl(invalid_set2, in_any_range)])
cat("Part 2:", format(part2, scientific = FALSE), "\n")
