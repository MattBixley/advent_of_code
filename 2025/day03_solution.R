library(tidyverse)

# -- Input --------------------------------------------------------------------
input <- read_lines("2025/day03_input.txt")

# sample <- c("987654321111111", "811111111111119", "234234234234278", "818181911112111")
# (expected Part 1: 357, Part 2: TBD)

# -- Parsing ------------------------------------------------------------------
# Each line is a bank of single digits (1-9).
# Select exactly 2 batteries (maintain order) to form the largest 2-digit number.

max_joltage <- function(line) {
  digits <- as.integer(str_split(line, "")[[1]])
  n <- length(digits)
  # For each possible first digit position, find largest digit after it
  # To maximise tens place: pick largest digit overall (as early as possible),
  # then pick largest digit after it.
  # Brute force is fine for ~100 char lines.
  best <- 0L
  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      val <- digits[i] * 10L + digits[j]
      if (val > best) best <- val
    }
  }
  best
}

# -- Part 1 -------------------------------------------------------------------
joltages <- map_int(input, max_joltage)
result1 <- sum(joltages)
cat("Part 1:", result1, "\n")

# -- Part 2 -------------------------------------------------------------------
# Pick exactly 12 digits (maintaining order) to maximise the 12-digit number.
# Greedy: for position i, pick max digit in range [start, n - (k - i)].

max_k_digits <- function(line, k = 12L) {
  digits <- as.integer(str_split(line, "")[[1]])
  n <- length(digits)
  if (n <= k) return(as.numeric(paste(digits, collapse = "")))
  result <- integer(k)
  start <- 1L
  for (i in seq_len(k)) {
    window <- digits[start:(n - k + i)]
    best <- which.max(window)
    result[i] <- window[best]
    start <- start + best
  }
  as.numeric(paste(result, collapse = ""))
}

part2 <- sum(map_dbl(input, max_k_digits))
cat("Part 2:", format(part2, scientific = FALSE), "\n")
