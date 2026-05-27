library(tidyverse)
source("scripts/utils.R")

equations <- tibble(line = read_lines("2024/day7_input.txt")) |>
  mutate(
    target = as.numeric(str_extract(line, "^[0-9]+")),
    nums   = map(str_extract(line, "(?<=: ).+"), ~ as.numeric(str_split(.x, " ")[[1]]))
  )

# Recursive DFS: accumulate from left, try +, *, and optionally ||
can_reach <- function(target, nums, use_concat = FALSE) {
  n <- length(nums)
  if (n == 1) return(nums[1] == target)

  check <- function(acc, idx) {
    if (idx > n) return(acc == target)
    # Prune: with only + and *, if acc already > target we can stop
    # (concat can make bigger, so only prune for non-concat case)
    if (!use_concat && acc > target) return(FALSE)
    v <- nums[idx]
    check(acc + v, idx + 1) ||
      check(acc * v, idx + 1) ||
      (use_concat && check(as.numeric(paste0(acc, v)), idx + 1))
  }

  check(nums[1], 2)
}

# Part 1
result1 <- equations |>
  mutate(valid = map2_lgl(target, nums, ~ can_reach(.x, .y, use_concat = FALSE))) |>
  filter(valid) |>
  summarise(total = sum(target)) |>
  pull(total)
cat("Part 1:", format(result1, scientific = FALSE), "\n")

# Part 2
result2 <- equations |>
  mutate(valid = map2_lgl(target, nums, ~ can_reach(.x, .y, use_concat = TRUE))) |>
  filter(valid) |>
  summarise(total = sum(target)) |>
  pull(total)
cat("Part 2:", format(result2, scientific = FALSE), "\n")
