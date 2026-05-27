library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day12_input.txt")

rows <- tibble(line = input) |>
  mutate(
    pattern = str_extract(line, "^\\S+"),
    groups  = map(str_extract(line, "(?<= ).+"),
                  ~ as.integer(str_split(.x, ",")[[1]]))
  )

count_arrangements <- function(pattern, groups) {
  chars   <- strsplit(pattern, "")[[1]]
  pat_len <- length(chars)
  grp_len <- length(groups)

  # Precompute: can_place[i] = can we start a group at position i?
  # (no '.' in chars[i..(i+g-1)] and char after is not '#')
  # Done lazily via memo; precompute prefix "no dot" for fast range check
  has_dot <- chars == "."
  # no_dot_from[i] = length of dot-free run starting at i
  no_dot_run <- integer(pat_len + 1L)
  for (i in rev(seq_len(pat_len))) {
    no_dot_run[i] <- if (has_dot[i]) 0L else no_dot_run[i + 1L] + 1L
  }

  memo <- matrix(-1.0, nrow = pat_len + 2L, ncol = grp_len + 2L)

  go <- function(pi, gi) {
    if (memo[pi, gi] >= 0) return(memo[pi, gi])

    if (gi > grp_len) {
      result <- if (pi > pat_len || all(chars[pi:pat_len] != "#")) 1.0 else 0.0
      memo[pi, gi] <<- result; return(result)
    }
    if (pi > pat_len) { memo[pi, gi] <<- 0.0; return(0.0) }

    result <- 0.0
    ch <- chars[pi]

    if (ch != "#") result <- result + go(pi + 1L, gi)

    if (ch != ".") {
      g   <- groups[gi]
      end <- pi + g - 1L
      if (end <= pat_len && no_dot_run[pi] >= g) {
        after_ok <- end == pat_len || chars[end + 1L] != "#"
        if (after_ok) result <- result + go(end + 2L, gi + 1L)
      }
    }

    memo[pi, gi] <<- result
    result
  }

  go(1L, 1L)
}

result1 <- sum(map2_dbl(rows$pattern, rows$groups, count_arrangements))
cat("Part 1:", result1, "\n")

# Part 2: unfold 5x
rows_p2 <- rows |>
  mutate(
    pattern = map_chr(pattern, ~ paste(rep(.x, 5), collapse = "?")),
    groups  = map(groups, ~ rep(.x, 5))
  )

result2 <- sum(map2_dbl(rows_p2$pattern, rows_p2$groups, count_arrangements))
cat("Part 2:", format(result2, scientific = FALSE), "\n")
