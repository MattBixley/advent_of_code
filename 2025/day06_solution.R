library(tidyverse)

# -- Input --------------------------------------------------------------------
# 5 rows: 4 number rows + 1 operator row (+ or *)
# Problems arranged vertically in columns, separated by all-space columns.

lines <- read_lines("2025/day06_input.txt")

# Pad all lines to same width
max_len <- max(str_length(lines))
lines <- str_pad(lines, max_len, "right")

n <- length(lines)
chars <- do.call(rbind, str_split(lines, ""))  # matrix: rows × cols

# Separator columns: every row is a space
sep <- apply(chars, 2, \(col) all(col == " "))

# Group contiguous non-separator columns into problems
non_sep <- which(!sep)
boundaries <- c(0, which(diff(non_sep) > 1), length(non_sep))
problems <- map(seq_along(boundaries[-1]), \(i) non_sep[(boundaries[i]+1):boundaries[i+1]])

# For each problem: extract numbers (rows 1..n-1), get operator (row n)
solve_problem <- function(cols) {
  nums <- map(1:(n - 1), \(r) {
    s <- str_trim(paste(chars[r, cols], collapse = ""))
    if (s == "") NA_real_ else as.numeric(s)
  }) |> compact() |> as.numeric()

  op <- str_trim(paste(chars[n, cols], collapse = "")) |> str_sub(1, 1)
  if (op == "*") prod(nums) else sum(nums)
}

# -- Part 1 -------------------------------------------------------------------
results <- map_dbl(problems, solve_problem)
part1 <- sum(results)
cat("Part 1:", format(part1, scientific = FALSE), "\n")

# -- Part 2 -------------------------------------------------------------------
