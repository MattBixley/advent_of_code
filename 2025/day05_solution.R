library(tidyverse)

# -- Input --------------------------------------------------------------------
input <- read_lines("2025/day05_input.txt")

# sample <- c(
#   "3-5",
#   "10-14",
#   "16-20",
#   "12-18",
#   "",
#   "1",
#   "5",
#   "8",
#   "11",
#   "17",
#   "32"
# )
# (expected Part 1: 3)

# -- Parse --------------------------------------------------------------------
blank <- which(input == "")

ranges_raw <- input[1:(blank - 1)]
ids_raw    <- input[(blank + 1):length(input)]

ranges <- ranges_raw |>
  str_split_fixed("-", 2) |>
  as_tibble(.name_repair = ~ c("lo", "hi")) |>
  mutate(across(everything(), as.numeric))

ids <- as.numeric(ids_raw)

# -- Part 1 -------------------------------------------------------------------
# Count how many ingredient IDs fall within any fresh range (inclusive)

is_fresh <- function(id, ranges) {
  any(id >= ranges$lo & id <= ranges$hi)
}

result1 <- sum(map_lgl(ids, is_fresh, ranges = ranges))

cat("Part 1:", result1, "\n")

# -- Part 2 -------------------------------------------------------------------
# Total number of unique ingredient IDs covered by all fresh ranges combined
# Merge overlapping/adjacent ranges, then sum their widths

lo_sorted <- ranges$lo[order(ranges$lo)]
hi_sorted <- ranges$hi[order(ranges$lo)]

merged_lo <- lo_sorted[1]
merged_hi <- hi_sorted[1]

for (i in seq_along(lo_sorted)[-1]) {
  n <- length(merged_lo)
  if (lo_sorted[i] <= merged_hi[n] + 1) {
    merged_hi[n] <- max(merged_hi[n], hi_sorted[i])
  } else {
    merged_lo <- c(merged_lo, lo_sorted[i])
    merged_hi <- c(merged_hi, hi_sorted[i])
  }
}

result2 <- sum(merged_hi - merged_lo + 1)

cat("Part 2:", format(result2, scientific = FALSE), "\n")
