library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2024/day9_input.txt")[1]

# Parse digits using tidyverse pipeline
digits <- str_split(input, "")[[1]] |> as.integer()

# Expand to block representation: file IDs or NA for free space
# Build a tibble of (file_id or NA, count) pairs then unnest to get the flat vector
expand_disk <- function(digits) {
  tibble(n = digits, idx = seq_along(digits)) |>
    mutate(
      is_file  = idx %% 2 == 1,
      file_id  = if_else(is_file, (idx - 1L) %/% 2L, NA_integer_)
    ) |>
    filter(n > 0) |>
    mutate(block = map2(file_id, n, ~ rep(.x, .y))) |>
    pull(block) |>
    reduce(c)
}

blocks <- expand_disk(digits)

# Part 1: Move individual blocks from rightmost file to leftmost free space
compact_individual <- function(b) {
  repeat {
    free_pos  <- which(is.na(b))
    file_pos  <- which(!is.na(b))
    if (length(free_pos) == 0) break
    leftmost_free   <- free_pos[1]
    rightmost_file  <- file_pos[length(file_pos)]
    if (leftmost_free >= rightmost_file) break
    b[leftmost_free]  <- b[rightmost_file]
    b[rightmost_file] <- NA_integer_
  }
  b
}

p1_blocks <- compact_individual(blocks)

# Checksum: sum(0-indexed position * file_id)
checksum1 <- tibble(block = p1_blocks) |>
  mutate(pos = row_number() - 1L) |>
  filter(!is.na(block)) |>
  summarise(cs = sum(pos * as.numeric(block))) |>
  pull(cs)

cat("Part 1:", format(checksum1, scientific = FALSE), "\n")

# Part 2: Move whole files in decreasing file-ID order
compact_whole_files <- function(b) {
  max_id <- max(b, na.rm = TRUE)

  for (fid in max_id:0) {
    file_pos <- which(b == fid)
    if (length(file_pos) == 0) next

    file_len  <- length(file_pos)
    file_start <- file_pos[1]

    # Find leftmost contiguous NA run of length >= file_len, before file_start
    free <- is.na(b)
    target_start <- NA_integer_
    run_start    <- NA_integer_
    run_len      <- 0L

    for (i in seq_len(file_start - 1)) {
      if (free[i]) {
        if (is.na(run_start)) run_start <- i
        run_len <- run_len + 1L
        if (run_len >= file_len) {
          target_start <- run_start
          break
        }
      } else {
        run_start <- NA_integer_
        run_len   <- 0L
      }
    }

    if (!is.na(target_start)) {
      b[target_start:(target_start + file_len - 1)] <- fid
      b[file_pos] <- NA_integer_
    }
  }
  b
}

p2_blocks <- compact_whole_files(blocks)

checksum2 <- tibble(block = p2_blocks) |>
  mutate(pos = row_number() - 1L) |>
  filter(!is.na(block)) |>
  summarise(cs = sum(pos * as.numeric(block))) |>
  pull(cs)

cat("Part 2:", format(checksum2, scientific = FALSE), "\n")
