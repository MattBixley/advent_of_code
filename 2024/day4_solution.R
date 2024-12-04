library(tidyverse)

# Read the input file
read_matrix <- function(path, sep = "", fill = NA, type = identity) {
  lines <- readLines(path)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(fill, nrow = length(lines), ncol = max(token_lengths))
  
  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- type(tokens[[i]])
  }
  res
}

input <- read_matrix("2024/day4_input.txt")

# Part 1
up <- apply(input, 1, identity, simplify = FALSE)
down <- lapply(up, rev)
left <- apply(input, 2, identity, simplify = FALSE)
right <- lapply(left, rev)

nrow <- nrow(input)

diag1 <- purrr::map(seq(-nrow - 1, nrow - 1), \(i) input[row(input) == (col(input) + i)])
diag2 <- purrr::map(seq(2, nrow * 2), \(i) input[row(input) + col(input) == i])

diag3 <- lapply(diag1, rev)
diag4 <- lapply(diag2, rev)

c(left, right, up, down, diag1, diag2, diag3, diag4) |>
  purrr::map_chr(\(x) paste(x, collapse = "")) |>
  stringr::str_count("XMAS") |>
  sum()

# Part 2
a_loc <- reshape2::melt(input == "A") |>
  dplyr::filter(value)

count <- 0

for (i in seq_len(nrow(a_loc))) {
  vals <- a_loc[i, ]
  x <- vals$Var1
  y <- vals$Var2
  
  if (x == 1 || x == nrow(input) || y == 1 || y == nrow(input)) {
    next
  }
  
  x_values <- paste0(
    input[x + 1, y + 1], input[x + 1, y - 1], 
    input[x - 1, y - 1], input[x - 1, y + 1],
    collapse = ""
  )
  
  valid <- c("MMSS", "MSSM", "SSMM", "SMMS")
  
  
  if (x_values %in% valid) {
    count <- count + 1
  }
}

count

