read_as_grid <- function(input, col_sep = "") {
  tibble(value = input) %>% 
    separate_rows(value, sep = "\n") %>% 
    mutate(y = 1:n()) %>% 
    separate_rows(value, sep = col_sep) %>% 
    filter(value != "") %>% 
    group_by(y) %>% 
    mutate(x = 1:n()) %>% 
    ungroup()
}

one_indexed_remainder <- function(value, mod) {
  if_else(value %% mod == 0, mod, value %% mod) 
}

directions <- tribble(
  ~direction, ~dx, ~dy,
  "tl", -1, -1,
  "t", 0, -1,
  "tr", 1, -1,
  "l", -1, 0,
  "self", 0, 0,
  "r", 1, 0,
  "bl", -1, 1,
  "b", 0, 1,
  "br", 1, 1
) %>% 
  select(-direction)

binary_vec_to_decimal <- function(binary) {
  sum(rev(binary) * 2 ^ (0:(length(binary) - 1)))
}

decimal_to_n_digit_binary <- function(num, n) {
  result <- num %% 2
  num <- (num - result) / 2
  while (num > 0) {
    result <- paste0(num %% 2, result)
    num <- (num - num %% 2) / 2
  }
  str_pad(result, n, pad = '0')
}

sort_chars <- function(word) {
  paste(sort(str_split(word, "")[[1]]), collapse = "")
}

parse.group <- function(regex,input) { #old faithful (from regexpr help file)
  result <- regexpr(regex, input, perl=TRUE)
  m <- do.call(rbind, lapply(seq_along(input), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(input[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}

# -- Tidyverse-friendly utilities (2025+) -------------------------------------

# Parse a character grid into a tibble with x, y, value columns.
# x increases left-to-right, y increases top-to-bottom (row 1 = top).
parse_grid <- function(input) {
  tibble(line = input) |>
    mutate(y = row_number()) |>
    mutate(chars = str_split(line, "")) |>
    unnest_longer(chars, indices_to = "x") |>
    rename(value = chars) |>
    select(x, y, value)
}

# Manhattan distance between two points (defaults to origin).
manhattan <- function(x1, y1, x2 = 0, y2 = 0) abs(x1 - x2) + abs(y1 - y2)

# Count occurrences of each element; returns a named integer vector.
count_vals <- function(x) {
  tbl <- table(x)
  setNames(as.integer(tbl), names(tbl))
}

# Read an input file and split into paragraphs (groups separated by blank lines).
# Returns a list of character vectors, one per paragraph.
read_paragraphs <- function(path) {
  lines <- read_lines(path)
  split(lines, cumsum(lines == "")) |>
    purrr::map(~ .x[.x != ""]) |>
    purrr::keep(~ length(.x) > 0)
}
