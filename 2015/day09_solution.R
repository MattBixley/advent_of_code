# Load necessary libraries
library(tidyverse)

# Read input data
input <- read_lines("2015/input09.txt")

# Part 1

input_tbl <- tibble(input) %>%
  separate(input, c("destinations", "distance"), " = ", convert = TRUE) %>%
  separate(destinations, c("to", "from"), " to ")

all_paths <- input_tbl %>%
  select(-distance) %>%
  unlist() %>%
  unique() %>%
  combinat::permn()

distances <- bind_rows(
  input_tbl,
  input_tbl %>% rename(from = to, to = from)
)

calc_distance <- function(x) {
  tibble(
    from = x[-length(x)],
    to = x[-1]
  ) %>%
    left_join(distances, by = c("from", "to")) %>%
    summarise(sum = sum(distance)) %>%
    pull(sum)
}

purrr::map_int(all_paths, calc_distance) %>%
  min()

# Part 2
purrr::map_int(all_paths, calc_distance) %>%
  max()