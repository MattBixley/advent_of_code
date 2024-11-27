library(tidyverse)

# Read the input file
input <- read_lines('2015/day14_input.txt')

# Part 1
flying_distance <- function(speed, speed_dur, rest_dur, time) {
  sum(rep(c(rep(speed, speed_dur), rep(0, rest_dur)), length.out = time))
}

numbers <- str_extract_all(input, "[0-9]+")
numbers <- map(numbers, as.numeric)
names(numbers) <- str_extract(input, "\\w*")

ditances <- map_dbl(numbers, ~ flying_distance(.x[1], .x[2], .x[3], 2503))
sort(ditances, decreasing = TRUE)[1]

# Part 2
flying_distance_cumsum <- function(speed, speed_dur, rest_dur, time) {
  cumsum(rep(c(rep(speed, speed_dur), rep(0, rest_dur)), length.out = time))
}

ditances <- map2_dfr(
  numbers,
  names(numbers),
  ~ data.frame(
    location = flying_distance_cumsum(.x[1], .x[2], .x[3], 2503),
    time = seq_len(2503),
    reindeer = .y
  )
)

ditances %>%
  group_by(time) %>%
  slice_max(location, n = 1) %>%
  ungroup() %>%
  count(reindeer, sort = TRUE)

