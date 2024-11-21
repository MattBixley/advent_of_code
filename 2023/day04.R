library(tidyverse)
library(adventdrob)

#usethis::edit_r_environ("project")

input <- advent_input(4, 2023)
input

# part 1
matches <- input |>
  extract(x, c("winning", "have"), ".*:(.*)\\|(.*)") |>
  mutate(winning = str_extract_all(winning,  "\\d+"),
         have = str_extract_all(have,  "\\d+")) |>
  mutate(overlap = map2(winning, have, intersect)) |>
  mutate(n_matches = lengths(overlap))

matches |>
  filter(n_matches>0) |>
  summarise(sum(2^(n_matches-1)))
  
# part 2
n_games <- nrow(matches)
m <- matches$n_matches
copies <- rep(1, n_games)

for(i in seq_len(n_games)){
  if(m[i] > 0) {
    range <- seq(i+1, min(i+m[i], n_games))
    copies[range] <- copies[range] + copies[i]
  }
}  

sum(copies)
