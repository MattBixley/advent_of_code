library(tidyverse)

# Read the input file
input <- read_lines('2015/day15_input.txt')

# Part 1
# Hand imported input
values <- list(
  Frosting =    c(capacity = 4, durability = -2, flavor = 0, texture = 0, calories = 5),
  Candy = c(capacity = 0, durability = 5, flavor = -1, texture = 0, calories = 8),
  Butterscotch = c(capacity = -1, durability = 0, flavor = 5, texture = 0, calories = 6),
  Sugar = c(capacity = 0, durability = 0, flavor = -2, texture = 2, calories = 1)
)

fourway_sum <- function(n) {
  expand.grid(Frosting = 0:100,
              Candy = 0:100,
              Butterscotch = 0:100) %>%
    filter(Frosting + Candy + Butterscotch == n) %>%
    mutate(Sugar = 100 - n)
}

combinations <- purrr::map_dfr(0:100, fourway_sum)

batter_mizer <- function(Frosting, Candy, Butterscotch, Sugar) {
  sum <- values[["Frosting"]] * Frosting +
    values[["Candy"]] * Candy +
    values[["Butterscotch"]] * Butterscotch +
    values[["Sugar"]] * Sugar
  
  sum <- pmax(sum, 0)
  
  prod(sum[1:4])
}

res <- purrr::pmap_dbl(combinations, batter_mizer)
max(res)

# Part 2

batter_mizer_500_cal <- function(Frosting, Candy, Butterscotch, Sugar) {
  sum <- values[["Frosting"]] * Frosting +
    values[["Candy"]] * Candy +
    values[["Butterscotch"]] * Butterscotch +
    values[["Sugar"]] * Sugar
  
  sum <- pmax(sum, 0)
  
  if (sum[5] != 500) return(0)
  
  prod(sum[1:4])
}

res <- purrr::pmap_dbl(combinations, batter_mizer_500_cal)
max(res)

