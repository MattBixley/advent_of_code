library(tidyverse)

# Read the input file
input <- 33100000

# Part 1
library(numbers)

i <- 0

repeat {
  i <- i + 1
  if (sum(divisors(i)) * 10 >= input) break
}
i

# Part 2
register <- numeric(input)

i <- 0

repeat {
  i <- i + 1
  
  divs <- divisors(i)
  
  register[divs] <- register[divs] + 1
  
  if (sum((register[divs] <= 50) * divs) * 11 >= input) break
  
}
i

