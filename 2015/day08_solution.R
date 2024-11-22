# Load necessary libraries
library(tidyverse)

# Read input data
input <- read_lines("2015/input08.txt")
#

# Part 1

sum(purrr::map_int(input, nchar)) -
  sum(purrr::map_int(input, ~nchar(eval(parse(text = .x)), type = "bytes")))

# Part 2

sum(purrr::map_int(stringi::stri_escape_unicode(input), nchar) + 2) -
  sum(purrr::map_int(input, nchar))

