library(tidyverse)

# Read the input file
input <- read_lines('2015/day19_input.txt')

# Part 1
molecules <- input[length(input)]

key <- input[-length(input)]
key <- key[key != ""]

key_tbl <- tibble(key) %>%
  separate(key, c("from", "to"), " => ")

keys <- split(key_tbl$to, key_tbl$from)

res <- character()

for (k in seq_along(keys)) {
  k <- keys[k]
  
  locs <- str_locate_all(molecules, names(k))[[1]]
  
  for (i in seq_len(nrow(locs))) {
    new_mole <- molecules
    
    str_sub(new_mole, locs[i, "start"], locs[i, "end"]) <- k[[1]]
    
    res <- c(res, new_mole)
  }
}

length(unique(res))

# Part 2
input <- read_lines('2015/day19_input.txt')

molecules <- input[length(input)]

key <- input[-length(input)]
key <- key[key != ""]

key_tbl <- tibble(key) %>%
  separate(key, c("from", "to"), " => ")

rev_keys <- split(key_tbl$from, key_tbl$to)
count <- 0

repeat {
  if (molecules == "e") break
  
  r_key <- sample(rev_keys, 1)
  
  
  if (str_detect(molecules, names(r_key))) {
    count <- count + str_count(molecules, names(r_key))
    molecules <- str_replace_all(molecules, names(r_key), r_key[[1]])
  }
  
}

count
