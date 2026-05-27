library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day15_input.txt")[1]

steps <- str_split(input, ",")[[1]]

hash_str <- function(s) {
  chars <- utf8ToInt(s)
  Reduce(function(acc, ch) ((acc + ch) * 17L) %% 256L, chars, 0L)
}

result1 <- sum(map_int(steps, hash_str))
cat("Part 1:", result1, "\n")

# Part 2: boxes as list of ordered key-value pairs
boxes <- vector("list", 256)
for (i in seq_along(boxes)) boxes[[i]] <- list()  # list of c(label, focal)

for (step in steps) {
  if (str_ends(step, "-")) {
    label <- str_sub(step, 1, -2)
    box   <- hash_str(label) + 1L
    boxes[[box]] <- Filter(function(lens) lens[1] != label, boxes[[box]])
  } else {
    parts <- str_split(step, "=")[[1]]
    label <- parts[1]; focal <- as.integer(parts[2])
    box   <- hash_str(label) + 1L
    existing <- map_chr(boxes[[box]], ~ .x[1])
    pos <- which(existing == label)
    if (length(pos) > 0) {
      boxes[[box]][[pos]] <- c(label, focal)
    } else {
      boxes[[box]] <- c(boxes[[box]], list(c(label, focal)))
    }
  }
}

result2 <- 0L
for (b in seq_along(boxes)) {
  if (length(boxes[[b]]) > 0) {
    for (s in seq_along(boxes[[b]])) {
      result2 <- result2 + b * s * as.integer(boxes[[b]][[s]][2])
    }
  }
}

cat("Part 2:", result2, "\n")
