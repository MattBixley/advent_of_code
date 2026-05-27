library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day19_input.txt")
blank <- which(input == "")

# Parse workflows
workflows <- list()
for (line in input[1:(blank - 1)]) {
  name      <- str_extract(line, "^\\w+")
  rules_str <- str_match(line, "\\{(.+)\\}")[, 2]
  rules     <- str_split(rules_str, ",")[[1]]
  workflows[[name]] <- rules
}

# Parse parts
parts <- map(input[(blank + 1):length(input)], function(line) {
  nums <- as.integer(str_extract_all(line, "\\d+")[[1]])
  setNames(nums, c("x", "m", "a", "s"))
})

apply_workflows <- function(part) {
  wf <- "in"
  repeat {
    if (wf == "A") return(TRUE)
    if (wf == "R") return(FALSE)
    rules <- workflows[[wf]]
    for (rule in rules) {
      if (!grepl(":", rule)) {
        wf <- rule
        break
      }
      cond <- str_match(rule, "([xmas])([<>])(\\d+):(.+)")
      cat_  <- cond[, 2]
      op    <- cond[, 3]
      val   <- as.integer(cond[, 4])
      dest  <- cond[, 5]
      pval  <- part[[cat_]]
      test  <- if (op == "<") pval < val else pval > val
      if (test) {
        wf <- dest
        break
      }
    }
  }
}

result1 <- sum(map_int(parts, function(p) {
  if (apply_workflows(p)) sum(p) else 0L
}))
cat("Part 1:", result1, "\n")

# Part 2: propagate ranges through workflows
# ranges: named list with x, m, a, s each c(lo, hi)
process_ranges <- function(wf_name, ranges) {
  if (wf_name == "R") return(0)
  if (wf_name == "A") {
    return(prod(map_dbl(ranges, ~ .x[2] - .x[1] + 1)))
  }

  rules <- workflows[[wf_name]]
  total <- 0
  cur   <- ranges

  for (rule in rules) {
    if (!grepl(":", rule)) {
      total <- total + process_ranges(rule, cur)
      break
    }
    cond <- str_match(rule, "([xmas])([<>])(\\d+):(.+)")
    cat_  <- cond[, 2]
    op    <- cond[, 3]
    val   <- as.integer(cond[, 4])
    dest  <- cond[, 5]
    rng   <- cur[[cat_]]

    if (op == "<") {
      # Pass: [lo, val-1]; Fail (continue): [val, hi]
      if (rng[1] < val) {
        pass         <- cur
        pass[[cat_]] <- c(rng[1], min(rng[2], val - 1))
        total        <- total + process_ranges(dest, pass)
      }
      if (rng[2] >= val) {
        cur[[cat_]] <- c(max(rng[1], val), rng[2])
      } else {
        break
      }
    } else {
      # ">" Pass: [val+1, hi]; Fail (continue): [lo, val]
      if (rng[2] > val) {
        pass         <- cur
        pass[[cat_]] <- c(max(rng[1], val + 1), rng[2])
        total        <- total + process_ranges(dest, pass)
      }
      if (rng[1] <= val) {
        cur[[cat_]] <- c(rng[1], min(rng[2], val))
      } else {
        break
      }
    }
  }
  total
}

init_ranges <- list(x = c(1, 4000), m = c(1, 4000), a = c(1, 4000), s = c(1, 4000))
result2 <- process_ranges("in", init_ranges)
cat("Part 2:", format(result2, scientific = FALSE), "\n")
