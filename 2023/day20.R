library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day20_input.txt")

parse_modules <- function(lines) {
  modules <- list()
  for (line in lines) {
    parts    <- str_split(line, " -> ")[[1]]
    name_raw <- parts[1]
    dests    <- str_split(parts[2], ", ")[[1]]
    if (name_raw == "broadcaster") {
      modules[["broadcaster"]] <- list(type = "broadcaster", dests = dests, state = FALSE)
    } else {
      type <- substr(name_raw, 1, 1)
      name <- substr(name_raw, 2, nchar(name_raw))
      if (type == "%") {
        modules[[name]] <- list(type = "ff", dests = dests, state = FALSE)
      } else {
        modules[[name]] <- list(type = "conj", dests = dests, memory = list())
      }
    }
  }
  # Initialise conjunction memories: all inputs default to "low"
  for (src in names(modules)) {
    for (dst in modules[[src]]$dests) {
      if (!is.null(modules[[dst]]) && modules[[dst]]$type == "conj") {
        modules[[dst]]$memory[[src]] <- "low"
      }
    }
  }
  modules
}

press_button <- function(modules, watch_conj = NULL) {
  low_count  <- 0L
  high_count <- 0L
  fired_high <- character(0)

  queue <- list(list(from = "button", to = "broadcaster", pulse = "low"))

  while (length(queue) > 0) {
    msg   <- queue[[1]]
    queue <- queue[-1]
    from  <- msg$from
    to    <- msg$to
    pulse <- msg$pulse

    if (pulse == "low") low_count  <- low_count  + 1L
    else                high_count <- high_count + 1L

    if (!is.null(watch_conj) && to == watch_conj && pulse == "high")
      fired_high <- c(fired_high, from)

    mod <- modules[[to]]
    if (is.null(mod)) next

    if (mod$type == "broadcaster") {
      for (dst in mod$dests)
        queue <- c(queue, list(list(from = to, to = dst, pulse = pulse)))

    } else if (mod$type == "ff") {
      if (pulse == "low") {
        modules[[to]]$state <- !modules[[to]]$state
        out <- if (modules[[to]]$state) "high" else "low"
        for (dst in mod$dests)
          queue <- c(queue, list(list(from = to, to = dst, pulse = out)))
      }

    } else if (mod$type == "conj") {
      modules[[to]]$memory[[from]] <- pulse
      out <- if (all(unlist(modules[[to]]$memory) == "high")) "low" else "high"
      for (dst in mod$dests)
        queue <- c(queue, list(list(from = to, to = dst, pulse = out)))
    }
  }

  list(modules = modules, low = low_count, high = high_count, fired = fired_high)
}

# Part 1: press button 1000 times, count pulses
mods        <- parse_modules(input)
total_low   <- 0L
total_high  <- 0L

for (i in seq_len(1000)) {
  res        <- press_button(mods)
  mods       <- res$modules
  total_low  <- total_low  + res$low
  total_high <- total_high + res$high
}

result1 <- total_low * total_high
cat("Part 1:", result1, "\n")

# Part 2: find minimum presses until rx receives a single low pulse
# rx is fed by one conjunction; that conjunction fires low only when all
# its inputs are high. Find the cycle length for each of those inputs.
mods <- parse_modules(input)

rx_feeder     <- names(Filter(function(m) "rx" %in% m$dests, mods))
feeder_inputs <- names(mods[[rx_feeder]]$memory)

cycles <- setNames(rep(NA_real_, length(feeder_inputs)), feeder_inputs)
press  <- 0L

while (any(is.na(cycles))) {
  press <- press + 1L
  res   <- press_button(mods, watch_conj = rx_feeder)
  mods  <- res$modules
  for (src in res$fired) {
    if (is.na(cycles[src])) cycles[src] <- press
  }
}

gcd2 <- function(a, b) if (b == 0) a else gcd2(b, a %% b)
lcm2 <- function(a, b) a / gcd2(a, b) * b

result2 <- format(Reduce(lcm2, cycles), scientific = FALSE)
cat("Part 2:", result2, "\n")
