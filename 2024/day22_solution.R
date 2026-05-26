library(tidyverse)

# Day 22: Monkey Market
# Part 1: Sum of 2000th secret numbers
# Part 2: Find the 4-change sequence that maximizes total bananas sold
#
# Secret evolution (XOR + mod 16777216):
#   1. mix (s * 64), prune
#   2. mix (s / 32 floor), prune
#   3. mix (s * 2048), prune
# Price = secret mod 10
#
# Sample Part 1: buyers 1, 10, 100, 2024 -> 37327623

MOD <- 16777216L

next_secret <- function(s) {
  s <- bitwXor(as.integer((as.numeric(s) * 64L) %% MOD), as.integer(s %% MOD))
  s <- bitwXor(as.integer(s %/% 32L), s)
  s <- as.integer(bitwXor(as.integer((as.numeric(s) * 2048L) %% MOD), as.integer(s %% MOD)))
  s
}

gen_secrets <- function(init, n = 2000L) {
  secrets <- integer(n + 1L)
  secrets[1L] <- as.integer(init)
  for (i in seq_len(n)) secrets[i + 1L] <- next_secret(secrets[i])
  secrets
}

# -- Input --------------------------------------------------------------------
buyers <- as.integer(readLines("2024/day22_input.txt"))
buyers <- buyers[!is.na(buyers)]

# Part 1: sum of 2000th secret
part1 <- sum(sapply(buyers, function(s) {
  for (i in seq_len(2000L)) s <- next_secret(s)
  as.numeric(s)
}))
cat("Part 1:", format(part1, scientific = FALSE), "\n")

# Part 2: find the 4-change sequence that yields the most bananas
# Encode 4-change sequence as single integer key (changes range -9..9, +9 to offset)
N_KEYS <- 19L^4L
totals <- integer(N_KEYS)

for (init_s in buyers) {
  secrets <- gen_secrets(init_s, 2000L)
  prices  <- secrets %% 10L
  changes <- diff(prices)
  seen <- logical(N_KEYS)
  for (i in seq_len(length(changes) - 3L)) {
    key <- ((changes[i] + 9L) * 6859L) +
           ((changes[i + 1L] + 9L) * 361L) +
           ((changes[i + 2L] + 9L) * 19L) +
           (changes[i + 3L] + 9L) + 1L
    if (!seen[key]) {
      seen[key] <- TRUE
      totals[key] <- totals[key] + prices[i + 4L]
    }
  }
}

part2 <- max(totals)
cat("Part 2:", part2, "\n")
