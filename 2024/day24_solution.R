library(tidyverse)

# -- Input --------------------------------------------------------------------
input <- read_lines("2024/day24_input.txt")

# Sample (expected Part 1: 2024):
# x00: 1, x01: 0, x02: 1, x03: 1, x04: 0, y00: 1, y01: 1, y02: 1, y03: 1, y04: 1, ...
# (see puzzle for full sample)

# -- Parse --------------------------------------------------------------------
blank <- which(input == "")
init_lines <- input[1:(blank - 1)]
gate_lines  <- input[(blank + 1):length(input)]

# Initial wire values
wires <- new.env(hash = TRUE, parent = emptyenv())
for (line in init_lines) {
  parts <- str_split_fixed(line, ": ", 2)
  wires[[parts[1]]] <- as.integer(parts[2])
}

# Gates: "a OP b -> out"
parse_gate <- function(line) {
  parts <- str_split(line, " ")[[1]]
  list(a = parts[1], op = parts[2], b = parts[3], out = parts[5])
}
gates <- map(gate_lines, parse_gate)

# -- Part 1 -------------------------------------------------------------------
# Simulate all gates until stable

remaining <- gates
while (length(remaining) > 0) {
  still_remaining <- list()
  for (g in remaining) {
    va <- wires[[g$a]]; vb <- wires[[g$b]]
    if (!is.null(va) && !is.null(vb)) {
      wires[[g$out]] <- switch(g$op,
        AND = bitwAnd(va, vb),
        OR  = bitwOr(va, vb),
        XOR = bitwXor(va, vb)
      )
    } else {
      still_remaining <- c(still_remaining, list(g))
    }
  }
  remaining <- still_remaining
}

# Build decimal from z-bits
z_names  <- ls(wires, pattern = "^z")
z_sorted <- z_names[order(as.integer(substring(z_names, 2)), decreasing = TRUE)]
bits     <- map_int(z_sorted, ~ wires[[.x]])

result1 <- sum(2^as.integer(substring(z_sorted[bits == 1], 2)))
cat("Part 1:", format(result1, scientific = FALSE), "\n")

# -- Part 2 -------------------------------------------------------------------
# The circuit is a ripple-carry adder. Find the 8 wrongly-wired outputs.
# Rules for a valid full adder:
#   R1. Every z-wire (except z45) must come from an XOR gate
#   R2. Every XOR gate not using x/y inputs must output to a z-wire
#   R3. Every AND output (except x00 AND y00) must feed into an OR gate
#   R4. Every XOR gate using x/y inputs (except bit 0) must NOT feed into OR

wrong <- character(0)

for (g in gates) {
  a <- g$a; b <- g$b; op <- g$op; out <- g$out
  is_xy <- grepl("^[xy]", a) && grepl("^[xy]", b)

  # R1: z-outputs (not z45) must come from XOR
  if (grepl("^z", out) && out != "z45" && op != "XOR") {
    wrong <- c(wrong, out)
  }

  # R2: XOR not on x/y inputs must output to z
  if (op == "XOR" && !is_xy && !grepl("^z", out)) {
    wrong <- c(wrong, out)
  }

  # R3: AND output (not x00/y00) must feed into OR
  if (op == "AND" && !(a %in% c("x00", "y00") && b %in% c("x00", "y00"))) {
    used_in_or <- any(map_lgl(gates, ~ (.x$a == out | .x$b == out) && .x$op == "OR"))
    if (!used_in_or) wrong <- c(wrong, out)
  }

  # R4: XOR on x/y (bit > 0) must not feed into OR
  if (op == "XOR" && is_xy) {
    bit_num <- as.integer(substring(a, 2))
    if (!is.na(bit_num) && bit_num > 0) {
      used_in_or <- any(map_lgl(gates, ~ (.x$a == out | .x$b == out) && .x$op == "OR"))
      if (used_in_or) wrong <- c(wrong, out)
    }
  }
}

result2 <- paste(sort(unique(wrong)), collapse = ",")
cat("Part 2:", result2, "\n")
