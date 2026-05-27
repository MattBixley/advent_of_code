library(tidyverse)

input <- read_lines("2024/day17_input.txt")

# ---- Parse input ----
reg_A <- as.numeric(sub("Register A: ", "", input[1]))
reg_B <- as.numeric(sub("Register B: ", "", input[2]))
reg_C <- as.numeric(sub("Register C: ", "", input[3]))

prog <- as.integer(strsplit(sub("Program: ", "", input[5]), ",")[[1]])

# ---- Interpreter ----
# Use double (numeric) for registers to handle large integers within 2^53 precision
run_program <- function(A, B, C, program) {
  ip  <- 0L
  out <- integer(0)

  combo <- function(op) {
    switch(op + 1L,
      0, 1, 2, 3, A, B, C, stop("invalid combo 7")
    )
  }

  while (ip < length(program)) {
    opcode  <- program[ip + 1L]
    operand <- program[ip + 2L]

    if (opcode == 0L) {        # adv
      A <- trunc(A / 2^combo(operand))
    } else if (opcode == 1L) { # bxl
      B <- bitwXor(as.integer(B %% 2^31), as.integer(operand))
    } else if (opcode == 2L) { # bst
      B <- combo(operand) %% 8
    } else if (opcode == 3L) { # jnz
      if (A != 0) {
        ip <- as.integer(operand)
        next
      }
    } else if (opcode == 4L) { # bxc
      B <- bitwXor(as.integer(B %% 2^31), as.integer(C %% 2^31))
    } else if (opcode == 5L) { # out
      out <- c(out, as.integer(combo(operand) %% 8))
    } else if (opcode == 6L) { # bdv
      B <- trunc(A / 2^combo(operand))
    } else if (opcode == 7L) { # cdv
      C <- trunc(A / 2^combo(operand))
    }
    ip <- ip + 2L
  }
  out
}

# Part 1
output1 <- run_program(reg_A, reg_B, reg_C, prog)
result1 <- paste(output1, collapse = ",")
cat("Part 1:", result1, "\n")

# ---- Part 2 ----
# The program produces one output digit per iteration, consuming 3 bits of A
# (A = A >> 3 each loop). So to make the program output itself (length n),
# we need to find A such that the n-th output matches prog[n], etc.
#
# Strategy: build A from most significant 3-bit chunk down to least significant.
# For the last output digit, try values 0..7 (those are the top 3 bits).
# For each earlier digit, shift left 3 bits and try appending 0..7.

find_A <- function(program) {
  n <- length(program)

  # Recursive search: given the partial A (accounting for outputs from index `pos`
  # to n), find a full A.
  # pos: 1-indexed position in program (1 = first output, n = last)
  # We build from last output to first (most significant bits first).

  search <- function(pos, current_A) {
    # Try all 3-bit suffixes
    for (bits in 0:7) {
      candidate <- current_A * 8 + bits
      if (candidate == 0 && pos == n) next  # A=0 is trivial/invalid (would not loop)

      out <- run_program(candidate, reg_B, reg_C, program)

      # Check if output matches program[pos..n]
      expected <- program[pos:n]
      if (length(out) == length(expected) && all(out == expected)) {
        if (pos == 1L) {
          return(candidate)  # Found full solution
        }
        result <- search(pos - 1L, candidate)
        if (!is.null(result)) return(result)
      }
    }
    NULL
  }

  search(n, 0)
}

result2 <- find_A(prog)
cat("Part 2:", result2, "\n")
