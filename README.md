# Advent of Code — R / Tidyverse Solutions

Personal solutions to [Advent of Code](https://adventofcode.com) puzzles, solved primarily in R with tidyverse. Years 2015 – 2024 complete (to varying degrees); 2025 scaffolded and ready.

**Coverage:** 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025

---

## Quick Start — New Day

With the project open in RStudio (`.Rprofile` auto-sources `scripts/create_files.R`):

```r
aoc(2025, 1)
# Creates: 2025/day01_solution.R, 2025/day01_input.txt, 2025/day01_sample.txt
# Opens solution + input in the editor
```

Then:

1. Go to [adventofcode.com](https://adventofcode.com), open today's puzzle.
2. Copy your personal puzzle input into `2025/day01_input.txt`.
3. Optionally copy the sample input into `2025/day01_sample.txt` for testing.
4. Write your solution in `day01_solution.R` — the template is pre-filled with `read_lines()` and part stubs.

The base template lives at `2025/template.R`. The `aoc()` function generates a lighter version of it automatically.

---

## Getting Your Input

### Manual

Copy the raw text from the puzzle input page and paste it directly into `dayNN_input.txt`.

### Session Cookie (auto-download)

AoC serves personalised inputs behind login. If you store your session cookie in `session.txt`, scripts can download inputs automatically.

**To get your session cookie:**

1. Log in to [adventofcode.com](https://adventofcode.com) in Chrome or Firefox.
2. Open DevTools (`F12`) → **Application** tab → **Cookies** → `https://adventofcode.com`.
3. Find the cookie named `session` and copy its value.
4. Paste just the value (no `session=` prefix) into `session.txt` at the repo root.

```r
# Example download helper (add to create_files.R or a script):
aoc_download <- function(year, day) {
  session <- readLines("session.txt", warn = FALSE)
  url <- sprintf("https://adventofcode.com/%d/day/%d/input", year, day)
  httr::GET(url, httr::set_cookies(session = session)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    writeLines(sprintf("%d/day%02d_input.txt", year, day))
}
```

> **Note:** Input files (`*_input.txt`, `*_sample.txt`) are gitignored — puzzle inputs are personal and should not be committed.

---

## Project Structure

```
advent_of_code/
├── scripts/
│   ├── create_files.R      # aoc() scaffold function — auto-sourced via .Rprofile
│   └── utils.R             # shared helper functions (source() as needed)
├── 2025/
│   ├── template.R           # base template for new days
│   ├── day01_solution.R     # solution file
│   ├── day01_input.txt      # your puzzle input (gitignored)
│   └── day01_sample.txt     # sample from problem statement (gitignored)
├── 2024/
│   └── day1_solution.R ... day5_solution.R
├── 2023/ ... 2015/          # prior years (varied naming: dayNN.R or dayNN_solution.R)
├── .Rprofile                # sources scripts/create_files.R on project open
├── session.txt              # AoC session cookie (gitignored)
└── advent_of_code.Rproj
```

**Naming note:** Older years use non-padded or bare `dayNN.R`. The `aoc()` function standardises on zero-padded `day01_solution.R` for 2025 onward (sorts correctly in file explorers).

---

## Utilities — scripts/utils.R

Source this file at the top of a solution when needed:

```r
source("scripts/utils.R")
```

### Grid parsing

**`parse_grid(input)`** — Preferred modern parser. Takes a character vector (one string per row) and returns a tibble with columns `x` (col), `y` (row), `value` (character). Row 1 is the top; x increases left-to-right.

```r
input <- read_lines("2025/day04_input.txt")
grid <- parse_grid(input)
# # A tibble: N x 3
# x  y  value
# 1  1  "A"
```

**`read_as_grid(input, col_sep = "")`** — Older grid parser with the same x/y/value output shape. Works on a single string with embedded newlines or a pre-split vector. Kept for compatibility with older solutions.

### Distance

**`manhattan(x1, y1, x2 = 0, y2 = 0)`** — Manhattan (L1) distance between two points. Defaults to distance from origin.

```r
manhattan(3, 4)        # 7
manhattan(1, 2, 4, 6)  # 7
```

### Coordinate neighbours

**`directions`** — A 9-row tribble with columns `dx` and `dy` covering all 8 neighbours plus self (`dx=0, dy=0`). Filter out self for 8-directional movement, or keep only cardinal directions as needed.

```r
# 4-directional neighbours of (x, y):
directions |>
  filter(!(dx == 0 & dy == 0), dx == 0 | dy == 0) |>
  mutate(nx = x + dx, ny = y + dy)
```

### Paragraphs / grouped input

**`read_paragraphs(path)`** — Reads a file and splits it into a list of character vectors, one per blank-line-separated block. Useful for puzzles with multiple sections (e.g., rules then data).

```r
groups <- read_paragraphs("2025/day05_input.txt")
rules  <- groups[[1]]
pages  <- groups[[2]]
```

### Number helpers

**`one_indexed_remainder(value, mod)`** — Modulo that returns `mod` instead of `0`. Useful when positions are 1-based and wrap around (e.g., circular buffers, elf gift assignments).

```r
one_indexed_remainder(10, 5)  # 5, not 0
one_indexed_remainder(7, 5)   # 2
```

**`binary_vec_to_decimal(binary)`** — Converts an integer vector of 0s and 1s (MSB first) to decimal.

```r
binary_vec_to_decimal(c(1, 0, 1, 1))  # 11
```

**`decimal_to_n_digit_binary(num, n)`** — Converts a decimal integer to a zero-padded binary string of width `n`.

```r
decimal_to_n_digit_binary(11, 8)  # "00001011"
```

### String helpers

**`sort_chars(word)`** — Sorts the characters within a string alphabetically. Useful for anagram detection.

```r
sort_chars("bca")  # "abc"
```

**`count_vals(x)`** — Returns a named integer vector of element frequencies. Fast alternative to `table()` when you need a named vector.

```r
count_vals(c("a", "b", "a"))  # a=2, b=1
```

### Regex capture groups

**`parse.group(regex, input)`** — Extracts named regex capture groups from a character vector. Returns a character matrix with capture group names as column names.

```r
result <- parse.group("(?P<x>\\d+),(?P<y>\\d+)", c("3,4", "10,20"))
result[, "x"]  # "3"  "10"
```

---

## Recommended Packages

| Package | Purpose | Install |
|---------|---------|---------|
| `tidyverse` | Core — already used everywhere | `install.packages("tidyverse")` |
| `adventdrob` | AoC input reading helpers (David Robinson) | `devtools::install_github("dgrtwo/adventdrob")` |
| `igraph` | Graph problems: shortest paths, connected components | `install.packages("igraph")` |
| `Rcpp` | Performance-critical inner loops (e.g., cellular automata, intcode VMs) | `install.packages("Rcpp")` |
| `R.utils` | Misc utilities including `intToBin()` | `install.packages("R.utils")` |
| `aochelpers` | Another R AoC helper package | `devtools::install_github("Selbosh/aochelpers")` |

The `adventdrob` package is particularly useful — its `adv_input()` function handles input reading and splitting in one call.

---

## Tips and Patterns

### Parsing

```r
# Numbers from a line
nums <- str_extract_all(line, "-?\\d+")[[1]] |> as.integer()

# Fixed-width delimited rows
read_delim("input.txt", delim = " ", col_names = FALSE)

# Named capture groups (alternative to parse.group)
str_match(lines, "(?P<a>\\w+) -> (?P<b>\\w+)")
```

### Grid traversal — BFS

```r
bfs <- function(grid, start) {
  visited <- tibble(x = start[1], y = start[2], dist = 0)
  queue   <- visited

  while (nrow(queue) > 0) {
    current <- queue[1, ]
    queue   <- queue[-1, ]

    neighbours <- directions |>
      filter(!(dx == 0 & dy == 0), dx == 0 | dy == 0) |>
      mutate(x = current$x + dx, y = current$y + dy, dist = current$dist + 1) |>
      semi_join(grid, by = c("x", "y")) |>
      anti_join(visited, by = c("x", "y")) |>
      select(x, y, dist)

    visited <- bind_rows(visited, neighbours)
    queue   <- bind_rows(queue, neighbours)
  }
  visited
}
```

### Step-by-step simulation with `accumulate()`

```r
# Simulate N steps, keeping all intermediate states
library(purrr)
states <- accumulate(1:N, ~ step(.x), .init = initial_state)
```

### Coordinate spaces with `expand_grid()`

```r
# All cells in a 100x100 grid
grid <- expand_grid(x = 1:100, y = 1:100)
```

### When to reach for Rcpp

Pure R loops over large state spaces (>10^5 iterations with non-vectorisable logic) are often too slow for AoC part 2. Common candidates:

- Intcode / VM simulation (2019)
- Cellular automata with many steps
- Pathfinding with large graphs where `igraph` isn't a clean fit

A minimal Rcpp inline function typically gives 10–100x speedup over an equivalent R loop.

---

## Inspiration and References

- **dgrtwo/adventdrob** — R package for reading AoC inputs
  https://github.com/dgrtwo/adventdrob

- **emilhvitfeldt/rstats-adventofcode** — curated collection of R AoC solutions
  https://emilhvitfeldt.github.io/rstats-adventofcode/

- **autoreleasefool/advent-of-code** — multi-language, multi-year solutions
  https://github.com/autoreleasefool/advent-of-code

- **cettt/Advent_of_Code2022** — R solutions, 2022 (source for days 9–12 in this repo)
  https://github.com/cettt/Advent_of_Code2022

- **plannapus/Advent_of_Code** — R solutions across multiple years (source for day 13, 2022)
  https://github.com/plannapus/Advent_of_Code

- **Selbosh/aochelpers** — another R AoC helper package
  https://github.com/Selbosh/aochelpers

- **r advent-of-code community** on GitHub
  https://github.com/topics/advent-of-code-r

---

## Progress

| Year | Days | Stars |
|------|------|-------|
| 2025 | 6 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2024 | 5 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2023 | 12 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2022 | 13 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2021 | 25 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2020 | 25 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2019 | 10 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2018 | 2 | ⭐⭐⭐⭐ |
| 2017 | 5 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |
| 2016 | 3 | ⭐⭐⭐⭐⭐⭐ |
| 2015 | 25 | ⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐⭐ |

Stars assume 2 per completed day. Update as you go.
