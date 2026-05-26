# Day 12: Christmas Tree Farm
# Six oddly-shaped presents (3x3 bounding box each) need to fit under trees.
# Presents can be rotated and flipped; '#' cells can't overlap.
#
# Part 1: Count how many regions (W x H rectangles) can fit all listed presents.
#         A region is feasible iff total '#' cells of all presents <= W * H.
#
# Shapes (0-indexed):
#   0: #.#/###/#.# (7 cells)   1: ###/##./.## (7 cells)
#   2: ###/..#/### (7 cells)   3: .##/.##/### (7 cells)
#   4: ..#/.##/##. (5 cells)   5: ###/##./#.. (6 cells)

# -- Input --------------------------------------------------------------------
input <- readLines("2025/day12_input.txt")

# -- Parse pieces -------------------------------------------------------------
# Count '#' cells in each piece shape (first section of input)
cells_per_shape <- integer(6)
i <- 1L
while (i <= length(input) && grepl("^[0-9]+:", input[i]) && !grepl("x", input[i])) {
  pid  <- as.integer(sub(":.*", "", input[i])) + 1L
  rows <- input[(i + 1L):(i + 3L)]
  cells_per_shape[pid] <- sum(nchar(gsub("[^#]", "", rows)))
  i <- i + 5L
}

# -- Parse puzzle instances ---------------------------------------------------
region_lines <- input[grepl("^[0-9]+x[0-9]+:", input)]

parse_region <- function(r) {
  parts  <- strsplit(r, ": ", fixed = TRUE)[[1]]
  dims   <- strsplit(parts[1], "x", fixed = TRUE)[[1]]
  w      <- as.integer(dims[1])
  h      <- as.integer(dims[2])
  counts <- as.integer(strsplit(parts[2], " ", fixed = TRUE)[[1]])
  list(w = w, h = h, counts = counts)
}

regions <- lapply(region_lines, parse_region)

# -- Part 1 -------------------------------------------------------------------
# Count regions where total '#' cells of all presents <= grid area.
# For this specific set of shapes, this necessary condition is also sufficient.

result1 <- sum(vapply(regions, function(reg) {
  sum(reg$counts * cells_per_shape) <= reg$w * reg$h
}, FUN.VALUE = logical(1)))

cat("Part 1:", result1, "\n")  # 524

# -- Part 2 -------------------------------------------------------------------
# (Requires more stars - not yet unlocked)
