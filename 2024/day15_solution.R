library(tidyverse)

input <- read_lines("2024/day15_input.txt")

# ---- Parse input ----
blank <- which(input == "")
grid_lines <- input[seq(1, blank - 1)]
move_lines  <- input[seq(blank + 1, length(input))]
moves <- paste(move_lines, collapse = "") |> strsplit("") |> unlist()

# Convert grid to matrix
to_matrix <- function(lines) {
  do.call(rbind, strsplit(lines, ""))
}

# ---- Direction helpers ----
dir_delta <- list(
  "^" = c(-1,  0),
  "v" = c( 1,  0),
  "<" = c( 0, -1),
  ">" = c( 0,  1)
)

# ---- Part 1 ----
simulate_p1 <- function(grid, moves) {
  pos <- which(grid == "@", arr.ind = TRUE)[1, ]
  grid[pos[1], pos[2]] <- "."

  for (m in moves) {
    d <- dir_delta[[m]]
    nxt <- pos + d

    cell <- grid[nxt[1], nxt[2]]

    if (cell == "#") {
      next
    } else if (cell == ".") {
      pos <- nxt
    } else if (cell == "O") {
      # Find end of chain
      chain_end <- nxt
      while (grid[chain_end[1], chain_end[2]] == "O") {
        chain_end <- chain_end + d
      }
      if (grid[chain_end[1], chain_end[2]] == "#") {
        next  # blocked
      }
      # Shift: place O at chain_end, robot moves to nxt
      grid[chain_end[1], chain_end[2]] <- "O"
      grid[nxt[1], nxt[2]] <- "."
      pos <- nxt
    }
  }
  grid
}

g1    <- to_matrix(grid_lines)
final <- simulate_p1(g1, moves)

box_pos <- which(final == "O", arr.ind = TRUE)
result1 <- sum(100L * (box_pos[, 1] - 1L) + (box_pos[, 2] - 1L))
cat("Part 1:", result1, "\n")

# ---- Part 2 ----
expand_grid <- function(lines) {
  expanded <- gsub("#", "##", lines)
  expanded <- gsub("O", "[]", expanded)
  expanded <- gsub("\\.", "..", expanded)
  expanded <- gsub("@", "@.", expanded)
  to_matrix(expanded)
}

simulate_p2 <- function(grid, moves) {
  pos <- which(grid == "@", arr.ind = TRUE)[1, ]
  grid[pos[1], pos[2]] <- "."

  for (m in moves) {
    d <- dir_delta[[m]]
    nxt <- pos + d
    cell <- grid[nxt[1], nxt[2]]

    if (cell == "#") {
      next
    } else if (cell == ".") {
      pos <- nxt
    } else if (cell %in% c("[", "]")) {
      if (m %in% c("<", ">")) {
        # Horizontal push: find end of chain
        chain_end <- nxt
        while (grid[chain_end[1], chain_end[2]] %in% c("[", "]")) {
          chain_end <- chain_end + d
        }
        if (grid[chain_end[1], chain_end[2]] == "#") next
        # Shift everything one step in direction d
        cur <- chain_end
        while (!all(cur == nxt)) {
          prev <- cur - d
          grid[cur[1], cur[2]] <- grid[prev[1], prev[2]]
          cur <- prev
        }
        grid[nxt[1], nxt[2]] <- "."
        pos <- nxt
      } else {
        # Vertical push: BFS to collect all boxes affected
        # Each box occupies two cells; [ is left, ] is right
        # Identify initial box half
        left_of <- function(r, c) {
          if (grid[r, c] == "[") c(r, c) else c(r, c - 1)
        }

        # Collect set of box left-half positions that must move
        seed <- left_of(nxt[1], nxt[2])
        to_move <- list()
        frontier <- list(seed)
        blocked <- FALSE

        while (length(frontier) > 0 && !blocked) {
          new_frontier <- list()
          for (box in frontier) {
            key <- paste(box, collapse = ",")
            if (!is.null(to_move[[key]])) next
            to_move[[key]] <- box

            # Check next row for both halves
            for (dc in c(0, 1)) {
              nr <- box[1] + d[1]
              nc <- box[2] + dc
              nc_cell <- grid[nr, nc]
              if (nc_cell == "#") {
                blocked <- TRUE
                break
              } else if (nc_cell == "[") {
                new_frontier <- c(new_frontier, list(c(nr, nc)))
              } else if (nc_cell == "]") {
                new_frontier <- c(new_frontier, list(c(nr, nc - 1)))
              }
            }
            if (blocked) break
          }
          frontier <- new_frontier
        }

        if (blocked) next

        # Move all boxes in reverse order (farthest first)
        boxes <- to_move
        # Sort by row: if moving up (d=-1) process smallest row first,
        # if moving down (d=+1) process largest row first
        rows <- sapply(boxes, function(b) b[1])
        ord  <- if (d[1] < 0) order(rows) else order(rows, decreasing = TRUE)
        for (i in ord) {
          b  <- boxes[[i]]
          nb <- b + d
          grid[nb[1], nb[2]]     <- "["
          grid[nb[1], nb[2] + 1] <- "]"
          grid[b[1],  b[2]]      <- "."
          grid[b[1],  b[2] + 1]  <- "."
        }
        pos <- nxt
      }
    }
  }
  grid
}

g2     <- expand_grid(grid_lines)
final2 <- simulate_p2(g2, moves)

box2_pos <- which(final2 == "[", arr.ind = TRUE)
result2  <- sum(100L * (box2_pos[, 1] - 1L) + (box2_pos[, 2] - 1L))
cat("Part 2:", result2, "\n")
