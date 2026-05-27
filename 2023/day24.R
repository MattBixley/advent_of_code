library(tidyverse)
source("scripts/utils.R")

input <- read_lines("2023/day24_input.txt")

hail <- tibble(line = input) |>
  mutate(vals = map(line, ~ as.numeric(str_extract_all(.x, "-?\\d+")[[1]]))) |>
  mutate(
    px = map_dbl(vals, 1), py = map_dbl(vals, 2), pz = map_dbl(vals, 3),
    vx = map_dbl(vals, 4), vy = map_dbl(vals, 5), vz = map_dbl(vals, 6)
  ) |>
  select(px, py, pz, vx, vy, vz)

# Part 1: count pairs whose XY paths intersect in future within test area
LO <- 200000000000000
HI <- 400000000000000

n <- nrow(hail)
count <- 0L
for (i in seq_len(n - 1)) {
  for (j in seq(i + 1, n)) {
    a <- hail[i, ]
    b <- hail[j, ]
    # Solve: t*a$vx - s*b$vx = b$px - a$px
    #        t*a$vy - s*b$vy = b$py - a$py
    # Matrix [a$vx, -b$vx; a$vy, -b$vy] * [t; s] = [dx; dy]
    det <- a$vx * (-b$vy) - a$vy * (-b$vx)
    if (abs(det) < 1e-9) next  # parallel paths
    dx <- b$px - a$px
    dy <- b$py - a$py
    t <- (dx * (-b$vy) - dy * (-b$vx)) / det
    s <- (a$vx * dy - a$vy * dx) / det
    if (t < 0 || s < 0) next  # intersection in the past
    xi <- a$px + t * a$vx
    yi <- a$py + t * a$vy
    if (xi >= LO && xi <= HI && yi >= LO && yi <= HI) {
      count <- count + 1L
    }
  }
}
result1 <- count
cat("Part 1:", result1, "\n")

# Part 2: find rock position (Px, Py, Pz) and velocity (Vx, Vy, Vz) such that
# for every hailstone i, at some time t_i the rock and stone occupy the same point.
#
# From (P - stone_i.p) x (V - stone_i.v) = 0  (collinear condition)
# Subtracting for two stones A and B eliminates the nonlinear P x V term:
#   P x (A.v - B.v) + (A.p - B.p) x V = A.p x A.v - B.p x B.v
#
# Letting dp = A.p - B.p, dv = A.v - B.v, column order [Px, Py, Pz, Vx, Vy, Vz]:
#   x:  0*Px + dvz*Py - dvy*Pz + 0*Vx - dpz*Vy + dpy*Vz = A.py*A.vz - A.pz*A.vy - (B.py*B.vz - B.pz*B.vy)
#   y: -dvz*Px + 0*Py + dvx*Pz + dpz*Vx + 0*Vy - dpx*Vz = A.pz*A.vx - A.px*A.vz - (B.pz*B.vx - B.px*B.vz)
#   z:  dvy*Px - dvx*Py + 0*Pz - dpy*Vx + dpx*Vy + 0*Vz = A.px*A.vy - A.py*A.vx - (B.px*B.vy - B.py*B.vx)
#
# Positions ~3e14 cause precision loss in RHS cross-products (~3e14 * 100 = 3e16).
# Scale positions by 1e13 before solving; multiply Px, Py, Pz back at the end.

K <- 1e13
hs <- hail
hs$px <- hs$px / K; hs$py <- hs$py / K; hs$pz <- hs$pz / K

build_equations <- function(A, B) {
  dpx <- A$px - B$px; dpy <- A$py - B$py; dpz <- A$pz - B$pz
  dvx <- A$vx - B$vx; dvy <- A$vy - B$vy; dvz <- A$vz - B$vz

  rhs_x <- A$py * A$vz - A$pz * A$vy - (B$py * B$vz - B$pz * B$vy)
  rhs_y <- A$pz * A$vx - A$px * A$vz - (B$pz * B$vx - B$px * B$vz)
  rhs_z <- A$px * A$vy - A$py * A$vx - (B$px * B$vy - B$py * B$vx)

  rbind(
    c(0,    dvz,  -dvy,  0,    -dpz,  dpy,  rhs_x),
    c(-dvz, 0,    dvx,   dpz,  0,    -dpx,  rhs_y),
    c(dvy,  -dvx, 0,    -dpy,  dpx,   0,    rhs_z)
  )
}

eqs <- rbind(
  build_equations(hs[1, ], hs[2, ]),
  build_equations(hs[1, ], hs[3, ])
)

A_mat <- eqs[, 1:6]
b_vec <- eqs[, 7]
sol   <- solve(A_mat, b_vec)

# sol[1:3] are Px, Py, Pz in scaled coordinates; sol[4:6] are Vx, Vy, Vz (unscaled)
Px <- sol[1] * K; Py <- sol[2] * K; Pz <- sol[3] * K
result2 <- format(round(Px + Py + Pz), scientific = FALSE)
cat("Part 2:", result2, "\n")
