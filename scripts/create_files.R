# create_files.R

# Default template used when 2025/template.R does not exist
.aoc_default_template <- function(year, day_pad) {
  paste0(
    'library(tidyverse)\n',
    '# Optional: library(adventdrob)  # devtools::install_github("dgrtwo/adventdrob")\n',
    '\n',
    '# -- Input --------------------------------------------------------------------\n',
    'input <- read_lines("', year, '/day', day_pad, '_input.txt")\n',
    '\n',
    '# sample <- read_lines("', year, '/day', day_pad, '_sample.txt")\n',
    '\n',
    '# -- Part 1 -------------------------------------------------------------------\n',
    '\n',
    '\n',
    '# -- Part 2 -------------------------------------------------------------------\n',
    '\n'
  )
}

aoc <- function(year, day) {
  day_pad <- formatC(as.integer(day), width = 2, flag = "0")

  # Create year directory if needed
  if (!dir.exists(as.character(year))) {
    dir.create(as.character(year), recursive = TRUE)
    message("Created directory: ", year)
  }

  # File paths
  r_file     <- file.path(year, paste0("day", day_pad, "_solution.R"))
  input_file <- file.path(year, paste0("day", day_pad, "_input.txt"))
  sample_file <- file.path(year, paste0("day", day_pad, "_sample.txt"))

  # Build solution content from template or fallback
  template_path <- file.path("2025", "template.R")
  if (file.exists(template_path)) {
    content <- readLines(template_path, warn = FALSE) |>
      paste(collapse = "\n") |>
      (\(x) paste0(x, "\n"))()
    # Replace NN placeholder and 2025 path prefix
    content <- gsub("dayNN", paste0("day", day_pad), content, fixed = TRUE)
    content <- gsub("2025/", paste0(year, "/"), content, fixed = TRUE)
  } else {
    content <- .aoc_default_template(year, day_pad)
  }

  # Write files (don't overwrite existing solution)
  if (!file.exists(r_file)) {
    writeLines(content, r_file)
  } else {
    message("Solution file already exists, skipping: ", r_file)
  }
  if (!file.exists(input_file))  file.create(input_file)
  if (!file.exists(sample_file)) file.create(sample_file)

  # Try to open in RStudio editor (silently fails outside RStudio)
  tryCatch(file.edit(r_file), error = function(e) invisible(NULL))
  tryCatch(file.edit(input_file), error = function(e) invisible(NULL))

  cat("Files ready for", year, "day", day_pad, ":\n")
  cat("  Solution:", r_file, "\n")
  cat("  Input:   ", input_file, "\n")
  cat("  Sample:  ", sample_file, "\n")

  invisible(list(solution = r_file, input = input_file, sample = sample_file))
}

aoc_setup <- function(year) {
  year <- as.character(year)

  # Create year directory
  if (!dir.exists(year)) {
    dir.create(year, recursive = TRUE)
    message("Created directory: ", year)
  } else {
    message("Directory already exists: ", year)
  }

  # Copy template into the year directory
  template_src <- file.path("2025", "template.R")
  template_dst <- file.path(year, "template.R")
  if (year != "2025" && file.exists(template_src) && !file.exists(template_dst)) {
    file.copy(template_src, template_dst)
    message("Copied template to: ", template_dst)
  }

  # Create .gitignore that excludes personal puzzle inputs
  gitignore_path <- file.path(year, ".gitignore")
  if (!file.exists(gitignore_path)) {
    writeLines(c("*_input.txt", "*_sample.txt", "*.RData", ".Rhistory"), gitignore_path)
    message("Created: ", gitignore_path)
  } else {
    message(".gitignore already exists: ", gitignore_path)
  }

  cat("Setup complete for year", year, "\n")
  invisible(year)
}
