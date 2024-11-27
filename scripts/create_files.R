# create_files.R

aoc <- function(year, day) {
  # Create the year directory if it doesn't exist
  if (!dir.exists(year)) {
    dir.create(year)
  }
  
  # Define file paths
  r_code_file <- file.path(year, paste0("day", day, "_solution.R"))
  input_file <- file.path(year, paste0("day", day, "_input.txt"))
  
  # Create and write to the R code file
  r_code_content <- paste0(
    "library(tidyverse)\n\n",
    "# Read the input file\n",
    "input <- read_lines('", input_file, "')\n\n",
    "# Part 1\n",
    "# Your solution code for Part 1 here\n\n",
    "# Part 2\n",
    "# Your solution code for Part 2 here\n"
  )
  writeLines(r_code_content, r_code_file)
  file.edit(r_code_file)
  
  # Create the input file
  file.create(input_file)
  file.edit(input_file)
  
  cat("Files created and opened in RStudio:\n")
  cat("R code file:", r_code_file, "\n")
  cat("Input file:", input_file, "\n")
}