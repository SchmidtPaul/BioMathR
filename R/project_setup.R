#' @title Initiate an R-project
#'
#' @description This function initiates a new R-project with the typical folder structure
#' and a \code{00 setup.R} file.
#'
#' @details
#' The root directory "10 RCode" is created in the working directory, and its
#' sub-directories are "code", "data", "src", and "out". The function also
#' creates a setup file "00 setup.R" in the "code" sub-directory. This file
#' contains calls to load certain libraries and defines a few utility
#' functions and a color palette. If the 'here' package is not installed, the
#' function stops and asks the user to install it.
#'
#' @return Prints a success message if the directories and setup file are created successfully.
#'
#' @export
project_setup <- function() {

  # Check if the 'here' package is installed
  assertthat::assert_that(requireNamespace("here", quietly = TRUE),
                          msg = "To use project_setup(), package 'here' must be installed.")

  # Define the root and sub-directories
  root_dir <- here::here("10 RCode")
  sub_dirs <- c("code", "data", "src", "out")

  # Create the root directory
  if (!dir.exists(root_dir)) {
    dir.create(root_dir)
  }

  # Create the sub-directories
  for (dir in sub_dirs) {
    dir_path <- file.path(root_dir, dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    }
  }

  # Create setup file
  setup_file <- file.path(root_dir, "code", "00 setup.R")
  setup_content <- 'library(BioMathR) # remotes::install_github("SchmidtPaul/BioMathR")
library(conflicted)
library(here)
library(tidyverse)

# conflicts
conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicts_prefer(dplyr::select, .quiet = TRUE)

# here wrapper functions
here_data <- function(...) {here("10 RCode", "data", ...)}
here_code <- function(...) {here("10 RCode", "code", ...)}
here_out  <- function(...) {here("10 RCode", "out", ...)}

# colors
BMcols <- BioMathR::palette_getset("BioMath")'
  writeLines(setup_content, setup_file)

  # Create import file
  import_file <- file.path(root_dir, "code", "01 import.R")
  import_content <- 'source(here::here("10 RCode", "code", "00 setup.R"), encoding = "UTF-8")'
  writeLines(import_content, import_file)

  # send message
  message("Directories, '00 setup.R' and '01 import.R' created successfully!")
}
