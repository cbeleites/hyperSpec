# ============================================================================
# This code prepares `flu` dataset
# ============================================================================

# Required packages ----------------------------------------------------------
library(hyperSpec)
library(usethis)

# Read the raw data ----------------------------------------------------------
source("vignettes/read.txt.PerkinElmer.R")

folder <- system.file("extdata/flu", package = "hyperSpec")
files <- Sys.glob(paste0(folder, "/flu?.txt"))
flu0 <- read.txt.PerkinElmer(files, skip = 54)

# Pre-process ----------------------------------------------------------------
flu0$c <- seq(from = 0.05, to = 0.30, by = 0.05)
flu0$filename <- basename(flu0$filename)
labels(flu0, "c") <- "c, mg/l"

flu <- flu0

# Save the prepared dataset as package data ----------------------------------
usethis::use_data(flu, overwrite = TRUE)
