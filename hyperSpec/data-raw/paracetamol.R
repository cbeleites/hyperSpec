# ============================================================================
# This code prepares `paracetamol` dataset
# ============================================================================

# Required packages ----------------------------------------------------------
library(hyperSpec)
library(usethis)

# Read the raw data ----------------------------------------------------------
file <- system.file("extdata/paracetamol.txt", package = "hyperSpec")
paracetamol0 <- read.txt.Renishaw(file, "spc")

# Pre-process ----------------------------------------------------------------
paracetamol0$filename <- basename(paracetamol0$filename)

paracetamol <- paracetamol0

# Save the prepared dataset as package data ----------------------------------
usethis::use_data(paracetamol, overwrite = TRUE)
