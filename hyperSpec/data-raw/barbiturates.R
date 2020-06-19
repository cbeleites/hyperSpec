# ============================================================================
# This code prepares `barbiturates` dataset
# ============================================================================

# Required packages ----------------------------------------------------------
library(hyperSpec)
library(usethis)

# Read the raw data ----------------------------------------------------------
file <- system.file("extdata/BARBITUATES.SPC", package = "hyperSpec")
barbiturates0 <- read.spc(file)

# Pre-process ----------------------------------------------------------------
barbiturates <-
  lapply(barbiturates0, function(x) {
    x$filename <- basename(x$filename)
    x
  })

# Save the prepared dataset as package data ----------------------------------
usethis::use_data(barbiturates, overwrite = TRUE)
