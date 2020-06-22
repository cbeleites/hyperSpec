# ============================================================================
# This code prepares `laser` dataset
# ============================================================================

# Required packages ----------------------------------------------------------
library(hyperSpec)
library(usethis)

# Read the raw data ----------------------------------------------------------
file <- system.file("extdata/laser.txt.gz", package = "hyperSpec")
laser0 <- read.txt.Renishaw(file, data = "ts") # Renishaw file

# Pre-process ----------------------------------------------------------------
laser0 <- laser0[, , -75 ~ 0]
wl(laser0) <- wl(laser0) + 50
wl(laser0) <- list(
  wl = 1e7 / (1 / 405e-7 - wl(laser0)),
  label = expression(lambda / nm)
)
# laser0$filename <- NULL
laser0$filename <- basename(laser0$filename)

laser <- laser0

# Save the prepared dataset as package data ----------------------------------
usethis::use_data(laser, overwrite = TRUE)
