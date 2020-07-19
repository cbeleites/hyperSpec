# ============================================================================
# This code prepares `barbiturates` dataset
# ============================================================================

# Required packages ----------------------------------------------------------
library(hyperSpec)
library(usethis)

# Read the raw data ----------------------------------------------------------
file <- system.file("extdata/BARBITUATES.SPC", package = "hyperSpec")
barbiturates <- read.spc(file)
barbiturates <- barbiturates[1:5]

# Save the prepared dataset as package data ----------------------------------
usethis::use_data(barbiturates, overwrite = TRUE)
# use_data will work only once the package root directory is also the root
# directory of the project.
# Until then, with working directory project root, use:
# save(barbiturates, file="hyperSpec/data/barbiturates.rda", compress = TRUE)
