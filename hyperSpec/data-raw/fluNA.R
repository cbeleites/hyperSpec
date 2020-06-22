# ============================================================================
# This code prepares internal datasets
# ============================================================================

# Dataset `fluNA` ------------------------------------------------------------
fluNA <- hyperSpec::flu
fluNA[[2, ]] <- NA
fluNA[[, , 406]] <- NA


# Save the prepared datasets as internal package data ------------------------
usethis::use_data(fluNA, internal = TRUE)
