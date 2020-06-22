.onAttach <- function(libname, pkgname) {
  unlockBinding(".options", asNamespace("hyperSpec"))

  desc <- utils::packageDescription("hyperSpec")
  first_url <- sub(",.*$", "", desc$URL) # To use the first URL only

  packageStartupMessage(
    "\n\n",
    "Package ", desc$Package, " (version ", desc$Version, ")\n\n",

    "To get started, try: \n",
    '   vignette("', desc$Package, '", package = "', desc$Package, '")', "\n",
    "   package?", desc$Package, "\n",
    '   browseVignettes(package = "', desc$Package, '")\n',
    '   vignette(package = "', desc$Package, '")', "\n",
    # '   browseURL("', first_url, '") # Online documentation \n',
    "\n",

    "If you use this package, please cite it appropriately.\n",
    "The correct reference is given by:\n",
    '   citation("', desc$Package, '")\n\n',

    "The project's website:\n   ", first_url, "\n\n",
    sep = ""
  )
}
