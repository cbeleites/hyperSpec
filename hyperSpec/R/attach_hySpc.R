#' Load all installed packages of the `hySpc.*` package family.
#'
#' This function loads all installed packages of the **`hySpc.`_something_**
#' package family.
#'
#' @param dont_attach Packages that should *not* be attached.
#' @param ... Further parameters are handed to [base::library()].
#' @param quiet (`TRUE`|`NA`|`FALSE`) Controls, which messages should be
#'        printed:
#'
#' - `FALSE`: no messages will be printed to the console.
#' - `NA` (default): short message with the names of `hySpc.*` packages to
#'    attach will be printed, but package startup messages will be suppressed.
#' - `TRUE`: both short and package startup messages will be printed.
#'
#' @return Invisibly returns a list with the output of [base::library()] for
#'   each **`hySpc.*`** package that was attached.
#'
#' @export
#' @importFrom utils installed.packages
#' @concept hyperSpec-main
#' @examples
#' \dontrun{
#' attach_hySpc()
#'
#' attach_hySpc(quiet = TRUE)
#' }

attach_hySpc <- function(dont_attach = "hySpc.testthat", ..., quiet = NA) {
  installed_pkgs <- row.names(installed.packages())
  hySpc_packages <- c("hyperSpec", grep("^hySpc[.]", installed_pkgs, value = TRUE))

  hySpc_packages <- setdiff(hySpc_packages, unique(c(dont_attach, .packages())))

  if (is.na(quiet) || isFALSE(quiet)) {

    if (length(hySpc_packages) > 0) {
      message(
        "\n-----------------------------------------------------------\n",
        "The following `hySpc` family packages are being attached: \n\n",
        paste0("  ", hySpc_packages, collapse = "\n"), "\n",
        "\n-----------------------------------------------------------\n"
      )
    } else {
      message("\nAll installed `hySpc` family packages are already attached.\n")
    }

  }

  attach_pkgs <- function() {
    lapply(hySpc_packages, function(x) {
      do.call("library", list(package = x, ...))
    })
  }

  out <-
    if (is.na(quiet) || isTRUE(quiet)) {
      suppressPackageStartupMessages({attach_pkgs()})

    } else {
      attach_pkgs()
    }

  invisible(out)
}

