
#' Load and attach `hyperSpec` and all installed *R hyperSpec* family (`hySpc.*`) packages
#'
#' This function loads and attaches **hyperSpec** and all installed
#' [*R hyperSpec*](https://r-hyperspec.github.io/) family packages, which
#' share common **`hySpc.`_something_** package name pattern.
#'
#' @param exclude Character vector with the names of the packages that should
#'        **not** be explicitly loaded. If they are already loaded and/or
#'        attached, they will *not* be unloaded or detached.
#' @param ... Further parameters to [base::library()].
#' @param quiet (`TRUE`|`NA`|`FALSE`) Controls verbosity of messages:
#'
#' - `FALSE`: no messages will be printed to the console.
#' - `NA` (default): short message with the names of `hySpc.*` packages to
#'    load and attach will be printed, but package startup messages will be
#'    suppressed.
#' - `TRUE`: both short and package startup messages will be printed.
#'
#' @return Invisibly returns a list with the output of [base::library()] for
#'   each *R hyperSpec* family package that was loaded and attached.
#'
#' @export
#' @importFrom utils installed.packages
#'
#' @concept utils
#'
#' @author V. Gegzna and C. Beleites
#'
#' @seealso [hy_list_installed_hySpc_packages()]
#'
#' @examples
#' \dontrun{
#' hyperSpec::hy_load_hySpc_packages()
#'
#' hyperSpec::hy_load_hySpc_packages(quiet = TRUE)
#' }

hy_load_hySpc_packages <- function(exclude = "hySpc.testthat", ..., quiet = NA) {

  hySpc_installed <- hy_list_installed_hySpc_packages()
  hySpc_to_attach <- setdiff(hySpc_installed, unique(c(exclude, .packages())))

  if (is.na(quiet) || isFALSE(quiet)) {

    if (length(hySpc_to_attach) > 0) {
      message(
        "\n------------------------------------\n",
        "Attaching the following r-hyperspec \n",
        "(`hySpc`) family packages: \n\n",
        paste0("  ", hySpc_to_attach, collapse = "\n"),
        "\n",
        "\n------------------------------------\n"
      )
    } else {
      message(
        "\n",
        "All installed r-hyperspec (`hySpc`) family packages are already attached.\n"
      )
    }
  }

  attach_pkgs <- function() {
    lapply(hySpc_to_attach, function(x) {
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

# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(hy_load_hySpc_packages) <- function() {
  context("hy_load_hySpc_packages")

  test_that("hy_load_hySpc_packages() works", {

    # Check with hyperSpec package only
    installed_pkgs <- row.names(installed.packages())
    exclude_pkgs   <- grep("^hySpc[.]", installed_pkgs, value = TRUE)

    # First check
    expect_silent(hyperSpec::hy_load_hySpc_packages(exclude_pkgs, quiet = TRUE))

    # quiet = NA
    suppressWarnings({detach("package:hyperSpec", force = TRUE)})
    expect_message(hyperSpec::hy_load_hySpc_packages(exclude_pkgs, quiet = NA), "hyperSpec")
    expect_message(hyperSpec::hy_load_hySpc_packages(exclude_pkgs, quiet = NA), "are already attached")

    # quiet = TRUE
    suppressWarnings({detach("package:hyperSpec", force = TRUE)})
    expect_silent(hyperSpec::hy_load_hySpc_packages(exclude_pkgs, quiet = TRUE))
    expect_silent(hyperSpec::hy_load_hySpc_packages(exclude_pkgs, quiet = TRUE))

    # quiet = FALSE
    suppressWarnings({detach("package:hyperSpec", force = TRUE)})
    expect_message(hyperSpec::hy_load_hySpc_packages(exclude_pkgs, quiet = FALSE), "To get started, try:")
    expect_message(hyperSpec::hy_load_hySpc_packages(exclude_pkgs, quiet = FALSE), "are already attached")
  })
}

