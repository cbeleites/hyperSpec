# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' List installed r-hyperSpec family packages
#'
#' Convenience function that lists all installed
#' [**`r-hyperspec`**](https://r-hyperspec.github.io/) family packages.
#' These include **hyperSpec** and all packages that share common
#' **`hySpc.`_something_** package name pattern.
#'
#' @return Character vector with package names.
#' @export
#'
#' @concept utils
#'
#' @seealso [hy_load_hySpc_packages()]
#' @examples
#' hy_list_installed_hySpc_packages()

hy_list_installed_hySpc_packages <- function() {
  installed_pkgs <- row.names(installed.packages())
  c("hyperSpec", grep("^hySpc[.]", installed_pkgs, value = TRUE))
}

# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(hy_list_installed_hySpc_packages) <- function() {
  context("hy_list_installed_hySpc_packages")

  test_that("hy_list_installed_hySpc_packages() works", {

    # First check
    expect_silent(pkgs <- hy_list_installed_hySpc_packages())
    expect_is(pkgs, "character")
    expect_true(all(c("hyperSpec", "hySpc.testthat") %in% pkgs))
  })
}

