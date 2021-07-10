#' Empty `hyperSpec` object
#'
#' Empty produces an `hyperSpec` object with the same columns and wavelengths
#' as `x`. The new object will either contain no rows at all (default), or the
#' given number of rows with all data initialized to `spc` and `extra`,
#' respectively.
#'
#' @aliases empty
#' @author C. Beleites
#' @keywords manip
#' @examples
#' empty(faux_cell, nrow = 2, spc = 0)
#' @param x hyperSpec object
#' @param nrow number of rows the new object should have
#' @param spc value to initialize the new spectra matrix with
#' @param extra value to initialize the new extra data with
#'
#' @export
#'
#' @concept manipulation
#'
empty <- function(x, nrow = 0, spc = NA, extra = NA) {
  if (nrow(x@data) == 0 && nrow > 0) {
    stop("Empty is not implemented for empty (0 row) objects")
  }

  x@data <- x@data[rep(1L, nrow), ]

  if (nrow > 0) {
    x@data$spc[TRUE] <- spc
    x@data[, !grepl("^spc$", colnames(x@data))] <- extra
  }

  x
}

# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(empty) <- function() {
  context("empty")

  # Perform tests
  test_that("empty() works", {
    expect_equal(nrow(empty(flu)), 0)
    expect_silent(empty(faux_cell, nrow = 2, spc = 0))
    expect_error(empty(new("hyperSpec")))
  })
  # FIXME: better tests are needed.
}
