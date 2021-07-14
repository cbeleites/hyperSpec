# Function -------------------------------------------------------------------

.droplevels <- function(x, ...) {
  x@data <- droplevels(x@data, ...)

  x
}

#' Droplevels for `hyperSpec` objects
#'
#' Calls [base::droplevels()] on the data.frame in `spc@data`.
#'
#' @param x hyperSpec object
#' @param ... handed to [base::droplevels.data.frame()]
#'
#' @return hyperSpec object with unused levels of all factors in `@data` dropped.
#' @seealso [base::droplevels()]
#'
#' @export
#'
#' @concept manipulation
#'
#' @examples
#' faux_cell[1:3]$region
#' droplevels(faux_cell[1:3])$region
setMethod("droplevels", signature = "hyperSpec", definition = .droplevels)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.droplevels) <- function() {
  context("droplevels")

  test_that("no change on object without levels to drop", {
    expect_equal(droplevels(faux_cell), faux_cell)
  })

  test_that("dropping levels", {
    tmp <- droplevels(faux_cell[1:3])
    expect_equal(tmp@data, droplevels(faux_cell@data[1:3, ]))

    expect_equal(
      tmp[, c("x", "y", "spc")],
      faux_cell[1:3, c("x", "y", "spc")]
    )

    expect_equal(tmp$region, factor(rep("matrix", 3)))
  })

  test_that("no change if factor is `except`ed", {
    expect_equal(droplevels(faux_cell, except = 4), faux_cell)
  })
}
