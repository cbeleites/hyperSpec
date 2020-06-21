.droplevels <- function(x, ...) {
  x@data <- droplevels(x@data, ...)

  x
}

#' droplevels for hyperSpec object
#'
#'  calls [base::droplevels()] on the data.frame in `spc@data`.
#'
#' @param x hyperSpec object
#' @param ... handed to [base::droplevels.data.frame()]
#'
#' @return hyperSpec object with unused levels of all factors in `@data` dropped.
#' @seealso [base::droplevels()]
#' @md
#' @export
#'
#' @examples
#'
#' fauxCell[1:3]$region
#' droplevels(fauxCell [1:3])$region
setMethod("droplevels", signature = "hyperSpec", definition = .droplevels)

.test(.droplevels) <- function() {
  context("droplevels")

  test_that("no change on object without levels to drop", {
    expect_equal(droplevels(fauxCell), fauxCell)
  })

  test_that("dropping levels", {
    tmp <- droplevels(fauxCell [1:3])
    expect_equal(tmp@data, droplevels(fauxCell@data [1:3, ]))

    expect_equal(
      tmp     [, c("x", "y", "spc")],
      fauxCell [1:3, c("x", "y", "spc")]
    )

    expect_equal(tmp$region, factor(rep("matrix", 3)))
  })

  test_that("no change if factor is `except`ed", {
    expect_equal(droplevels(fauxCell, except = 4), fauxCell)
  })
}
