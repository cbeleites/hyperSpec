
#' Sequence generation along spectra or wavelengths
#'
#' This function generates sequences along the spectra (rows) or wavelengths of
#' `hyperSpec` objects.
#'
#' Note that [wl2i()] generates sequences of indices along the wavelength axis.
#'
#' [seq()] had to be implemented as S3 method as the generic has only \dots{}
#' arguments (on which no dispatch with differing types is possible).
#'
#' [base::seq_along()] is not generic, but returns a sequence of the `length`
#' of the object. As `hyperSpec` provides a Method [length()], it can be used.
#' The result is a sequence of indices for the spectra.
#'
#' @aliases seq seq,hyperSpec-method
#' @param x the `hyperSpec` object
#' @param from,to arguments handed to [base::seq.int()]
#' @param ... arguments for [base::seq()], namely `by`, `length.out`
#' @param index should a vector with indices be returned rather than a hyperSpec object?
#' @return a numeric or hyperSpec object, depending on `index`.
#' @author C. Beleites
#' @seealso [wl2i()] to construct sequences of wavelength indices.
#'
#' [base::seq()]
#' @rdname seq
#' @method seq hyperSpec
#'
#' @export
#'
#' @keywords manip
#' @concept manipulation
#'
#' @examples
#'
#' seq(flu, index = TRUE)
#' seq_along(flu)
#' seq(flu, length.out = 3, index = TRUE) # return value is numeric, not integer!
#' seq(flu, by = 2, index = TRUE) # return value is numeric, not integer!
#'
#' plot(flu, col = "darkgray")
#' plot(seq(flu, by = 2), add = TRUE, col = "red")
#' plot(seq(flu, length.out = 2), add = TRUE, col = "blue")
#'
#' ### needs to be an S3 function as S4 ... dispatch has to have the same signature for all parameters
seq.hyperSpec <- function(x, from = 1, to = nrow(x), ..., index = FALSE) {
  validObject(x)

  s <- seq(from = from, to = to, ...)

  if (index) {
    s
  } else {
    .extract(x, i = s)
  }
}

## internal abbreviation function

row.seq <- function(x, from = 1, to = nrow(x@data), ...) {
  if (nrow(x@data) == 0) {
    integer(0)
  } else {
    seq(from = from, to = to, ...)
  }
}

col.seq <- function(x, from = 1, to = ncol(x@data), ...) {
  if (ncol(x@data) == 0) {
    integer(0)
  } else {
    seq(from = from, to = to, ...)
  }
}

wl.seq <- function(x, from = 1, to = ncol(x@data$spc), ...) {
  if (ncol(x@data$spc) == 0) {
    integer(0)
  } else {
    seq(from = from, to = to, ...)
  }
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(seq.hyperSpec) <- function() {
  context("seq.hyperSpec")

  # Perform tests
  test_that("seq.hyperSpec() works", {
    sp <- generate_hy_spectra()

    expect_equal(seq(sp, index = TRUE), 1:nrow(sp))

    expect_is(seq(sp), "hyperSpec")
    expect_is(seq_along(sp), "integer")

    expect_equal(seq_along(sp), seq(sp, index = TRUE))

    x <- seq(sp, length.out = 3, index = TRUE) # return value is numeric, not integer!
    expect_is(x, "numeric")
    expect_equal(x, c(1.0, 10.5, 20.0))

    y <- seq(sp, by = 5, index = TRUE) # return value is numeric, not integer!
    expect_is(y, "numeric")
    expect_equal(y, c(1, 6, 11, 16))
  })

  test_that("row.seq() works", {
    sp <- generate_hy_spectra()

    expect_equal(max(row.seq(sp)), nrow(sp))
    expect_equal(row.seq(sp), seq(sp, index = TRUE))

    x <- row.seq(sp[0, ])
    expect_length(x, 0)
    expect_is(x, "integer")
  })


  test_that("col.seq() works", {
    sp <- generate_hy_spectra()

    expect_equal(max(col.seq(sp)), ncol(sp))

    y <- col.seq(sp[, "spc"])
    expect_length(y, 1)
    expect_is(y, "integer")
  })


  test_that("wl.seq() works", {
    sp <- generate_hy_spectra()

    expect_equal(max(wl.seq(sp)), nwl(sp))

    w <- wl.seq(sp[, , 0, wl.index = TRUE])
    expect_length(w, 0)
    expect_is(w, "integer")
  })
}
