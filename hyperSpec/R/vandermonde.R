# Set generic ----------------------------------------------------------------

#' Function evaluation on `hyperSpec` objects
#'
#' Function `vanderMonde()` generates Vandermonde matrices, the `hyperSpec`
#' method generates a `hyperSpec` object containing the Vandermonde matrix
#' of the wavelengths of a `hyperSpec` object.
#'
#' It is often numerically preferable to map `wl(x)` to \[0, 1\], see the
#' example.
#'
#' @name vanderMonde
#' @rdname vanderMonde
#' @concept data generation
#'
#' @param x object to evaluate the polynomial on
#' @param order of the polynomial
#'
#' @return Vandermonde matrix
#'
#' @author C. Beleites
#'
#' @export

vanderMonde <- function(x, order, ...) {
  if (nargs() > 2) {
    stop("Unknown arguments: ", names(c(...)))
  }

  outer(x, 0:order, `^`)
}

#' @noRd
setGeneric("vanderMonde")


# Function -------------------------------------------------------------------

.vanderMonde <- function(x, order, ..., normalize.wl = normalize01) {
  validObject(x)

  wl <- normalize.wl(x@wavelength)

  x <- decomposition(x, t(vanderMonde(wl, order)),
    scores = FALSE, ...
  )
  x$.vdm.order <- 0:order
  x
}

#' @rdname vanderMonde
#'
#' @param normalize.wl function to transform the wavelengths before evaluating
#'        the polynomial (or other function). [hyperSpec::normalize01()] maps
#'        the wavelength range to the interval \[0, 1\]. Use [base::I()] to
#'        turn off.
#' @param ... hyperSpec method: further arguments to [hyperSpec::decomposition()]
#'
#' @return `hyperSpec` method: hyperSpec object containing Vandermonde matrix
#'         as spectra and an additional column `$.vdm.order` giving the order
#'         of each spectrum (term).
#'
#' @seealso
#' [hyperSpec::wl_eval()] for calculating arbitrary functions of the wavelength
#'
#' [hyperSpec::normalize01()]
#'
#' @concept data generation
#'
#' @export
#'
#' @examples
#' plot(vanderMonde(flu, 2))
#' plot(vanderMonde(flu, 2, normalize.wl = I))
setMethod("vanderMonde", signature = signature(x = "hyperSpec"), .vanderMonde)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(vanderMonde) <- function() {
  context("vanderMonde")

  test_that("vector against manual calculation", {
    expect_equal(
      vanderMonde(c(1:3, 5), 2),
      matrix(c(1, 1, 1, 1, 1, 2, 3, 5, 1, 4, 9, 25), nrow = 4)
    )
  })

  test_that("default method doesn't provide normalization", {
    expect_error(vanderMonde(1, 0, normalize.wl = normalize01))
  })

  test_that("hyperSpec objects", {
    expect_true(chk.hy(vanderMonde(flu, 0)))
    expect_true(validObject(vanderMonde(flu, 0)))

    tmp <- vanderMonde(paracetamol, 3, normalize.wl = I)
    dimnames(tmp$spc) <- NULL
    expect_equal(tmp[[]], t(vanderMonde(wl(paracetamol), 3)))

    tmp <- vanderMonde(paracetamol, 3, normalize.wl = normalize01)
    dimnames(tmp$spc) <- NULL
    expect_equal(tmp[[]], t(vanderMonde(normalize01(wl(paracetamol)), 3))
    )
  })
}
