#' @name DEPRECATED-wl.eval
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Evaluate function on wavelengths of `hyperSpec` object
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead.
#'
#' `_____________`
#'
#' This is useful for generating certain types of baseline "reference spectra".
#'
#' @param x either `hyperSpec` object or numeric vector.
#' @param ... expressions to be evaluated.
#' @param normalize.wl function to transform the wavelengths before evaluating
#' the polynomial (or  other function). Use [hyperSpec::normalize01()] to map
#' the wavelength range to the interval \[0, 1\].
#' @return `hyperSpec` object containing one spectrum for each expression.
#'
#' @export
#' @include wl_eval.R
#'
#' @seealso
#'
#' - [hyperSpec::vanderMonde()] for polynomials,
#' - [hyperSpec::normalize01()] to normalize the wavenumbers before evaluating
#' the function.
#'
#' @author C. Beleites, V. Gegzna
#'

wl.eval <- function(x, ..., normalize.wl = I) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("wl_eval")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  UseMethod("wl_eval")
}

#' @rdname DEPRECATED-wl.eval
#' @export
wl.eval.hyperSpec <- function(x, ..., normalize.wl = I) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("wl_eval.hyperSpec")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  wl_eval.hyperSpec(x, ..., normalize.wl = normalize.wl)
}


#' @rdname DEPRECATED-wl.eval
#' @export
wl.eval.numeric <- function(x, ..., normalize.wl = I) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("wl_eval.numeric")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  wl_eval.numeric(x, ..., normalize.wl = normalize.wl)

}


# Unit tests -----------------------------------------------------------------


hySpc.testthat::test(wl.eval.hyperSpec) <- function() {
  context("wl.eval")

  test_that("deprecated", {
    expect_warning(
      wl.eval.hyperSpec(flu, function(x) x),
      "Function 'wl.eval.hyperSpec' is deprecated."
    )
    expect_warning(
      wl.eval.numeric(1:5, function(x) x),
      "Function 'wl.eval.numeric' is deprecated."
    )
  })
}
