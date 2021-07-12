#' @name DEPRECATED-baselines
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Polynomial baseline fitting
#'
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
#' These functions fit polynomial baselines.
#'
#' @details
#' Both functions fit polynomials to be used as baselines. If `apply.to`
#' is `NULL`, a `hyperSpec` object with the polynomial coefficients
#' is returned, otherwise the polynomials are evaluated on the spectral range
#' of `apply.to`.
#'
#' `spc.fit.poly()` calculates the least squares fit of order
#' `poly.order` to the *complete* spectra given in `fit.to`.
#' Thus `fit.to` needs to be cut appropriately.
#'
#' @param ... handed to [hyperSpec::spc_fit_poly_below()]
#' @return `hyperSpec` object containing the baselines in the spectra matrix,
#'         either as polynomial coefficients or as polynomials evaluted on
#'         the spectral range of `apply.to`
#' @author C. Beleites
#' @include spc_fit_poly.R
#'
#' @seealso `vignette("baseline", package = "hyperSpec")`
#'
#' @export
#'
#' @keywords manip datagen
spc.fit.poly <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("spc_fit_poly")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  spc_fit_poly(...)
}

hySpc.testthat::test(spc.fit.poly) <- function() {
  context("spc.fit.poly")

  test_that(
    "deprecated",
    expect_warning(
      spc.fit.poly(flu),
      "Function 'spc.fit.poly' is deprecated."
    )
  )
}

#' @rdname DEPRECATED-baselines
#'
#' @details
#' `spc.fit.poly.below()` tries to fit the baseline on appropriate spectral
#' ranges of the spectra in `fit.to`.  For details, see the
#' `vignette("baseline")`.
#'
#' @param ... handed to [hyperSpec::spc_fit_poly_below()]
#'
#' @export
spc.fit.poly.below <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("spc_fit_poly_below")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  spc_fit_poly_below(...)

}

hySpc.testthat::test(spc.fit.poly.below) <- function() {
  context("spc.fit.poly.below")

  test_that(
    "deprecated",
    expect_warning(
      spc.fit.poly.below(flu),
      "Function 'spc.fit.poly.below' is deprecated."
    )
  )
}
