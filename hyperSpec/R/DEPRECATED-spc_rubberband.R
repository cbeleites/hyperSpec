#' @name DEPRECATED-spc-rubberband
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Rubberband baseline correction
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
#' Baseline with support points determined from a convex hull of the spectrum.
#'
#' Use `debuglevel >= 1` to obtain debug plots, either directly via function
#' argument or by setting hyperSpec's `debuglevel` option.
#' @param ... handed to [hyperSpec::spc_rubberband()]
#
# @param upper logical indicating whether the lower or upper part of the
#        hull should be used
# @param noise noise level to be taken into account
# @param spline logical indicating whether the baseline should be an
#        interpolating spline through the support points or piecewise linear.
#'
#' @return `hyperSpec` object containing the baselines
#'
#' @author Claudia Beleites
#' @seealso  [hyperSpec::spc_rubberband()]
#'
#' `vignette ("baseline")`
#'
#' [hyperSpec::hy.setOptions()]
#'
#' @export
#' @include spc_rubberband.R
#'
#'
spc.rubberband <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("spc_rubberband")

  spc_rubberband(...)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc.rubberband) <- function() {
  context("spc.rubberband")

  test_that(
    "deprecated",
    expect_warning(
      spc.rubberband(paracetamol),
      "Function 'spc.rubberband' is deprecated."
    )
  )
}
