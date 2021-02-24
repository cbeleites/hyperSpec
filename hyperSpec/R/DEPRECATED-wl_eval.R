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
#' @param normalize.wl function to transorm the wavelengths before evaluating
#' the polynomial (or  other function). Use [hyperSpec::normalize01()] to map
#' the wavelength range to the interval \[0, 1\].
#' @return `hyperSpec` object containing one spectrum for each expression.
#'
#' @export
#'
#' @seealso
#'
#' - [hyperSpec::vanderMonde()] for  polynomials,
#' - [hyperSpec::normalize01()] to normalize the wavenumbers before evaluating
#' the function.
#'
#' @author C. Beleites, V. Gegzna
#'
#' @examples
#' plot(wl.eval(laser, exp = function(x) exp(-x)))
#'
#' plot(wl.eval(1000:4000, y = function(x) 1/log(x)))
#'
#' plot(wl.eval(300:550, y2 = function(x) x*2, y3 = function(x) x*3))
#'
wl.eval <- function(x, ..., normalize.wl = I) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("wl_eval")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  UseMethod("wl.eval")
}

#' @rdname DEPRECATED-wl.eval
#' @export
wl.eval.hyperSpec <- function(x, ..., normalize.wl = I) {
  chk.hy(x)
  validObject(x)

  fun <- list(...)

  wl <- normalize.wl(x@wavelength)

  x <- decomposition(x, t(sapply(fun, function(f) f(wl))), scores = FALSE)
  x$.f <- if (is.null(names(fun))) {
    rep(NA, length(fun))
  } else {
    names(fun)
  }
  x
}


#' @rdname DEPRECATED-wl.eval
#' @export
wl.eval.numeric <- function(x, ..., normalize.wl = I) {
  if (!is.vector(x)) {
    class_txt <- paste(class(x), collapse = ", ")
    stop("`x` must be a vector. Now it is ", class_txt, ".")
  }
  x <- new("hyperSpec", spc = seq_along(x), wavelength = x)
  wl.eval(x, ..., normalize.wl = normalize.wl)
}


# Unit tests -----------------------------------------------------------------


hySpc.testthat::test(wl.eval.hyperSpec) <- function() {
  context("wl.eval")

  test_that("error on function not returning same length as input", {
    expect_error(wl.eval(flu, function(x) 1))
  })

  test_that("wl.eval(<hyperSpec>) against manual evaluation", {
    expect_equivalent(
      wl.eval(flu, function(x) rep(5, length(x)), normalize.wl = normalize01)[[]],
      matrix(rep(5, nwl(flu)), nrow = 1)
    )

    expect_equivalent(
      wl.eval(flu, function(x) x),
      vanderMonde(flu, 1)[2]
    )

    expect_equivalent(
      wl.eval(flu, function(x) exp(-x))[[]],
      matrix(exp(-flu@wavelength), nrow = 1)
    )
  })

  test_that("normalization", {
    expect_equivalent(
      wl.eval(flu, function(x) rep(5, length(x)), normalize.wl = normalize01)[[]],
      matrix(rep(5, nwl(flu)), nrow = 1)
    )

    expect_equivalent(
      wl.eval(flu, function(x) x, normalize.wl = normalize01)[[]],
      matrix(seq(0, 1, length.out = nwl(flu)), nrow = 1)
    )

    expect_equivalent(
      wl.eval(flu, function(x) exp(x), normalize.wl = normalize01)[[]],
      matrix(exp(seq(0, 1, length.out = nwl(flu))), nrow = 1)
    )
  })


  test_that("multiple functions", {
    expect_equivalent(
      wl.eval(flu, function(x) rep(1, length(x)), function(x) x),
      vanderMonde(flu, 1)
    )
  })

  test_that("function names", {
    tmp <- wl.eval(flu, f = function(x) x, g = function(x) exp(-x))

    expect_equal(tmp$.f, c("f", "g"))
  })

  test_that("wl.eval(<numeric>) works", {

    expect_equal(
      as.vector(wl.eval(1:10, f = function(x) x)$spc),
      1:10
    )

    expect_equal(
      as.vector(wl.eval(1:10, f = function(x) x**2)$spc),
      (1:10)**2
    )

    expect_equal(
      wl.eval(wl(flu), f = function(x) x)$.f,
      wl.eval(   flu,  f = function(x) x)$.f
    )

    expect_warning(
      tmp <- wl.eval(300:500, f = function(x) x, g = function(x) exp(-x)),
      "deprecated"
    )
    expect_equal(tmp$.f, c("f", "g"))
  })

  test_that("wl.eval fails with matrix input", {
    expect_error(wl.eval(matrix(1:10), f = function(x) x))
    expect_error(wl.eval(matrix(), f = function(x) x))
  })
}