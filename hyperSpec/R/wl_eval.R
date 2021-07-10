#' Evaluate function on wavelengths of `hyperSpec` object
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
#'
#' @concept wavelengths
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
#' plot(wl_eval(laser, exp = function(x) exp(-x)))
#'
#' plot(wl_eval(1000:4000, y = function(x) 1 / log(x)))
#'
#' plot(wl_eval(300:550, y2 = function(x) x * 2, y3 = function(x) x * 3))
wl_eval <- function(x, ..., normalize.wl = I) {
  UseMethod("wl_eval")
}

#' @rdname wl_eval
#' @export
wl_eval.hyperSpec <- function(x, ..., normalize.wl = I) {
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


#' @rdname wl_eval
#' @export
wl_eval.numeric <- function(x, ..., normalize.wl = I) {
  if (!is.vector(x)) {
    class_txt <- paste(class(x), collapse = ", ")
    stop("`x` must be a vector. Now it is ", class_txt, ".")
  }
  x <- new("hyperSpec", spc = seq_along(x), wavelength = x)
  wl_eval(x, ..., normalize.wl = normalize.wl)
}


# Unit tests -----------------------------------------------------------------


hySpc.testthat::test(wl_eval.hyperSpec) <- function() {
  context("wl_eval")

  test_that("error on function not returning same length as input", {
    expect_error(wl_eval(flu, function(x) 1))
  })

  test_that("wl_eval(<hyperSpec>) against manual evaluation", {
    expect_equivalent(
      wl_eval(flu, function(x) rep(5, length(x)), normalize.wl = normalize01)[[]],
      matrix(rep(5, nwl(flu)), nrow = 1)
    )

    expect_equivalent(
      wl_eval(flu, function(x) x),
      vanderMonde(flu, 1)[2]
    )

    expect_equivalent(
      wl_eval(flu, function(x) exp(-x))[[]],
      matrix(exp(-flu@wavelength), nrow = 1)
    )
  })

  test_that("normalization", {
    expect_equivalent(
      wl_eval(flu, function(x) rep(5, length(x)), normalize.wl = normalize01)[[]],
      matrix(rep(5, nwl(flu)), nrow = 1)
    )

    expect_equivalent(
      wl_eval(flu, function(x) x, normalize.wl = normalize01)[[]],
      matrix(seq(0, 1, length.out = nwl(flu)), nrow = 1)
    )

    expect_equivalent(
      wl_eval(flu, function(x) exp(x), normalize.wl = normalize01)[[]],
      matrix(exp(seq(0, 1, length.out = nwl(flu))), nrow = 1)
    )
  })


  test_that("multiple functions", {
    expect_equivalent(
      wl_eval(flu, function(x) rep(1, length(x)), function(x) x),
      vanderMonde(flu, 1)
    )
  })

  test_that("function names", {
    tmp <- wl_eval(flu, f = function(x) x, g = function(x) exp(-x))

    expect_equal(tmp$.f, c("f", "g"))
  })

  test_that("wl_eval(<numeric>) works", {
    expect_equal(
      as.vector(wl_eval(1:10, f = function(x) x)$spc),
      1:10
    )

    expect_equal(
      as.vector(wl_eval(1:10, f = function(x) x**2)$spc),
      (1:10)**2
    )

    expect_equal(
      wl_eval(wl(flu), f = function(x) x)$.f,
      wl_eval(flu, f = function(x) x)$.f
    )

    expect_silent(
      tmp <- wl_eval(300:500, f = function(x) x, g = function(x) exp(-x))
    )
    expect_equal(tmp$.f, c("f", "g"))
  })

  test_that("wl_eval fails with matrix input", {
    expect_error(wl_eval(matrix(1:10), f = function(x) x))
    expect_error(wl_eval(matrix(), f = function(x) x))
  })
}
