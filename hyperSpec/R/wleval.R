##' Evaluate function on wavelengths of hyperSpec object
##'
##' This is useful for generating certain types of baseline "reference spectra".
##'
##' @param x hyperSpec object
##' @param ... hyperSpec method: expressions to be evaluated
##' @param normalize.wl function to transorm the wavelengths before evaluating the polynomial (or
##' other function). Use \code{\link[hyperSpec]{normalize01}} to map the wavelength range to the interval [0, 1].
##' @return hyperSpec object containing one spectrum for each expression
##' @export
##' @seealso \code{\link[hyperSpec]{vanderMonde}} for  polynomials,
##'
##' \code{\link[hyperSpec]{normalize01}} to normalize the wavenumbers before evaluating the function
##' @author C. Beleites
##' @examples
##' plot (wl.eval (laser, exp = function (x) exp (-x)))
wl.eval <- function (x, ..., normalize.wl = I){
  chk.hy (x)
  validObject (x)

  fun <- list (...)

  wl <- normalize.wl (x@wavelength)

  x <- decomposition (x, t (sapply (fun, function (f) f (wl))), scores = FALSE)
  x$.f <- if (is.null (names (fun)))
              rep (NA, length (fun))
          else
              names (fun)
  x
}

##' @include unittest.R
.test (wl.eval) <- function (){
  context ("wl.eval")

  test_that("error on function not returning same length as input", {
    expect_error (wl.eval (flu, function (x) 1))
  })

  test_that("wl.eval against manual evaluation", {
    expect_equivalent (wl.eval (flu, function (x) rep (5, length (x)), normalize.wl = normalize01) [[]],
                       matrix (rep (5, nwl (flu)), nrow = 1))

    expect_equivalent (wl.eval (flu, function (x) x),
                       vanderMonde(flu, 1)[2])

    expect_equivalent (wl.eval (flu, function (x) exp (-x)) [[]],
                       matrix (exp (-flu@wavelength), nrow = 1))
  })

  test_that("normalization", {
    expect_equivalent (wl.eval (flu, function (x) rep (5, length (x)), normalize.wl = normalize01) [[]],
                       matrix (rep (5, nwl (flu)), nrow = 1))

    expect_equivalent (wl.eval (flu, function (x) x, normalize.wl = normalize01) [[]],
                       matrix (seq (0, 1, length.out = nwl (flu)), nrow = 1))

    expect_equivalent (wl.eval (flu, function (x) exp (x), normalize.wl = normalize01) [[]],
                       matrix (exp (seq (0, 1, length.out = nwl (flu))), nrow = 1))
  })


  test_that("multiple functions", {
    expect_equivalent (wl.eval (flu, function (x) rep (1, length (x)), function (x) x),
                       vanderMonde(flu, 1))

  })

  test_that("function names", {
    tmp <- wl.eval (flu, f = function (x) x, g = function (x) exp (-x))

    expect_equal(tmp$.f, c ("f", "g"))
  })
}
