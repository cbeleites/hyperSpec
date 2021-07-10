#' @rdname spc-loess
#'
#' @title LOESS smoothing interpolation for spectra
#'
#' @description
#' Spectra smoothing and interpolation on a new wavelength axis using
#' [stats::loess()].
#'
#' @details
#' Applying [stats::loess()] to each of the spectra, an interpolation onto a new
#' wavelength axis is performed. At the same time, the spectra are smoothed in
#' order to increase the signal to noise ratio. See [stats::loess()] and
#' [stats::loess.control()] on the parameters that control the amount of
#' smoothing.
#'
#' @param spc The `hyperSpec` object.
#' @param newx Wavelength axis to interpolate on.
#' @param enp.target,surface,... Further parameters for [stats::loess()] and
#'        [stats::loess.control()].
#'
#' @return A new  [`hyperSpec`][hyperSpec::hyperSpec-class] object.
#'
#' @seealso [stats::loess()], [stats::loess.control()]
#'
#' @export
#'
#' @keywords manip datagen
#' @concept spectra smoothing
#' @concept spectra preprocessing
#'
#' @author C. Beleites
#'
#' @examples
#' data(flu, package = "hyperSpec")
#' nwl(flu)
#'
#' smoothed_flu <- spc_loess(flu, seq(420, 470, 5))
#' nwl(smoothed_flu)
#'
#' plot(flu, col = "darkgray")
#' plot(smoothed_flu, add = TRUE, col = "red")
#'
#'
#' flu_na <- flu
#' flu_na[[3, ]] <- NA_real_
#' flu_na_smoothed <- spc_loess(flu_na, seq(420, 470, 5))
#' flu_na_smoothed[[]]
#'
#' plot(flu, col = "darkgray")
#' plot(flu_na_smoothed, add = TRUE, col = "blue")
spc_loess <- function(spc, newx, enp.target = nwl(spc) / 4, surface = "direct",
                      ...) {
  .loess <- function(y, x) {
    if (all(is.na(y))) {
      NA
    } else {
      loess(y ~ x, enp.target = enp.target, surface = surface, ...)
    }
  }

  .predict <- function(loess, x) {
    if (!is(loess, "loess") && is.na(loess)) {
      rep(NA_real_, length(x))
    } else {
      predict(loess, x)
    }
  }

  chk.hy(spc)
  validObject(spc)

  loess <- apply(t(spc[[]]), 2, .loess, spc@wavelength)

  spc@data$spc <- t(sapply(loess, .predict, newx))
  .wl(spc) <- newx

  if (any(is.na(spc@data$spc))) {
    warning(
      "NAs were generated. ",
      "Probably `newx` was outside the spectral range covered by `spc`."
    )
  }

  .spc_fix_colnames(spc)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc_loess) <- function() {
  context("spc_loess")

  # Perform tests
  test_that("spc_loess() returns correct spc colnames", {
    expect_silent(res <- spc_loess(flu, seq(420, 470, 5)))

    spc_col_names <- as.numeric(colnames(res$spc))
    expect_equal(spc_col_names, wl(res))
    expect_equal(spc_col_names, seq(420, 470, 5))
  })

  test_that("spc_loess() returns errors", {
    expect_error(spc_loess())
  })

  test_that("spc_loess() returns warnings", {
    flu[[3, ]] <- NA_real_
    expect_warning(spc_loess(flu, seq(420, 470, 5)), "NAs were generated.")
  })


  # FIXME (tests): add tests to check the correctness of the output!!!
}
