#' @name DEPRECATED-spc.loess
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        LOESS smoothing interpolation for spectra
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
#'
#' @author C. Beleites
#'
#' @examples
#' data(flu, package = "hyperSpec")
#' nwl(flu)
#'
#' smoothed_flu <- spc.loess(flu, seq(420, 470, 5))
#' nwl(smoothed_flu)
#'
#' plot(flu, col = "darkgray")
#' plot(smoothed_flu, add = TRUE, col = "red")
#'
#'
#' flu_na <- flu
#' flu_na[[3, ]] <- NA_real_
#' flu_na_smoothed <- spc.loess(flu_na, seq(420, 470, 5))
#' flu_na_smoothed[[]]
#'
#' plot(flu, col = "darkgray")
#' plot(flu_na_smoothed, add = TRUE, col = "blue")
spc.loess <- function(spc, newx, enp.target = nwl(spc) / 4, surface = "direct",
                      ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("spc_loess")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  .fix_spc_colnames(spc)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc.loess) <- function() {
  context("spc.loess")

  # Perform tests
  test_that("spc.loess() returns correct spc colnames", {
    expect_warning(res <- spc.loess(flu, seq(420, 470, 5)), "deprecated")

    spc_col_names <- as.numeric(colnames(res$spc))
    expect_equal(spc_col_names, wl(res))
    expect_equal(spc_col_names, seq(420, 470, 5))
  })

  test_that("spc.loess() returns errors", {
    expect_warning(expect_error(spc.loess()), "deprecated")
  })

  test_that("spc.loess() returns warnings", {
    flu[[3, ]] <- NA_real_
    expect_warning(spc.loess(flu, seq(420, 470, 5)), "NAs were generated.")
  })


  # FIXME (tests): add tests to check the correctness of the output!!!
}
