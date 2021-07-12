#' @name DEPRECATED-spc.spline
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Spectral smoothing by splines
#'
#' @description
#'
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained anymore.
#' Currently they are present due to back-compatibility reasons and will be removed
#' in the next release of the package.
#' Please, use the suggested alternative functions instead.
#'
#' `_____________`
#'
#' Spectral smoothing by splines.
#' @param spc `hyperSpec` object
#' @param newx  wavelengh axis to interpolate on
#' @param ... further parameters handed to [stats::smooth.spline()]
#' @return `hyperSpec` object containing smoothed spectra
#'
#' @author Claudia Beleites
#' @seealso [hyperSpec::spc.loess()]
#'
#' [stats::smooth.spline()]
#' @note This function is still experimental
#'
#' @export
#'
#' @examples
#' p <- paracetamol[, , 2200 ~ max]
#' plot(p, col = "gray")
#' smooth <- spc.smooth.spline(p[, , c(2200 ~ 2400, 2500 ~ 2825, 3150 ~ max)],
#'   wl(paracetamol[, , 2200 ~ max]),
#'   df = 4, spar = 1
#' )
#' plot(smooth, col = "red", add = TRUE)
#'
#' plot(p - smooth)
spc.smooth.spline <- function(spc, newx = wl(spc), ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("spc_smooth_spline")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  .spline <- function(x, y, newx) {
    pts <- !is.na(y)
    fit <- smooth.spline(x[pts], y[pts], ...)$fit
    predict(fit, newx, deriv = 0)$y
  }

  spc <- orderwl(spc) # includes chk.hy and validObject

  newspc <- matrix(NA_real_, ncol = length(newx), nrow = nrow(spc))
  i <- rowSums(is.na(spc@data$spc)) < nwl(spc)

  newspc[i, ] <- t(apply(spc@data$spc[i, , drop = FALSE], 1,
    .spline,
    x = spc@wavelength, newx = newx
  ))

  if (any(is.na(newspc[i, ]))) {
    warning("NAs generated. Probably newx was outside the spectral range covered by spc.")
  }

  spc@data$spc <- newspc
  .wl(spc) <- newx

  validObject(spc)

  spc
}

# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(spc.smooth.spline) <- function() {
  context("spc.smooth.spline")

  # Perform tests
  test_that("spc.smooth.spline() returnts output silently", {
    expect_warning(expect_error(spc.smooth.spline()), "deprecated")
    expect_warning(hy <- spc.smooth.spline(flu), "deprecated")
    expect_is(hy, "hyperSpec")
  })
}
