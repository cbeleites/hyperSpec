#' Impute missing data points
#'
#' Replace `NA`s in the spectra matrix by interpolation. With
#' less than 4 points available linear interpolation of the 2 neighbor points
#' is used. For larger numbers of neighbor points, smoothing interpolation is
#'  performed by [stats::smooth.spline()].
#'
#' @param spc hyperSpec object with spectra matrix containing `NA`s
#' @param neighbours how many neighbor data points should be used to fit the
#'   line
#' @param w,df,spar see [stats::smooth.spline()]
#' @param debuglevel see [hyperSpec::options()]
#' @return hyperSpec object
#'
#' @export
#'
#' @concept manipulation
#' @concept imputation
#'
#' @author Claudia Beleites
#' @examples
#' fluNA <- hyperSpec:::fluNA
#' spc_na_approx(fluNA[, , min ~ 410], debuglevel = 1)
#' spc_na_approx(fluNA[1, , min ~ 410], debuglevel = 2)
#' spc_na_approx(fluNA[4, , min ~ 410], neighbours = 3, df = 4, debuglevel = 2)
spc_na_approx <- function(spc, neighbours = 1, w = rep(1, 2 * neighbours),
                          df = 1 + .Machine$double.eps, spar = NULL,
                          debuglevel = hy.getOption("debuglevel")) {
  chk.hy(spc)
  validObject(spc)

  all.na <- which(apply(is.na(spc@data$spc), 1, all))
  if (length(all.na) > 0) {
    warning("Spectra containing only NAs found. They will not be approximated.")
  }
  stopifnot(neighbours >= 1L)

  ispc <- which(is.na(spc@data$spc), arr.ind = TRUE)

  ispc <- setdiff(unique(ispc[, "row"]), all.na)

  if (debuglevel == 1L) {
    plot(spc[ispc], col = "gray")
  }

  for (i in ispc) {
    if (debuglevel == 2L) {
      plot(spc[i], col = "gray")
    }

    nas <- which(is.na(spc@data$spc[i, ]))

    start <- c(0, which(diff(nas) > 1)) + 1
    end <- c(start[-1] - 1, length(nas))

    for (j in seq(along = start)) {
      pts <- nas[start[j]]:nas[end[j]]

      xneighbours <- c(
        -(1:neighbours) + nas[start[j]],
        (1:neighbours) + nas[end[j]]
      )
      mask <- xneighbours > 0 & xneighbours <= nwl(spc)
      xneighbours <- xneighbours[mask]

      if (sum(mask) == 0) { # should not happen as all NA-only spectra were excluded
        stop("No data to interpolate from.")
      } else if (sum(mask) == 1) {
        spc@data$spc[i, pts] <- spc@data$spc[i, xneighbours]

        if (debuglevel == 2L) {
          points(x = spc@wavelength[xneighbours], y = spc@data$spc[i, xneighbours])
        }
      } else if (sum(mask) < 4) { # old behaviour using linear interpolation
        spc@data$spc[i, pts] <- approx(
          x = spc@wavelength[xneighbours],
          y = spc@data$spc[i, xneighbours],
          xout = spc@wavelength[pts],
          method = "linear",
          rule = 2
        )$y
        if (debuglevel == 2L) {
          lines(x = spc@wavelength[xneighbours], y = spc@data$spc[i, xneighbours])
        }
      } else { # more neighbours: interpolation spline
        spline <- smooth.spline(
          x = spc@wavelength[xneighbours],
          y = spc@data$spc[i, xneighbours],
          w = w[mask], df = df, spar = spar,
          cv = FALSE, all.knots = TRUE, keep.data = FALSE
        )
        spc@data$spc[i, pts] <- predict(spline, x = spc@wavelength[pts])$y

        if (debuglevel == 2L) {
          wlr <- seq(
            from = min(spc@wavelength[xneighbours]),
            to = max(spc@wavelength[xneighbours]),
            length.out = 100
          )
          lines(predict(spline, wlr))
        }
      }

      if (debuglevel == 2L) {
        plot(spc[i, , xneighbours, wl.index = TRUE],
          add = TRUE,
          lines.args = list(type = "p", pch = 20), col = 1
        )
      }
      if (debuglevel >= 1L) {
        plot(spc[i, , pts, wl.index = TRUE],
          add = TRUE,
          lines.args = list(type = "p", pch = 20), col = 2
        )
      }
    }
  }

  spc
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc_na_approx) <- function() {
  context("spc_na_approx")

  test_that("linear interpolation", {
    tmp <- spc_na_approx(fluNA[-2, , min ~ 410])
    expect_equivalent(
      as.numeric(tmp[[, , 406]]),
      rowMeans(fluNA[[-2, , 405.5 ~ 406.5]], na.rm = TRUE)
    )
  })

  test_that("spline interpolation", {
    tmp <- spc_na_approx(fluNA[-2, , min ~ 410], neighbours = 2)
    expect_true(
      all(abs(tmp[[, , 406]] - rowMeans(fluNA[[-2, , 405 ~ 407]],
        na.rm = TRUE
      )) <= 1e-5)
    )
    # version on CRAN throws error on `expect_equal(tolerance = 1e-5)`
    # TODO => change back ASAP
  })

  test_that("edge treatment and debuglevel", {
    ranges <- list(405 ~ 407, 405.5 ~ 406.5, 405.6 ~ 406)
    for (d in 0:2) {
      for (r in ranges) {
        tmp <- spc_na_approx(fluNA[-2, , r], neighbours = 3, debuglevel = d)
        # expect_equal(round(as.numeric(tmp[[,, 406]]), 5),
        #   round(rowMeans(fluNA[[-2,, r]], na.rm = TRUE), 5),
        #   tolerance = 1e-5,
        #   info = paste0(
        #     "debuglevel = ", d, ", range = ",
        #     paste0(r[c(2, 1, 3)], collapse = ""))
        # )
        # version on CRAN throws error on `expect_equal (tolerance = 1e-5)`
        # TODO => change back ASAP
        expect_true(
          all(abs(tmp[[, , 406]] - rowMeans(fluNA[[-2, , r]], na.rm = TRUE)) <= 1e-5),
          info = paste0("debuglevel = ", d, ", range = ", paste0(r[c(2, 1, 3)], collapse = ""))
        )
      }
    }
  })
}
