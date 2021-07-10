#' @title Rubberband baseline correction
#'
#' @description
#' Baseline with support points determined from a convex hull of the spectrum.
#'
#' Use `debuglevel >= 1` to obtain debug plots, either directly via function
#' argument or by setting hyperSpec's `debuglevel` option.
#' @param spc `hyperSpec` object
#' @param ... further parameters handed to [stats::smooth.spline()]
#' @param upper logical indicating whether the lower or upper part of the hull should be used
#' @param noise noise level to be taken into account
#' @param spline logical indicating whether the baseline should be an interpolating spline through
#' the support points or piece wise linear.
#' @return `hyperSpec` object containing the baselines
#' @rdname spc-rubberband
#' @author Claudia Beleites
#' @seealso [hyperSpec::spc_fit_poly()], [hyperSpec::spc_fit_poly_below()]
#'
#' `vignette ("baseline")`
#'
#' [hyperSpec::hy.setOptions()]
#'
#' @note This function is still experimental
#' @export
#'
#' @concept baseline
#'
#' @examples
#' plot(paracetamol[, , 175 ~ 1800])
#' bl <- spc_rubberband(paracetamol[, , 175 ~ 1800], noise = 300, df = 20)
#' plot(bl, add = TRUE, col = 2)
#'
#' plot(paracetamol[, , 175 ~ 1800] - bl)
spc_rubberband <- function(spc, ..., upper = FALSE, noise = 0, spline = TRUE) {
  spc <- wl_sort(spc)

  if (upper) spc@data$spc <- -spc@data$spc

  spc@data$spc <- .rubberband(spc@wavelength, spc@data$spc,
    noise = noise, spline = spline, ...
  )

  if (upper) spc@data$spc <- -spc@data$spc

  spc
}

#' @importFrom grDevices chull
.rubberband <- function(x, y, noise, spline, ..., debuglevel = hy.getOption("debuglevel")) {
  for (s in seq_len(nrow(y))) {
    use <- which(!is.na(y[s, ]))

    pts <- chull(x[use], y[s, use])
    pts <- use[pts]

    if (debuglevel >= 1L) {
      plot(x, y[s, ], type = "l")
      points(x[pts], y[s, pts], pch = 1, col = palette_matlab_dark(length(pts)))
    }

    ## `chull` returns points in cw order
    ## => points between ncol (y) and 1 are lower part of hull
    imax <- which.max(pts) - 1

    ## if necessary, rotate pts so that ncol (y) is at position 1
    if (imax > 0L) {
      pts <- c(pts[-seq_len(imax)], pts[seq_len(imax)])
    }

    ## now keep only pts until column index 1
    pts <- pts[1:which.min(pts)]

    ## check whether first and last point are minima,
    ## if not remove them.
    ## If they are minima, 2nd and 2nd last point do not appear in pts
    ## last point:
    if (pts[2] == pts[1] - 1) pts <- pts[-1]

    ## now sort ascending (anyways needed later on)
    pts <- rev(pts)

    ## fist point:
    if (pts[2] == pts[1] + 1) pts <- pts[-1]

    if (debuglevel >= 1L) {
      points(x[pts], y[s, pts], pch = 19, col = palette_matlab_dark(length(pts)), cex = 0.7)
    }

    tmp <- approx(x = x[pts], y = y[s, pts], xout = x, method = "linear")$y

    if (spline) {
      pts <- which(y[s, ] <= tmp + noise)

      if (length(pts) > 3) {
        tmp <- predict(smooth.spline(x[pts], y[s, pts], ...)$fit, x, 0)$y
      } else {
        tmp <- spline(x[pts], y[s, pts], xout = x)$y
      }
    }

    y[s, ] <- tmp
  }

  y
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc_rubberband) <- function() {
  context("spc_rubberband")

  ## use data that yields fairly stable baseline solution
  paracetamol <- paracetamol[, , 300 ~ 550]


  test_that("spectrum containing NA inside", {
    tmp <- paracetamol
    tmp[[, , 400]] <- NA

    coefs <- spc_rubberband(tmp)
    expect_equal(
      coefs[[, , !is.na(tmp)]],
      spc_rubberband(paracetamol[, , !is.na(tmp)])[[]]
    )

    ## bug was: all coefficients were silently 0
    expect_true(all(abs(coefs[[]]) > sqrt(.Machine$double.eps)))
  })

  test_that("spectrum containing NA at first wavelength (issue #95)", {
    tmp <- paracetamol
    tmp[[, , 1, wl.index = TRUE]] <- NA

    coefs <- spc_rubberband(tmp)
    expect_equal(
      coefs[[, , !is.na(tmp)]],
      spc_rubberband(paracetamol[, , !is.na(tmp)])[[]]
    )
  })

  test_that("spectrum containing NA at end", {
    tmp <- paracetamol[1]
    tmp[[, , nwl(paracetamol), wl.index = TRUE]] <- NA

    coefs <- spc_rubberband(tmp)
    expect_equal(
      coefs[[, , !is.na(tmp)]],
      spc_rubberband(paracetamol[1, , !is.na(tmp)])[[]]
    )
  })
}
