#' Polynomial baseline fitting
#'
#' These functions fit polynomial baselines.
#'
#' @details
#' Both functions fit polynomials to be used as baselines. If `apply.to`
#' is `NULL`, a `hyperSpec` object with the polynomial coefficients
#' is returned, otherwise the polynomials are evaluated on the spectral range
#' of `apply.to`.
#'
#' `spc_fit_poly()` calculates the least squares fit of order
#' `poly.order` to the *complete* spectra given in `fit.to`.
#' Thus `fit.to` needs to be cut appropriately.
#'
#' @rdname baselines
#' @concept baseline
#' @param fit.to `hyperSpec` object on which the baselines are fitted
#' @param apply.to `hyperSpec` object on which the baselines are evaluted
#'   If `NULL`, a `hyperSpec` object containing the polynomial
#'   coefficients rather than evaluted baselines is returned.
#' @param poly.order order of the polynomial to be used
#' @param offset.wl should the wavelength range be mapped to -> \[0, delta wl\]?
#' This enhances numerical stability.
#' @return `hyperSpec` object containing the baselines in the spectra
#'   matrix, either as polynomial coefficients or as polynomials evaluted on
#'   the spectral range of `apply.to`
#' @author C. Beleites
#'
#' @seealso `vignette("baseline", package = "hyperSpec")`
#'
#' @export
#'
#' @keywords manip datagen
#' @concept baseline
#'
#' @examples
#' \dontrun{
#' vignette("baseline", package = "hyperSpec")
#' }
#'
#' spc <- faux_cell[1:10]
#' baselines <- spc_fit_poly(spc[, , c(625 ~ 640, 1785 ~ 1800)], spc)
#' plot(spc - baselines)
spc_fit_poly <- function(fit.to, apply.to = NULL, poly.order = 1,
                         offset.wl = !(is.null(apply.to))) {
  chk.hy(fit.to)
  if (!is.null(apply.to)) {
    chk.hy(apply.to)
  }

  validObject(fit.to)
  validObject(apply.to)

  x <- fit.to@wavelength

  if (offset.wl) {
    minx <- min(x)
    x <- x - min(x)
  } else {
    minx <- 0
  }

  x <- vanderMonde(x, poly.order) # Vandermonde matrix of x

  p <- apply(
    fit.to, 1,
    function(y, x) {
      x <- x[!is.na(y), , drop = FALSE]
      y <- y[!is.na(y)]
      qr.solve(x, y)
    },
    x
  )

  if (is.null(apply.to)) {
    colnames(p@data$spc) <- paste0("(x - minx)^", 0:poly.order)

    p$min.x <- minx
    return(p)
  } else {
    wl <- apply.to@wavelength - minx

    x <- vanderMonde(wl, poly.order) # Vandermonde matrix of x
    apply.to@data$spc <- I(t(apply(p[[]], 1, function(p, x) {
      x %*% p
    }, x)))

    validObject(apply.to)

    apply.to
  }
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc_fit_poly) <- function() {
  context("spc_fit_poly")

  test_that(
    "no normalization",
    {
      bl.nonorm <- spc_fit_poly(flu, flu, poly.order = 3, offset.wl = FALSE)
    }
  )

  # test effect of wavelength axis normalization
  # was issue 1 on github
  tmp <- flu
  wl(tmp) <- wl(tmp) + 1e4

  test_that("normalization/offset wavelengths", {
    expect_error(spc_fit_poly(tmp, poly.order = 3, offset.wl = FALSE))

    bl.1e4 <- spc_fit_poly(tmp, tmp, poly.order = 3, offset.wl = TRUE)
    bl.nonorm <- spc_fit_poly(flu, flu, poly.order = 3, offset.wl = FALSE)
    expect_equal(bl.nonorm[[]], bl.1e4[[]])
  })

  test_that("spectrum containing NA", {
    tmp <- faux_cell[1]
    tmp[[, , 1600]] <- NA

    coefs <- spc_fit_poly(tmp, apply.to = NULL)[[]]
    expect_equal(
      coefs,
      spc_fit_poly(faux_cell[1, , !is.na(tmp)], apply.to = NULL)[[]]
    )

    ## bug was: all coefficients were silently 0
    expect_true(all(abs(coefs) > sqrt(.Machine$double.eps)))
  })
}


# ... ------------------------------------------------------------------------

#' @details
#' `spc_fit_poly_below()` tries to fit the baseline on appropriate spectral
#' ranges of the spectra in `fit.to`.
#' For details, see the `vignette("baseline")`.
#' @rdname baselines
#' @param npts.min minimal number of points used for fitting the polynomial
#' @param noise noise level to be considered during the fit. It may be given as
#'   one value for all the spectra, or for each spectrum separately.
#' @param max.iter stop at the latest after so many iterations.
#' @param stop.on.increase additional stopping rule: stop if the number of
#'   support points would increase, regardless whether `npts.min` was reached or
#'   not.
#' @param debuglevel  additional output: `1` shows `npts.min`,
#'   `2` plots support points for the final baseline of 1st spectrum,
#'   `3` plots support points for 1st spectrum, `4` plots support
#'   points for all spectra.
#' @seealso  see [hyperSpec::options()] for more on `debuglevel`
#'
#' @export
#'
#' @concept baseline
#'
#' @examples
#'
#' baselines <- spc_fit_poly_below(spc)
#' plot(spc - baselines)
#'
#' spc_fit_poly_below(faux_cell[1:3], debuglevel = 1)
#' spc_fit_poly_below(faux_cell[1:3], debuglevel = 2)
#' spc_fit_poly_below(faux_cell[1:3],
#'   debuglevel = 3,
#'   noise = sqrt(rowMeans(faux_cell[[1:3]]))
#' )
spc_fit_poly_below <- function(fit.to, apply.to = fit.to, poly.order = 1,
                               npts.min = max(
                                 round(nwl(fit.to) * 0.05),
                                 3 * (poly.order + 1)
                               ),
                               noise = 0, offset.wl = FALSE,
                               max.iter = nwl(fit.to),
                               stop.on.increase = FALSE,
                               debuglevel = hy.getOption("debuglevel")) {
  ## for debuglevel >= 2L
  cols <- palette_matlab_dark(max.iter)

  chk.hy(fit.to)
  if (!is.null(apply.to)) {
    chk.hy(apply.to)
  }

  validObject(fit.to)
  validObject(apply.to)

  if (missing(npts.min) && debuglevel >= 1L) {
    message("Fitting with npts.min = ", npts.min, "\n")
  }

  if (npts.min <= poly.order) {
    npts.min <- poly.order + 1
    warning(paste("npts.min too small: adjusted to", npts.min))
  }

  if (length(noise) == 1) {
    noise <- rep(noise, nrow(fit.to))
  }

  x <- fit.to@wavelength

  if (offset.wl) {
    minx <- min(x)
    x <- x - min(x)
  } else {
    minx <- 0
  }

  vdm <- vanderMonde(x, poly.order)
  y <- t(fit.to[[]])

  p <- matrix(nrow = nrow(fit.to), ncol = poly.order + 1)
  for (i in row.seq(fit.to)) {
    use.old <- logical(nwl(fit.to))
    use <- !is.na(y[, i])

    if (debuglevel %in% c(2L, 3L) && i == 1L || debuglevel >= 4L) {
      plot(fit.to[i], title.args = list(main = paste("spectrum", i)))
      message("start: ", sum(use, na.rm = TRUE), " support points")
    }

    for (iter in 1:max.iter) {
      p[i, ] <- qr.solve(vdm[use, ], y[use, i])
      bl <- vdm %*% p[i, ]
      use.old <- use
      use <- y[, i] < bl + noise[i] & !is.na(y[, i])

      if ((debuglevel == 3L && i == 1L || debuglevel >= 4L) && sum(use) > 0L) {
        plot(fit.to[i, , use],
          add = TRUE,
          lines.args = list(pch = 20, type = "p"), col = cols[iter]
        )

        lines(fit.to@wavelength, bl, col = cols[iter])
        lines(fit.to@wavelength, bl + noise, col = cols[iter], lty = 2)

        message(
          "Iteration ", iter, ": ", sum(use, na.rm = TRUE),
          " support points"
        )
      }

      if ((sum(use, na.rm = TRUE) < npts.min) ||
        all(use == use.old, na.rm = TRUE)) {
        break
      }

      if (sum(use, na.rm = TRUE) > sum(use.old, na.rm = TRUE) &&
        stop.on.increase) {
        warning(
          "Iteration ", iter, ": ",
          "Number of support points is about to increase again. ",
          "Stopping with ", sum(use.old, na.rm = TRUE),
          " support points, but this may be a local minimum."
        )

        break
      }
    }

    if (iter == max.iter) {
      if ((sum(use.old, na.rm = TRUE) == npts.min) &&
        !all(use == use.old, na.rm = TRUE) &&
        !sum(use, na.rm = TRUE) < npts.min) {
        warning(
          "Reached npts.min, but the solution is not stable. ",
          "Stopped after ", iter, " iterations."
        )
      } else if (sum(use, na.rm = TRUE) >= npts.min) {
        warning(
          "Stopped after ", iter, " iterations with ",
          sum(use.old, na.rm = TRUE), " support points."
        )
      }
    }

    if (debuglevel >= 1L) {
      message(sprintf(
        "spectrum % 6i: % 5i support points, noise = %0.1f, %3i iterations",
        i, sum(use.old, na.rm = TRUE), noise[i], iter
      ))
    }

    if ((debuglevel == 2L) && (i == 1L)) {
      plot(fit.to[i, , use.old],
        add = TRUE,
        lines.args = list(pch = 20, type = "p"), col = cols[iter]
      )

      lines(fit.to@wavelength, bl, col = cols[iter])

      lines(fit.to@wavelength, bl + noise, col = cols[iter], lty = 2)
    }
  }

  if (is.null(apply.to)) {
    fit.to <- new("hyperSpec", spc = p, wavelength = 0:poly.order)
    colnames(fit.to@data$spc) <- paste0("(x - minx)^", 0:poly.order)

    validObject(fit.to)

    fit.to$min.x <- minx
    return(fit.to)
  } else {
    x <- apply.to@wavelength - minx

    vdm <- vanderMonde(x, poly.order) # Vandermonde matrix of x

    apply.to@data$spc <- I(t(apply(p, 1, function(p, x) {
      x %*% p
    }, vdm)))

    validObject(apply.to)

    apply.to
  }
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc_fit_poly_below) <- function() {
  context("spc_fit_poly_below")

  test_that(
    "no normalization",
    {
      bl.nonorm <- spc_fit_poly_below(flu, flu,
        poly.order = 3, offset.wl = FALSE,
        npts.min = 25
      )
    }
  )

  # test effect of wavelength axis normalization
  # was issue 1 on github
  tmp <- flu
  wl(tmp) <- wl(tmp) + 1e4

  test_that("normalization/offset wavelengths", {
    expect_error(spc_fit_poly_below(tmp,
      poly.order = 3, offset.wl = FALSE,
      npts.min = 25
    ))

    bl.1e4 <- spc_fit_poly_below(tmp, tmp,
      poly.order = 3, offset.wl = TRUE,
      npts.min = 25
    )

    bl.nonorm <- spc_fit_poly_below(flu, flu,
      poly.order = 3, offset.wl = FALSE,
      npts.min = 25
    )

    expect_equal(bl.nonorm[[]], bl.1e4[[]])
  })

  test_that("stopping rules for unstable solutions - issue #58", {
    # test object origninally created from chondro:
    # tmp <- chondro[103,,c(600 ~ 700, 1650 ~ 1800)]
    # tmp[[]] <- round(tmp[[]], digits = 1)

    tmp <- t(c(
      331.8, 336.7, 325.3, 313.2, 328.6, 348.5, 304.6, 286.8, 283.9,
      294.2, 323.3, 312.2, 298.8, 299.8, 299.7, 301.8, 305.2, 308.4,
      311.2, 318.2, 321, 322.1, 323, 336.7, 362.1, 776.9, 835.3, 902,
      967, 1019.3, 1020.5, 942.3, 848.8, 774.8, 701.1, 612.1, 514.4,
      420.8, 340.1, 282.5, 242.7, 220, 206, 196.8, 192.1, 189.1, 185.3,
      184, 181.8, 178.7, 178.8, 174.8, 175.6, 173.2, 174.3, 173.1,
      173.2, 171.4, 171.5, 171.9, 171.3, 171.1, 171.8
    ))
    tmp <- as.hyperSpec(tmp)
    wl(tmp) <- c(seq(602, 698, by = 4), seq(1650, 1798, by = 4))

    expect_warning(
      spc_fit_poly_below(tmp, npts.min = 2),
      "Reached npts.min, but the solution is not stable."
    )
    expect_warning(
      spc_fit_poly_below(tmp,
        npts.min = 2,
        stop.on.increase = TRUE
      ),
      "Number of support points is about to increase again."
    )
  })

  test_that("spectrum containing NA", {
    tmp <- faux_cell[1]
    tmp[[, , 1600]] <- NA

    coefs <- spc_fit_poly_below(tmp, apply.to = NULL)[[]]
    expect_equal(
      coefs,
      spc_fit_poly_below(faux_cell[1, , !is.na(tmp)], apply.to = NULL)[[]]
    )

    ## bug was: all coefficients were silently 0
    expect_true(all(abs(coefs) > sqrt(.Machine$double.eps)))
  })
}
