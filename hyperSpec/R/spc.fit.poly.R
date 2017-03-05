##' Polynomial Baseline Fitting
##' These functions fit polynomal baselines.
##'
##' Both functions fit polynomials to be used as baselines. If \code{apply.to}
##' is \code{NULL}, a \code{hyperSpec} object with the polynomial coefficients
##' is returned, otherwise the polynomials are evaluated on the spectral range
##' of \code{apply.to}.
##'
##' \code{spc.fit.poly} calculates the least squares fit of order
##' \code{poly.order} to the \emph{complete} spectra given in \code{fit.to}.
##' Thus \code{fit.to} needs to be cut appropriately.
##'
##' @rdname baselines
##' @concept baseline
##' @aliases spc.fit.poly spc.fit.poly.below
##' @param fit.to \code{hyperSpec} object on which the baselines are fitted
##' @param apply.to \code{hyperSpec} object on which the baselines are evaluted
##'   If \code{NULL}, a \code{hyperSpec} object containing the polynomial
##'   coefficients rather than evaluted baselines is returned.
##' @param poly.order order of the polynomial to be used
##' @param offset.wl should the wavelength range be mapped to -> [0, delta wl]?
##' This enhances numerical stability.
##' @param debuglevel  see \code{\link[hyperSpec]{options}}
##' @return \code{hyperspec} object containing the baselines in the spectra
##'   matrix, either as polynomial coefficients or as polynomials evaluted on
##'   the spectral range of \code{apply.to}
##' @author C. Beleites
##' @seealso \code{vignette ("baseline", package = "hyperSpec")}
##' @keywords manip datagen
##' @export
##' @examples
##'
##' \dontrun{vignette ("baseline", package = "hyperSpec")}
##'
##' spc <- chondro [1 : 10]
##' baselines <- spc.fit.poly(spc [,, c (625 ~ 640, 1785 ~ 1800)], spc)
##' plot(spc - baselines)
##'
spc.fit.poly <- function (fit.to, apply.to = NULL, poly.order = 1, offset.wl = ! (is.null (apply.to)),
                          debuglevel = hy.getOption("debuglevel")){
  chk.hy (fit.to)
  if (! is.null (apply.to))
    chk.hy (apply.to)

  validObject (fit.to)
  validObject (apply.to)

  x <- fit.to@wavelength

  if (offset.wl){
    minx <- min (x)
    x <- x - min (x)
  } else {
    minx <- 0
  }

  x <- vanderMonde (x, poly.order)          # Vandermonde matrix of x

  p <- apply (fit.to, 1, function (y, x){qr.solve (x, y)}, x)

  if (is.null (apply.to)){
    colnames (p@data$spc) <- paste0 ("(x - minx)^", 0 : poly.order)

    list (coef = p, min.x = minx)

  } else {
    wl <- apply.to@wavelength - minx

    x <- vanderMonde(wl, poly.order)   # Vandermonde matrix of x
    apply.to@data$spc <- I (t (apply (p[[]], 1, function (p, x) {x %*% p}, x)))

    validObject(apply.to)

    apply.to
  }
}

.test (spc.fit.poly) <- function (){
  context ("spc.fit.poly")

  test_that("no normalization",
            bl.nonorm <- spc.fit.poly (flu, flu, poly.order = 3, offset.wl = FALSE)
  )

  # test effect of wavelength axis normalization
  # was issue 1 on github
  tmp <- flu
  wl (tmp) <- wl (tmp) + 1e4

  test_that("normalization/offset wavelengths", {
    expect_error (spc.fit.poly(tmp, poly.order = 3, offset.wl = FALSE))

    bl.1e4 <- spc.fit.poly(tmp, tmp, poly.order = 3, offset.wl = TRUE)
    bl.nonorm <- spc.fit.poly (flu, flu, poly.order = 3, offset.wl = FALSE)
    expect_equal (bl.nonorm [[]], bl.1e4 [[]])
  })
  }

##'
##' \code{spc.fit.poly.below} tries to fit the baseline on appropriate spectral
##' ranges of the spectra in \code{fit.to}.  For details, see the
##' \code{vignette ("baseline")}.
##' @rdname baselines
##' @param npts.min minmal number of points used for fitting the polynomial
##' @param noise noise level to be considered during the fit. It may be given
##'   as one value for all the spectra, or for each spectrum separately.
##' @export
##' @examples
##'
##' baselines <- spc.fit.poly.below (spc)
##' plot (spc - baselines)
##'
spc.fit.poly.below <- function (fit.to, apply.to = fit.to, poly.order = 1,
                                npts.min = NULL, noise = 0, offset.wl = FALSE){
  chk.hy (fit.to)
  if (! is.null (apply.to))
    chk.hy (apply.to)

  validObject (fit.to)
  validObject (apply.to)

  if (is.null (npts.min)){
    npts.min <- max (round (nwl(fit.to) * 0.05), 3 * (poly.order + 1))
    cat ("Fitting with npts.min = ",  npts.min, "\n")
  } else  if (npts.min <= poly.order){
    npts.min <- poly.order + 1
    warning (paste ("npts.min too small: adjusted to", npts.min))
  }

  if (length (noise) == 1)
    noise <- rep (noise, nrow (fit.to))

  x <- fit.to@wavelength

  if (offset.wl){
    minx <- min (x)
    x <- x - min (x)
  } else {
    minx <- 0
  }

  vdm <- vanderMonde (x, poly.order)
  y <- t (fit.to [[]])

  p <- matrix (nrow = nrow(fit.to) , ncol = poly.order + 1)
  for (i in row.seq (fit.to)){
    use.old <- logical (nwl (fit.to))
    use <- !use.old

    repeat {
      p[i,] <- qr.solve (vdm[use,], y[use, i])
      bl <- vdm %*% p [i,]
      use.old <- use
      use <- y[, i] < bl + noise [i]

      if (debuglevel > 0) {
        plot (fit.to[,, use.old], col = cl, add = TRUE, lines.args = list (pch = 20, type = "p"));
        lines (fit.to@wavelength, bl, col = cl);
      }

      if ((sum (use, na.rm=TRUE) < npts.min) || all (use == use.old, na.rm = TRUE))
        break
    }
  }
  if (is.null (apply.to)){
    fit.to@data$spc <- p
    .wl (fit.to) <- 0 : poly.order
    colnames (fit.to@data$spc) <- paste0 ("(x - minx)^", 0 : poly.order)

    validObject (fit.to)

    list (coef = fit.to, min.x = minx)
  } else {
    x <- apply.to@wavelength - minx

    vdm <- vanderMonde(x, poly.order)             # Vandermonde matrix of x

    apply.to@data$spc <- I (t (apply (p, 1, function (p, x) {x %*% p}, vdm)))

    validObject (apply.to)

    apply.to
  }
}

.test (spc.fit.poly.below) <- function (){
  context ("spc.fit.poly.below")

  test_that("no normalization",
            bl.nonorm <- spc.fit.poly.below (flu, flu, poly.order = 3, offset.wl = FALSE, npts.min = 25)
  )

  # test effect of wavelength axis normalization
  # was issue 1 on github
  tmp <- flu
  wl (tmp) <- wl (tmp) + 1e4

  test_that("normalization/offset wavelengths", {
    expect_error (spc.fit.poly.below (tmp, poly.order = 3, offset.wl = FALSE, npts.min = 25))

    bl.1e4 <- spc.fit.poly.below (tmp, tmp, poly.order = 3, offset.wl = TRUE, npts.min = 25)
    bl.nonorm <- spc.fit.poly.below (flu, flu, poly.order = 3, offset.wl = FALSE, npts.min = 25)

    expect_equal (bl.nonorm [[]], bl.1e4 [[]])
  })
}


