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
##' @param fit.to \code{hyperSpec} object on which the baselines are fitted
##' @param apply.to \code{hyperSpec} object on which the baselines are evaluted
##'   If \code{NULL}, a \code{hyperSpec} object containing the polynomial
##'   coefficients rather than evaluted baselines is returned.
##' @param poly.order order of the polynomial to be used
##' @param offset.wl should the wavelength range be mapped to -> [0, delta wl]?
##' This enhances numerical stability.
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
spc.fit.poly <- function (fit.to, apply.to = NULL, poly.order = 1, offset.wl = ! (is.null (apply.to))){
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

  p <- apply (fit.to, 1, 
              function (y, x){
                x <- x [! is.na (y),,drop = FALSE]
                y <- y [! is.na (y)]
                qr.solve (x, y)
              }, 
              x)

  if (is.null (apply.to)){
    colnames (p@data$spc) <- paste0 ("(x - minx)^", 0 : poly.order)

    p$min.x = minx
    return (p)

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
  
  test_that("spectrum containing NA", {
    tmp <- chondro [1]
    tmp [[,, 1600]] <- NA
    
    coefs <- spc.fit.poly (tmp, apply.to = NULL) [[]]
    expect_equal(
      coefs,
      spc.fit.poly(chondro [1,, !is.na (tmp)], apply.to = NULL) [[]]
    )
    
    ## bug was: all coefficients were silently 0 
    expect_true (all (abs (coefs) > sqrt (.Machine$double.eps)))
  })
  
}

##'
##' \code{spc.fit.poly.below} tries to fit the baseline on appropriate spectral
##' ranges of the spectra in \code{fit.to}.  For details, see the
##' \code{vignette ("baseline")}.
##' @rdname baselines
##' @param npts.min minimal number of points used for fitting the polynomial
##' @param noise noise level to be considered during the fit. It may be given
##'   as one value for all the spectra, or for each spectrum separately.
##' @param max.iter stop at the latest after so many iterations. 
##' @param stop.on.increase additional stopping rule: stop if the number of support points would increase, 
##' regardless whether npts.min was reached or not.
##' @param debuglevel  additional output:
##'    \code{1} shows \code{npts.min}, 
##'    \code{2} plots support points for the final baseline of 1st spectrum, 
##'    \code{3} plots support points for 1st spectrum,
##'    \code{4} plots support points for all spectra.
##' @seealso  see \code{\link[hyperSpec]{options}} for more on \code{debuglevel}
##' @export
##' @examples
##'
##' baselines <- spc.fit.poly.below (spc)
##' plot (spc - baselines)
##'
##' spc.fit.poly.below(chondro [1:3], debuglevel = 1)
##' spc.fit.poly.below(chondro [1:3], debuglevel = 2)
##' spc.fit.poly.below(chondro [1:3], debuglevel = 3, noise = sqrt (rowMeans (chondro [[1:3]])))
##' 
spc.fit.poly.below <- function (fit.to, apply.to = fit.to, poly.order = 1,
                                npts.min = max (round (nwl (fit.to) * 0.05), 3 * (poly.order + 1)),
                                noise = 0, offset.wl = FALSE, max.iter = nwl (fit.to), 
                                stop.on.increase = FALSE,
                                debuglevel = hy.getOption("debuglevel")){
  ## for debuglevel >= 2L
  cols <- matlab.dark.palette(max.iter)
  
  chk.hy (fit.to)
  if (! is.null (apply.to))
    chk.hy (apply.to)

  validObject (fit.to)
  validObject (apply.to)

  if (missing (npts.min) && debuglevel >= 1L)
    message ("Fitting with npts.min = ",  npts.min, "\n")

  if (npts.min <= poly.order){
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
    use <- !is.na (y [, i])

    if (debuglevel %in% c(2L, 3L) && i == 1L || debuglevel >= 4L) {
      plot(fit.to [i], title.args = list (main = paste ("spectrum", i)))
      message ("start: ", sum (use, na.rm=TRUE), " support points")
    }

    for (iter in 1 : max.iter) {
      p[i,] <- qr.solve (vdm[use,], y[use, i])
      bl <- vdm %*% p [i,]
      use.old <- use
      use <- y[, i] < bl + noise [i] & !is.na (y [, i])

      if (debuglevel == 3L && i == 1L || debuglevel >= 4L) {
        plot (fit.to[i,, use], add = TRUE, lines.args = list (pch = 20, type = "p"), col= cols [iter])
        lines (fit.to@wavelength, bl, col = cols [iter])
        lines (fit.to@wavelength, bl + noise, col = cols [iter], lty = 2)
        message ("Iteration ", iter, ": ", sum (use, na.rm=TRUE), " support points")
      }

      if ((sum (use, na.rm=TRUE) < npts.min) || all (use == use.old, na.rm = TRUE))
        break
      
      if (sum (use, na.rm=TRUE) > sum (use.old, na.rm=TRUE) && stop.on.increase){
        warning("Iteration ", iter, ": Number of support points is about to increase again. Stopping with ", 
                sum (use.old, na.rm=TRUE), " support points, but this is a local minimum only.")
        break
      }
    }
    
    if (iter == max.iter)
      if ((sum (use.old, na.rm = TRUE) == npts.min) && 
          ! all (use == use.old, na.rm = TRUE) && 
          ! sum (use, na.rm = TRUE) < npts.min){
        warning("Reached npts.min, but the solution is not stable. Stopped after ", iter, " iterations.")
      } else if (sum (use, na.rm=TRUE) >= npts.min) {
        warning ("Stopped after ", iter, " iterations with ", sum (use.old, na.rm = TRUE), " support points.")
      }

    if (debuglevel >= 1L)
      message (sprintf ("spectrum % 6i: % 5i support points, noise = %0.1f, %3i iterations", i, sum (use.old, na.rm = TRUE), noise [i], iter))
    if ((debuglevel == 2L) && (i == 1L)){
      plot (fit.to[i,, use.old], add = TRUE, lines.args = list (pch = 20, type = "p"), col= cols [iter])
      lines (fit.to@wavelength, bl, col = cols [iter])
      lines (fit.to@wavelength, bl + noise, col = cols [iter], lty = 2)
    }
      
  }
  if (is.null (apply.to)){
    fit.to <- new("hyperSpec", spc=p, wavelength=0 : poly.order)
    colnames (fit.to@data$spc) <- paste0 ("(x - minx)^", 0 : poly.order)

    validObject (fit.to)

    fit.to$min.x = minx
    return (fit.to)

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
  
  test_that("requesting 2 support points working - issue #58", {
    expect_warning (spc.fit.poly.below(chondro[103], npts.min = 2), "Stopped after")
    expect_warning (spc.fit.poly.below(chondro[103], npts.min = 2, stop.on.increase = TRUE), "about to increase again")
  })
  
  test_that("spectrum containing NA", {
    tmp <- chondro [1]
    tmp [[,, 1600]] <- NA
    
    coefs <- spc.fit.poly.below(tmp, apply.to = NULL) [[]]
    expect_equal(
      coefs,
      spc.fit.poly.below(chondro [1,, !is.na (tmp)], apply.to = NULL) [[]]
    )
    
    ## bug was: all coefficients were silently 0 
    expect_true (all (abs (coefs) > sqrt (.Machine$double.eps)))
  })
}


