#' Identifying spectra and spectral data points
#'
#' This function allows to identify the spectrum and the wavelength of a point
#' in a plot produced by [plotspc()].
#'
#' This function first finds the spectrum with a point closest to the clicked
#' position (see [graphics::locator()]). The distance to the clicked
#' point is evaluated relative to the size of the tolerance window.
#'
#' In a second step, `max.fn` searches for the actual point to label
#' within the specified wavelength window of that spectrum. This allows to
#' label maxima (or minima) without demanding too precise clicks. Currently,
#' the following functions to determine the precise point: \tabular{ll}{
#' spc.point.default \tab uses the clicked wavelength together with its
#' spectral intensity\cr spc.point.max \tab the point with the highest
#' intensity in the wavelength window \cr spc.point.min \tab the point with
#' the lowest intensity in the wavelength window \cr spc.point.sqr \tab
#' maximum of a parabola fit throug the point with highest intensity and the
#' two surrounding points \cr } `point.fn` is called with the arguments
#' `wl` containing the considered wavelength window, `spc` the
#' respective intensities of the closest spectrum, and `wlclick` the
#' wavelength that was clicked. They return a vector of two elements
#' (wavelength and intensity).
#'
#' As a last step, a label for the point produced by `formatter` and plotted
#' using [graphics::text()]. Currently, the following `formatter`s are
#' available: \tabular{ll}{spc.label.default \tab
#' spectrum number, wavelength \cr spc.label.wlonly \tab wavelength\cr }
#' `formatter` functions receive the number of the spectrum `ispc`,
#' the wavelength `wl`, and the spectral intensity `spc` and produce
#' a character variable suitable for labelling. The predefined formatters
#' surround the label text by spaces in order to easily have an appropriate
#' offset from the point of the spectrum.
#'
#' The warning issued if no spectral point is inside the tolerance window may
#' be switched of by `warn = FALSE`. In that case, the click will produce
#' a row of `NA`s in the resulting data.frame.
#'
#' `spc.identify` uses option `debuglevel` to determine whether debugging output
#' should be produced. `debuglevel == 2` will plot the tolerance window for
#' every clicked point, `debuglevel == 1` will plot the tolerance window only
#' if no data point was inside. See [`hyperSpec options`][hyperSpec::options]
#' for details about retrieving and setting
#' options.
#'
#' You may want to adjust the plot's `ylim` to ensure that the labels are
#' not clipped. As a dirty shortcut, `xpd = NA` may help.
#'
#' @aliases spc.identify spc.label.default spc.label.wlonly spc.point.default
#'   spc.point.max spc.point.min spc.point.sqr
#'
#' @param x either the abscissa coordinates or the list returned by [plotspc()]
#'
#' @param y the ordinate values. Giving `y` will override any values from `x$y`.
#' @param wavelengths the wavelengths for the data points.
#'        Giving `wavelengths` will override any values from `x$wavelengths`.
#' @param tol.wl,tol.spc tolerance in wavelength and spectral intensity to
#'        search around the clicked point. See details.
#' @param point.fn `function(wl, spc, wlclick)` to determine the actual
#'        point to label, see details.
#' @param formatter `function(i, wl, spc)` that produces the labels.
#'        If `NULL`, no labels are displayed.
#' @param ... passed to [graphics::text()] in order to produce the
#'        labels
#' @param  cex,adj,srt see [graphics::par()]
#'
#' @param warn Should the user be warned if no point is in the considered
#'             window? In addition, see the discussion of option `debuglevel` in
#'             the details.
#'
#'             If `FALSE`, the resulting data.frame will have a row of `NA`s
#'             instead.
#'
#' @param delta `spc.point.sqr` fits the parabola in the window wlclick
#'   \eqn{\pm}{+-} delta points.
#'
#' @return a `data.frame` with columns \item{ispc}{spectra indices of the
#'   identified points, i.e. the rows of the `hyperSpec` object that was
#'   plotted.
#'
#' If `ispc` is given, `ispc [i]` is returned rather than `i`.
#'   } \item{wavelengths}{the wavelengths of the identified points}
#'   \item{spc}{the intensities of the identified points}
#' @author C. Beleites
#' @seealso [graphics::locator()], [plotspc()],
#'   [`hyperSpec options()`][hyperSpec::options]
#'
#' [map.identify()], [map.sel.poly()]
#'
#' @keywords iplot
#' @concept plotting
#' @concept plotting tools
#'
#' @rdname spc-identify
#' @export
#' @examples
#'
#' \dontrun{\donttest{
#'   ispc <- sample(nrow(laser), 10)
#'   ispc
#'
#'   identified <- spc.identify(plotspc(laser[ispc]))
#'
#'   ## convert to the "real" spectra indices
#'   ispc [identified$ispc]
#'   identified$wl
#'   identified$spc
#'
#'   ## allow the labels to be plotted into the plot margin
#'   spc.identify(plotspc(laser[ispc]), ispc = ispc, xpd = NA)
#'
#'   spc.identify(plotspc(paracetamol,
#'     xoffset = 1100,
#'     wl.range = c(600 ~ 1700, 2900 ~ 3150)
#'   ),
#'   formatter = spc.label.wlonly
#'   )
#'
#'   ## looking for minima
#'   spc.identify(plot(-paracetamol, wl.reverse = TRUE),
#'     point.fn = spc.point.min, adj = c(1, 0.5)
#'   )
#' }}
spc.identify <- function(x, y = NULL, wavelengths = NULL, ispc = NULL,
                         tol.wl = diff(range(x)) / 200,
                         tol.spc = diff(range(y)) / 50,
                         point.fn = spc.point.max, # function to find the maximum
                         formatter = spc.label.default, # NULL: suppress labels
                         ..., cex = 0.7, adj = c(0, 0.5), srt = 90, # for the label text
                         warn = TRUE) {
  if (!interactive()) {
    stop("spc.identify works only on interactive graphics devices.")
  }

  if (is.list(x)) {
    if (is.null(wavelengths)) {
      wavelengths <- x$wavelengths
    }
    if (is.null(y)) {
      y <- x$y
    }
    x <- x$x
  }

  debuglevel <- hy.getOption("debuglevel")

  if ((length(x) != length(y)) | (length(x) != length(wavelengths))) {
    stop("x, y, and wavelength need to have the same length.")
  }

  if (is.null(ispc)) {
    ispc <- row(y)
  } else {
    ispc <- ispc[row(y)]
  }

  pts <- data.frame(ispc = rep(NA, 50), wl = NA, spc = NA)
  pos <- 1

  while (!is.null(tmp <- locator(n = 1))) {
    wl <- approx(x, wavelengths, tmp$x, rule = 2)$y # return wl_min / wl_max for outside pts.

    if (debuglevel == 2L) {
      points(tmp$x, tmp$y, pch = ".", col = "red")
      rect(tmp$x - tol.wl, tmp$y - tol.spc, tmp$x + tol.wl, tmp$y + tol.spc,
        border = "red", col = NA
      )
    }

    i.window <- wavelengths >= wl - tol.wl & # window to search for the closest spectrum
      wavelengths <= wl + tol.wl &
      y >= tmp$y - tol.spc &
      y <= tmp$y + tol.spc

    if (!any(i.window)) {
      if (warn) {
        warning("No spectra in specified window.")
      } else {
        pos <- pos + 1
      }

      if (debuglevel == 1L) {
        points(tmp$x, tmp$y, pch = ".", col = "red")
        rect(tmp$x - tol.wl, tmp$y - tol.spc, tmp$x + tol.wl, tmp$y + tol.spc,
          border = "red", col = NA
        )
      }
    } else {

      ## find spectrum closest to clicked point.
      ## x and y distances are scaled according to tolerance.
      tmp <- ((wl - wavelengths[i.window]) / tol.wl)^2 +
        ((tmp$y - y[i.window]) / tol.spc)^2
      tmp <- which(i.window)[which.min(tmp)]

      pts[pos, "ispc"] <- ispc[tmp] # closest spectrum;
      # this will grow the data.frame if necessary
      # no time concern with hand-clicked points

      ## search for the max (min) of spectrum pt within tmp$x +- tol.wl
      i.window <- which(ispc == ispc[tmp] &
        wavelengths >= wl - tol.wl &
        wavelengths <= wl + tol.wl)

      pts[pos, 2:3] <- point.fn(
        wl = wavelengths[i.window],
        spc = y[i.window],
        wlclick = wl
      )

      ## label the point
      if (!is.null(formatter)) {
        lab <- formatter(pts[pos, 1], pts[pos, 2], pts[pos, 3])

        text(approx(wavelengths, x, pts[pos, 2], rule = 2),
          pts[pos, 3],
          labels = lab, cex = cex, adj = adj, srt = srt, ...
        )
      }

      pos <- pos + 1
    }
  }

  pts[seq_len(pos - 1), ]
}

#' @rdname spc-identify
#' @param wl the wavelength to label
#' @param spc the intensity to label
#' @param wlclick the clicked wavelength
#' @export
spc.point.max <- function(wl, spc, wlclick) {
  i <- which.max(spc)
  c(wl = wl[i], spc = spc[i])
}

#' @rdname spc-identify
#' @export
spc.point.default <- function(wl, spc, wlclick) {
  i <- round(approx(wl, seq_along(wl), wlclick, rule = 2)$y)
  c(wl = wl[], spc = spc[i])
}

#' @rdname spc-identify
#' @export
spc.point.min <- function(wl, spc, wlclick) {
  i <- which.min(spc)
  c(wl = wl[i], spc = spc[i])
}

#' @rdname spc-identify
#' @export
spc.point.sqr <- function(wl, spc, wlclick, delta = 1L) {
  i <- which.max(spc)

  ## points (wl [i], spc [i])
  if (i > 1L && i < length(wl)) {
    i <- i + (-delta:delta)
    i <- i %in% seq_along(wl) # make sure the indices exist

    p <- outer(wl[i], 0:2, "^") # Vandermonde matrix
    p <- qr.solve(p, spc[i])

    i <- -p[2] / p[3] / 2

    ## lines (wl, outer (wl, 0 : 2, "^") %*% p, col = "red")
    c(wl = i, spc = sum(p * c(1, i, i^2)))
  } else {
    c(wl = wl[i], spc = spc[i])
  }
}

#' @param ispc if a selection of spectra was plotted, their indices can be
#'        given in `ispc`. In this case `ispc [i]` is returned rather than `i`.
#' @param digits how many digits of the wavelength should be displayed?
#' @rdname spc-identify
#' @export
spc.label.default <- function(ispc, wl, spc, digits = 3) {
  sprintf(" %i, %s ", ispc, format(wl, digits = digits))
}

#' @rdname spc-identify
#' @export
spc.label.wlonly <- function(ispc, wl, spc, digits = 3) {
  sprintf(" %s ", format(wl, digits = digits))
}
