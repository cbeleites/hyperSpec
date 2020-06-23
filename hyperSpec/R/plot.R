### -----------------------------------------------------------------------------
###
###  plot methods
###

### -----------------------------------------------------------------------------
###
### .plot: main switchyard for plotting functions
###
#' @importFrom utils modifyList
.plot <- function(x, y, ...) {
  ##    'spc'        ... spectra
  ##    'map'        ... map
  ##    'voronoi'    ... voronoi tiled map
  ##    'mat'        ... spectra matrix
  ##    'c'          ... concentration: plotc
  ##    'ts'         ... time series: plotc
  ##    'depth'      ... concentration or time series
  ##    'spcmeansd'  ... mean spectrum +- 1 standard deviation
  ##    'spcprctile' ... median spectrum , 16th and 84th percentile
  ##    'spcprctl5'  ... spcprctile plus 5th and 95th percentile

  dots <- list(...) # to allow optional argument checks

  if (missing(y)) {
    stop("second argument to plot is missing. Should be a character indicating the type of plot.")
    y <- "spc"
  }

  switch(tolower(y),

    spc = plotspc(x, ...),

    spcmeansd = {
      dots <- modifyList(
        list(object = mean_pm_sd(x), fill = c(1, NA, 1)),
        dots
      )

      do.call(plotspc, dots)
    },

    spcprctile = {
      dots <- modifyList(
        list(object = quantile(x, probs = c(0.16, 0.5, 0.84)), fill = c(1, NA, 1)),
        dots
      )

      do.call(plotspc, dots)
    },

    spcprctl5 = {
      dots <- modifyList(
        list(object = quantile(x, probs = c(0.05, 0.16, 0.5, 0.84, 0.95)), fill = c(1, 2, 3, 2, 1), fill.col = c("#00000040")),
        dots
      )

      do.call(plotspc, dots)
    },

    map = plotmap(x, ...),

    voronoi = plotvoronoi(x, ...),

    mat = plotmat(x, ...),

    c = plotc(x, ...),

    ts = plotc(x, spc ~ t, ...),

    depth = plotc(x, spc ~ z, ...),

    stop(paste("y = ", y, "unknown.", collapse = " "))
  )
}

#' @noRd
#' @export
setGeneric("plot")

#' Plotting `hyperSpec` Objects.
#'
#' The `plot` method for `hyperSpec` objects is a switchyard to [plotspc()],
#' [plotmap()], and [plotc()].
#'
#' It also supplies some convenient abbrevations for much used plots.
#'
#' If `y` is missing, `plot` behaves like `plot (x, y = "spc")`.
#'
#' Supported values for `y` are:
#'
#' \describe{ \item{"spc"}{calls [plotspc()] to produce a spectra
#' plot.}
#'
#' \item{"spcmeansd"}{plots mean spectrum +/- one standard deviation}
#'
#' \item{"spcprctile"}{plots 16th, 50th, and 84th percentile spectre. If the
#' distributions of the intensities at all wavelengths were normal, this would
#' correspond to `"spcmeansd"`. However, this is frequently not the case.
#' Then `"spcprctile"` gives a better impression of the spectral data
#' set.}
#'
#' \item{"spcprctl5"}{like `"spcprctile"`, but additionally the 5th and
#' 95th percentile spectra are plotted.}
#'
#' \item{"map"}{calls [plotmap()] to produce a map plot.}
#'
#' \item{"voronoi"}{calls [plotvoronoi()] to produce a Voronoi plot
#' (tesselated plot, like "map" for hyperSpec objects with uneven/non-rectangular
#' grid).}
#'
#' \item{"mat"}{calls [plotmat()] to produce a plot of the spectra
#' matrix (not to be confused with [graphics::matplot()]).}
#'
#' \item{"c"}{calls [plotc()] to produce a calibration (or time
#' series, depth-profile, or the like)}
#'
#' \item{"ts"}{plots a time series: abbrevation for `[plotc] (x,
#' use.c = "t")`}
#'
#' \item{"depth"}{plots a depth profile: abbrevation for `[plotc]
#' (x, use.c = "z")`} }
#'
#' @name plot-methods
#' @rdname plot
#' @aliases plot plot,ANY,ANY-method plot,hyperSpec,character-method
#'   plot,hyperSpec,missing-method
#' @docType methods
#' @param x the `hyperSpec` object
#' @param y selects what plot should be produced
#' @param ... arguments passed to the respective plot function
#' @author C. Beleites
#' @seealso [plotspc()] for spectra plots (intensity over
#'   wavelength),
#'
#' [plotmap()] for plotting maps, i.e. color coded summary value on
#'   two (usually spatial) dimensions.
#'
#' [plotc()]
#'
#' [graphics::plot()]
#' @keywords methods hplot
#' @export
#' @examples
#'
#' plot(flu)
#'
#' plot(flu, "c")
#'
#' plot(laser, "ts")
#'
#' spc <- apply(faux_cell, 2, quantile, probs = 0.05)
#' spc <- sweep(faux_cell, 2, spc, "-")
#' plot(spc, "spcprctl5")
#' plot(spc, "spcprctile")
#' plot(spc, "spcmeansd")
#'
#' ### use plotspc as default plot function
setMethod(
  "plot",
  signature(x = "hyperSpec", y = "missing"),
  function(x, y, ...) plotspc(x, ...)
)

### allow choice of spectral or map plot by second argument
#' @rdname plot
#' @export
setMethod(
  "plot",
  signature(x = "hyperSpec", y = "character"), .plot
)
