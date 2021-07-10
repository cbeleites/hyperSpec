#' Barbiturates spectra from `.spc` example files
#'
#' A time series of mass spectra in a list of `hyperSpec` objects.
#'
#'
#' @name barbiturates
#' @docType data
#' @format The data sets consists of a list of 5 hyperSpec objects with a single
#'   spectrum each. They are the first five spectra of the BARBITUATES.SPC
#'   example data from Thermo Galactic's spc file format specification.
#'
#' @author C. Beleites and Thermo Galactic
#' @references The raw data is available with package **hySpc.read.spc**.
#' @keywords datasets
#' @concept datasets
#' @examples
#'
#' barbiturates
#' length(barbiturates)
#'
#' barb <- collapse(barbiturates, collapse.equal = FALSE)
#' barb <- wl_sort(barb)
#'
#' plot(barb,
#'   lines.args = list(type = "h"),
#'   col = palette_matlab_dark(5), stacked = TRUE,
#'   stacked.args = list(add.factor = .2)
#' )
#'
#' if (require(latticeExtra)) {
#'   levelplot(spc ~ .wavelength * z, log(barb),
#'     panel = panel.levelplot.points,
#'     cex = 0.3, col = "#00000000", col.regions = palette_matlab(20)
#'   )
#' }
#'
#' plotc(apply(barb[, , 42.9 ~ 43.2], 1, sum, na.rm = TRUE), spc ~ z,
#'   panel = panel.lines, ylab = expression(I[m / z == 43] / "a.u.")
#' )
"barbiturates"
