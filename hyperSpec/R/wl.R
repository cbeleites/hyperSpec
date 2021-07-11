#' Getting and setting the wavelength axis
#'
#' `wl()` returns the wavelength axis, `wl<-` sets it.
#'
#' The wavelength axis of a `hyperSpec` object can be retrieved and
#' replaced with `wl` and `wl<-`, respectively.
#'
#' When the wavelength axis is replaced, the colnames of `x@@data$spc` are
#' replaced by the rounded new wavelengths.  `digits` specifies the how
#' many significant digits should be used.
#'
#' There are two ways to set the label of the new wavelength axis, see the
#' examples.  If no label is given, a warning will be issued.
#'
#' @aliases wl
#' @param x a `hyperSpec` object
#' @return a numeric vector
#' @note `wl<-` always sets the complete wavelength axis, without
#'   changing the columns of the spectra matrix. If you rather want to cut the
#'   spectral range, use \code{\link[hyperSpec:extractreplace]{[}}, for
#'   interpolation along the spectral axis see
#'   [hyperSpec::spc_loess()] and for spectral binning
#'   [hyperSpec::spc_bin()].
#' @author C. Beleites
#'
#' @export
#'
#' @concept wavelengths
#'
#' @seealso [base::signif()]
#'
#' cutting the spectral range: \code{\link[hyperSpec:extractreplace]{[}}
#'
#' interpolation along the spectral axis: [hyperSpec::spc_loess()]
#'
#' spectral binning: [hyperSpec::spc_bin()]
#' @examples
#'
#' wl(laser)
wl <- function(x) {
  chk.hy(x)
  validObject(x)

  x@wavelength
}

### -----------------------------------------------------------------------------
###
###  .wl
###
###
".wl<-" <- function(x, value) {
  x@wavelength <- value
  spc <- .spc_fix_colnames(x)

  x
}

#' @rdname wl
#' @export "wl<-"
#' @aliases wl<-
#' @usage
#' wl(x, label = NULL, digits = 6) <- value
#'
#' @param value either a numeric containing the new wavelength vector, or a
#'   list with `value$wl` containing the new wavelength vector and
#'   `value$label` holding the corresponding `label`.
#' @param label The label for the new wavelength axis. See [initialize]
#'   for details.
#' @param digits handed to [base::signif()]. See details.
#' @return `hyperSpec` object
#'
#' @concept wavelengths
#'
#' @examples
#' # convert from wavelength to frequency
#' plot(laser)
#' wl(laser, "f / Hz") <- 2.998e8 * wl(laser) * 1e9
#' plot(laser)
#'
#' # convert from Raman shift to wavelength
#' # excitation was at 785 nm
#' plot(faux_cell[1])
#' wl(faux_cell) <- list(
#'   wl = 1e7 / (1e7 / 785 - wl(faux_cell)),
#'   label = expression(lambda / nm)
#' )
#' plot(faux_cell[1])
"wl<-" <- function(x, label = NULL, digits = 6, value) {
  chk.hy(x)
  validObject(x)

  if (is.list(value)) {
    label <- value$label
    value <- value$wl
  }

  .wl(x) <- value

  x@label$.wavelength <- label

  validObject(x)

  x
}
