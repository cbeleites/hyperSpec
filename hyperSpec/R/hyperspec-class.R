#' Class `hyperSpec`
#'
#' This package gives an interface to handle hyperspectral data sets in R.
#' Hyperspectral data are spatially or time-resolved spectra, or spectra with
#' any other kind of information associated with the spectra. E.g. spectral
#' maps or images, time series, calibration series, etc.\cr
#' The spectra can be data as obtained in XRF, UV/VIS, Fluorescence, AES, NIR,
#' IR, Raman, NMR, MS, etc.\cr
#' More generally, any data that is recorded over a discretized variable, e.g.
#' `absorbance = f(wavelength)`, stored as a vector of absorbance values for
#' discrete wavelengths is suitable.
#'
#' @aliases hyperSpec-class
#' @docType class
#' @name hyperSpec-class
#' @rdname hyperSpec-class
#' @slot wavelength wavelengths (wavenumbers, frequencies, etc.) for each of the columns of the
#' spectra matrix
#' @slot data  the data (extra data and spectra matrix)
#' @slot label expressions for column labels (incl. units). The label of the wavelength axis is in
#' the special element `.wavelength`.
#' @slot log deprecated.
#' @note Please note that the logbook is now removed.
#' @author C. Beleites
#' @seealso See the vignette "hyperspec" for an introduction to \pkg{hyperSpec}
#'   from a spectroscopic point of view.
#'
#' @keywords classes
#' @concept hyperSpec-main
#'
#' @include hy_validate.R
#' @export
#'
#' @examples
#' showClass("hyperSpec")
#' \dontrun{
#' vignette("hyperSpec")
#' }
setClass("hyperSpec",
  representation = representation(
    wavelength = "numeric", # spectral abscissa
    data = "data.frame", # data: spectra & information related to each spectrum
    label = "list", # labels and units of the stored
    log = "data.frame" # deprecated
  ),
  prototype = prototype(
    wavelength = numeric(0),
    data = data.frame(spc = I(matrix(NA, 0, 0))),
    label = list(.wavelength = NULL, "spc" = NULL)
  ),
  validity = .validate
)
