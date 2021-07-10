#' Quinine fluorescence spectra
#'
#' Fluorescence spectra of different dilutions of quinine forming a
#' calibration set.
#'
#' See the vignette: `vignette("flu", package = "hyperSpec")`
#'
#' @name flu
#' @docType data
#' @format The data set has 6 fluorescence emission spectra measured on
#'   quinine concentrations between 0.05 mg/l and 0.30 mg/l.  Each spectrum
#'   consists of 181 data points in the range of 405 nm to 495 nm.
#' @author M. Kammer and C. Beleites
#' @keywords datasets
#' @concept datasets
#' @include wl2i.R
#' @examples
#'
#' flu
#'
#' plot(flu)
#'
#' plotc(flu)
NULL
