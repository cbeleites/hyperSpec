#' @name hyperSpec-package
#' @title Package "hyperSpec": interface for hyperspectral datasets
#' @description
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
#' @docType package
#' @author C. Beleites
#'
#' Maintainer: Claudia Beleites <claudia.beleites@@chemometrix.eu>
#' @seealso
#'
#' - `citation("hyperSpec")` produces the correct citation.
#' - `package?hyperSpec` for information about the package.
#' - `class?hyperSpec` for details on the S4 class provided by this package.
#'
#' @rdname hyperSpec-package
#' @include flu.R
#' @include faux_cell.R
#' @include laser.R
#' @include paracetamol.R
#' @include barbiturates.R

#'
#' @keywords package
#' @concept hyperSpec-main

NULL
