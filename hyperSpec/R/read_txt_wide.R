### -----------------------------------------------------------------------------
###
###  read_txt_wide
###
###  Format:
###  x y ... int (wl1)  int (wl2) ... int (wl p) z ...
###

#' Import/export of `hyperSpec` objects to/from ASCII files
#'
#' @details
#' A detailed discussion of \pkg{hyperSpec}'s file import and export
#' capabilities is given in vignette `fileio`.
#'
#' Besides [base::save()] and [base::load()], two general ways to import and
#' export data into `hyperSpec` objects exist.
#'
#' Firstly, hyperSpec objects can be imported  and exported as ASCII files.
#'
#'   A second option is using the package [R.matlab::R.matlab()]
#'   which provides the functions [R.matlab::readMat()] and
#'   [R.matlab::writeMat()].
#'
#'   \pkg{hyperSpec} comes with a number of pre-defined functions to import
#'   manufacturer specific file formats. For details, see `vignette("fileio")`.
#'
#'   [hyperSpec::read.spc()] imports Thermo Galactic's .spc file
#'   format, and ENVI files may be read using
#'   [hyperSpec::read.ENVI()].
#'
#' These functions are very flexible and provide lots of arguments.
#'
#' If you use them to read or write manufacturer specific ASCII formats,
#' please consider writing a wrapper function and contributing this
#' function to \pkg{hyperSpec}.  An example is in the \dQuote{flu} vignette
#' (see `vignette("flu", package = "hyperSpec"`).
#'
#' Note that R accepts many packed formats for ASCII files, see
#' [base::connections()]. For .zip files, see [utils::unzip()].
#'
#' For further information, see the examples below, `vignette("fileio")` and the documentation
#' of [R.matlab::R.matlab()].
#' @seealso `vignette("fileio")`
#' @aliases read_txt_wide
#' @rdname read_txt
#' @param check.names handed to [utils::read.table()]. Make sure this is `FALSE`, if
#' the column names of the spectra are the wavelength values.
#'
#' @concept io
#'
#' @export
#' @importFrom utils read.table head
read_txt_wide <- function(file = stop("file is required"),
                          cols = list(
                            spc = "I / a.u.",
                            .wavelength = expression(lambda / nm)
                          ),
                          sep = "\t",
                          row.names = NULL,
                          check.names = FALSE,
                          ...) {
  .wavelength <- match(".wavelength", names(cols))
  if (is.na(.wavelength)) {
    cols <- as.list(c(cols, .wavelength = expression(lambda / nm)))
  } else
  if (.wavelength != length(cols)) { # .wavelength should be at the end of cols
    cols <- cols[c(seq_along(cols)[-.wavelength], .wavelength)]
  }

  ## columns containing the spectra
  spc <- match("spc", names(cols))
  if (is.na(spc)) {
    stop("cols$spc must exist.")
  }

  txtfile <- read.table(
    file = file, check.names = check.names, row.names = row.names,
    sep = sep, ...
  )

  ispc <- 0:(ncol(txtfile) - length(cols) + 1) + spc

  spc.data <- as.matrix(txtfile[, ispc])
  txtfile <- txtfile[, -ispc, drop = FALSE]

  ## enforce colnames given by cols
  colnames(txtfile) <- head(names(cols)[-spc], -1)

  spc <- new("hyperSpec", spc = spc.data, data = txtfile, labels = cols)

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, filename = file)
}
