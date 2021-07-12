#' @name DEPRECATED-read.asc.Andor
#' @concept moved to hySpc.read.txt
#'
#' @title (DEPRECATED)
#'        Import Raman spectra/maps from Andor cameras/solis ASCII files
#'
#' @description
#' These data input functions are **deprecated** and they will be removed in
#' the next release of \pkg{hyperspec} package.
#' Now functions in package \pkg{hySpc.read.txt}
#' ([link](https://r-hyperspec.github.io/hySpc.read.txt/reference/index.html))
#' should be used as the alternatives.
#'
#' @details
#'
#' `read.asc.Andor()` reads Andor Solis ASCII (`.asc`) files where the first
#'  column gives the wavelength axes and the other columns the spectra.
#'
#' @param file filename or connection to ASCII file
#' @param ...,quiet,dec,sep handed to [base::scan()]
#' @return a hyperSpec object
#' @author Claudia Beleites
#' @seealso `vignette ("fileio")` for more information on file import and
#'
#' [options()] for details on options.
#' @include DEPRECATED-read.txt.Witec.R
#' @include spc_io_postprocess_optional.R
#'
#'
#' @export
read.asc.Andor <- function(file = stop("filename or connection needed"),
                           ..., quiet = TRUE, dec = ".", sep = ",") {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## check for valid data connection
  .check.con(file = file)

  ## read spectra
  tmp <- readLines(file)
  nwl <- length(tmp)
  txt <- scan(text = tmp, dec = dec, sep = sep, quiet = quiet, ...)

  dim(txt) <- c(length(txt) / nwl, nwl)

  ## fix: Andor Solis may have final comma without values
  if (all(is.na(txt [nrow(txt), ]))) {
    txt <- txt [-nrow(txt), ]
  }

  spc <- new("hyperSpec", wavelength = txt [1, ], spc = txt [-1, ])

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}

hySpc.testthat::test(read.asc.Andor) <- function() {
  context("read.asc.Andor")
  test_that("Andor Solis .asc text files", {
    skip_if_not_fileio_available()
    expect_known_hash(read.asc.Andor("fileio/asc.Andor/ASCII-Andor-Solis.asc"), "9ead937f51")
  })
}
