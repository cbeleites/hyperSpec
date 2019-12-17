##' Import Raman Spectra/Maps from Andor Cameras/Solis ASCII files
##'
##' \code{read.asc.Andor} reads Andor Solis ASCII (\code{.asc}) files where the first column gives the wavelength
##' axes and the other columns the spectra.
##'
##' @title File Import Andor Solis
##' @param file filename or connection to ASCII file
##' @param ...,quiet,dec,sep handed to \code{\link[base]{scan}}
##' @return a hyperSpec object
##' @author Claudia Beleites
##' @seealso \code{vignette ("fileio")} for more information on file import and
##'
##' \code{\link{options}} for details on options.
##' @include read.txt.Witec.R
##' @include fileio.optional.R
##' @export
read.asc.Andor <- function (file = stop ("filename or connection needed"),
                            ..., quiet = TRUE, dec = ".", sep = ","){

  ## check for valid data connection
  .check.con (file = file)

  ## read spectra
  tmp <- readLines (file)
  nwl <- length (tmp)
  txt <- scan (text = tmp, dec = dec, sep = sep, quiet = quiet, ...)

  dim (txt) <- c (length (txt) / nwl, nwl)

  ## fix: Andor Solis may have final comma without values
  if (all (is.na (txt [nrow (txt), ])))
    txt <- txt [- nrow (txt), ]

  spc <- new ("hyperSpec", wavelength = txt [1, ], spc = txt [-1, ])

   ## consistent file import behaviour across import functions
  .fileio.optional (spc, file)
}

.test (read.asc.Andor) <- function (){
  context ("read.asc.Andor")
  test_that("Andor Solis .asc text files", {
    skip_if_not_fileio_available()
    expect_known_hash (read.asc.Andor("fileio/asc.Andor/ASCII-Andor-Solis.asc"), "9ead937f51")
  })
}
