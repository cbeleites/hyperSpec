#' @name DEPRECATED-read.txt.Horiba
#' @concept moved to hySpc.read.txt
#'
#' @title (DEPRECATED)
#'        Import Horiba Labspec exported ASCII files
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
#' Read ASCII (`.txt`) files exported by Horiba's Labspec software
#' (LabRAM spectrometers)
#'
#' `read.txt.Horiba.xy` reads maps, i.e. `.txt` files where the first two columns give x and y coordinates.
#'
#' @param file connection (file name and path) to the `.txt` file
#' @param cols,header,sep,row.names,check.names,... further parameters are handed over to [hyperSpec::read.txt.wide()]
#' @author C. Beleites
#' @return hyperSpec object
#'
#' @export
read.txt.Horiba <- function(file, cols = c(
                              spc = "I / a.u.",
                              .wavelength = expression(Delta * tilde(nu) / cm^-1)
                            ),
                            header = TRUE, sep = "\t", row.names = NULL,
                            check.names = FALSE, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt(new = "read_txt_Horiba")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spc <- read.txt.wide(file,
    cols = cols,
    header = header, sep = sep, row.names = row.names,
    check.names = check.names, ...
  )

  ## consistent file import behaviour across import functions
  ## is already provided by read.txt.wide

  spc
}

#' @rdname DEPRECATED-read.txt.Horiba
#' @export
#'
# @concept io
#' @concept moved to hySpc.read.txt
#'
read.txt.Horiba.xy <- function(file, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  read.txt.Horiba(
    file = file,
    cols = c(
      x = expression(x / mu * m),
      y = expression(y / mu * m),
      spc = "I / a.u.",
      .wavelength = expression(Delta * tilde(nu) / cm^-1)
    ),
    ...
  )
}

#' @rdname DEPRECATED-read.txt.Horiba
#'
#' @details
#' `read.txt.Horiba.t`  reads time series, i.e. .txt files with the time in the first column
#'
#' @export
#'
# @concept io
#' @concept moved to hySpc.read.txt
#'
read.txt.Horiba.t <- function(file, header = TRUE, sep = "\t", row.names = NULL,
                              check.names = FALSE, ...) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  read.txt.Horiba(file,
    cols = c(
      t = "t / s",
      spc = "I / a.u.",
      .wavelength = expression(Delta * tilde(nu) / cm^-1)
    ),
    ...
  )
}
