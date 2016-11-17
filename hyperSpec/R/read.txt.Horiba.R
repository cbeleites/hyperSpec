##' Read ASCII (.txt) files exported by Horiba's Labspec software (LabRAM spectrometers)
##'
##' \code{read.txt.Horiba.xy} reads maps, i.e. .txt files where the first two columns give x and y coordinates.
##'
##' @title Import Horiba Labspec exported ASCII files
##' @param file connection (file name and path) to the .txt file
##' @param cols,header,sep,row.names,check.names,... further parameters are handed over to \code{\link[hyperSpec]{read.txt.wide}}
##' @rdname read.txt.Horiba
##' @author C. Beleites
##' @return hyperSpec object
##' @export
read.txt.Horiba <- function (file, cols = c (spc = "I / a.u.",
                                             .wavelength = expression (Delta*tilde(nu) / cm^-1)),
                             header = TRUE, sep = "\t", row.names = NULL,
                             check.names = FALSE, ...){
  spc <- read.txt.wide (file, cols = cols,
                       header = header, sep = sep, row.names = row.names,
                       check.names = check.names, ...)

  ## consistent file import behaviour across import functions
  ## is already provided by read.txt.wide
  
  spc
}

##' @rdname read.txt.Horiba
##' @export
read.txt.Horiba.xy <- function (file, ...){
  read.txt.Horiba (file = file,
                    cols = c (x = expression (x / mu*m),
                              y = expression (y / mu*m),
                              spc = "I / a.u.",
                              .wavelength = expression (Delta*tilde(nu) / cm^-1)),
                       ...)
}

##' \code{read.txt.Horiba.t}  reads time series, i.e. .txt files with the time in the first column
##' @rdname read.txt.Horiba
##' @export
read.txt.Horiba.t <- function (file, header = TRUE, sep = "\t", row.names = NULL,
                                 check.names = FALSE, ...){
  read.txt.Horiba (file,
                   cols = c (t = "t / s",
                             spc = "I / a.u.",
                             .wavelength = expression (Delta*tilde(nu) / cm^-1)),
                    ...)
}

