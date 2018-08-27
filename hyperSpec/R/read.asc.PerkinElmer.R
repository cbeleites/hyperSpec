#' File import filter PerkinElmer ASCII spectra
#'
#' Imports a single spectrum in PerkinElmer's ASCII format. This function is experimental.
#'
#' @param file filename (or connection)
#' @param ... further parameters are handed to \code{\link[hyperSpec]{read.txt.long}}
#'
#' @return hyperSpec object
#' @importFrom utils packageDescription
#' @export
#'
read.asc.PerkinElmer <- function (file = stop ("filename or connection needed"), ...){
  file <- readLines(con = file)

  message ("read.asc.PerkinElmer is experimental, hyperSpec so far has no test data for PE .asc files.",
           " Please consider submitting your spectrum in an enhancement request to ", packageDescription("hyperSpec")$BugReports,
           " in order to help the development of hyperSpec.")

  ## find beginning of DATA section
  startDATA <- grep ("DATA", file)

  if (length (startDATA) != 1L)
    stop ("read.asc.PerkinElmer so far can deal with single spectra files only.",
          " Please file an enhancement request at", packageDescription("hyperSpec")$BugReports,
          " with your file as an example or contact the maintainer (",
          maintainer ("hyperSpec"), ").")

  ## Spectra values are stored
  file <- file [- seq_len(startDATA)]

  spc <- read.txt.long (textConnection(file), header = FALSE, sep = "\t", ...)

  ## consistent file import behaviour across import functions
  .fileio.optional (spc, file)
}
