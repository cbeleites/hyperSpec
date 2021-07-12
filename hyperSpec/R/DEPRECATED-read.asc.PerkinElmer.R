#' @name DEPRECATED-read.asc.PerkinElmer
#' @concept moved to hySpc.read.txt
#'
#' @title (DEPRECATED)
#'        File import filter PerkinElmer ASCII spectra
#'
#' @description
#'
#' These data input functions are **deprecated** and they will be removed in
#' the next release of \pkg{hyperspec} package.
#' Now functions in package \pkg{hySpc.read.txt}
#' ([link](https://r-hyperspec.github.io/hySpc.read.txt/reference/index.html))
#' should be used as the alternatives.
#'
#' @details
#'
#' Imports a single spectrum in PerkinElmer's ASCII format.
#' This function is experimental.
#'
#' @param file filename (or connection)
#' @param ... further parameters are handed to [hyperSpec::read.txt.long()]
#'
#' @return hyperSpec object
#' @importFrom utils packageDescription
#' @export
#'

read.asc.PerkinElmer <- function(file = stop("filename or connection needed"), ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  content <- readLines(con = file)

  message(
    "read.asc.PerkinElmer is experimental, hyperSpec so far has no test data for PE .asc files.",
    " Please consider submitting your spectrum in an enhancement request to ", packageDescription("hyperSpec")$BugReports,
    " in order to help the development of hyperSpec."
  )

  ## find beginning of DATA section
  startDATA <- grep("DATA", content)

  if (length(startDATA) != 1L) {
    stop(
      "read.asc.PerkinElmer so far can deal with single spectra files only.",
      " Please file an enhancement request at", packageDescription("hyperSpec")$BugReports,
      " with your file as an example or contact the maintainer (",
      maintainer("hyperSpec"), ")."
    )
  }

  ## Spectra values are stored
  content <- content[-seq_len(startDATA)]

  spc <- read.txt.long(textConnection(content), header = FALSE, sep = "\t", ...)
  spc$filename <- NULL # not meaningful due to textConnection use

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}
