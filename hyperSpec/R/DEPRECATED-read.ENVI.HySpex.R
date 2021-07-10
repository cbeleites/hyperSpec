#' @describeIn  DEPRECATED-read.ENVI
#' @include DEPRECATED-read.ENVI.R
#' @export

read.ENVI.HySpex <- function(file = stop("read.ENVI.HySpex: file name needed"),
                             headerfile = NULL, header = list(), keys.hdr2data = NULL, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_envi()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  headerfile <- .find.ENVI.header(file, headerfile)
  keys <- readLines(headerfile)
  keys <- .read.ENVI.split.header(keys)
  keys <- keys [c("pixelsize x", "pixelsize y", "wavelength units")]

  header <- modifyList(keys, header)

  ## most work is done by read.ENVI
  spc <- read.ENVI(file = file, headerfile = headerfile, header = header, ..., pull.header.lines = FALSE)

  label <- list(
    x = "x / pixel",
    y = "y / pixel",
    spc = "I / a.u.",
    .wavelength = as.expression(bquote(lambda / .(u), list(u = keys$`wavelength units`)))
  )

  labels(spc) <- label

  spc
}

hySpc.testthat::test(read.ENVI.HySpex) <- function() {
  context("read.ENVI.HySpex")

  test_that("Hyspex ENVI file", {
    skip_if_not_fileio_available()
    expect_known_hash(
      read.ENVI.HySpex("fileio/ENVI/HySpexNIR.hyspex"),
      "cf35ba92334f22513486f25c5d8ebe32")
  })
}
