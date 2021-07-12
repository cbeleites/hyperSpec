#' @name DEPRECATED-read.txt.Witec
#' @concept moved to hySpc.read.txt
#'
#' @title (DEPRECATED)
#'        Import Raman spectra from Witec files
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
#' Import Raman Spectra/Maps from Witec Instrument via ASCII files
#'
#' - `read.txt.Witec()` reads Witec ASCII files where the first column gives the
#'  wavelength axes and the other columns the spectra.
#' - `read.dat.Witec()` reads Witec's ASCII exported data, which comes in separate
#'  files with x and y data.
#'
#' @param file filename or connection to ASCII file
#' @param points.per.line number of spectra in x direction of the map
#' @param lines.per.image number of spectra in y direction
#' @param type type of spectra: `single` for single spectra (including time series), `map` for imaging data.
#' @param hdr.label WITec Project exports the spectra names (contain information of map position or number of spectra) within the `file`.
#' @param hdr.units WITec Project exports the spectra units within the `file`.
#' @param encoding character encoding, see [base::readLines()]
#' @param ...,quiet handed to [base::scan()]
#'
#' @return a hyperSpec object
#'
#' @author Claudia Beleites and Marcel Dahms
#'
#' @seealso `vignette("fileio")` for more information on file import
#'
#' [options()] for details on options.
#' @export
#'
#' @importFrom utils head
read.txt.Witec <- function(file = NULL,
                           points.per.line = NULL,
                           lines.per.image = NULL,
                           type = c("single", "map"),
                           hdr.label = FALSE,
                           hdr.units = FALSE,
                           encoding = "unknown",
                           ...,
                           quiet = TRUE) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt()

  if (is.null (file)) return (NA) # allow checking for deprecation w/o failure

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## check for valid data connection
  .check.con(file = file)

  ## check for valid input
  type <- .check.valid(type, hdr = NULL, points.per.line, lines.per.image)

  ## manage possible header lines by export function 'Table' in WITec Control/Project (version 4)
  skip <- hdr.label + hdr.units

  ## read spectra
  tmp <- readLines(file, encoding = encoding)
  nwl <- length(tmp) - skip
  txt <- scan(text = tmp, skip = skip, quiet = quiet, encoding = encoding, ...)

  dim(txt) <- c(length(txt) / nwl, nwl)

  hdr <- head(tmp, skip)

  ## fix: Witec/Andor may have final comma without values
  if (all(is.na(txt [nrow(txt), ]))) {
    txt <- txt [-nrow(txt), ]
  }

  spc <- new("hyperSpec", wavelength = txt [1, ], spc = txt [-1, ])

  ## add header information
  if (hdr.label | hdr.units) {
    spc <- .parse.hdr(spc, hdr, hdr.label)
  }

  ## add map information
  if (type == "map") {
    spc <- .parse.xy(spc, hdr, hdr.label, points.per.line, lines.per.image)
  }

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}

hySpc.testthat::test(read.txt.Witec) <- function() {
  context("read.txt.Witec")

  test_that("deprecated", expect_warning(read.txt.Witec (), "deprecated"))
}

#' @rdname DEPRECATED-read.txt.Witec
#' @concept moved to hySpc.read.txt
#'
#' @param filex filename wavelength axis file
#' @param filey filename intensity file
#'
#' @export
read.dat.Witec <- function(filex = NULL,
                           filey = sub("-x", "-y", filex),
                           points.per.line = NULL,
                           lines.per.image = NULL,
                           type = c("single", "map"),
                           encoding = "unknown",
                           ...,
                           quiet = hy.getOption("debuglevel") < 1L) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt()

  if (is.null (filex)) return (NA) # allow checking for deprecation w/o failure

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## check valid data connection
  .check.con(filex = filex, filey = filey)

  ## check valid input
  type <- .check.valid(
    type = type, points.per.line = points.per.line,
    lines.per.image = lines.per.image
  )

  ## read data
  wl <- scan(file = filex, ..., quiet = quiet, encoding = encoding)
  spc <- scan(file = filey, ..., quiet = quiet, encoding = encoding)

  dim(spc) <- c(length(wl), length(spc) / length(wl))

  spc <- new("hyperSpec", wavelength = wl, spc = t(spc))

  ## add map information
  if (type == "map") {
    spc <- .parse.xy(spc = spc, points.per.line = points.per.line, lines.per.image = lines.per.image)
  }

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, filey)
}

hySpc.testthat::test(read.dat.Witec) <- function() {
  context("read.dat.Witec")

  test_that("deprecated", expect_warning(read.dat.Witec (), "deprecated"))

}


#' @rdname DEPRECATED-read.txt.Witec
#' @concept moved to hySpc.read.txt
#'
#' @param headerfile filename or connection to ASCII file with header information
#'
#' @export
read.txt.Witec.Graph <- function(headerfile = NULL,
                                 filex = gsub("Header", "X-Axis", headerfile),
                                 filey = gsub("Header", "Y-Axis", headerfile),
                                 type = c("single", "map"), encoding = "unknown",
                                 ..., quiet = TRUE) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt()

  if (is.null (headerfile)) return (NA) # allow checking for deprecation w/o failure

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## check for valid data connection
  .check.con(headerfile, filex, filey)

  ## processing headerfile
  hdr <- read.ini(headerfile, skip = 1, encoding = encoding)
  hdr <- sapply(hdr, function(x) unlist(x, recursive = FALSE)) # returns a matrix with colnames and rownames for better adressing

  ## check valid input
  type <- .check.valid(
    type = type, hdr = hdr,
    ...
  )

  ## read spectra and header
  wl <- scan(filex, quiet = quiet, encoding = encoding)
  nwl <- length(wl)

  txt <- scan(filey, quiet = quiet, encoding = encoding)
  dim(txt) <- c(nwl, length(txt) / nwl)

  spc <- new("hyperSpec", wavelength = wl, spc = t(txt))

  ## cross validation of parameters and information provided by header file
  if (nwl != hdr["SizeGraph", ]) {
    stop(paste("length of wavelength axis in file '", filex,
      "' differs from 'SizeGraph' in header file '", headerfile, "'",
      sep = ""
    ))
  }

  ## add header information
  spc <- .parse.hdr(spc, hdr)

  ## add map information
  if (type == "map") {
    spc <- .parse.xy(spc, hdr, ...)
  }

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, filex)
}

hySpc.testthat::test(read.txt.Witec.Graph) <- function() {
  context("read.txt.Witec.Graph")

  test_that("deprecated", expect_warning(read.txt.Witec.Graph (), "deprecated"))
}

### -------- helpers ------------------------

### checking file connection
.check.con <- function(headerfile, filex, filey, file) {
  ## check for valid data connection
  if (!missing(headerfile) && !file.exists(headerfile)) {
    stop("Header file not found!")
  }

  if (!missing(filex) && !file.exists(filex)) {
    stop("Wavelength axis file not found!")
  }

  if (!missing(filey) && !file.exists(filey)) {
    stop("Intensity file not found!")
  }

  if (!missing(file) && !file.exists(file)) {
    stop("Spectra file not found!")
  }
}

### checking for valid input
.check.valid <- function(type, hdr, points.per.line, lines.per.image) {
  ## check valid input
  type <- match.arg(type, c("single", "map"))

  if (type == "single" && !missing(points.per.line) && !is.null(points.per.line) && points.per.line != 1) { # TODO: better to prove for values > 1?
    warning("points.per.line != 1 given for single spectrum")
  }

  if (type == "single" && !missing(lines.per.image) && !is.null(lines.per.image) && lines.per.image != 1) { # TODO: see above
    warning("lines.per.image != 1 are defined for single spectrum")
  }

  if (type == "single" && !missing(hdr) && !is.null(hdr) && hdr ["SizeY", ] != 1) {
    warning("header provides spatial information in y direction for single spectra")
  }

  return(type)
}

### parsing header information
.parse.hdr <- function(spc, hdr, hdr.label) {
  if (!missing(hdr) && !missing(hdr.label)) {
    hdr <- strsplit(hdr, "\t")

    if (length(hdr) == 2) {
      spc@data$spcname <- hdr[[1]][-1]
      labels(spc, ".wavelength") <- hdr[[2]] [1]
      labels(spc, "spc") <- unique(hdr[[2]] [-1])
    } else if (length(hdr) == 1 && hdr.label) {
      spc@data$spcname <- hdr[[1]][-1]
    } else {
      labels(spc, ".wavelength") <- hdr[[1]] [1]
      labels(spc, "spc") <- unique(hdr[[1]] [-1])
    }
  }

  if (!missing(hdr) && missing(hdr.label)) {
    spc@data$spcname <- hdr ["GraphName", ]
    if ("FileName" %in% rownames(hdr)) {
      spc@data$WIPname <- hdr ["FileName", ]
    }
    labels(spc, "spc") <- hdr ["DataUnit", ]
  }
  return(spc)
}

### parsing map information
.parse.xy <- function(spc, hdr, hdr.label, points.per.line, lines.per.image, ...) {

  ## set points.per.line and lines.per.image, if at least one is set unequal NULL
  if (xor(
    !missing(points.per.line) && !is.null(points.per.line),
    !missing(lines.per.image) && !is.null(lines.per.image)
  )) {
    if ((missing(points.per.line) || is.null(points.per.line)) &&
      !is.null(lines.per.image)) {
      points.per.line <- nrow(spc) / lines.per.image
    } else {
      lines.per.image <- nrow(spc) / points.per.line
    }
  } else if (!missing(points.per.line) && !missing(lines.per.image) &&
    is.null(points.per.line) && is.null(points.per.line) &&
    !missing(hdr.label) && hdr.label) { # TODO: only read, if not yet calculated?
    x <- sub("^.*\\(([[:digit:]]+)/[[:digit:]]+\\)$", "\\1", hdr[1])
    y <- sub("^.*\\([[:digit:]]+/([[:digit:]]+)\\)$", "\\1", hdr[1])
    points.per.line <- as.numeric(x) + 1
    lines.per.image <- as.numeric(y) + 1
  } else if ((missing(points.per.line) || missing(lines.per.image)) &&
    !missing(hdr) && missing(hdr.label)) { # TODO: only read, if not yet calculated?
    points.per.line <- as.numeric(hdr ["SizeX", ])
    lines.per.image <- as.numeric(hdr ["SizeY", ])
  } else if (is.null(points.per.line) && is.null(lines.per.image)) {
    warning("no spatial information provided")
    return(spc)
  }

  if (points.per.line * lines.per.image == nrow(spc)) {
    spc@data$x <- rep(seq_len(points.per.line), lines.per.image)
    spc@data$y <- rep(-seq_len(lines.per.image), each = points.per.line)
  } else {
    warning("number of spectra and number of points in map are not equal!")
  }

  return(spc)
}
