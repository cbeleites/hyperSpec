##' Import Raman Spectra/Maps from Witec Instrument via ASCII files
##'
##' \code{read.txt.Witec} reads Witec ASCII files where the first column gives the wavelength
##' axes and the other columns the spectra. \code{read.dat.Witec} reads Witec's ASCII exported data
##' which comes in separate files with x and y data.
##' @title File Import Witec Raman
##' @param file filename or connection to ASCII file
##' @param points.per.line number of spectra in x direction of the map
##' @param lines.per.image number of spectra in y direction
##' @param nwl is deprecated and will be removed soon. Number of wavelengths is calculated automatically.
##' @param remove.zerospc is deprecated and will be removed soon. Use \code{\link{hy.setOptions} (file.remove.emptyspc = TRUE)} instead.
##' @param type type of spectra: \code{single} for single spectra (including time series), \code{map} for imaging data.
##' @param hdr.label WITec Project exports the spectra names (contain information of map position or number of spectra) within the \code{file}.
##' @param hdr.units WITec Project exports the spectra units within the \code{file}.
##' @param encoding character encoding, see \code{\link[base]{readLines}}
##' @param ...,quiet handed to \code{\link[base]{scan}}
##' @return a hyperSpec object
##' @author Claudia Beleites and Marcel Dahms
##' @seealso \code{vignette ("fileio")} for more information on file import and
##'
##' \code{\link{options}} for details on options.
##' @export
##' @importFrom utils head
read.txt.Witec <- function (file = stop ("filename or connection needed"),
                            points.per.line = NULL,
                            lines.per.image = NULL,
                            type = c ("single", "map"),
                            hdr.label = FALSE,
                            hdr.units = FALSE,
                            encoding = "unknown",
                            ...,
                            quiet = TRUE){

    ## check for valid data connection
    .check.con (file = file)

    ## check for valid input
    type <- .check.valid (type, hdr = NULL, points.per.line, lines.per.image)

    ## manage possible header lines by export function 'Table' in WITec Control/Project (version 4)
    skip <- hdr.label + hdr.units

    ## read spectra
    tmp <- readLines (file, encoding = encoding)
    nwl <- length (tmp) - skip
    txt <- scan (text = tmp, skip = skip, quiet = quiet, encoding = encoding, ...)

    dim (txt) <- c (length (txt) / nwl, nwl)

    hdr <- head (tmp, skip)

    ## fix: Witec/Andor may have final comma without values
    if (all (is.na (txt [nrow (txt), ])))
        txt <- txt [- nrow (txt), ]

    spc <- new ("hyperSpec", wavelength = txt [1, ], spc = txt [-1, ])

    ## add header information
    if (hdr.label | hdr.units)
        spc <- .parse.hdr (spc, hdr, hdr.label)

    ## add map information
    if (type == "map")
        spc <- .parse.xy (spc, hdr, hdr.label, points.per.line, lines.per.image)

    ## consistent file import behaviour across import functions
    .fileio.optional (spc, file)
}

.test (read.txt.Witec) <- function (){
  context ("read.txt.Witec")

  test_that("Map with neither header nor label lines", {
    skip_if_not_fileio_available()
    expect_error (suppressWarnings (read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt",
                                                   type = "map", hdr.units = TRUE, hdr.label = TRUE)
                                    ))
    expect_warning (read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map"))

    spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", points.per.line = 5, lines.per.image = 5)
    expect_known_hash (spc, hash = "6816a87cf3")
  })

  test_that("Map: one of points.per.line and lines.per.image is sufficient", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", lines.per.image = 5)
    expect_known_hash (spc, hash = "6816a87cf3")

    spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt", type = "map", points.per.line = 5)
    expect_known_hash (spc, hash = "6816a87cf3")
  })

  test_that("Map with label line but no units header", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_label.txt", type = "map", hdr.units = FALSE, hdr.label = TRUE)
    expect_known_hash(spc, "c4a384d6b2")
  })

  test_that("Map with units header line but no labels", {
    skip_if_not_fileio_available()
    expect_warning (spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_unit.txt", type = "map", hdr.units = TRUE, hdr.label = FALSE))
    expect_null(spc$x)
    expect_null(spc$y)

    spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_unit.txt", type = "map", hdr.units = TRUE, hdr.label = FALSE,
                          points.per.line = 5, lines.per.image = 5)
    expect_known_hash(spc, "86ecc17360")
  })

  test_that("Map with header and label lines", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_full.txt", type = "map", hdr.units = TRUE, hdr.label = TRUE)
    expect_known_hash(spc, "76db6397fc")
  })

  test_that ("Map can be read as time series", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-Map_no.txt")
    expect_known_hash(spc, "6213aefc6b")
    expect_null(spc$x)
    expect_null(spc$y)
  })


  test_that ("parameter default type = 'single'", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_no.txt")
    expect_known_hash(spc, "1a8c3be079")
  })

  test_that("Time series with neither header nor label lines", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_no.txt")
    expect_known_hash(spc, "1a8c3be079")
  })

  test_that("Time series with label line but no units header", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_label.txt", hdr.units = FALSE, hdr.label = TRUE)
    expect_known_hash(spc, "4cb098a671")
  })

  test_that("Time series with units header line but no labels", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_unit.txt",  hdr.units = TRUE, hdr.label = FALSE)

    expect_known_hash(spc, "6b6abac4e8")
  })

  test_that("Time series with header and label lines", {
    skip_if_not_fileio_available()
    expect_error (spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_full.txt"))

    spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_full.txt", hdr.units = TRUE, hdr.label = TRUE)
    expect_known_hash(spc, "db5b1a5db0")
  })

  test_that("encoding", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec("fileio/txt.Witec/Witec-timeseries_full.txt", hdr.units = TRUE, hdr.label = TRUE,
                          encoding = "ascii")
    expect_known_hash(spc, "db5b1a5db0")
  })
}

##' @rdname read.txt.Witec
##' @param filex filename wavelength axis file
##' @param filey filename intensity file
##' @export
read.dat.Witec <- function (filex = stop ("filename or connection needed"),
                            filey = sub ("-x", "-y", filex),
                            points.per.line = NULL,
                            lines.per.image = NULL,
                            type = c ("single", "map"),
                            encoding = "unknown",
                            ...,
                            quiet = hy.getOption ("debuglevel") < 1L){
    ## check valid data connection
    .check.con (filex = filex, filey = filey)

    ## check valid input
    type <- .check.valid (type = type, points.per.line = points.per.line,
                          lines.per.image = lines.per.image)

    ## read data
    wl <- scan (file = filex, ..., quiet = quiet, encoding = encoding)
    spc <- scan (file = filey, ..., quiet = quiet, encoding = encoding)

    dim (spc) <- c (length (wl), length (spc) / length (wl))

    spc <- new ("hyperSpec", wavelength = wl, spc = t (spc))

    ## add map information
    if (type == "map")
        spc <- .parse.xy (spc = spc, points.per.line = points.per.line, lines.per.image = lines.per.image)

    ## consistent file import behaviour across import functions
    .fileio.optional (spc, filey)
}

.test (read.dat.Witec) <- function (){
  context("read.dat.Witec")

  test_that ("-y file guessing", {
    skip_if_not_fileio_available()
    spc <- read.dat.Witec("fileio/txt.Witec/Witec-timeseries-x.dat")
    expect_known_hash(spc, "9562f59323")
  })

  test_that ("encoding", {
    skip_if_not_fileio_available()
    spc <- read.dat.Witec("fileio/txt.Witec/Witec-timeseries-x.dat", encoding = "ascii")
    expect_known_hash(spc, "9562f59323")
  })

  test_that ("Time series", {
    skip_if_not_fileio_available()
    spc <- read.dat.Witec("fileio/txt.Witec/Witec-timeseries-x.dat", "fileio/txt.Witec/Witec-timeseries-y.dat")
    expect_known_hash(spc, "9562f59323")
  })

  test_that ("Map: .dat does not have spatial information", {
    skip_if_not_fileio_available()
    spc <- read.dat.Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat")
    expect_null(spc$x)
    expect_null(spc$y)
    expect_known_hash(spc, "8a7ed06b0b")
  })

  test_that ("Map", {
    skip_if_not_fileio_available()
    expect_warning(read.dat.Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat",
                                  points.per.line = 5, lines.per.image = 5)
    )

    spc <- read.dat.Witec("fileio/txt.Witec/Witec-Map-x.dat", "fileio/txt.Witec/Witec-Map-y.dat",
                          type = "map", points.per.line = 5, lines.per.image = 5)
    expect_known_hash(spc, "3d6339675b")
  })


}


##' @rdname read.txt.Witec
##' @param headerfile filename or connection to ASCII file with header information
##' @export
read.txt.Witec.Graph <- function (headerfile = stop ("filename or connection needed"),
                                  filex = gsub ("Header", "X-Axis", headerfile),
                                  filey = gsub ("Header", "Y-Axis", headerfile),
                                  type = c ("single", "map"), encoding = "unknown",
                                  ..., quiet = TRUE){
    ## check for valid data connection
    .check.con (headerfile, filex, filey)

    ## processing headerfile
    hdr <- read.ini (headerfile, skip = 1, encoding = encoding)
    hdr <- sapply (hdr, function (x) unlist (x, recursive = FALSE)) # returns a matrix with colnames and rownames for better adressing

    ## check valid input
    type <- .check.valid (type = type, hdr = hdr,
                          ...)

    ## read spectra and header
    wl <- scan (filex, quiet = quiet, encoding = encoding)
    nwl <- length (wl)

    txt <- scan (filey, quiet = quiet, encoding = encoding)
    dim (txt) <- c (nwl, length (txt) / nwl)

    spc <- new ("hyperSpec", wavelength = wl, spc = t (txt))

    ## cross validation of parameters and information provided by header file
    if (nwl != hdr["SizeGraph", ])
        stop (paste ("length of wavelength axis in file '", filex,
                     "' differs from 'SizeGraph' in header file '", headerfile, "'", sep =""))

    ## add header information
    spc <- .parse.hdr (spc, hdr)

    ## add map information
    if (type == "map")
        spc <- .parse.xy (spc, hdr, ...)

    ## consistent file import behaviour across import functions
    .fileio.optional (spc, filex)
}

.test (read.txt.Witec.Graph) <- function (){
  context ("read.txt.Witec.Graph")

  test_that ("defaults and (X-Axis)/(Y-Axis) file guessing", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec.Graph("fileio/txt.Witec/Witec-timeseries (Header).txt")
    expect_known_hash(spc, "295499c43c")
  })

  test_that ("encoding", {
    skip_if_not_fileio_available()
    expect_warning(read.txt.Witec.Graph("fileio/txt.Witec/nofilename (Header).txt"))

    spc <- read.txt.Witec.Graph("fileio/txt.Witec/nofilename (Header).txt", encoding = "latin1")
    expect_known_hash(spc, "2bad36adb3")
  })

  test_that ("Time Series", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec.Graph("fileio/txt.Witec/Witec-timeseries (Header).txt", type = "single")
    expect_known_hash(spc, "295499c43c")
  })

  test_that ("Map", {
    skip_if_not_fileio_available()
    expect_warning (read.txt.Witec.Graph("fileio/txt.Witec/Witec-Map (Header).txt"))
    expect_warning (read.txt.Witec.Graph("fileio/txt.Witec/Witec-Map (Header).txt", type = "single"))

    spc <- read.txt.Witec.Graph("fileio/txt.Witec/Witec-Map (Header).txt", type = "map")
    expect_known_hash(spc, "cb9cd9757a")
  })

  test_that("missing filename", {
    skip_if_not_fileio_available()
    spc <- read.txt.Witec.Graph("fileio/txt.Witec/nofilename (Header).txt", encoding = "latin1")
    expect_known_hash(spc, "2bad36adb3")
  })

  test_that ("wrong combination of file names", {
    skip_if_not_fileio_available()
    expect_error (read.txt.Witec.Graph("fileio/txt.Witec/Witec-timeseries (Header).txt", "fileio/txt.Witec/Witec-timeseries (Y-Axis).txt"))
  })

}

### -------- helpers ------------------------

###checking file connection
.check.con <- function (headerfile, filex, filey, file){
    ## check for valid data connection
    if (!missing (headerfile) && !file.exists (headerfile))
        stop ("Header file not found!")

    if (!missing (filex) && !file.exists (filex))
        stop ("Wavelength axis file not found!")

    if (!missing (filey) && !file.exists (filey))
        stop ("Intensity file not found!")

    if (!missing (file) && !file.exists (file))
        stop ("Spectra file not found!")
}

###checking for valid input
.check.valid <- function (type, hdr, points.per.line, lines.per.image){
    ## check valid input
    type <- match.arg (type, c ("single", "map"))

    if (type == "single" && !missing (points.per.line) && !is.null (points.per.line) && points.per.line != 1)#TODO: better to prove for values > 1?
        warning ("points.per.line != 1 given for single spectrum")

    if (type == "single" && !missing (lines.per.image) && !is.null (lines.per.image) && lines.per.image != 1)#TODO: see above
        warning ("lines.per.image != 1 are defined for single spectrum")

    if (type == "single" && !missing (hdr) && !is.null (hdr) && hdr ["SizeY", ] != 1)
        warning ("header provides spatial information in y direction for single spectra")

    return (type)
}

### parsing header information
.parse.hdr <- function (spc, hdr, hdr.label) {
    if (!missing (hdr) && !missing (hdr.label)){
        hdr <- strsplit (hdr, "\t")

        if (length (hdr) == 2){
            spc@data$spcname <- hdr [[1]][-1]
            labels (spc, ".wavelength") <- hdr [[2]] [1]
            labels (spc, "spc") <- unique (hdr [[2]] [-1])
        } else if (length (hdr) == 1 && hdr.label){
            spc@data$spcname <- hdr [[1]][-1]
        } else {
            labels (spc, ".wavelength") <- hdr [[1]] [1]
            labels (spc, "spc") <- unique (hdr [[1]] [-1])
        }
    }

    if (!missing (hdr) && missing (hdr.label)){
        spc@data$spcname <- hdr ["GraphName", ]
        if ("FileName" %in% rownames (hdr))
          spc@data$WIPname <- hdr ["FileName", ]
        labels (spc, "spc") <- hdr ["DataUnit", ]
    }
    return (spc)
}

### parsing map information
.parse.xy <- function (spc, hdr, hdr.label, points.per.line, lines.per.image, ...){

    ## set points.per.line and lines.per.image, if at least one is set unequal NULL
    if (xor (!missing (points.per.line) && !is.null (points.per.line),
             !missing (lines.per.image) && !is.null(lines.per.image))){
        if ((missing (points.per.line) || is.null (points.per.line)) &&
            !is.null (lines.per.image)) {
            points.per.line <- nrow (spc) / lines.per.image
        } else {
            lines.per.image <- nrow (spc) / points.per.line
        }

    } else if (!missing (points.per.line) && !missing (lines.per.image) &&
               is.null (points.per.line) && is.null (points.per.line) &&
               !missing (hdr.label) && hdr.label) {#TODO: only read, if not yet calculated?
        x <- sub ("^.*\\(([[:digit:]]+)/[[:digit:]]+\\)$", "\\1",  hdr[1])
        y <- sub ("^.*\\([[:digit:]]+/([[:digit:]]+)\\)$", "\\1", hdr[1])
        points.per.line <- as.numeric (x) + 1
        lines.per.image <- as.numeric (y) + 1
    } else if ((missing (points.per.line) || missing (lines.per.image)) &&
               !missing (hdr) && missing (hdr.label)){#TODO: only read, if not yet calculated?
        points.per.line <- as.numeric (hdr ["SizeX", ])
        lines.per.image <- as.numeric (hdr ["SizeY", ])
    } else if (is.null (points.per.line) && is.null (lines.per.image)) {
        warning ("no spatial information provided")
        return (spc)
    }

    if (points.per.line * lines.per.image == nrow (spc)){
        spc@data$x <- rep (seq_len (points.per.line), lines.per.image)
        spc@data$y <- rep (- seq_len (lines.per.image), each = points.per.line)
    } else
        warning ("number of spectra and number of points in map are not equal!")

    return (spc)
}
