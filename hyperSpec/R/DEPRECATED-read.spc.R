### read.spc - Import Thermo Galactic's .spc file format into an hyperSpec Object
###
### C. Beleites 2009/11/29
###
#####################################################################################################

## Define constants ---------------------------------------------------------------------------------

.nul <- as.raw(0)

## header sizes
.spc.size <- c(hdr = 512, subhdr = 32, subfiledir = 12, loghdr = 64)

.spc.default.keys.hdr2data <- c("fexper", "fres", "fsource")
.spc.default.keys.log2data <- FALSE

## axis labeling ------------------------------------------------------------------------------------

## x-axis units .....................................................................................
.spc.FXTYPE <- c(
  expression(`/`(x, "a. u.")), # 0
  expression(`/`(tilde(nu), cm^-1)),
  expression(`/`(lambda, (mu * m))),
  expression(`/`(lambda, nm)),
  expression(`/`(t, s)),
  expression(`/`(t, min)),
  expression(`/`(f, Hz)),
  expression(`/`(f, kHz)),
  expression(`/`(f, MHz)),
  expression(`/`(frac(m, z), frac(u, e))),
  expression(`/`(delta, ppm)), # 10
  expression(`/`(t, d)),
  expression(`/`(t, a)),
  expression(`/`(Delta * tilde(nu), cm^-1)),
  expression(`/`(E, eV)),
  NA, # old version file uses label in gcatxt
  "Diode No",
  "Channel",
  expression(`/`(x, degree)),
  expression(`/`(T, degree * F)),
  expression(`/`(T, degree * C)), # 20
  expression(`/`(T, K)),
  "Data Point",
  expression(`/`(t, ms)),
  expression(`/`(t, micro * s)),
  expression(`/`(t, ns)),
  expression(`/`(f, GHz)),
  expression(`/`(lambda, cm)),
  expression(`/`(lambda, m)),
  expression(`/`(lambda, mm)),
  expression(`/`(t, h)) # 30
)

.spc.xlab <- function(x) {
  if (is.character(x)) {
    x
  } else if (x <= length(.spc.FXTYPE) + 1) {
    .spc.FXTYPE [x + 1]
  } else {
    ## x = 255 is for double interferogram and supposed not to have a label.
    ## Thus, returning NA is appropriate
    NA
  }
}

## y-axis units .....................................................................................
.spc.FYTYPE <- c(
  expression(`/`(I[Ref], "a. u.")), # -1
  expression(`/`(I, "a. u.")),
  expression(`/`(I[IGRM], "a. u.")),
  "A",
  expression(frac((1 - R)^2, 2 * R)),
  "Counts",
  expression(`/`(U, V)),
  expression(`/`(y, degree)),
  expression(`/`(I, mA)),
  expression(`/`(l, mm)),
  expression(`/`(U, mV)),
  expression(-log(R)), # 10
  expression(`/`(y, "%")),
  expression(`/`(I, "a. u.")),
  expression(I / I[0]),
  expression(`/`(E, J)),
  NA, # old version file uses label in gcatxt
  expression(`/`(G, dB)),
  NA, # old version file uses label in gcatxt
  NA, # old version file uses label in gcatxt
  expression(`/`(T, degree * F)),
  expression(`/`(T, degree * C)), # 20
  expression(`/`(T, K)),
  "n",
  "K", # extinction coeaffictient
  expression(Re(y)),
  expression(Im(y)),
  "y (complex)", # complex
  "T",
  "R",
  expression(`/`(I, "a. u.")),
  expression(`/`(I[Emission], "a. u."))
)
.spc.ylab <- function(x) {
  if (is.character(x)) {
    x
  } else if (x <= 26) {
    .spc.FYTYPE [x + 2]
  } else if (x %in% 128:131) {
    .spc.FYTYPE [x - 99]
  } else {
    NA
  }
}

## helper functions ---------------------------------------------------------------------------------
### raw.split.nul - rawToChar conversion, splitting at \0
#' @importFrom utils tail
raw.split.nul <- function(raw, trunc = c(TRUE, TRUE), firstonly = FALSE, paste.collapse = NULL) {
  # todo make better truncation
  trunc <- rep(trunc, length.out = 2)

  if (trunc [1] && raw [1] == .nul) {
    raw <- raw [-1]
  }
  if (trunc [2]) {
    tmp <- which(raw > .nul)
    if (length(tmp) == 0) {
      return("")
    }
    raw <- raw [1:tail(tmp, 1)]
  }
  if (raw [length(raw)] != .nul) {
    raw <- c(raw, .nul)
  }

  tmp <- c(0, which(raw == .nul))

  out <- character(length(tmp) - 1)
  for (i in 1:(length(tmp) - 1)) {
    if (tmp [i] + 1 < tmp [i + 1] - 1) {
      out [i] <- rawToChar(raw [(tmp [i] + 1):(tmp [i + 1] - 1)])
    }
  }

  if (length(out) > 1L) {
    if (firstonly) {
      message("multiple strings encountered in spc file ", paste(out, collapse = ", "), ": using only the first one.")
      out <- out [1]
    } else if (!is.null(paste.collapse)) {
      if (hy.getOption("debuglevel") > 2L) {
        message("multiple strings encountered in spc file ", paste(out, collapse = ", "), " => pasting.")
      }

      out <- paste(out, collapse = paste.collapse)
    }
  }

  out
}

## file part reading functions ----------------------------------------------------------------------

## read file header .................................................................................
##
##

#' @importFrom utils maintainer
.spc.filehdr <- function(raw.data) {
  ## check file format

  ## Detect Shimadzu SPC (which is effectively a variant of OLE CF format)
  if (isTRUE(all.equal(
    raw.data[1:4],
    as.raw(c("0xD0", "0xCF", "0x11", "0xE0"))
  ))) {
    stop("Support for Shimadzu SPC file format (OLE CF) is not yet implemented")
  }

  ## NEW.LSB = 75 supported,
  ## NEW.MSB = 76 not supported (neither by many Grams software according to spc doc)
  ## OLD     = 77 not supported (replaced by new format in 1996)
  if (raw.data [2] != 75) {
    stop(
      "Wrong spc file format version (or no spc file at all).\n",
      "Only 'new' spc files (1996 file format) with LSB word order are supported."
    )
  }

  hdr <- list(
    ftflgs = readBin(raw.data [1], "integer", 1, 1, signed = FALSE),
    ## byte 2 is already interpreted
    fexper = readBin(raw.data [3], "integer", 1, 1, signed = TRUE),
    fexp = readBin(raw.data [4], "integer", 1, 1, signed = TRUE),
    fnpts = readBin(raw.data [5:8], "integer", 1, 4),
    ffirst = readBin(raw.data [9:16], "double", 1, 8),
    flast = readBin(raw.data [17:24], "double", 1, 8),
    fnsub = readBin(raw.data [25:28], "integer", 1, 4),
    fxtype = readBin(raw.data [29], "integer", 1, 1, signed = FALSE),
    fytype = readBin(raw.data [30], "integer", 1, 1, signed = FALSE),
    fztype = readBin(raw.data [31], "integer", 1, 1, signed = FALSE),
    fpost = readBin(raw.data [32], "integer", 1, 1, signed = TRUE),
    fdate = readBin(raw.data [33:36], "integer", 1, 4),
    fres = raw.split.nul(raw.data [37:45], paste.collapse = "\r\n"),
    fsource = raw.split.nul(raw.data [46:54], paste.collapse = "\r\n"),
    fpeakpt = readBin(raw.data [55:56], "integer", 1, 2, signed = FALSE),
    fspare = readBin(raw.data [57:88], "numeric", 8, 4),
    fcmnt = raw.split.nul(raw.data [89:218], paste.collapse = "\r\n"),
    fcatxt = raw.split.nul(raw.data [219:248], trunc = c(FALSE, TRUE)),
    flogoff = readBin(raw.data [249:252], "integer", 1, 4), # , signed = FALSE),
    fmods = readBin(raw.data [253:256], "integer", 1, 4), # , signed = FALSE),
    fprocs = readBin(raw.data [257], "integer", 1, 1, signed = TRUE),
    flevel = readBin(raw.data [258], "integer", 1, 1, signed = TRUE),
    fsampin = readBin(raw.data [259:260], "integer", 1, 2, signed = FALSE),
    ffactor = readBin(raw.data [261:264], "numeric", 1, 4),
    fmethod = raw.split.nul(raw.data [265:312]),
    fzinc = readBin(raw.data [313:316], "numeric", 1, 4), # , signed = FALSE),
    fwplanes = readBin(raw.data [317:320], "integer", 1, 4), # , signed = FALSE),
    fwinc = readBin(raw.data [321:324], "numeric", 1, 4),
    fwtype = readBin(raw.data [325], "integer", 1, 1, signed = TRUE),
    ## 187 bytes reserved
    .last.read = .spc.size ["hdr"]
  )

  ## R doesn't have unsigned long int .................................
  if (any(unlist(hdr [c("flogoff", "fmods", "fwplanes")]) < 0)) {
    stop(
      "error reading header: R does not support unsigned long integers.",
      "Please contact the maintainer of the package."
    )
  }



  ## do some post processing ..........................................

  experiments <- c(
    "General", "Gas Chromatogram", "General Chromatogram", "HPLC Chromatogram",
    "NIR Spectrum", "UV-VIS Spectrum", "* reserved *", "X-ray diffraction spectrum",
    "Mass Spectrum", "NMR Spectrum", "Raman Spectrum", "Fluorescence Spectrum",
    "Atomic Spectrum", "Chroatography Diode Array Data"
  )
  hdr$fexper <- factor(hdr$fexper + 1, levels = seq_along(experiments))
  levels(hdr$fexper) <- experiments

  hdr$ftflgs <- .spc.ftflags(hdr$ftflgs)

  hdr$fdate <- ISOdate(
    year = hdr$fdate %/% 1048560,
    month = hdr$fdate %/% 65536 %% 16,
    day = hdr$fdate %/% 2048 %% 32,
    hour = hdr$fdate %/% 64 %% 32,
    min = hdr$fdate %% 64
  )

  ## interferogram ?
  ## if not, hdr$fpeakpt is set to NULL
  if (hdr$fytype == 1) {
    if (hdr$fpeakpt != 0) {
      hdr$fpeakpt <- hdr$fpeakpt + 1
    }
  } else {
    hdr$fpeakpt <- NULL
  }

  ## set the axis labels
  if (hdr$ftflgs ["TALABS"]) {
    # TODO: find test data
    tmp <- rep(0, 4)
    tmp [seq_along(hdr$fcatxt)] <- nchar(hdr$fcatxt)

    if (tmp [1] > 0) hdr$fxtype <- hdr$fcatxt[1]
    if (tmp [2] > 0) hdr$fytype <- hdr$fcatxt[2]
    if (tmp [3] > 0) hdr$fztype <- hdr$fcatxt[3]
    if (tmp [4] > 0) hdr$fwtype <- hdr$fcatxt[4]
  }
  hdr$fxtype <- .spc.xlab(hdr$fxtype)
  hdr$fytype <- .spc.ylab(hdr$fytype)
  hdr$fztype <- .spc.xlab(hdr$fztype)
  hdr$fwtype <- .spc.xlab(hdr$fwtype)


  ## File with subfiles with individual x axes?
  ## Then there should be a subfile directory:
  if (hdr$ftflgs ["TXYXYS"] && hdr$ftflgs ["TMULTI"]) {
    ## try to reject impossible values for the subfiledir offset
    if (hdr$fnpts > length(raw.data) ||
      (hdr$fnpts > hdr$flogoff && hdr$flogoff > 0) ||
      hdr$fnpts < 512) {
      .spc.error(
        ".spc.read.hdr", list(hdr = hdr),
        "file header flags specify TXYXYS and TMULTI, ",
        "but fnpts does not give a valid offset for the subfile directory.\n hdr$ftflgs = ",
        paste(names(hdr$ftflgs)[hdr$ftflgs], collapse = " | "),
        " (", sum(2^(0:7) [hdr$ftflgs]), ")\n",
        "You can try to read the file using hdr$ftflgs & ! TXYXYS (",
        sum(2^(0:7) [hdr$ftflgs & c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)]),
        "). This assumes that all subfiles do have the same x axis.\n\n"
      )
    }

    hdr$subfiledir <- hdr$fnpts
    hdr$fnpts <- 0
  } else {
    hdr$subfiledir <- 0
  }


  ## some checks ......................................................

  if (hdr$ftflgs ["TMULTI"]) {
    ## multiple spectra in file
    if (hdr$fnsub <= 1) {
      if (hy.getOption("debuglevel") >= 2L) {
        message("spc file header specifies multiple spectra but only zero or one subfile.")
      }
    }
  } else {
    ## single spectrum file
    if (hdr$fnsub == 0) {
      hdr$fnsub <- 1
    }

    if (hdr$fnsub > 1) {
      warning(
        "spc file header specifies single spectrum file  but ", hdr$fnsub,
        " subfiles (spectra).\nOnly first subfile will be read."
      )
      hdr$fnsub <- 1
    }

    if (hdr$ftflgs ["TRANDM"]) {
      message("spc file header: file type flag TRANDM encountered => Enforcing TMULTI.")
    }

    if (hdr$ftflgs ["TORDRD"]) {
      message("spc file header: file type flag TORDRD encountered => Enforcing TMULTI.")
    }

    if ((hdr$ftflgs ["TRANDM"] || hdr$ftflgs ["TORDRD"]) && hdr$fnsub > 1) {
      hdr$ftflgs ["TMULTI"] <- TRUE
    }
  }

  if (hdr$ftflgs ["TXYXYS"] && !hdr$ftflgs ["TXVALS"]) {
    warning("spc file header: file type flag TXYXYS encountered => Enforcing TXVALS.")
    hdr$ftflgs ["TXVALS"] <- TRUE
  }

  if (hdr$fwplanes > 0) {
    warning(
      "w planes found! This is not yet tested as the developer didn't have access to such files.\n",
      "Please contact the package maintainer ", maintainer("hyperSpec"),
      " stating whether the file was imported successfully or not."
    )
  }

  hdr
}

## read sub file header .............................................................................
##
## needs header for consistency checks
##

.spc.subhdr <- function(raw.data, pos, hdr) {
  subhdr <- list(
    subflgs = raw.data [pos + (1)],
    subexp = readBin(raw.data [pos + (2)], "integer", 1, 1, signed = TRUE),
    subindx = readBin(raw.data [pos + (3:4)], "integer", 1, 2, signed = FALSE),
    subtime = readBin(raw.data [pos + (5:8)], "numeric", 1, 4),
    subnext = readBin(raw.data [pos + (9:12)], "numeric", 1, 4),
    subnois = readBin(raw.data [pos + (13:16)], "numeric", 1, 4),
    subnpts = readBin(raw.data [pos + (17:20)], "integer", 1, 4), # , signed = FALSE),
    subscan = readBin(raw.data [pos + (21:24)], "integer", 1, 4), # , signed = FALSE),
    subwlevel = readBin(raw.data [pos + (25:28)], "numeric", 1, 4)
  )
  ## 4 bytes reserved

  ## R doesn't have unsigned long int .................................
  if (any(unlist(subhdr [c("subnpts", "subscan")]) < 0)) {
    stop(
      "error reading subheader: R does not support unsigned long integers.",
      "Please contact the maintainer of the package."
    )
  }

  hdr$.last.read <- pos + .spc.size ["subhdr"]

  ## checking
  if (subhdr$subexp == -128 && hdr$fexp != -128) {
    message(
      "subfile ", subhdr$subindx, " specifies data type float, but file header doesn't.",
      "\n=> Data will be interpreted as float unless TMULTI is set."
    )
  }

  if (subhdr$subnpts > 0 && subhdr$subnpts != hdr$fnpts && !hdr$ftflgs ["TXYXYS"]) {
    message(
      "subfile ", subhdr$subindx, ": number of points in file header and subfile header ",
      "inconsistent. => Going to use subheader."
    )
  }

  if (subhdr$subnpts == 0) {
    if (hdr$ftflgs ["TXYXYS"]) {
      message(
        "subfile ", subhdr$subindx, ": number of data points per spectrum not specified. ",
        "=> Using file header information (", hdr$fnpts, ")."
      )
    }
    subhdr$subnpts <- hdr$fnpts
  }

  if (!hdr$ftflgs ["TXYXYS"]) {
    if (hdr$fnpts != subhdr$subnpts) {
      .spc.error(
        ".spc.read.subhdr", list(hdr = hdr, subhdr = subhdr),
        "hdr and subhdr differ in number of points per spectrum, ",
        "but TXYXYS is not specified.\n hdr$ftflgs = ",
        paste(names(hdr$ftflgs)[hdr$ftflgs], collapse = " | "),
        " (", sum(2^(0:7) [hdr$ftflgs]), ")\n",
        "You can try to read the file using hdr$ftflgs | TMULTI | TXYXYS (",
        sum(2^(0:7) [hdr$ftflgs |
          c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)]),
        ").\n\n"
      )
    }
  }

  #  str (subhdr)
  ## according to .spc file documentation:
  if (!hdr$ftflgs ["TMULTI"]) {
    subhdr$subexp <- hdr$fexp
  } else if (hdr$fexp == -128 && subhdr$subexp != -128) {
    message(
      "Header file specifies float data format, but subfile uses integer exponent. ",
      "=> Using file header settings."
    )
    subhdr$subexp <- -128
  }

  ## the z values
  if (hdr$fzinc == 0) { # should only happen for the first subfile...
    hdr$fzinc <- subhdr$subnext - subhdr$subtime
  }

  if (subhdr$subindx == 0) {
    hdr$firstz <- subhdr$subtime
  }

  if (subhdr$subtime == 0) {
    subhdr$subtime <- subhdr$subindx * hdr$fzinc + hdr$firstz
  }

  ## the w values
  if (hdr$fwplanes > 0) {
    if (hdr$fwinc == 0) { ## unevenly spaced w planes
    }

    # if (subhdr$subwlevel != 0) {
    # 	subhdr$w <- subhdr$subwlevel
    #
    # } else if (subhdr$subindx %% hdr$fwplanes == 1)
    # 	subhdr$w <- hdr$subhdr$w +  hdr$fwinc
    # else
    # 	subhdr$w <- hdr$subhdr$w
  }


  hdr$subhdr <- subhdr

  hdr
}

## read subfile directory ...........................................................................
##

.spc.subfiledir <- function(raw.data, pos, nsub) {
  dir <- data.frame(
    ssfposn = rep(NA, nsub),
    ssfsize = rep(NA, nsub),
    ssftime = rep(NA, nsub)
  )

  for (s in seq_len(nsub)) {
    dir [s, ] <- c(
      readBin(raw.data [pos + (1:4)], "integer", 1, 4), # , signed = FALSE),
      readBin(raw.data [pos + (5:8)], "integer", 1, 4), # , signed = FALSE),
      readBin(raw.data [pos + (9:12)], "numeric", 1, 4)
    )
    pos <- pos + .spc.size ["subfiledir"]
  }

  ## R doesn't have unsigned long int .................................
  if (any(dir [, 1:2] < 0)) {
    stop(
      "error reading subfiledir: R does not support unsigned long integers.",
      "Please contact the maintainer of the package."
    )
  }

  # 	dir$ssfposn <- dir$ssfposn
  dir
}

## read log block header ............................................................................
##
#' @importFrom utils head tail
.spc.log <- function(raw.data, pos, log.bin, log.disk, log.txt, keys.log2data,
                     replace.nul = as.raw(255), iconv.from = "latin1", iconv.to = "utf8") {
  if (pos == 0) { # no log block exists
    return(list(
      data = list(),
      log = list()
    ))
  }

  loghdr <- list(
    logsizd = readBin(raw.data [pos + (1:4)], "integer", 1, 4), #  , signed = FALSE),
    logsizm = readBin(raw.data [pos + (5:8)], "integer", 1, 4), # , signed = FALSE),
    logtxto = readBin(raw.data [pos + (9:12)], "integer", 1, 4), # , signed = FALSE),
    logbins = readBin(raw.data [pos + (13:16)], "integer", 1, 4), # , signed = FALSE),
    logdsks = readBin(raw.data [pos + (17:20)], "integer", 1, 4), # , signed = FALSE),
    ## 44 bytes reserved
    .last.read = pos + .spc.size ["loghdr"]
  )

  ## R doesn't have unsigned long int .................................
  if (any(unlist(loghdr) < 0)) {
    stop(
      "error reading log: R does not support unsigned long integers.",
      "Please contact the maintainer of the package."
    )
  }

  log <- list()
  data <- list()

  ## read binary part of log
  if (log.bin) {
    log$.log.bin <- raw.data [loghdr$.last.read + seq_len(loghdr$logbins)]
  }

  ## read binary on-disk-only part of log
  if (log.disk) {
    log$.log.disk <- raw.data [loghdr$.last.read + loghdr$logbins + seq_len(loghdr$logdsks)]
  }

  ## read text part of log
  if (log.txt & loghdr$logsizd > loghdr$logtxto) {
    log.txt <- raw.data [pos + loghdr$logtxto + seq_len(loghdr$logsizd - loghdr$logtxto)]
    if (tail(log.txt, 1) == .nul) { # throw away nul at the end
      log.txt <- head(log.txt, -1)
    }
    log.txt [log.txt == .nul] <- replace.nul
    log.txt <- readChar(log.txt, length(log.txt), useBytes = T)
    log.txt <- gsub(rawToChar(replace.nul), "\r\n", log.txt)
    log.txt <- iconv(log.txt, iconv.from, iconv.to)
    log.txt <- split.string(log.txt, "\r\n") ## spc file spec says \r\n regardless of OS
    log.txt <- split.line(log.txt, "=")
    data <- getbynames(log.txt, keys.log2data)
  }

  list(log.long = log, extra.data = data)
}


## read y data ......................................................................................
##

.spc.read.y <- function(raw.data, pos, npts, exponent, word) {
  if (exponent == -128) { # 4 byte float

    list(
      y = readBin(raw.data [pos + seq_len(npts * 4)], "numeric", npts, 4),
      .last.read = pos + npts * 4
    )
  } else if (word) { # 2 byte fixed point integer = word

    list(
      y = readBin(raw.data [pos + seq_len(npts * 2)], "integer", npts, 2, signed = TRUE) *
        2^(exponent - 16),
      .last.read = pos + npts * 2
    )
  } else { # 4 byte fixed point integer = dword
    list(
      y = readBin(raw.data [pos + seq_len(npts * 4)], "integer", npts, 4) *
        2^(exponent - 32),
      .last.read = pos + npts * 4
    )
  }
}

## read x data ......................................................................................
##

.spc.read.x <- function(raw.data, pos, npts) {
  list(
    x = readBin(raw.data [pos + seq_len(npts * 4)], "numeric", npts, 4),
    .last.read = pos + 4 * npts
  )
}

## error .............................................................................................
#' @importFrom utils str
.spc.error <- function(fname, objects, ...) {
  cat("ERROR in read.spc function ", fname, "\n\n")
  for (i in seq_along(objects)) {
    cat(names(objects) [i], ":\n")
    str(objects[[i]], vec.len = 20)
  }
  stop(...)
}

.spc.ftflags <- function(x) {
  ftflgs <- as.logical(x %/% 2^(0:7) %% 2)
  names(ftflgs) <- c(
    "TSPREC", "TCGRAM", "TMULTI", "TRANDM",
    "TORDRD", "TALABS", "TXYXYS", "TXVALS"
  )
  ftflgs
}

#####################################################################################################

#' @name DEPRECATED-read.spc
#' @concept moved to hySpc.read.spc
#'
#' @title (DEPRECATED)
#'        Import for Thermo Galactic's `spc` file format
#'
#' @description
#'
#' These data input functions are **deprecated** and they will be removed in
#' the next release of \pkg{hyperspcc} package.
#' Now functions in package \pkg{hySpc.read.spc}
#' ([link](https://r-hyperspcc.github.io/hySpc.read.spc/reference/index.html))
#' should be used as the alternatives.
#'
#'
#' **Old description:**
#'
#' These functions allow to import Thermo Galactic/Grams `.spc` files.
#'
#' @param filename The complete file name of the `.spc` file.
#' @param keys.hdr2data,keys.log2data character vectors with the names of parameters in the `.spc`
#' file's log block (log2xxx) or header (hdr2xxx) that should go into the extra data (yyy2data) of
#' the returned hyperSpec object.
#'
#' All header fields specified in the `.spc` file format specification (see
#'   below) are imported and can be referred to by their de-capitalized names.
#' @param log.txt Should the text part of the `.spc` file's log block be read?
#' @param log.bin,log.disk Should the normal and on-disk binary parts of the
#'   `.spc` file's log block be read?  If so, they will be put as raw vectors
#'   into the hyperSpec object's log.
#' @param hdr A list with fileheader fields that overwrite the settings of
#'   actual file's header.
#'
#' Use with care, and look into the source code for detailed insight on the
#'   elements of this list.
#' @param no.object If `TRUE`, a list with wavelengths, spectra, labels,
#'   log and data are returned instead of a hyperSpec object.
#'
#' This parameter will likely be subject to change in future - use with care.
#' @return If the file contains multiple spectra with individual wavelength
#'   axes, `read.spc` returns a list of hyperSpec objects.  Otherwise the
#'   result is a hyperSpec object.
#'
#' `read.spc.KaiserMap` returns a hyperSpec object with data columns x,
#'   y, and z containing the stage position as recorded in the `.spc` files'
#'   log.
#' @note Only a restricted set of test files was available for development.
#'   Particularly, the w-planes feature could not be tested.
#'
#' If you have `.spc` files that cannot be read with these function, don't
#'   hesitate to contact the package maintainer with your code patch or asking
#'   advice.
#' @author C. Beleites
#' @references Source development kit and file format specification of `.spc`
#'   files.
#' @export
#'
#' @keywords IO file
#'
#' @examples
#'
#' ## get the sample .spc files from ftirsearch.com (see above)
#' \dontrun{
#' # single spectrum
#' spc <- read.spc("BENZENE.SPC")
#' plot(spc)
#'
#' # multi-spectra .spc file with common wavelength axis
#' spc <- read.spc("IG_MULTI.SPC")
#' spc
#'
#' # multi-spectra .spc file with individual wavelength axes
#' spc <- read.spc("BARBITUATES.SPC")
#' plot(spc[[1]], lines.args = list(type = "h"))
#' }
#'
#' @importFrom utils modifyList
read.spc <- function(filename,
                     keys.hdr2data = FALSE, keys.log2data = FALSE,
                     log.txt = TRUE, log.bin = FALSE, log.disk = FALSE,
                     hdr = list(),
                     no.object = FALSE) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_spc()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## f contains the raw bytes of the file

  ## fpos marks the position of the last read byte
  ## this is the same as the offset from beginning of the file (count 0) in the .spc definition

  f <- readBin(filename, "raw", file.info(filename)$size, 1)

  hdr <- modifyList(.spc.filehdr(f), hdr)
  fpos <- hdr$.last.read

  if (!hdr$ftflgs ["TXYXYS"]) {
    if (!hdr$ftflgs ["TXVALS"]) {
      ## spectra with common evenly spaced wavelength axis
      wavelength <- seq(hdr$ffirst, hdr$flast, length.out = hdr$fnpts)
    } else {
      ## spectra with common unevenly spaced wavelength axis
      # 	if (! hdr$ftflgs ['TMULTI']) { # also for multifile with common wavelength axis
      tmp <- .spc.read.x(f, fpos, hdr$fnpts)
      wavelength <- tmp$x
      fpos <- tmp$.last.read
    }
    # }
  }

  ## otherwise (TXYXYS set) hdr$fnpts gives offset to subfile directory if that exists

  ## obtain labels from file hdr or from parameter
  label <- list(
    .wavelength = hdr$fxtype, spc = hdr$fytype,
    z = hdr$fztype, z.end = hdr$fztype
  )

  if (hdr$fwplanes > 0) {
    label$w <- hdr$fwtype
  }

  ## prepare list for hyperSpec log and data.frame for extra data

  data <- list(z = NA, z.end = NA)
  if (hdr$fwplanes > 0) {
    data <- c(data, w = NA)
  }

  ## process the log block
  tmp <- .spc.log(
    f, hdr$flogoff,
    log.bin, log.disk, log.txt,
    keys.log2data
  )
  ## TODO: remove data2log

  data <- c(data, tmp$extra.data, getbynames(hdr, keys.hdr2data))

  ## preallocate spectra matrix or list for multispectra file with separate wavelength axes
  ## populate extra data
  if (hdr$ftflgs ["TXYXYS"] && hdr$ftflgs ["TMULTI"]) {
    spc <- list()
    data <- .prepare.hdr.df(data, nsubfiles = 1L)
  } else {
    spc <- matrix(NA, nrow = hdr$fnsub, ncol = hdr$fnpts)
    data <- .prepare.hdr.df(data, nsubfiles = hdr$fnsub)
  }

  ## read subfiles
  if (hdr$subfiledir) { ## TXYXYS
    hdr$subfiledir <- .spc.subfiledir(f, hdr$subfiledir, hdr$fnsub)

    for (s in seq_len(hdr$fnsub)) {
      hdr <- .spc.subhdr(f, hdr$subfiledir$ssfposn [s], hdr)
      fpos <- hdr$.last.read
      wavelength <- .spc.read.x(f, fpos, hdr$subhdr$subnpts)
      fpos <- wavelength$.last.read

      y <- .spc.read.y(f, fpos,
        npts = hdr$subhdr$subnpts, exponent = hdr$subhdr$subexp,
        word = hdr$ftflgs ["TSPREC"]
      )
      fpos <- y$.last.read

      data$z <- hdr$subhdr$subtime
      data$z.end <- hdr$subhdr$subnext

      if (hdr$fwplanes > 0) {
        data$w <- hdr$subhdr$w
      }

      if (!exists("wavelength")) {
        .spc.error(
          "read.spc", list(hdr = hdr),
          "wavelength not read. This may be caused by wrong header information."
        )
      }

      spc[[s]] <- new("hyperSpec",
        spc = y$y,
        wavelength = wavelength$x,
        data = data,
        labels = label
      )
    }
  } else { ## multiple y data blocks behind each other
    for (s in seq_len(hdr$fnsub)) {
      hdr <- .spc.subhdr(f, fpos, hdr)
      fpos <- hdr$.last.read
      tmp <- .spc.read.y(f, fpos,
        npts = hdr$subhdr$subnpts, exponent = hdr$subhdr$subexp,
        word = hdr$ftflgs ["TSPREC"]
      )
      fpos <- tmp$.last.read

      spc [s, ] <- tmp$y

      data [s, c("z", "z.end")] <- unlist(hdr$subhdr [c("subtime", "subnext")])

      if (hdr$fwplanes > 0) {
        data [s, "w"] <- hdr$subhdr$w
      }
    }
  }

  if (hdr$ftflgs ["TXYXYS"] && hdr$ftflgs ["TMULTI"]) {
    ## list of hyperSpec objects
    ## consistent file import behaviour across import functions
    lapply(spc, .spc_io_postprocess_optional, filename = filename)
  } else if (no.object) {
    list(spc = spc, wavelength = wavelength, data = data, labels = label)
  } else {
    if (hdr$fnsub > 1L && nrow(data) == 1L) {
      data <- data [rep(1L, hdr$fnsub), ]
    }

    spc <- new("hyperSpec",
      spc = spc, wavelength = wavelength,
      data = data, labels = label
    )

    ## consistent file import behaviour across import functions
    .spc_io_postprocess_optional(spc, filename)
  }
}


hySpc.testthat::test(read.spc) <- function() {
  context("read.spc")

  old.spc <- paste0("fileio/spc/", c("CONTOUR.SPC", "DEMO 3D.SPC", "LC DIODE ARRAY.SPC"))
  wplanes <- "fileio/spc/wplanes.spc"
  other.spc <- setdiff(Sys.glob("fileio/spc/*.[sS][pP][cC]"), c(old.spc, wplanes))

  test_that("old file format -> error", {
    skip_if_not_fileio_available()
    for (f in old.spc) {
      expect_error(read.spc(f))
    }
  })

  test_that("SPC SDK example files", {
    skip_if_not_fileio_available()

    checksums <- c(
      `fileio/spc/BARBITUATES.SPC` = "f49bbc854c",
      `fileio/spc/barbsvd.spc` = "8a4d30672c",
      `fileio/spc/BENZENE.SPC` = "6fc7901d15",
      `fileio/spc/DRUG SAMPLE_PEAKS.SPC` = "a600cd05e2",
      `fileio/spc/DRUG SAMPLE.SPC` = "981e42bfb8",
      `fileio/spc/FID.SPC` = "ab65b6bb23",
      `fileio/spc/HCL.SPC` = "c657dd8279",
      `fileio/spc/HOLMIUM.SPC` = "18dc3b1ca3",
      `fileio/spc/IG_BKGND.SPC` = "0b083dab3a",
      `fileio/spc/IG_MULTI.SPC` = "fed652db3b",
      `fileio/spc/IG_SAMP.SPC` = "c72dd5fc70",
      `fileio/spc/KKSAM.SPC` = "8e905a5500",
      `fileio/spc/POLYR.SPC` = "78b5987d93",
      `fileio/spc/POLYS.SPC` = "608c01f69b",
      `fileio/spc/SINGLE POLYMER FILM.SPC` = "0e13423de4",
      `fileio/spc/SPECTRUM WITH BAD BASELINE.SPC` = "a05b77fada",
      `fileio/spc/time.spc` = "98eabdd347",
      `fileio/spc/TOLUENE.SPC` = "eb08948be8",
      `fileio/spc/TriVista-linear.spc` = "31b30dac34",
      `fileio/spc/TriVista-normal.spc` = "15d5d219b0",
      `fileio/spc/TUMIX.SPC` = "7f8db885fb",
      `fileio/spc/TWO POLYMER FILMS.SPC` = "173a0bb6d3",
      `fileio/spc/Witec-timeseries.spc` = "65f84533d8",
      `fileio/spc/XYTRACE.SPC` = "28594b6078"
    )

    for (f in other.spc) {
      ## for wholesale output of current hashes:
      # cat (sprintf ("`%s` = '%s',\n", f, digest (read.spc (f))))
      expect_known_hash(read.spc(f), checksums [f])
    }
  })

  test_that("LabRam spc files", {
    skip_if_not_fileio_available()
    expect_known_hash(read.spc("fileio/spc.LabRam/LabRam-1.spc"), "d67562e4b4")
    expect_known_hash(read.spc("fileio/spc.LabRam/LabRam-2.spc"), "c87094210a")
  })

  test_that("Shimadzu spc files do not yet work", {
    skip_if_not_fileio_available()
    expect_error(read.spc("fileio/spc.Shimadzu/F80A20-1.SPC"))

    fname <- "fileio/spc.Shimadzu/UV-2600_labeled_DNA"
    # TODO #102 - implement support for Shimadzu files
    SHIMADZU_SPC_IMPLEMENTED <- F
    if (SHIMADZU_SPC_IMPLEMENTED) {
      # Compare data from SPC file and from CSV file. They should be equal
      spc <- read.spc(paste0(fname, ".spc"))
      expected <- read.txt.long(paste0(fname, ".csv"), sep = ",")
      expect_true(all.equal(spc$spc, expected$spc))
    } else {
      # IF NOT IMPLEMENTED
      # expect_error (read.spc("fileio/spc.Shimadzu/F80A20-1.SPC"), regexp = 'Shimadzu SPC')
      expect_error(read.spc(paste0(fname, ".spc")),
        regexp = "Support for Shimadzu SPC file format (OLE CF) is not yet implemented",
        fixed = T
      )
    }
  })



  test_that("Witec: some files supported", {
    skip_if_not_fileio_available()

    expect_error(read.spc("fileio/spc.Witec/P_A32_006_Spec.Data 1.spc"))
    expect_error(read.spc("fileio/spc.Witec/P_A32_007_Spec.Data 1.spc"))

    tmp <- read.spc("fileio/spc.Witec/Witec-Map.spc")
    expect_known_hash(tmp, "d737a0a777")
    ## no spatial information
    expect_null(tmp$x)
    expect_null(tmp$y)

    ## spectra numbered in z
    tmp <- read.spc("fileio/spc.Witec/Witec-timeseries.spc")
    expect_known_hash(tmp, "d6879317f2")
  })


  ## Kaiser spc files tested mostly in Kaiser-specific read.spc.Kaiser* unit tests


  test_that("wplanes", {
    skip_if_not_fileio_available()
    skip("wplanes do not yet work")
    # wplanes
  })

  test_that("option file.keep.name", {
    skip_if_not_fileio_available()
    file.keep.name <- hy.getOption("file.keep.name")

    hy.setOptions(file.keep.name = FALSE)
    expect_null(read.spc("fileio/spc.LabRam/LabRam-2.spc")$filename)
    hy.setOptions(file.keep.name = TRUE)
    expect_equal(read.spc("fileio/spc.LabRam/LabRam-2.spc")$filename, "fileio/spc.LabRam/LabRam-2.spc")

    hy.setOptions(file.keep.name = file.keep.name)
  })

  test_that("option file.remove.emptyspc", {
    skip("no spc files with empty spectra available so far")
    skip_if_not_fileio_available()
    file.remove.emptyspc <- hy.getOption("file.remove.emptyspc")

    hy.setOptions(file.remove.emptyspc = FALSE)
    expect_equal(nrow(read.spc("")), NA)
    hy.setOptions(file.remove.emptyspc = TRUE)
    expect_equal(nrow(read.spc("")), NA)

    hy.setOptions(file.keep.name = file.remove.emptyspc)
  })

  test_that("hdr2data", {
    skip_if_not_fileio_available()
    expect_equal(
      colnames(read.spc("fileio/spc.LabRam/LabRam-2.spc", keys.hdr2data = TRUE)),
      c(
        "z", "z.end", "ftflgs", "fexper", "fexp", "fnpts", "ffirst",
        "flast", "fnsub", "fxtype", "fytype", "fztype", "fpost", "fdate",
        "fres", "fsource", "fspare", "fcmnt", "fcatxt", "flogoff", "fmods",
        "fprocs", "flevel", "fsampin", "ffactor", "fmethod", "fzinc",
        "fwplanes", "fwinc", "fwtype", ".last.read", "subfiledir", "spc",
        "filename"
      )
    )
  })


  test_that("log2data", {
    skip_if_not_fileio_available()
    expect_equal(
      colnames(read.spc("fileio/spc.Kaisermap/ebroAVII.spc", keys.log2data = TRUE)),
      c(
        "z", "z.end", "Grams_File_Name", "HoloGRAMS_File_Name", "Acquisition_Date_Time",
        "Lambda", "Accuracy_Mode", "Dark_subtracted", "Dark_File_Name",
        "Auto_New_Dark_Curve", "Background_subtracted", "Background_File_Name",
        "Intensity_Corrected", "Intensity_Calibration_Available", "Intensity_Correction_File",
        "Intensity_Correction_Threshold", "Intensity_Source_Correction",
        "Intensity_Source_Correction_File", "Comment", "Cosmic_Ray_Filtering",
        "Total_Cosmic_Count", "Exposure_Length", "Accumulations", "Accumulation_Method",
        "Calibration_File", "Comment.1", "Temperature_Status", "Temperature",
        "HoloGRAMS_File_Version", "File_Type", "Operator", "Stage_X_Position",
        "Stage_Y_Position", "Stage_Z_Position", "AutoFocusUsed", "WLInterval",
        "CalInterval", "FFTFillFactor", "FFTApT", "SamplingMethod", "Has_MultiPlex_Laser",
        "External_Trigger", "Laser_Wavelength", "Default_Laser_Wavelength",
        "Laser_Tracking", "Laser_Block_Active", "Pixel_Fill_minimum",
        "Pixel_Fill_maximum", "Binning_Start", "Binning_End", "NumPoints",
        "First", "last", "spc", "filename"
      )
    )
  })
}


.prepare.hdr.df <- function(data, nsubfiles) {
  ## the *type header elements are expressions. They need to be converted to character.
  data <- lapply(data, function(x) {
    if (mode(x) == "expression") {
      as.character(x)
    } else {
      x
    }
  })

  ## convert vectors to matrix, otherwise the data.frame will contain one  row per element.
  ## matrices need to be protected during as.data.frame

  vector.entries <- which(sapply(data, length) > 1L)
  for (v in vector.entries) {
    data[[v]] <- I(t(as.matrix(data[[v]])))
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- data [rep(1L, nsubfiles), ]

  for (v in vector.entries) {
    data[[v]] <- unclass(data[[v]])
  } # remove AsIs protection

  data
}

# Helper functions -----------------------------------------------------------
### -----------------------------------------------------------------------------
###
### split.string - split string at pattern
###
###

split.string <- function(x, separator, trim.blank = TRUE, remove.empty = TRUE) {
  pos <- gregexpr(separator, x)
  if (length(pos) == 1 && pos[[1]] == -1) {
    return(x)
  }

  pos <- pos[[1]]

  pos <- matrix(c(
    1, pos + attr(pos, "match.length"),
    pos - 1, nchar(x)
  ),
  ncol = 2
  )

  if (pos[nrow(pos), 1] > nchar(x)) {
    pos <- pos[-nrow(pos), ]
  }

  x <- apply(pos, 1, function(p, x) substr(x, p[1], p[2]), x)

  if (trim.blank) {
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    x <- sub(blank.pattern, "\\1", x)
  }

  if (remove.empty) {
    x <- x[sapply(x, nchar) > 0]
  }

  x
}

# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(split.string) <- function() {
  context("split.string")

  # Perform tests
  test_that("split.string() returnts output silently", {
    expect_error(split.string())
    expect_error(split.string(letters))

    expect_silent(split.string("letters", "r"))
  })

  # FIXME (tests): add tests to check the correctness of the output!!!
}


### -----------------------------------------------------------------------------
###
### getbynames - get list elements by name and if no such element exists, NA
###
###

getbynames <- function(x, e) {
  x <- x[e]
  if (length(x) > 0) {
    if (is.character(e)) {
      names(x) <- e
    }
    x[sapply(x, is.null)] <- NA
    x
  } else {
    list()
  }
}



# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(getbynames) <- function() {

  context("getbynames")

  # Perform tests
  test_that("getbynames() works", {

    lst <- list(a = 1, b = "b", c = 2i)

    expect_equal(getbynames(lst, "a"), list(a = 1))
    expect_equal(getbynames(lst, 1),   list(a = 1))
    expect_equal(getbynames(lst, 2),   list(b = "b"))
    expect_equal(getbynames(lst, 6)[[1]], NA)

  })
}

