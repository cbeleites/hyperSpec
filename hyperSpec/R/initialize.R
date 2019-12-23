###-----------------------------------------------------------------------------
###
###  initialize -- initialization, called by new ("hyperSpec", ...)
###
###  C. Beleites
###

##' @include paste.row.R
##' @noRd
.initialize <- function (.Object, spc = NULL, data = NULL, wavelength = NULL, labels = NULL){

  ## do the small stuff first, so we need not be too careful about copies

  ## the wavelength axis
  if (! is.null (wavelength) && ! is.numeric (wavelength))
    stop ("wavelength is not numeric but ", class (wavelength), ".")

  if (!is.null (spc)){
    if (is.null (dim (spc))){
      nwl <- length (spc)
      if (.options$gc) gc ()
      dim (spc) <- c(1, nwl)
      if (.options$gc) gc ()
    } else {
      nwl <- ncol (spc)
    }
  } else if (!is.null (data$spc))
    nwl <- ncol (data$spc)
  else
    nwl <- 0


  if (is.null (wavelength)){
    ## guess from spc's colnames
    if (!is.null (spc))
      wavelength <- as.numeric (colnames (spc))

    if (length (wavelength) == 0L || any (is.na (wavelength)))
      wavelength <- as.numeric (colnames (data$spc))

    if (length (wavelength) == 0L || any (is.na (wavelength)))
      wavelength <- seq_len (nwl) # guessing didn't work
  } else if (! is.numeric (wavelength)) {
      stop ("wavelength must be numeric.")
  }
  .Object@wavelength <- wavelength

  ## column + wavelength axis labels
  if (is.null (labels) || length (labels) == 0L){
    cln <- c (colnames (data), '.wavelength')
    if (! any (grepl ("spc", cln)))
      cln <- c (cln, "spc")
    labels <- vector ("list", length (cln))
    names (labels) <- cln
    rm (cln)
  }

  ## transform labels into expressions
  .make.expression <- function (x){
    if (is.language (x) && ! is.expression (x))
      class (x) <- "expression"
    else if (is.character (x))
      x <- as.expression (x)
    x
  }

  labels <- lapply (labels, .make.expression)

  .Object@label <- labels

  rm (labels)
  if (.options$gc) gc ()

  if (! is.null (data$spc) && ! (is.null (spc)))
    warning ("Spectra in data are overwritten by argument spc.")

  ## deal with spectra
  if (is.null (spc) && is.null (data$spc)){
    spc <- structure(numeric (0), .Dim = c(0L, 0L))
  }

  if (! is.null (spc) && !is.matrix (spc)) {
    spc <- as.matrix (spc)
    if (ncol (spc) == 1L)
        spc <- t (spc)
  }


  if (!is.null (spc) && !is.numeric(spc) && !all (is.na (spc))){
    dim <- dim (spc)
    spc <- suppressWarnings (as.numeric(spc))
    if (all (is.na (spc)))
      stop ("spectra matrix needs to be numeric or convertable to numeric")
    else
      warning ("spectra matrix is converted from ", class (data$spc), " to numeric.")

    dim (spc) <- dim
  }

  if (.options$gc) gc ()

  if (! is.null (spc)){
    attr (spc, "class") <- "AsIs"       # I seems to make more than one copy
    if (.options$gc) gc ()
  }
  
  ## deal with extra data
  if (is.null (data)){
    data <- data.frame (spc = spc)
  } else if (! is.null (spc)){
    if (nrow (data) == 1 && nrow (spc) > 1)
      data <- data [rep (1, nrow (spc)), , drop = FALSE]

    data$spc <- spc
  }

  rm (spc)
  if (.options$gc) gc ()

  attr (data$spc, "class") <- NULL      # more than one copy!?
  if (.options$gc) gc ()

  colnames (data$spc) <- signif (wavelength, digits = 6) # for consistency with .wl<-
  
  .Object@data <- data
  if (.options$gc) gc ()


  ## finally: check whether we got a valid hyperSpec object
  validObject (.Object)

  .Object
}

##' Creating a hyperSpec Object
##'
##' Like other S4 objects, a hyperSpec object can be created by \code{new}. The
##' hyperSpec object is then \code{initialize}d using the given parameters.
##'
##' If option \code{gc} is \code{TRUE}, the initialization will have frequent
##' calls to \code{gc ()} which can help to avoid swapping or running out of
##' memory.
##'
##' @name initialize
##' @rdname initialize
##' @aliases initialize,hyperSpec-method initialize create
##'   create,hyperSpec-method new,hyperSpec-method new
##' @docType methods
##' @param .Object the new \code{hyperSpec} object.
##' @param data \code{data.frame}, possibly with the spectra in
##'   \code{data$spc}, and further variates in more columns.  A matrix can be
##'   entered as \emph{one} column of a data frame by: \code{data.frame (spc =
##'   I (as.matrix (spc)))}.
##'
##' However, it will usually be more convenient if the spectra are given in
##'   \code{spc}
##' @param spc the spectra matrix.
##'
##' \code{spc} does not need to be a matrix, it is converted explicitly by
##'   \code{I (as.matrix (spc))}.
##' @param wavelength The wavelengths corresponding to the columns of
##'   \code{data}. If no wavelengths are given, an appropriate vector is
##'   derived from the column names of \code{data$spc}. If this is not
##'   possible, \code{1 : ncol (data$spc)} is used instead.
##' @param labels A \code{list} containing the labels for the columns of the
##'   \code{data} slot of the \code{hyperSpec} object and for the wavelength
##'   (in \code{label$.wavelength}). The labels should be given in a form ready
##'   for the text-drawing functions (see \code{\link[grDevices]{plotmath}}).
##'
##' If \code{label} is not given, a list containing \code{NULL} for each of the
##'   columns of\code{data} and \code{wavelength} is used.
##' @author C.Beleites
##' @seealso \code{\link[methods]{new}} for more information on creating and
##'   initializing S4 objects.
##'
##' \code{\link[grDevices]{plotmath}} on expressions for math annotations as
##'   for slot \code{label}.
##'
##' \code{\link{hy.setOptions}}
##' @keywords methods datagen
##' @examples
##'
##' new ("hyperSpec")
##'
##' spc <- matrix (rnorm (12), ncol = 4)
##' new ("hyperSpec", spc = spc)
##' new ("hyperSpec", data = data.frame (x = letters[1:3]),
##'      spc = spc)
##'
##' colnames (spc) <- 600:603
##' new ("hyperSpec", spc = spc)  # wavelength taken from colnames (spc)
##'
##' # given wavelengths precede over colnames of spc
##' new ("hyperSpec", spc = spc, wavelength = 700:703)
##'
##' # specifying labels
##' h <- new ("hyperSpec", spc = spc, data = data.frame (pos = 1 : 3),
##'           label = list (spc = "I / a.u.",
##'                         .wavelength = expression (tilde (nu) / cm^-1),
##'                         pos = expression ("/" (x, mu*m)))
##' )
##'
##' plot (h)
##' plotc (h, spc ~ pos)
##'
setMethod ("initialize", "hyperSpec", .initialize)

##' @include unittest.R
.test (.initialize) <- function (){
  context (".initialize / new (\"hyperSpec\")")

  test_that("empty hyperSpec object", {
    expect_equal (dim (new ("hyperSpec")), c (nrow = 0L, ncol = 1L, nwl = 0L))
  })

  test_that("vector for spc", {
    h <- new ("hyperSpec", spc = 1 : 4)
    expect_equal (h@data$spc, matrix (1 : 4, nrow = 1, dimnames = list (NULL, 1:4)))
    expect_equal (as.numeric (colnames (h@data$spc)), 1:4)
    expect_equal (dim (h), c (nrow = 1L, ncol = 1L, nwl = 4L))
    expect_equal (h@wavelength, 1 : 4)
  })

  test_that("matrix for spc", {
    spc <- matrix (c(1 : 12), nrow = 3)
    h <- new ("hyperSpec", spc = spc)
    
    expect_equivalent (h@data$spc, spc)
    expect_equal (dimnames (h@data$spc), list (NULL, as.character (1:4)))
    expect_equal (dim (h@data$spc), dim (spc))
    
    expect_equal (dim (h), c (nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal (h@wavelength, 1 : 4)
  })

  spc <- matrix (c(1 : 12), nrow = 3)
  test_that("matrix with numbers in colnames for spc", {
    colnames(spc) <- c(600, 601, 602, 603)
    h <- new ("hyperSpec", spc = spc)
    expect_equal (h@data$spc, spc)
    expect_equal (dim (h), c (nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal (h@wavelength, c(600, 601, 602, 603))
  })

  colnames(spc) <- c(600, 601, 602, 603)
  test_that("spc and data given", {
    h <- new ("hyperSpec", spc = spc, data = data.frame (x = 3))
    expect_equal (h@data$spc, spc)
    expect_equal (dim (h), c (nrow = 3L, ncol = 2L, nwl = 4L))
    expect_equal (h@wavelength, c(600, 601, 602, 603))
    expect_equal (h@data$x, rep (3, 3L))
  })

  test_that("spc and data given, data has $spc column (which should be overwritten with warning)", {
    expect_warning(h <- new ("hyperSpec", spc = spc, data = data.frame (spc = 11:13)))
    expect_equal (h@data$spc, spc)
    expect_equal (dim (h), c (nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal (h@wavelength, c(600, 601, 602, 603))
  })

  test_that("spc and data given, different numbers of rows", {
    expect_error (new ("hyperSpec", spc = spc, data = data.frame (x = 11:12)))
  })

  test_that("only data given, data has $spc column with `I()`-protected matrix", {
    h <- new ("hyperSpec", data = data.frame (spc = I (spc)))
    expect_equal (h@data$spc, spc)
    expect_equal (dim (h), c (nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal (h@wavelength, c(600, 601, 602, 603))
  })

  test_that("spc is data.frame", {
    h <- new ("hyperSpec", spc = as.data.frame (spc))
    expect_equal (h@data$spc, spc)
    expect_equal (dim (h), c (nrow = 3L, ncol = 1L, nwl = 4L))
  })

  test_that("uncommon spectra matrix class that can be converted to numeric", {
    expect_warning (new ("hyperSpec", flu > 100))
  })

  test_that("spectra matrix class cannot be converted to numeric", {
    expect_error (new ("hyperSpec", matrix (letters [1:6], 3)))
  })

  test_that ("error if wavelength is not numeric", {
    expect_error(new ("hyperSpec", spc = NA, wavelength = letters [1:3]))
  })


  test_that("gc option", {
    option <- hy.getOption("gc")
    on.exit(hy.setOptions(gc = option))
    hy.setOptions(gc = TRUE)

    spc <- new ("hyperSpec", spc = flu [[]])
    expect_equal(spc [[]], flu [[]]) 
  })
}



#' as.hyperSpec: convenience conversion functions
#'
#' These functions are shortcuts to convert other objects into hypeSpec objects.
#'
#' @param X the object to convert.
#' A matrix is assumed to contain the spectra matrix,
#' a data.frame is assumed to contain extra data.
#' @param ... additional parameters that should be handed over to \code{new ("hyperSpec")} (initialize)
#'
#' @return hyperSpec object
#' @seealso \code{\link[hyperSpec]{initialize}}
#' @export
setGeneric ("as.hyperSpec",
            function (X, ...){
              stop ("as.hyperSpec is not available for objects of class ", class (X))
            }
)

#' @include guesswavelength.R
.as.hyperSpec.matrix <- function (X, wl = guess.wavelength (colnames (X)), ...){
  new ("hyperSpec", spc = X, wavelength = wl, ...)
}

#' @rdname as.hyperSpec
#' @param wl wavelength vector. Defaults to guessing from the column names in \code{X}
#' @param spc spectra matrix
#' @param labels list with labels
#' @export
#'
#' @examples
#' tmp <- data.frame(flu [[,, 400 ~ 410]])
#' (wl <- colnames (tmp))
#' guess.wavelength (wl)

setMethod ("as.hyperSpec", "matrix", .as.hyperSpec.matrix)

.as.hyperSpec.data.frame <- function (X, spc = NULL, wl = guess.wavelength (spc), labels = attr (X, "labels"), ...){
  # TODO: remove after 31.12.2020
  if (!all (!is.na (guess.wavelength(colnames(X)))))
    warning ("as.hyperSpec.data.frame has changed its behaviour. Use as.hyperSpec (as.matrix (X)) instead.")

  if (is.null (spc)){
    spc <- matrix (ncol = 0, nrow = nrow (X))
    wl <- numeric (0)
  }

  new ("hyperSpec", data = X, wavelength = wl, spc = spc, labels = labels, ...)
}

#' @rdname as.hyperSpec
#' @note \emph{Note that the behaviour of \code{as.hyperSpec (X)} was changed: it now assumes \code{X} to be extra data,
#' and returns a hyperSpec object with 0 wavelengths. To get the old behaviour}
setMethod ("as.hyperSpec", "data.frame", .as.hyperSpec.data.frame)

##' @include unittest.R
.test (as.hyperSpec) <- function (){
    context ("as.hyperSpec")

    spc <- matrix(1:12,ncol = 3)
    wl <- seq(600, 601, length.out = ncol(spc))

    test_that("only spc is given", {
        expect_identical (new ("hyperSpec", spc = spc), as.hyperSpec(X = spc))
    })

    test_that("data.frame", {
        tmp <- as.hyperSpec(flu$..)
        expect_equal(tmp$.., flu$..)
        expect_equal(dim (tmp), c (nrow = 6L, ncol = 3L, nwl = 0L))
        expect_equal(wl (tmp), numeric (0))
    })

    test_that("data.frame with labels attribute", {
      tmp <- flu$..
      attr (tmp, "labels") <- labels (flu)

      tmp <- as.hyperSpec(tmp)

      expect_equal(tmp$.., flu$..)
      expect_equal(dim (tmp), c (nrow = 6L, ncol = 3L, nwl = 0L))
      expect_equal(wl (tmp), numeric (0))
      expect_equal(labels (tmp) [order (names (labels (tmp)))],
                   lapply (labels (flu) [order (names (labels (flu)))], as.expression))
    })

    test_that("spc with characters in colnames", {
        colnames(spc) <- make.names(wl)
        h <- as.hyperSpec(X = spc)
        expect_equivalent (h@data$spc, spc)
        expect_equal (dim (h@data$spc), dim (spc))
        expect_equal (dim (h), c (nrow = nrow(spc), ncol = 1L, nwl = ncol(spc)))
        expect_equal (h@wavelength, wl)
        expect_equal (as.numeric (colnames (h@data$spc)), wl)
    })

    test_that("ignore colnames if wl is set", {
        colnames(spc) <- c(601,602,603)
        expect_identical (new ("hyperSpec", spc = spc, wavelength = wl), as.hyperSpec(X = spc, wl = wl))
    })

    test_that("set additional parameters", {
        dt <- data.frame(x=1:4,y=letters[1:4])
        lbs <-  list (spc = "I / a.u.", .wavelength = expression (tilde (nu) / cm^-1))
        expect_identical (new ("hyperSpec", spc = spc, data = dt, label = lbs), as.hyperSpec(X = spc, data = dt, label = lbs))
    })


    test_that ("error on unknown class", {
      tmp <- NA
      class (tmp) <- "foo"
      expect_error (as.hyperSpec(tmp))
    })

    test_that ("colnames of spectra matrix correctly set (as done by wl<-)", {
      tmp <- new ("hyperSpec", spc = spc, wavelength = wl)
      expect_equal (colnames (tmp$spc), as.character (signif (wl, 6)))
    })
}


