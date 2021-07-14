
# Function -------------------------------------------------------------------

.replace <- function(x, i, j, ..., value) {
  validObject(x)

  if (missing(i)) i <- row.seq(x)
  if (missing(j)) j <- col.seq(x)

  if (is(value, "hyperSpec")) {
    validObject(value)
    x@data[i, j] <- value@data
  } else {
    x@data[i, j] <- value
  }

  validObject(x)

  x
}

#' @rdname extractreplace
#' @name [<-
#' @aliases [<-,hyperSpec-method
#'
#' @usage
#'
#' \S4method{[}{hyperSpec}(x, i, j, \dots) <- value
#'
#' @param value the replacement value
#'
#' @concept manipulation
#'
#' @include wl2i.R
#' @include paste_row.R
#'
#' @export
#'
#' @examples
#' ## replacement functions
#' spc <- flu
#' \dontrun{
#' spc$.
#' }
#' spc[, "c"] <- 16:11
#' ## be careful:
#' plot(spc)
#' spc[] <- 6:1
#' spc$..
#' plot(spc)
setReplaceMethod("[", signature = signature(x = "hyperSpec"), .replace)


# Function -------------------------------------------------------------------

.replace2 <- function(x, i, j, l, wl.index = FALSE, ..., value) {
  validObject(x)

  if (is(value, "hyperSpec")) {
    validObject(value)
    value <- value@data$spc
  }

  ## check wheter a index matrix is used
  if (!missing(i) && is.matrix(i)) {
    if (is.logical(i)) {
      x@data$spc[i] <- value
    } else if (is.numeric(i) && ncol(i) == 2) {
      if (!wl.index) {
        i[, 2] <- .getindex(x, i[, 2], extrapolate = FALSE)
        if (any(is.na(i[, 2]))) {
          stop("wavelength specification outside spectral range")
        }
      }
      x@data$spc[i] <- value
    } else {
      stop(
        "Index matrix i  must either be logical of the size of x$spc,",
        "or a n by 2 matrix."
      )
    }
  } else { # index by row and columns
    if (!missing(j)) {
      stop(
        "The spectra matrix may only be indexed by i (spectra) and l",
        " (wavelengths). j (data column) must be missing."
      )
    }

    if (!missing(l) && !wl.index) {
      l <- wl2i(x, l)
    }

    x@data$spc[i, l] <- value
  }

  validObject(x)

  x
}


#' @rdname extractreplace
#' @name [[<-
#' @aliases [[<-,hyperSpec-method
#'
#' @usage
#'
#' \S4method{[[}{hyperSpec}(x, i, j, l, wl.index = FALSE, \dots) <- value
#'
#'
#' @concept manipulation
#'
#' @include wl2i.R
#' @export
#'
#' @examples
#' spc <- flu[, , 405 ~ 410]
#' spc[[]]
#' spc[[3]] <- -spc[[3]]
#' spc[[]]
#' spc[[, , 405:410]] <- -spc[[, , 405:410]]
#' spc[[]]
#' spc[[, , 405 ~ 410]] <- -spc[[, , 405 ~ 410]]
#'
#' ## indexing with logical matrix
#' spc <- flu[, , min ~ 410]
#' spc < 125
#' spc[[spc < 125]] <- NA
#' spc[[]]
#'
#' ## indexing with n by 2 matrix
#' ind <- matrix(c(1, 2, 4, 406, 405.5, 409), ncol = 2)
#' ind
#' spc[[ind]] <- 3
#' spc[[]]
#'
#' ind <- matrix(c(1, 2, 4, 4:6), ncol = 2)
#' ind
#' spc[[ind, wl.index = TRUE]] <- 9999
#' spc[[]]
setReplaceMethod("[[", signature = signature(x = "hyperSpec"), .replace2)


# Function -------------------------------------------------------------------

.replace_dollar <- function(x, name, value) {
  validObject(x)

  if (is.list(value) && (length(value) == 2)) {
    ilabel <- match("label", names(value))

    if (is.na(ilabel)) {
      ilabel <- 2
    }

    label <- value[[ilabel]]

    value <- value[[3 - ilabel]] ## the other of the 2 entries
  } else {
    label <- name
  }

  if (name == "..") { ## shortcut
    i <- -match("spc", colnames(x@data))
    x@data[, i] <- value

    if (!is.null(label)) {
      i <- colnames(x@data)[i]
      i <- match(i, names(x@label))
      x@label[i] <- label
    }
  } else {
    dots <- list(x = x@data, name = name, value = value)
    x@data <- do.call("$<-", dots)
    x@label[[name]] <- label
  }

  x
}


#' @rdname extractreplace
#' @name $<-
#' @aliases $<-,hyperSpec-method
#'
#' @usage
#'
#' \S4method{$}{hyperSpec}(x, name) <- value
#'
#' @concept manipulation
#'
#' @export
#'
#' @examples
#' spc$.
#' spc$z <- 1:6
#' spc
#' spc$z <- list(1:6, "z / a.u.")
setReplaceMethod("$", signature = signature(x = "hyperSpec"), .replace_dollar)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.replace) <- function() {
  ## replacement functions
  context("replace")

  test_that("replacement function `[<-` works", {
    spc <- flu

    # [ ]
    expect_silent(spc[, "c"] <- 16:11)
    expect_silent(spc[] <- 6:1)
  })

  test_that("replacement function `[[<-` works", {
    expect_silent(spc0 <- flu[, , 405 ~ 410])

    # [[ ]]
    spc <- spc0
    expect_silent(spc[[]])
    expect_silent(spc[[3]] <- -spc[[3]])
    expect_silent(spc[[]])

    spc <- spc0
    expect_silent(spc[[, , 405:410]] <- -spc[[, , 405:410]])
    expect_silent(spc[[]])

    spc <- spc0
    expect_silent(spc[[, , 405 ~ 410]] <- -spc[[, , 405 ~ 410]])

    ## indexing with logical matrix
    spc <- flu[, , min ~ 410]
    expect_silent(spc < 125)
    expect_silent(spc[[spc < 125]] <- NA)
    expect_silent(spc[[]])

    ## indexing with n by 2 matrix
    ind <- matrix(c(1, 2, 4, 406, 405.5, 409), ncol = 2)

    expect_silent(spc[[ind]] <- 3)
    expect_silent(spc[[]])

    ind <- matrix(c(1, 2, 4, 4:6), ncol = 2)
    expect_silent(spc[[ind, wl.index = TRUE]] <- 9999)
    expect_silent(spc[[]])
  })


  test_that("replacement function `$<-` works", {
    spc0 <- flu[, , 405 ~ 410]

    # $
    spc <- spc0
    expect_is(spc$.,  "data.frame")
    expect_is(spc$.., "data.frame")

    spc <- spc0
    expect_silent(spc$z <- 1:6)
    expect_true("z" %in% colnames(spc))
    expect_equal(spc$z, 1:6)

    spc <- spc0
    expect_silent(spc$z <- list(1:6, "z / a.u."))
    expect_equal(labels(spc, "z"),   "z / a.u.")
    expect_true("z" %in% colnames(spc))

    spc <- spc0
    expect_silent(spc$.. <- NULL)
    expect_equal(ncol(spc$..), 0) # empty data frame.
    expect_equal(ncol(spc),    1) # only "spc" column is left.
  })
}
