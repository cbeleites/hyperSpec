### -----------------------------------------------------------------------------
###
### .extract - internal function doing the work for extracting with [] and [[]]
###

#' @include wl2i.R
#' @noRd
.extract <- function(x, i, j, l,
                     ...,
                     wl.index = FALSE) {
  if (!missing(i)) {
    x@data <- x@data[i, , drop = FALSE]
  }

  if (!missing(j)) {
    x@data <- x@data[, j, drop = FALSE]
    x@label <- x@label [c(".wavelength", colnames(x@data))]
  }

  if (!missing(l)) {
    if (is.null(x@data$spc)) {
      warning("Selected columns do not contain specta. l ignored.")
    } else {
      if (!wl.index) {
        l <- wl2i(x, l)
      }

      x@data$spc <- x@data$spc[, l, drop = FALSE]
      .wl(x) <- x@wavelength[l]
    }
  }

  x
}

#' These Methods allow to extract and replace parts of the `hyperSpec` object.
#'
#' They work with respect to the spectra (rows of `x`), the columns of the data matrix, and the
#' wavelengths (columns of the spectra matrix).
#'
#' Thus, they can be used for selecting/deleting spectra, cutting the spectral range, and extracting
#' or setting the data belonging to the spectra.
#'
#' Convenient shortcuts for access of the spectra matrix and the `data.frame` in slot
#' `data` are provided.
#'
#' *Extracting: `[`, `[[`, and `$`*.
#'
#' The version with single square brackets (`[`) returns the resulting `hyperSpec` object.
#'
#' `[[` yields `data.frame` of slot `@@data` of that corresponding `hyperSpec`
#' object returned with the same arguments by `[` if columns were selected (i.e. `j` is
#' given), otherwise the spectra `matrix` `x@@data$spc`.
#'
#' `$` returns the selected column of the `data.frame` in slot `@@data`.
#'
#' *Shortcuts.* Three shortcuts to conveniently extract much needed parts of the object are
#' defined:
#'
#' `x[[]]` returns the spectra matrix.
#'
#' `x$.` returns the complete slot `@@data`, including the spectra matrix in column
#' `$spc`, as a `data.frame`.
#'
#' `x$..` returns a `data.frame` like `x$.` but without the spectra matrix.
#'
#' *Replacing: `[<-`, `[[<-`, and `$<-`*.
#' \preformatted{
#' ## S4 method for signature 'hyperSpec':
#' x [i, j, l, \dots] <- value
#'
#' ## S4 method for signature 'hyperSpec':
#' x[[i, j, l, wl.index = FALSE, \dots]] <- value
#'
#' ## S4 method for signature 'hyperSpec':
#' x$name <- value
#' }
#'
#' `value` gives the values to be assigned.\cr
#'
#' For `$`, this can also be a list of the form `list (value =
#' value, label = label)`, with `label` containing the label for data
#' column `name`.
#'
#' `[[<-` replaces parts of the spectra matrix.
#'
#' `[<-` replaces parts of the `data.frame` in slot `x@@data`.
#'
#' `$<-` replaces a column of the `data.frame` in slot
#' `x@@data`.  The `value` may be a list with two elements,
#' `value` and `label`.  In this case the label of the data column
#' is changed accordingly.
#'
#' `$..<-` is again an abbreviation for the data.frame without the
#' spectra matrix.
#'

#' @title Extract and Replace parts of hyperSpec objects
#' @rdname extractreplace
#' @docType methods
#' @aliases [ [,hyperSpec-method
#' @param x a `hyperSpec` Object
#' @param i row index: selects spectra
#'
#' `[[` and code{[[<-} accept indexing with logical matrix or a n by 2
#'   integer index matrix. In this case the indexing is done inside the
#'   spectra matrix. See the examples below.
#' @param j selecting columns of `x@@data`
#' @param l selecting columns of the spectra matrix. If `l` is numeric,
#'   the default behaviour is treating `l` as wavelengths, *not* as
#'   indices.
#' @param wl.index If `TRUE` (default), the value(s) in `l` are
#'   treated as column indices for the spectral matrix. Otherwise, the numbers
#'   in `l` are treated as wavelengths and the corresponding column
#'   indices are looked up first via \code{\link{wl2i}}.
#' @param drop For `[[`: drop unnecessary dimensions, see
#'   \code{\link[base]{drop}} and \code{\link[base]{Extract}}. Ignored for
#'   `[`, as otherwise invalid `hyperSpec` objects might result.
#' @param ... ignored
#' @return For `[`, `[<-`, `[[<-`, and `$<-` a `hyperSpec` object,
#'
#' for `[[` a matrix or `data.frame`, and
#'
#' for `$` the column of the `data.frame` `@@data`.
#'
#' `x[[]]` returns the complete spectra matrix.
#'
#' `x$.` returns the complete slot `@@data`,
#'
#' `x$..` returns the `data.frame` in `@@data` but without the column
#' `@@data$spc` containing the spectra matrix.
#' @seealso \code{\link{wl2i}} on conversion of wavelength ranges to indices.
#'
#' \code{\link[base]{drop}} and \code{\link[base]{Extract}} on `drop`.
#' @keywords methods manip
#' @examples
#'
#' ## index into the rows (spectra) -------------------------------------
#' ## make some "spectra"
#'
#' ## numeric index
#' plot(flu, "spc", lines.args = list(lty = 2))
#' plot(flu[1:3], "spc", add = TRUE, col = "red") # select spectra
#' plot(flu[-(1:3)], "spc", add = TRUE, col = "blue") # delete spectra
#'
#' ## logic index
#' plot(flu, "spc", lines.args = list(lty = 2))
#' index <- rnorm(6) > 0
#' index
#' plot(flu[index], "spc", add = TRUE, col = "red") # select spectra
#' plot(flu[!index], "spc", add = TRUE, col = "blue") # select spectra
#'
#' ## index into the data columns ---------------------------------------
#' range(faux_cell[[, "x"]])
#' colnames(faux_cell[[, 1]])
#' dim(faux_cell[[, c(TRUE, FALSE, FALSE)]])
#' faux_cell$x
#'
#'
#' ## the shortcut functions --------------------------------------------
#'
#' ## extract the spectra matrix
#' flu[[]]
#'
#' ## indexing via logical matrix
#' summary(flu[[flu < 125]])
#'
#' ## indexing the spectra matrix with index matrix n by 2
#' ind <- matrix(c(1, 2, 4, 406, 405.5, 409), ncol = 2)
#' ind
#' flu[[ind]]
#'
#' ind <- matrix(c(1, 2, 4, 4:6), ncol = 2)
#' ind
#' flu[[ind, wl.index = TRUE]]
#'
#' pca <- prcomp(flu[[]])
#'
#' ## result is data.frame, if j is given:
#' result <- flu[[, 1:2, 405 ~ 410]]
#' result
#' class(result)
#' colnames(result)
#'
#' ## extract the data.frame including the spectra matrix
#' flu$.
#' dim(flu$.)
#' colnames(flu$.)
#' flu$.$spc
#'
#' calibration <- lm(spc ~ c, data = flu[, , 450]$.)
#' calibration
#'
#' flu$..
#' colnames(flu$..)
#' @include call.list.R
#' @export
setMethod("[",
  signature = signature(x = "hyperSpec"),
  function(x, i, j, l, ...,
           wl.index = FALSE,
           drop = FALSE # drop has to be at end
  ) {
    validObject(x)

    if (drop) {
      warning("Ignoring drop = TRUE.")
    }

    dots <- list(...)
    if (length(dots) > 0L) {
      warning("Ignoring additional parameters: ", .pastenames(dots))
    }

    x <- .extract(x, i, j, l, wl.index = wl.index)

    if (is.null(x@data$spc)) {
      x@data$spc <- matrix(NA, nrow(x@data), 0)
      x@wavelength <- numeric(0)
    }

    x
  }
)

#' @rdname extractreplace
#' @export
#' @aliases [[ [[,hyperSpec-method
## ' @name [[
setMethod("[[",
  signature = signature(x = "hyperSpec"),
  function(x, i, j, l, ...,
           wl.index = FALSE,
           drop = FALSE) {
    validObject(x)

    dots <- list(...)
    if (length(dots) > 0L) {
      warning("Ignoring additional parameters: ", .pastenames(dots))
    }

    ## check wheter a index matrix is used
    if (!missing(i) && is.matrix(i)) {
      if (!is.logical(i) && !(is.numeric(i) && ncol(i) == 2)) {
        stop(
          "Index matrix i  must either be logical of the size of x$spc,",
          "or a n by 2 matrix."
        )
      }

      if (is.numeric(i) && !wl.index) {
        i [, 2] <- .getindex(x, i [, 2], extrapolate = FALSE)
      }

      x@data$spc [i] # return value
    } else { # index by row and columns
      x <- .extract(x, i, j, l, wl.index = wl.index)
      if (missing(j)) {
        unclass(x@data$spc[, , drop = drop])
      } # retrun value; removes the "AsIs"
      else {
        x@data[, , drop = drop] # return value: data.frame
      }
    }
  }
)

#' @rdname extractreplace
#' @param name name of the data column to extract. `$spc` yields the spectra matrix.
#' @aliases $ $,hyperSpec-method
#' @export
setMethod("$",
  signature = signature(x = "hyperSpec"),
  function(x, name) {
    validObject(x)

    if (name == ".") { ## shortcut
      x@data [, , drop = FALSE]
    } else if (name == "..") {
      x@data[, -match("spc", colnames(x@data)), drop = FALSE]
    } else {
      x@data[[name]]
    }
  }
)
