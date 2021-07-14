# Function -------------------------------------------------------------------
.ncol <- function(x) {
  validObject(x)
  ncol(x@data)
}

#' Number of rows (spectra), columns, and data points per spectrum of a
#' `hyperSpec` object
#'
#' These functions return the number of rows (spectra), columns, and/or
#' data points per spectrum of a `hyperSpec` object.
#' See section "Details".
#'
#' @details
#' - `ncol()` returns the number of columns in `x@@data`. I.e. the number
#' of columns with additional information to each spectrum (e.g. "x", "y",
#' \dots{}) + 1 (for column `spc` containing the spectra).
#'
#' @rdname dim
#' @docType methods
#'
#' @param x a `hyperSpec` object
#'
#' @return `nrow()`, `ncol()`, `nwl()`, and `length()` return an `integer`.
#'
#' @author C. Beleites
#'
#' @concept summary
#' @seealso [base::ncol()]
#'
#' @export
#'
#' @examples
#' ncol(faux_cell)
setMethod("ncol", signature = signature("hyperSpec"), .ncol)


# Function -------------------------------------------------------------------

.nrow <- function(x) {
  validObject(x)

  nrow(x@data)
}

#' @rdname dim
#'
#' @details
#' - `nrow()` yields the number of rows in `x@@data`, i.e. the number of
#'   spectra in the `hyperSpec` object.
#'
#' @seealso [base::nrow()]
#' @export
#' @examples
#'
#' nrow(faux_cell)
setMethod("nrow", signature = signature("hyperSpec"), .nrow)


# Function -------------------------------------------------------------------

#' @rdname dim
#' @aliases nwl
#'
#' @details
#' - `nwl()` returns the number of columns in `x@@data$spc`, i.e. thelength of
#'   each spectrum.
#'
#' @export
#' @examples
#'
#' nwl(faux_cell)
nwl <- function(x) {
  chk.hy(x)
  validObject(x)

  ncol(x@data$spc)
}


# Function -------------------------------------------------------------------

.dim <- function(x) {
  validObject(x)
  c(nrow = nrow(x@data), ncol = ncol(x@data), nwl = ncol(x@data$spc))
}

#' @rdname dim
#' @details
#' - `dim()` returns all three values in a vector.
#'
#' @return
#'
#' `dim()` returns a vector of length 3.
#'
#' @seealso [base::dim()]
#' @keywords methods
#'
#' @export
#'
#' @examples
#'
#' dim(faux_cell)
setMethod("dim", signature = signature("hyperSpec"), .dim)


# Function -------------------------------------------------------------------

.length <- function(x) {
  validObject(x)
  nrow(x@data)
}

#' @rdname dim
#'
#' @details
#' - `length()` is a synonym for `nrow()`. It is supplied so that `seq_along(x)`
#' returns a sequence to index each spectrum.
#' @seealso [base::length()]
#'
#' @export
#'
#' @examples
#'
#' length(faux_cell)
setMethod("length", signature = signature("hyperSpec"), .length)


# Unit tests -----------------------------------------------------------------

# TODO: add unit tests
