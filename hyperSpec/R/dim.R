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
#' @param x a `hyperSpec` object
#' @author C. Beleites
#' @seealso [base::ncol()]
#'
#' @return `nrow()`, `ncol()`, `nwl()`, and `length()`, return an `integer`.
#'
#' @export
#'
#' @concept summary
#'
#' @examples
#' ncol(faux_cell)
setMethod("ncol", signature = signature("hyperSpec"), function(x) {
  validObject(x)

  ncol(x@data)
})

#' @details
#' - `nrow()` yields the number of rows in `x@@data`, i.e. the number of
#'   spectra in the `hyperSpec` object.
#'
#' @rdname dim
#' @seealso [base::nrow()]
#' @export
#' @examples
#'
#' nrow(faux_cell)
setMethod("nrow", signature = signature("hyperSpec"), function(x) {
  validObject(x)

  nrow(x@data)
})

#' @details
#' - `nwl()` returns the number of columns in `x@@data$spc`, i.e. thelength of
#'   each spectrum.
#'
#' @rdname dim
#' @aliases nwl
#' @export
#' @examples
#'
#' nwl(faux_cell)
nwl <- function(x) {
  chk.hy(x)
  validObject(x)

  ncol(x@data$spc)
}



#' @details
#' - `dim()` returns all three values in a vector.
#'
#' @rdname dim
#' @return
#'
#' `dim()` returns a vector of length 3.
#' @seealso [base::dim()]
#' @keywords methods
#' @export
#' @examples
#'
#' dim(faux_cell)
setMethod("dim", signature = signature("hyperSpec"), function(x) {
  validObject(x)
  c(nrow = nrow(x@data), ncol = ncol(x@data), nwl = ncol(x@data$spc))
})

#' @details
#' - `length()` is a synonym for `nrow()`. It is supplied so that `seq_along(x)`
#' returns a sequence to index each spectrum.
#' @rdname dim
#' @seealso [base::length()]
#' @export
#' @examples
#'
#' length(faux_cell)
setMethod("length", signature = signature("hyperSpec"), function(x) {
  validObject(x)
  nrow(x@data)
})
