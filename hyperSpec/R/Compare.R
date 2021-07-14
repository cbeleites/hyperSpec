# Function -------------------------------------------------------------------

.Compare_hh <-  function(e1, e2) {
    validObject(e1)
    validObject(e2)

    callGeneric(e1[[]], e2[[]])
  }


#' @title Comparison of `hyperSpec` objects
#'
#' @description
#' The comparison operators `>`, `<`, `>=`, `<=`, `==`, and `!=` for `hyperSpec`
#' objects.
#'
#' `all.equal` checks the equality of two `hyperSpec` objects.
#'
#' The comparison operators `>`, `<`, `>=`, `<=`,
#' `==`, and `!=` work on the spectra matrix of the `hyperSpec`
#' object. They have their usual meaning (see [base::Comparison()]).
#' The operators work also with one `hyperSpec` object and a numeric
#' (scalar) object or a matrices of the same size as the spectra matrix of the
#' `hyperSpec` object.
#'
#' With numeric vectors [hyperSpec::sweep()] might be more
#' appropriate.
#'
#' If you want to calculate on the `data.frame` `hyperSpec@data`,
#' you have to do this directly on `hyperSpec@data`.
#'
#' @name Comparison
#' @rdname Comparison
#' @aliases Comparison Operators
#' Comparison,hyperSpec
#' comparison,hyperSpec
#' Operators,hyperSpec
#' operators,hyperSpec
#' compare,hyperSpec
#' Compare,hyperSpec-method
#' Compare,hyperSpec,hyperSpec-method
#' Compare,hyperSpec,matrix-method
#' Compare,hyperSpec,numeric-method
#' Compare,matrix,hyperSpec-method
#' Compare,numeric,hyperSpec-method
#' <,hyperSpec,hyperSpec-method
#' >,hyperSpec,hyperSpec-method
#' <=,hyperSpec,hyperSpec-method
#' >=,hyperSpec,hyperSpec-method
#' ==,hyperSpec,hyperSpec-method
#' !=,hyperSpec,hyperSpec-method
#' @docType methods
#'
#' @param e1,e2 Either two `hyperSpec` objects or one `hyperSpec`
#'   object and matrix of same size as `hyperSpec[[]]` or a scalar
#'   (numeric of length 1).
#'
#' As `hyperSpec` objects must have numeric spectra matrices, the
#'   resulting matrix of the comparison is returned directly.
#' @return a logical matrix for the comparison operators.
#' @seealso [hyperSpec::sweep-methods()] for calculations involving
#'   a vector and the spectral matrix.
#'
#' [methods::S4groupGeneric()] for group generic methods.
#'
#' [base::Comparison()] for the base comparison functions.
#'
#' [hyperSpec::Arith()] for arithmetic operators,
#'   [hyperSpec::Math()] for mathematical group generic functions
#'   (groups Math and Math2) working on `hyperSpec` objects.
#'
#' @author C. Beleites
#'
#' @keywords methods arith
#' @concept manipulation
#'
#' @export
#'
#' @examples
#'
#' flu[, , 445 ~ 450] > 300
#'
#' all(flu == flu[[]])
setMethod("Compare", signature(e1 = "hyperSpec", e2 = "hyperSpec"), .Compare_hh)


# Function -------------------------------------------------------------------

.compx <- function(e1, e2) {
  validObject(e1)
  callGeneric(e1[[]], e2)
}

#' @rdname Comparison
setMethod("Compare", signature(e1 = "hyperSpec", e2 = "numeric"), .compx)

#' @rdname Comparison
setMethod("Compare", signature(e1 = "hyperSpec", e2 = "matrix"), .compx)


# Function -------------------------------------------------------------------

.compy <- function(e1, e2) {
  validObject(e2)
  callGeneric(e1, e2[[]])
}

#' @rdname Comparison
setMethod("Compare", signature(e1 = "numeric", e2 = "hyperSpec"), .compy)

#' @rdname Comparison
setMethod("Compare", signature(e1 = "matrix", e2 = "hyperSpec"), .compy)


# Unit tests -----------------------------------------------------------------

# TODO: add unit tests

