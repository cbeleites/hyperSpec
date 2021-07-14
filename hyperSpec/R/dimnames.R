# Function -------------------------------------------------------------------

.dimnames <- function(x) {
  validObject(x)

  list(
    row  = rownames(x@data),
    data = colnames(x@data),
    wl   = colnames(x@data$spc)
  )
}

#' Dimnames for `hyperSpec` objects
#'
#' `hyperSpec` objects can have row- and column names like data.frames.
#' The "names" of the wavelengths are treated separately: see [wl()].
#'
#' @param x the `hyperSpec` object
#'
#' @rdname dimnames
#' @aliases dimnames
#' @docType methods
#'
#' @author C. Beleites
#'
#' @keywords methods
#' @concept manipulation
#' @seealso [wl()] for the wavelength dimension
#'
#' [base::dimnames()]
#'
#' @export
#'
#' @examples
#' dimnames(flu)
setMethod("dimnames", signature = signature(x = "hyperSpec"), .dimnames)


# Function -------------------------------------------------------------------

.rownames <- function(x, do.NULL = TRUE, prefix = "row") {
  validObject(x)

  rownames(x@data, do.NULL = do.NULL, prefix = prefix)
}

#' @rdname dimnames
#' @aliases rownames
#'
#' @param do.NULL handed to [base::rownames()] or [base::colnames()]: logical.
#'        Should this create names if they are `NULL`?
#' @param prefix handed to [base::rownames()] or [base::colnames()]
#'
#' @seealso [base::rownames()]
#'
#' @concept manipulation
#'
#' @export
#'
#' @examples
#' rownames(flu)
setMethod("rownames", signature = signature(x = "hyperSpec"), .rownames)


# Function -------------------------------------------------------------------

.rownames_replace <- function(x, value) {
  validObject(x)

  rownames(x@data) <- value
  x
}

#' @name rownames<-
#' @rdname dimnames
#' @aliases rownames<-,hyperSpec-method
#'
#' @param value the new names
#' @usage
#' \S4method{rownames}{hyperSpec}(x) <- value
#'
#' @export "rownames<-"
#'
#' @concept manipulation
#'
setReplaceMethod("rownames",
  signature = signature(x = "hyperSpec"),
  .rownames_replace
)


# Function -------------------------------------------------------------------

.colnames <- function(x, do.NULL = TRUE, prefix = "col") {
  validObject(x)
  colnames(x@data, do.NULL = do.NULL, prefix = prefix)
}

#' @rdname dimnames
#' @aliases colnames
#' @seealso [base::colnames()]
#'
#' @concept manipulation
#'
#' @export
#'
#' @examples
#' colnames(faux_cell)
setMethod("colnames", signature = signature(x = "hyperSpec"), .colnames)


# Function -------------------------------------------------------------------

.colnames_replace <- function(x, value) {
  validObject(x)

  names(x@label[colnames(x@data)]) <- value
  colnames(x@data) <- value

  validObject(x) # necessary: $spc could be renamed!
  x
}

#' @name colnames<-
#' @rdname dimnames
#' @aliases colnames<-,hyperSpec-method
#'
#' @usage
#' \S4method{colnames}{hyperSpec}(x) <- value
#'
#' @export "colnames<-"
#'
#' @concept manipulation
#'
setReplaceMethod("colnames",
  signature = signature(x = "hyperSpec"),
  .colnames_replace
)


# Unit tests -----------------------------------------------------------------

# TODO: add unit tests
