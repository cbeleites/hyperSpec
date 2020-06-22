#' dimnames for hyperSpec objects
#'
#' hyperSpec objects can have row- and column names like data.frames. The "names" of the wavelengths
#' are treated separately: see [wl()]
#'
#' @param x the hyperSpec object
#' @aliases dimnames
#' @keywords methods
#' @rdname dimnames
#' @docType methods
#' @author C. Beleites
#' @seealso [wl()] for the wavelength dimension
#'
#' [base::dimnames()]
#' @export
#' @examples
#' dimnames(flu)
setMethod("dimnames", signature = signature(x = "hyperSpec"), function(x) {
  validObject(x)

  list(
    row = rownames(x@data), data = colnames(x@data),
    wl = colnames(x@data$spc)
  )
})

#' @rdname dimnames
#' @aliases rownames
#' @param do.NULL handed to [base::rownames()] or [base::colnames()]: logical.
#' Should this create names if they are `NULL`?
#' @param prefix handed to [base::rownames()] or [base::colnames()]
#' @seealso [base::rownames()]
#' @export
#' @examples
#' rownames(flu)
setMethod("rownames", signature = signature(x = "hyperSpec"), function(x, do.NULL = TRUE, prefix = "row") {
  validObject(x)

  rownames(x@data, do.NULL = do.NULL, prefix = prefix)
})

#' @param value the new names
#' @usage
#' \S4method{rownames}{hyperSpec} (x) <- value
#' @aliases rownames<-,hyperSpec-method
#' @rdname dimnames
#' @name rownames<-
#' @export "rownames<-"
setReplaceMethod("rownames", signature = signature(x = "hyperSpec"), function(x, value) {
  validObject(x)

  rownames(x@data) <- value
  x
})

#' @rdname dimnames
#' @aliases colnames
#' @seealso [base::colnames()]
#' @export
#' @examples
#' colnames(faux_cell)
setMethod("colnames",
  signature = signature(x = "hyperSpec"),
  function(x, do.NULL = TRUE, prefix = "col") {
    validObject(x)
    colnames(x@data, do.NULL = do.NULL, prefix = prefix)
  }
)

#' @rdname dimnames
#' @usage
#' \S4method{colnames}{hyperSpec} (x) <- value
#' @aliases colnames<-,hyperSpec-method
#' @name colnames<-
#' @export "colnames<-"
setReplaceMethod("colnames",
  signature = signature(x = "hyperSpec"),
  function(x, value) {
    validObject(x)

    names(x@label [colnames(x@data)]) <- value
    colnames(x@data) <- value

    validObject(x) # necessary: $spc could be renamed!
    x
  }
)
