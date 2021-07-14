# Function -------------------------------------------------------------------

.subset <- function(x, ...) {
  validObject(x)
  x@data <- subset(x@data, ...)
  validObject(x)

  x
}


#' Subset `hyperSpec` object
#'
#' @name subset
#' @aliases subset subset,hyperSpec-method
#' @docType methods
#'
#' @param x `hyperSpec` object
#' @param ... handed to [base::subset()] (data.frame method)
#'
#' @return `hyperSpec` object containing the respective subset of spectra.
#'
#' @author Claudia Beleites
#'
#' @seealso [base::subset()]
#'
#' @export
#'
#' @concept manipulation
#'
setMethod("subset", signature = signature(x = "hyperSpec"), .subset)


# Unit tests -----------------------------------------------------------------

# TODO: add unit tests
