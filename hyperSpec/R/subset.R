
.subset <- function(x, ...) {
  validObject(x)
  x@data <- subset(x@data, ...)
  validObject(x)

  x
}


#' Subset `hyperSpec` object
#'
#' @name subset
#' @param x `hyperSpec` object
#' @param ... handed to [base::subset()] (data.frame method)
#' @docType methods
#' @aliases subset subset,hyperSpec-method
#' @return `hyperSpec` object containing the respective subset of spectra.
#' @author Claudia Beleites
#' @seealso [base::subset()]
#'
#' @export
#'
#' @concept manipulation
#'
setMethod("subset", signature = signature(x = "hyperSpec"), .subset)
