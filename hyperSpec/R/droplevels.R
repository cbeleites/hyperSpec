.droplevels <- function (x, ...){
  x@data <- droplevels(x@data, ...)

  x
}

#' droplevels for hyperSpec object
#'
#'  calls droplevels on the data.frame in spc@data.
#'
#'  Use parameter \code{except} to exclude columns from dropping.
#'
#' @param x hyperSpec object
#' @param ... handed to \link[base]{droplevels}
#'
#' @return hyperSpec object with unused levels of all factors in @data dropped.
#' @seealso \link[base]{droplevels}
#' @export
#'
#' @examples
#'
#' chondro[1:3]$clusters
#' droplevels (chondro [1:3])$clusters
setMethod("droplevels", signature = "hyperSpec", definition = .droplevels)
