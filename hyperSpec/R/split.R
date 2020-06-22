.split <- function(x, f, drop = TRUE) {
  validObject(x)

  hyperlist <- split(seq(x, index = TRUE), f, drop)

  for (i in seq_along(hyperlist)) {
    hyperlist[[i]] <- x[hyperlist[[i]], ]
  }

  hyperlist
}

#' Split a hyperSpec object according to groups
#' `split` divides the `hyperSpec` object into a list of
#' `hyperSpec` objects according to the groups given by `f`.
#'
#' The `hyperSpec` objects in the list may be bound together again by
#' \code{\link{bind} ("r", list_of_hyperSpec_objects)}.
#'
#' @name split
#' @rdname split
#' @aliases split split-methods split,ANY-method split,hyperSpec-method
#' @docType methods
#' @param x the `hyperSpec` object
#' @param f a factor giving the grouping (or a variable that can be converted
#'   into a factor by `as.factor`)
#' @param drop if `TRUE`, levels of`f` that do not occur are
#'   dropped.
#' @return A list of `hyperSpec` objects.
#' @author C. Beleites
#' @seealso \code{\link[base]{split}}
#' @keywords methods
#' @export
#' @examples
#'
#' dist <- pearson.dist(faux_cell[[]])
#' dend <- hclust(dist, method = "ward")
#' z <- cutree(dend, h = 0.15)
#'
#' region <- split(faux_cell, z)
#' length(region)
#'
#' # difference in cluster mean spectra
#' plot(apply(region[[2]], 2, mean) - apply(region[[1]], 2, mean))
setMethod("split", signature = signature(x = "hyperSpec"), .split)
