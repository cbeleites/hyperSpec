#' Covariance matrices for hyperSpec objects
#'
#'
#' @param x hyperSpec object
#' @param y not supported
#' @param use,method handed to  \code{\link[stats]{cov}}
#' @return covariance matrix of size `nwl (x)` x  `nwl (x)`
#' @seealso \code{\link[stats]{cov}}
#' @author C. Beleites
#' @rdname cov
#' @export
#' @examples
#' image(cov(faux_cell))
setMethod("cov", signature = signature(x = "hyperSpec", y = "missing"), function(x, y, use, method) {
  validObject(x)

  cov(x@data$spc, use = use, method = method)
})


#' @param ... ignored
#' @param regularize regularization of the covariance matrix. Set `0` to switch off
#'
#' `pooled.cov` calculates pooled covariance like e.g. in LDA.
#' @param groups factor indicating the groups
#' @rdname cov
#' @export
#' @examples
#' pcov <- pooled.cov(faux_cell, faux_cell$region)
#' plot(pcov$means)
#' image(pcov$COV)
pooled.cov <- function(x, groups, ..., regularize = 1e-5 * max(abs(COV))) {
  chk.hy(x)
  validObject(x)

  if (!is.factor(groups)) {
    stop("groups must be a factor")
  }

  x <- x      [!is.na(groups)]
  groups <- groups [!is.na(groups)]

  means <- aggregate(x, groups, "mean") # TODO: speed up?

  COV <- cov(x@data$spc - means@data$spc [as.numeric(groups), , drop = FALSE])

  ## regularization
  COV <- COV + diag(regularize, nrow(COV))

  list(
    COV = COV,
    means = means
  )
}
