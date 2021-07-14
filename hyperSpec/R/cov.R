# Function -------------------------------------------------------------------

.cov_h_ <-  function(x, y, use, method) {
    validObject(x)

    cov(x@data$spc, use = use, method = method)
  }

#' Covariance matrices for `hyperSpec` objects
#'
#' @rdname cov
#'
#' @param x `hyperSpec` object
#' @param y not supported
#' @param use,method handed to [stats::cov()]
#'
#' @return covariance matrix of size `nwl(x)` x `nwl(x)`
#'
#' @concept stats
#'
#' @author C. Beleites
#'
#' @export
#'
#' @seealso [stats::cov()]
#'
#' @examples
#' image(cov(faux_cell))
setMethod("cov", signature = signature(x = "hyperSpec", y = "missing"), .cov_h_)


# Function -------------------------------------------------------------------

#' @rdname cov
#'
#' @param ... ignored
#' @param regularize regularization of the covariance matrix. Set `0` to switch off
#'
#' `pooled.cov` calculates pooled covariance like e.g. in LDA.
#' @param groups factor indicating the groups
#'
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

  x <- x[!is.na(groups)]
  groups <- groups[!is.na(groups)]

  means <- aggregate(x, groups, "mean") # TODO: speed up?

  COV <- cov(x@data$spc - means@data$spc[as.numeric(groups), , drop = FALSE])

  ## regularization
  COV <- COV + diag(regularize, nrow(COV))

  list(
    COV = COV,
    means = means
  )
}
