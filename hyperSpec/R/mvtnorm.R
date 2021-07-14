# Set generic ----------------------------------------------------------------

.rmmvnorm <- function(n, mean, sigma) {
  if (!requireNamespace("mvtnorm")) {
    stop("package 'mvtnorm' needed to generate multivariate normal random data.")
  }

  .group <- rep.int(seq_along(n), n)

  ## make indices so that pooled or individual covariance matrices can be used.
  if (length(dim(sigma)) == 3L) {
    isigma <- seq_len(dim(sigma)[3])
  } else {
    isigma <- rep(1L, nrow(mean))
    dim(sigma) <- c(dim(sigma), 1L)
  }

  tmp <- matrix(NA_real_, sum(n), ncol(mean))
  for (i in seq_along(n)) {
    tmp[.group == i, ] <- mvtnorm::rmvnorm(n[i], mean[i, ], sigma[, , isigma[i]])
  }

  attr(tmp, "group") <- .group

  tmp
}

#' @export
#' @name rmmvnorm
setGeneric("rmmvnorm", .rmmvnorm)


# Function -------------------------------------------------------------------

.rmmvnorm_nhm <- function(n, mean, sigma) {
    tmp <- .rmmvnorm(n, mean@data$spc, sigma)

    data <- mean[attr(tmp, "group"), , drop = FALSE]
    if (hy.getOption("gc")) gc()
    data@data$spc <- tmp
    if (hy.getOption("gc")) gc()
    data$.group <- attr(tmp, "group")
    if (hy.getOption("gc")) gc()
    data
  }

#' Multivariate normal random numbers
#'
#' Interface functions to use [mvtnorm::rmvnorm()] for
#' [hyperSpec::hyperSpec-class()] objects.
#'
#' The `mvtnorm` method for `hyperSpec` objects supports producing multivariate
#' normal data for groups with different mean but common covariance matrix,
#' see the examples.
#'
#' @rdname rmmvnorm
#' @aliases rmmvnorm rmmvnorm,hyperSpec-method
#' @docType methods
#'
#' @param n vector giving the number of cases to generate for each group
#' @param mean matrix with mean cases in rows
#' @param sigma common covariance matrix or array
#' (`ncol(mean)` x `ncol(mean)` x `nrow(mean)`) with individual covariance
#' matrices for the groups.
#'
#' @concept data generation
#' @seealso [mvtnorm::rmvnorm()]
#'
#' [hyperSpec::cov()] and [hyperSpec::pooled.cov()] about calculating covariance
#' of `hyperSpec` objects.
#'
#' @export
#'
#' @examples
#' ## multiple groups, common covariance matrix
#'
#' if (require("mvtnorm")) {
#'   pcov <- pooled.cov(faux_cell, faux_cell$region)
#'   rnd <- rmmvnorm(rep(10, 3), mean = pcov$mean, sigma = pcov$COV)
#'   plot(rnd, col = rnd$.group)
#' }
setMethod("rmmvnorm",
  signature(n = "numeric", mean = "hyperSpec", sigma = "matrix"),
  .rmmvnorm_nhm
)


# Function -------------------------------------------------------------------

.rmmvnorm_nha <- function(n, mean, sigma) {
  tmp <- .rmmvnorm(n, mean@data$spc, sigma)

  data <- mean[attr(tmp, "group"), , drop = FALSE]
  if (hy.getOption("gc")) gc()
  data@data$spc <- tmp
  if (hy.getOption("gc")) gc()
  data$.group <- attr(tmp, "group")
  if (hy.getOption("gc")) gc()
  data
}

#' @rdname rmmvnorm
#' @export
setMethod("rmmvnorm",
  signature(n = "numeric", mean = "hyperSpec", sigma = "array"),
  .rmmvnorm_nha
)


# Function -------------------------------------------------------------------

#' @rdname rmmvnorm
#' @export
setMethod("rmmvnorm",
  signature(n = "numeric", mean = "matrix", sigma = "matrix"),
  .rmmvnorm
)


# Function -------------------------------------------------------------------

#' @rdname rmmvnorm
#' @export
setMethod("rmmvnorm",
  signature(n = "numeric", mean = "matrix", sigma = "array"),
  .rmmvnorm
)

# FIXME:
# produces matrices instead of hyperSpec objects.
# mapply(rmvnorm, n = 1:3, mean = pcov$mean, MoreArgs= list(sigma = pcov$COV), SIMPLIFY = FALSE)


# Unit testes ----------------------------------------------------------------

# TODO: add unit tests
