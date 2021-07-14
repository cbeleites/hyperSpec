# Set generic ----------------------------------------------------------------
# make generic functions without default

#' @noRd
setGeneric("mean_sd", function(x, na.rm = TRUE, ...) standardGeneric("mean_sd"))

#' @noRd
setGeneric("mean_pm_sd", function(x, na.rm = TRUE, ...) standardGeneric("mean_pm_sd"))


# Function -------------------------------------------------------------------

.mean_sd_num <- function(x, na.rm = TRUE, ...) {
  c(
    mean = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm)
  )
}

#' Mean and standard deviation
#'
#' Calculate mean and standard deviation, and mean, mean \eqn{\pm}{+-} one
#' standard deviation, respectively.
#'
#' These functions are provided for convenience.
#'
#' @aliases mean_sd
#' @rdname mean_sd
#'
#' @param x a numeric vector
#' @param na.rm handed to [base::mean()] and [stats::sd()]
#'
#' @param ... ignored (needed to make function generic)
#'
#' @return `mean_sd` returns a vector with two values (mean and standard
#'         deviation) of `x`.
#'
#' @keywords multivar
#' @concept stats
#' @seealso [base::mean()], [stats::sd()]
#'
#' @export
#'
#' @examples
#'
#' mean_sd(flu [, , 405 ~ 410])
setMethod("mean_sd", signature = signature(x = "numeric"), .mean_sd_num)


# Function -------------------------------------------------------------------

.mean_sd_mat <- function(x, na.rm = TRUE, ...) {
  m <- colMeans(x, na.rm = na.rm)
  s <- apply(x, 2, sd, na.rm = na.rm)
  rbind(mean = m, sd = s)
}

#' @rdname mean_sd
#' @return `mean_sd (matrix)` returns a matrix with the mean spectrum in the first row and the standard deviation in the 2nd.
#' @export
#' @examples
#'
#' mean_sd(flu$spc)
setMethod("mean_sd", signature = signature(x = "matrix"), .mean_sd_mat)


# Function -------------------------------------------------------------------

.mean_sd_hy <- function(x, na.rm = TRUE, ...) {
  decomposition(x, mean_sd(x@data$spc), scores = FALSE)
}

#' @rdname mean_sd
#' @return `mean_sd` returns a hyperSpec object with the mean spectrum in the first row and the standard deviation in the 2nd.
#' @author C. Beleites
#' @seealso [base::mean()], [stats::sd()]
#' @keywords univar
#' @export
#' @examples
#'
#' mean_sd(flu)
setMethod("mean_sd", signature = signature(x = "hyperSpec"), .mean_sd_hy)


# Function -------------------------------------------------------------------

.mean_pm_sd_num <- function(x, na.rm = TRUE, ...) {
    m <- mean(x, na.rm = na.rm)
    s <- sd(x, na.rm = na.rm)
    c("mean.minus.sd" = m - s, "mean" = m, "mean.plus.sd" = m + s)
  }

#' @rdname mean_sd
#' @aliases mean_pm_sd
#' @return
#'
#' `mean_pm_sd` returns a vector with 3 values: mean - 1 sd, mean, mean + 1 sd
#' @export
#' @examples
#'
#' mean_pm_sd(flu$c)
setMethod("mean_pm_sd",
  signature = signature(x = "numeric"), .mean_pm_sd_num)

# Function -------------------------------------------------------------------

.mean_pm_sd_mat <- function(x, na.rm = TRUE, ...) {
  m <- colMeans(x, na.rm = na.rm)
  s <- apply(x, 2, sd, na.rm = na.rm)
  rbind("mean - sd" = m - s, mean = m, "mean + sd" = m + s)
}

#' @rdname mean_sd
#' @return `mean_pm_sd (matrix)` returns a matrix containing mean - sd, mean,
#'         and mean + sd rows.
#' @export
#' @examples
#'
#' mean_pm_sd(flu$spc)
setMethod("mean_pm_sd", signature = signature(x = "matrix"), .mean_pm_sd_mat)


# Function -------------------------------------------------------------------

.mean_pm_sd_hy <- function(x, na.rm = TRUE, ...) {
  decomposition(x, mean_pm_sd(x@data$spc))
}

#' @rdname mean_sd
#' @return For hyperSpec objects, `mean_pm_sd` returns a hyperSpec object
#'         containing mean - sd, mean, and mean + sd spectra.
#' @export
#' @examples
#'
#' mean_pm_sd(flu)
setMethod("mean_pm_sd", signature = signature(x = "hyperSpec"), .mean_pm_sd_hy)


# Function -------------------------------------------------------------------

.mean_hy <- function(x, na.rm = TRUE, ...) {
  m <- structure(colMeans(x@data$spc),
    dim = c(1, length(x@wavelength)),
    dimnames = list("mean", NULL)
  )
  decomposition(x, m)
}

#' @rdname mean_sd
#' @return For hyperSpec object, `mean` returns a hyperSpec object containing
#'        the mean spectrum.
#' @export
#' @examples
#'
#' plot(mean(faux_cell))
setMethod("mean", signature = signature(x = "hyperSpec"), .mean_hy)


# Unit tests -----------------------------------------------------------------

# TODO: add unit tests
