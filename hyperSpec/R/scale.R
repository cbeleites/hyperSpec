#' Center and scale hyperSpec object
#'
#' \code{link[base]{scale}}s the spectra matrix. `scale (x, scale = FALSE)` centers the data.
#'
#' Package `scale` provides a fast alternative for \code{base::\link[base]{scale}}
#'
#' @name scale,hyperSpec-method
#' @rdname scale
#' @aliases scale scale-methods scale,hyperSpec-method
#' @docType methods
#' @param x the `hyperSpec` object
#' @param center if `TRUE`, the data is centered to `colMeans (x)`, `FALSE`
#' suppresses centering. Alternatively, an object that can be converted to numeric of length
#' `nwl (x)` by \code{\link[base]{as.matrix}} (e.g. hyperSpec object containing 1 spectrum) can
#' specify the center spectrum.
#' @param scale if `TRUE`, the data is scaled to have unit variance at each wavelength,
#' `FALSE` suppresses scaling. Alternatively, an object that can be converted to numeric of
#' length `nwl (x)` by \code{\link[base]{as.matrix}} (e.g. hyperSpec object containing 1 spectrum)
#' can specify the center spectrum.
#' @return the centered & scaled `hyperSpec` object
#' @author C. Beleites
#' @seealso \code{\link[base]{scale}}
#'
#' package scale.
#' @keywords methods
#' @export
#' @examples
#'
#' ## mean center & variance scale
#' tmp <- scale(faux_cell)
#' plot(tmp, "spcmeansd")
#' plot(sample(tmp, 5), add = TRUE, col = 2)
#'
#' ## mean center only
#' tmp <- scale(faux_cell, scale = FALSE)
#' plot(tmp, "spcmeansd")
#' plot(sample(tmp, 5), add = TRUE, col = 2)
#'
#' ## custom center
#' tmp <- sweep(faux_cell, 1, mean, `/`)
#' plot(tmp, "spcmeansd")
#' tmp <- scale(tmp, center = quantile(tmp, .05), scale = FALSE)
setMethod("scale",
  signature = signature(x = "hyperSpec"),
  function(x, center = TRUE, scale = TRUE) {
    validObject(x)

    if (!is.logical(center)) center <- as.matrix(center)
    if (!is.logical(scale)) scale <- as.matrix(scale)

    x@data$spc <- scale(x@data$spc, center, scale)

    x
  }
)
