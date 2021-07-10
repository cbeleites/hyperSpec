
#' Sorting the wavelengths of a `hyperSpec` object
#'
#' @description
#' Rearranges the `hyperSpec` object so that the wavelength vector is in
#' increasing (or decreasing) order.
#'
#' The wavelength vector is sorted and the columns of the spectra matrix are
#' rearranged accordingly.
#'
#' @param x The `hyperSpec` object.
#' @param na.last,decreasing Handed to [base::order()].
#' @return A `hyperSpec` object.
#' @author C. Beleites
#' @export
#'
#' @concept wavelengths
#'
#' @seealso [base::order()]
#' @examples
#'
#' ## Example 1: different drawing order in plotspc
#' spc <- new("hyperSpec", spc = matrix(rnorm(5) + 1:5, ncol = 5))
#' spc <- cbind(spc, spc + .5)
#'
#' plotspc(spc)
#' text(wl(spc), spc[[]], as.character(1:10), col = "darkred")
#'
#' spc_sorted <- wl_sort(spc)
#' plotspc(spc_sorted)
#' text(wl(spc_sorted), spc_sorted[[]], as.character(1:10), col = "darkred")
#'
#' ## Example 2
#' spc <- new("hyperSpec", spc = matrix(rnorm(5) * 2 + 1:5, ncol = 5))
#' spc <- cbind(spc, spc)
#'
#' plot(seq_len(nwl(spc)), spc[[]], type = "b")
#' spc[[]]
#'
#' spc_sorted <- wl_sort(spc)
#' lines(seq_len(nwl(spc_sorted)), spc_sorted[[]], type = "l", col = "red")
#' spc_sorted[[]]
wl_sort <- function(x, na.last = TRUE, decreasing = FALSE) {
  chk.hy(x)
  validObject(x)

  ord <- order(x@wavelength, na.last = na.last, decreasing = decreasing)

  if (any(ord != seq_along(x@wavelength))) {
    x@data$spc <- x@data$spc[, ord, drop = FALSE]
    .wl(x) <- x@wavelength[ord]
  }

  x
}
