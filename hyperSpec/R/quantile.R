.quantile <- function(x, probs = seq(0, 1, 0.5), na.rm = TRUE, names = "num", ...) {
  x <- apply(x, 2, quantile,
    probs = probs, na.rm = na.rm, names = FALSE, ...,
    long = list(probs = probs, na.rm = na.rm, names = names, ...)
  )

  if (names == "pretty") {
    rownames(x@data) <- paste(
      format(100 * probs,
        format = "fg", width = 1,
        justify = "right",
        digits = getOption("digits")
      ),
      "%"
    )
  } else if (names == "num") {
    rownames(x@data) <- probs
  }

  x
}

#' @rdname mean_sd
#' @return For `hyperSpec` object, `quantile()` returns a `hyperSpec` object
#' containing the respective quantile spectra.
#' @param probs the quantiles, see [stats::quantile()]
#' @param names `"pretty"` results in percentages (like [stats::quantile()]'s
#' `names = TRUE`), `"num"` results in the row names being `as.character(probs)`
#' (good for ggplot2 getting the order of the quantiles right). Otherwise, no
#' names are assigned.
#' @seealso  [stats::quantile()]
#' @export
#' @examples
#'
#' plot(quantile(faux_cell))
#'
#' flu_quantiles <- quantile(flu)
#' rownames(flu_quantiles)
#' flu_quantiles$..
#'
#' flu_pretty_quantiles <- quantile(flu, names = "pretty")
#' rownames(flu_pretty_quantiles)
#' flu_pretty_quantiles$..
#'
setMethod("quantile", signature = signature(x = "hyperSpec"), .quantile)

