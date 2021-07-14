# Function -------------------------------------------------------------------

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
#'
#' @param probs the quantiles, see [stats::quantile()]
#' @param names `"pretty"` results in percentages (like [stats::quantile()]'s
#' `names = TRUE`), `"num"` results in the row names being `as.character(probs)`
#' (good for ggplot2 getting the order of the quantiles right). Otherwise, no
#' names are assigned.
#'
#' @return For `hyperSpec` object, `quantile()` returns a `hyperSpec` object
#' containing the respective quantile spectra.
#'
#' @concept stats
#' @concept manipulation
#'
#' @seealso  [stats::quantile()]
#' @export
#'
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
setMethod("quantile", signature = signature(x = "hyperSpec"), .quantile)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.quantile) <- function() {
  context("quantile")

  test_that("quantile() works", {
    sp <- generate_hy_spectra()

    # Check ronames
    expect_silent(hy_q <- quantile(sp))
    expect_is(hy_q, "hyperSpec")
    expect_equal(rownames(hy_q), c("0", "0.5", "1"))

    # Check ronames (%)
    expect_silent(hy_q_pretty <- quantile(sp, names = "pretty"))
    expect_equal(rownames(hy_q_pretty), c("  0 %", " 50 %", "100 %"))

    # Check values
    probs <- c(0, .25, .50, .75, 1)
    expect_equal(
      quantile(sp, probs = probs)$spc, # on hyperSpec
      apply(sp$spc, 2, quantile, probs = probs), # on matrix
      check.attributes = FALSE
    )
  })
}
