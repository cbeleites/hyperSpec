# Function -------------------------------------------------------------------

.sweep <- function(x, MARGIN, STATS, FUN = "-",
                   check.margin = TRUE, ...) {
  validObject(x)

  if (is(STATS, "hyperSpec")) {
    validObject(STATS)
    STATS <- STATS@data$spc
  } else if (is(STATS, "function")) {
    STATS <- apply(x, MARGIN, STATS)@data$spc
  }

  x@data$spc <- sweep(
    x = x@data$spc, MARGIN = MARGIN, STATS = STATS,
    FUN = FUN, check.margin = check.margin, ...
  )

  x
}

#' Sweep summary statistic out of `hyperSpec` objects
#'
#' [base::sweep()] for `hyperSpec` objects.
#'
#'
#' Calls [base::sweep()] for the spectra matrix.
#'
#' `sweep()` is useful for some spectra pre-processing, like offset
#' correction, subtraction of background spectra, and normalization of the
#' spectra.
#'
#' @name sweep
#' @rdname sweep
#' @aliases sweep-methods sweep,hyperSpec-method
#' @docType methods
#'
#' @param x a `hyperSpec object.`
#' @param MARGIN direction of the spectra matrix that `STATS` goees
#'        along.
#' @param STATS the summary statistic to sweep out. Either a vector or a
#'        `hyperSpec` object.
#'
#' \pkg{hyperSpec} offers a non-standard convenience function: if `STATS` is a
#'   function, this function is applied first (with the same `MARGIN`) to
#'   compute the statistic. However, no further arguments to the apply
#'   function can be given.  See the examples.
#' @param FUN the function to do the sweeping, e.g. `-` or `/`.
#' @param check.margin If `TRUE` (the default), warn if the length or
#'   dimensions of `STATS` do not match the specified dimensions of
#'   `x`.  Set to `FALSE` for a small speed gain when you
#'   *know* that dimensions match.
#' @param ... further arguments for `FUN`
#'
#' @return A `hyperSpec` object.
#'
#' @author C. Beleites
#' @seealso [base::sweep()]
#'
#' @export
#'
#' @keywords methods
#' @concept stats
#' @concept preprocessing
#' @concept manipulation
#'
#' @examples
#'
#' ## Subtract the background / slide / blank spectrum
#' # the example data does not have spectra of the empty slide,
#' # so instead the overall composition of the sample is subtracted
#' background <- apply(faux_cell, 2, quantile, probs = 0.05)
#' corrected <- sweep(faux_cell, 2, background, "-")
#' plot(corrected, "spcprctl5")
#'
#' ## Offset correction
#' offsets <- apply(faux_cell, 1, min)
#' corrected <- sweep(faux_cell, 1, offsets, "-")
#' plot(corrected, "spcprctl5")
#'
#' ## Min-max normalization (on max amide I)
#' # the minimum is set to zero by the offset correction.
#' factor <- apply(corrected, 1, max)
#' mm.corrected <- sweep(corrected, 1, factor, "/")
#' plot(mm.corrected, "spcprctl5")
#'
#' ## convenience: give function to compute STATS:
#' mm.corrected2 <- sweep(corrected, 1, max, "/")
#' plot(mm.corrected2)
#'
#' ## checking
#' stopifnot(all(mm.corrected2 == mm.corrected))
setMethod("sweep", signature = signature(x = "hyperSpec"), .sweep)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.sweep) <- function() {
  context("sweep")

  # Perform tests
  test_that("sweep() returnts output silently", {
    expect_silent(sweep(flu, 1, max, "/"))
    expect_silent(sweep(flu, 1, max(flu), "/"))
  })

  # FIXME (tests): add tests to check the correctness of the output!!!
}
