# Set generic ----------------------------------------------------------------

#' Functions `colSums()`, `colMeans()`, `rowSums()`, and `rowMeans()` for
#' `hyperSpec` objects
#'
#' `hyperSpec` objects can use the base functions [base::colMeans()],
#' [base::colSums()], [base::rowMeans()] and [base::rowSums()].
#'
#' @name colSums
#' @rdname colSums
#'
#'
#' @param x `hyperSpec` object
#' @param label.spc labels for the intensity axis for loadings-like (col)
#'        statistics
#' @param label.wavelength labels for the wavelength axis for scores-like (row)
#'        statistics
#' @param na.rm,... further parameters to the base functions
#'
#' `na.rm` defaults to `TRUE` for `hyperSpec` objects.
#'
#' @seealso [colSums][base::colSums]
#'
#' @concept stats
#'
NULL

#' @noRd
setGeneric("colMeans") # , package = 'matrixStats')


# Function -------------------------------------------------------------------

.colMeans <- function(x, na.rm = TRUE, ..., label.spc) {
  result <- colMeans(x@data$spc, na.rm = na.rm, ...)
  if (is.matrix(result) && ncol(result) != nwl(x) && nrow(result) == nwl(x)) {
    result <- t(result)
  }

  decomposition(x, result, scores = FALSE, label.spc = label.spc)
}

#' @rdname colSums
#' @export
#'
#' @concept stats
#'
#' @examples
#' colMeans(flu)
setMethod("colMeans", signature = signature(x = "hyperSpec"), .colMeans)


# Set generic ----------------------------------------------------------------

#' @noRd
setGeneric("colSums") # , package = 'matrixStats')


# Function -------------------------------------------------------------------

.colSums <- function(x, na.rm = TRUE, ..., label.spc) {
  result <- colSums(x@data$spc, na.rm = na.rm, ...)
  if (is.matrix(result) && ncol(result) != nwl(x) && nrow(result) == nwl(x)) {
    result <- t(result)
  }

  decomposition(x, result, scores = FALSE, label.spc = label.spc)
}

#' @rdname colSums
#' @export
#'
#' @concept stats
#'
#' @examples
#' colSums(flu)
setMethod("colSums", signature = signature(x = "hyperSpec"), .colSums)


# Set generic ----------------------------------------------------------------

#' @noRd
setGeneric("rowMeans") # , package = 'matrixStats')

# Function -------------------------------------------------------------------

.rowMeans <- function(x, na.rm = TRUE, ..., label.wavelength) {
  result <- rowMeans(x@data$spc, na.rm = na.rm, ...)
  if (is.matrix(result) && nrow(result) != nrow(x) && ncol(result) == nrow(x)) {
    result <- t(result)
  }

  decomposition(x, result, scores = TRUE, label.wavelength = label.wavelength)
}

#' @rdname colSums
#' @export
#'
#' @concept stats
#'
#' @examples
#' colSums(flu)
setMethod("rowMeans", signature = signature(x = "hyperSpec"), .rowMeans)


# Set generic ----------------------------------------------------------------

#' @noRd
setGeneric("rowSums") # , package = 'matrixStats')


# Function -------------------------------------------------------------------

.rowSums <- function(x, na.rm = TRUE, ..., label.wavelength) {
  result <- rowSums(x@data$spc, na.rm = na.rm, ...)
  if (is.matrix(result) && nrow(result) != nrow(x) && ncol(result) == nrow(x)) {
    result <- t(result)
  }

  decomposition(x, result, scores = TRUE, label.wavelength = label.wavelength)
}

#' @rdname colSums
#' @export
#'
#' @concept stats
#'
#' @examples
#' rowSums(flu)
setMethod("rowSums", signature = signature(x = "hyperSpec"), .rowSums)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.colMeans) <- function() {
  for (fun in c("colMeans", "colSums", "rowMeans", "rowSums")) {
    context(fun)
    f <- get(fun, mode = "function")
    test_that("basic operation", {
      expect_equal(
        as.numeric(f(flu)[[]]),
        as.numeric(f(flu[[]], na.rm = TRUE)),
        label = fun
      )
    })

    test_that("behaviour with NAs", {
      expect_equal(
        as.numeric(f(fluNA)[[]]),
        as.numeric(f(fluNA[[]], na.rm = TRUE)),
        label = fun
      )

      expect_equal(
        as.numeric(f(fluNA, na.rm = FALSE)[[]]),
        as.numeric(f(fluNA[[]], na.rm = FALSE)),
        label = fun
      )
    })
  }
}
