#' Convert principal component decomposition or the like into a `hyperSpec` object
#'
#' Decomposition of the spectra matrix is a common procedure in chemometric
#' data analysis. `scores` and `loadings` convert the result matrices
#' into new `hyperSpec` objects.
#'
#' Multivariate data are frequently decomposed by methods like principal
#' component analysis, partial least squares, linear discriminant analysis, and
#' the like.  These methods yield latent spectra (or latent variables,
#' loadings, components, \dots{}) that are linear combination coefficients
#' along the wavelength axis and scores for each spectrum and loading.
#'
#' The loadings matrix gives a coordinate transformation, and the scores are
#' values in that new coordinate system.
#'
#' The obtained latent variables are spectra-like objects: a latent variable
#' has a coefficient for each wavelength. If such a matrix (with the same
#' number of columns as `object` has wavelengths) is given to
#' `decomposition` (also setting `scores = FALSE`), the spectra
#' matrix is replaced by `x`. Moreover, all columns of `object@@data`
#' that did not contain the same value for all spectra are set to `NA`.
#' Thus, for the resulting `hyperSpec` object, [plotspc()] and
#' related functions are meaningful. [hyperSpec::plotmap()] cannot be
#' applied as the loadings are not laterally resolved.
#'
#' The scores matrix needs to have the same number of rows as `object` has
#' spectra. If such a matrix is given, `decomposition` will replace the
#' spectra matrix is replaced by `x` and `object@@wavelength` by
#' `wavelength`. The information related to each of the spectra is
#' retained. For such a `hyperSpec` object, [plotmap()] and
#' [plotc()] and the like can be applied. It is also possible to use
#' the spectra plotting, but the interpretation is not that of the spectrum any
#' longer.
#'
#' @param object A `hyperSpec` object.
#' @param x matrix with the new content for `object@@data$spc`.
#'
#'   Its size must correspond to rows (for `scores`) and to either columns
#'   or rows (for `loadings`) of `object`.
#' @param wavelength for a scores-like `x`: the new `object@@wavelength`.
#' @param label.wavelength The new label for the wavelength axis (if `x`
#'   is scores-like). If not given, the label of `object` is kept.
#' @param label.spc The new label for the spectra matrix. If not given, the
#'   label of `object` is kept.
#' @param scores is `x` a scores-like matrix?
#' @param retain.columns for loading-like decompostition (i.e. `x` holds
#'   loadings, pure component spectra or the like), the data columns need
#'   special attention.
#'
#'   Columns with different values across the rows will be set to `NA` if
#'   `retain.columns` is `TRUE`, otherwise they will be deleted.
#' @param ... ignored.
#' @return A `hyperSpec` object, updated according to `x`
#' @author C. Beleites
#' @seealso See [%*%] for matrix multiplication of `hyperSpec` objects.
#'
#'   See e.g. [stats::prcomp()] and [stats::princomp()] for
#'   principal component analysis, and package `pls` for Partial Least
#'   Squares Regression.
#'
#' @export
#'
#' @keywords methods manip
#' @concept manipulation
#'
#' @include apply.R
#' @examples
#' pca <- prcomp(flu)
#'
#' pca.loadings <- decomposition(flu, t(pca$rotation), scores = FALSE)
#' pca.center <- decomposition(flu, pca$center, scores = FALSE)
#' pca.scores <- decomposition(flu, pca$x)
#'
#' plot(pca.center)
#' plot(pca.loadings, col = c("red", "gray50"))
#' plotc(pca.scores, groups = .wavelength)
decomposition <- function(object, x, wavelength = seq_len(ncol(x)),
                          label.wavelength, label.spc,
                          scores = TRUE, retain.columns = FALSE,
                          ...) {
  #  message ("decomposition will be deprecated: please change your code to use `loadings` or `scores` instead.")

  validObject(object)

  if (is.vector(x)) {
    if (nrow(object) == length(x)) {
      dim(x) <- c(length(x), 1)
    } else {
      dim(x) <- c(1, length(x))
    }
  }

  if ((nrow(x) == nrow(object)) && scores) {
    ## scores-like object?

    object@data$spc <- x
    .wl(object) <- wavelength

    if (!missing(label.wavelength)) {
      object@label$.wavelength <- label.wavelength
    }
  } else if (ncol(x) == nwl(object)) {

    ## loadings-like object
    spc <- match("spc", colnames(object@data))

    ## apply changes type of retained columns to character!!!
    ## must be done in a loop one column after the other otherwise a matrix or a list in a column
    ## (e.g. for the independent variate of PLS, POSIXlt) will cause an error

    cols <- rep(TRUE, ncol(object@data)) # columns to keep

    for (i in seq_len(ncol(object@data))[-spc]) {
      tmp <- as.data.frame(lapply(object@data[, i, drop = FALSE], .na.if.different))
      object@data[1, i] <- tmp
      if (all(is.na(tmp))) {
        cols[i] <- FALSE
      }
    }

    if (!retain.columns) {
      object@label[colnames(object@data)[!cols]] <- NULL
      object@data <- object@data[, cols, drop = FALSE]
    }

    object@data <- object@data[rep(1, nrow(x)), , drop = FALSE]
    colnames(x) <- colnames(object@data$spc)
    object@data$spc <- x
  } else {
    stop(
      "Either rows (if scores == TRUE) or columns (if scores == FALSE) of",
      " x and object must correspond"
    )
  }

  rownames(object@data) <- rownames(x)

  if (!missing(label.spc)) object@label$spc <- label.spc

  object@data$spc <- unclass(object@data$spc) # remove AsIs

  object <- .spc_fix_colnames(object)

  validObject(object)

  object
}


hySpc.testthat::test(decomposition) <- function() {
  context("decomposition")

  test_that("scores-like", {
    flu$matrix <- cbind(flu$c, flu$c)
    expect_true(is.matrix(flu$matrix))

    tmp <- flu[, , 405 ~ 410]
    tmp@wavelength <- seq_len(nwl(tmp))
    colnames(tmp@data$spc) <- seq_len(nwl(tmp))

    scores <- decomposition(flu, flu[[, , 405 ~ 410]])
    expect_equal(scores, tmp)
  })

  test_that("spc labels", {
    scores <- decomposition(flu, flu[[, , 405 ~ 410]], label.spc = "bla")
    expect_equal(labels(scores, "spc"), "bla")
  })

  test_that("wl labels", {
    scores <- decomposition(flu, flu[[, , 405 ~ 410]], label.wavelength = "bla")
    expect_equal(labels(scores, ".wavelength"), "bla")
  })

  test_that("check loadings-like", {
    tmp <- flu[1, c("spc"), ]
    loadings <- decomposition(flu, flu[[1, , ]])
    expect_equal(loadings, tmp)
  })

  test_that("POSIXct", {
    flu$ct <- as.POSIXct(Sys.time())
    expect_equal(decomposition(flu, flu[[]], scores = FALSE)$ct, flu$ct)
  })

  test_that("POSIXlt", {
    flu$lt <- as.POSIXlt(Sys.time())
    expect_equal(decomposition(flu, flu[[]], scores = FALSE)$lt, flu$lt)
  })

  test_that("spc labels", {
    tmp <- decomposition(flu, flu[[]], scores = FALSE, label.spc = "bla")
    expect_equal(labels(tmp, "spc"), "bla")
  })

  test_that("wl labels: should *not* be changed for loadings-like decomposition", {
    tmp <- decomposition(flu, flu[[]], scores = FALSE, label.wavelength = "bla")
    expect_equal(labels(tmp, ".wavelength"), labels(flu, ".wavelength"))
  })

  test_that("decomposition fails on empty hyperSpec object", {
    # FIXME: these unit test below must be reviewed

    spc_empty <- new("hyperSpec",
      wavelength = 1:5,
      spc = matrix(NA, ncol = 5, nrow = 0)
    )

    loading_matrix <- matrix(5:1, ncol = 5, nrow = 1)

    expect_error(decomposition(spc_empty, scores = TRUE))
    expect_error(decomposition(spc_empty, scores = FALSE))
  })
}
