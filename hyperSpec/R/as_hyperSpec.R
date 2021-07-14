# Set generic ----------------------------------------------------------------

#' `as.hyperSpec`: convenience conversion functions
#'
#' These functions are shortcuts to convert other objects into `hypeSpec`
#' objects.
#'
#' @param X the object to convert. If `X` is:
#'
#' - a `matrix`, it is assumed to contain the spectra matrix,
#' - a `data.frame`, it is assumed to contain extra data.
#'
#' @param ... additional parameters that should be handed over to
#'       `new("hyperSpec")` (initialize).
#'
#' @return hyperSpec object
#' @seealso [hyperSpec::initialize()]
#' @export
#'
#' @concept hyperSpec conversion
#'
setGeneric("as.hyperSpec", function(X, ...) {
  stop("as.hyperSpec is not available for objects of class ", class(X))
})


# Function -------------------------------------------------------------------

#' @include extract_numbers.R
.as.hyperSpec.matrix <- function(X, wl = NULL, ...) {
  if (is.null(wl)) wl <- extract_numbers(colnames(X))
  new("hyperSpec", spc = X, wavelength = wl, ...)
}

#' @rdname as.hyperSpec
#' @param wl wavelength vector. Defaults to guessing from the column names in `X`.
#' @param spc spectra matrix.
#' @param labels list with labels.
#' @export
#'
#' @concept hyperSpec conversion
#'
#' @examples
#' tmp <- data.frame(flu[[, , 400 ~ 410]])
#' (wl <- colnames(tmp))
#' extract_numbers(wl)
setMethod("as.hyperSpec", "matrix", .as.hyperSpec.matrix)


# Function -------------------------------------------------------------------

.as.hyperSpec.data.frame <- function(X, spc = NULL, wl = NULL,
                                     labels = attr(X, "labels"), ...) {
  if (is.null(wl)) wl <- extract_numbers(X)
  # TODO: remove after 31.12.2020
  if (!all(!is.na(extract_numbers(colnames(X))))) {
    warning(
      "Method as.hyperSpec(<data.frame>) has changed its behaviour. ",
      "Use as.hyperSpec(as.matrix(X)) instead."
    )
  }

  if (is.null(spc)) {
    spc <- matrix(ncol = 0, nrow = nrow(X))
    wl <- numeric(0)
  }

  new("hyperSpec", data = X, wavelength = wl, spc = spc, labels = labels, ...)
}

#' @rdname as.hyperSpec
#' @note Note that the behaviour of `as.hyperSpec(X)` was changed when `X` is a
#' `data.frame`: it now assumes `X` to be extra data, and returns a `hyperSpec`
#' object with 0 wavelengths. To get the old behaviour, use
#' `as.hyperSpec(as.matrix(X))`.

setMethod("as.hyperSpec", "data.frame", .as.hyperSpec.data.frame)


# Function -------------------------------------------------------------------

.as.hyperSpec.hyperSpec <- function(X) {
  X
}

#' @rdname as.hyperSpec
setMethod("as.hyperSpec", "hyperSpec", .as.hyperSpec.hyperSpec)


# Unit tests -----------------------------------------------------------------


hySpc.testthat::test(as.hyperSpec) <- function() {
  context("as.hyperSpec")

  spc <- matrix(1:12, ncol = 3)
  wl <- seq(600, 601, length.out = ncol(spc))

  test_that("only spc is given", {
    expect_identical(new("hyperSpec", spc = spc), as.hyperSpec(X = spc))
  })

  test_that("data.frame", {
    tmp <- as.hyperSpec(flu$..)
    expect_equal(tmp$.., flu$..)
    expect_equal(dim(tmp), c(nrow = 6L, ncol = 3L, nwl = 0L))
    expect_equal(wl(tmp), numeric(0))
  })

  test_that("data.frame with labels attribute", {
    tmp <- flu$..
    attr(tmp, "labels") <- labels(flu)

    tmp <- as.hyperSpec(tmp)

    expect_equal(tmp$.., flu$..)
    expect_equal(dim(tmp), c(nrow = 6L, ncol = 3L, nwl = 0L))
    expect_equal(wl(tmp), numeric(0))
    expect_equal(
      labels(tmp)[order(names(labels(tmp)))],
      lapply(labels(flu)[order(names(labels(flu)))], as.expression)
    )
  })

  test_that("spc with characters in colnames", {
    colnames(spc) <- make.names(wl)
    h <- as.hyperSpec(X = spc)
    expect_equivalent(h@data$spc, spc)
    expect_equal(dim(h@data$spc), dim(spc))
    expect_equal(dim(h), c(nrow = nrow(spc), ncol = 1L, nwl = ncol(spc)))
    expect_equal(h@wavelength, wl)
    expect_equal(as.numeric(colnames(h@data$spc)), wl)
  })

  test_that("ignore colnames if wl is set", {
    colnames(spc) <- c(601, 602, 603)
    expect_identical(
      new("hyperSpec", spc = spc, wavelength = wl),
      as.hyperSpec(X = spc, wl = wl)
    )
  })

  test_that("set additional parameters", {
    dt <- data.frame(x = 1:4, y = letters[1:4])
    lbs <- list(spc = "I / a.u.", .wavelength = expression(tilde(nu) / cm^-1))
    expect_identical(
      new("hyperSpec", spc = spc, data = dt, label = lbs),
      as.hyperSpec(X = spc, data = dt, label = lbs)
    )
  })

  test_that("error on unknown class", {
    tmp <- NA
    class(tmp) <- "foo"
    expect_error(as.hyperSpec(tmp))
  })

  test_that("colnames of spectra matrix correctly set (as done by wl<-)", {
    tmp <- new("hyperSpec", spc = spc, wavelength = wl)
    expect_equal(colnames(tmp$spc), as.character(signif(wl, 6)))
  })

  test_that("as.hyperSpec(<hyperSpec>) works", {
    expect_equal(as.hyperSpec(flu), flu)
  })
}
