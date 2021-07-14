# naming scheme for the internal functions:
# .<generic>_<x><y> with <x> and <y> the first and second parameter/operand:
# h ... hyperSpec object
# n ... numeric (scalar, vector, or matrix)
# m ... matrix
# _ ... missing


# Function -------------------------------------------------------------------

#' @include expand.R
## arithmetic function called with both parameters hyperSpec
.arith_hh <- function(e1, e2) {
  validObject(e1)
  validObject(e2)

  if (length(e2[[]]) > length(e1[[]])) {
    e1 <- .expand(e1, dim(e2)[c(1, 3)])
  }
  if (length(e1[[]]) > length(e2[[]])) {
    e2 <- .expand(e2, dim(e1)[c(1, 3)])
  }

  e1[[]] <- callGeneric(e1[[]], e2[[]])

  e1 <- merge_data(e1, e2)

  e1
}

#' @title Arithmetical operators: `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`, `%*%`
#'        for `hyperSpec` objects
#'
#' @description
#' The arithmetical operators `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`, and `%*%`
#' `hyperSpec` objects.
#'
#' @details
#' You can use these operators in different ways:
#' \preformatted{
#' e1 + e2
#'
#' `+`(e1, e2)
#'
#' x %*% y `%*%`(x, y)
#'
#' -x }
#'
#' The arithmetical operators `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`, and
#' `%*%` work on the  spectra matrix of the `hyperSpec` object. They have their
#' usual meaning (see [base::Arithmetic]).  The operators work also with
#' one `hyperSpec` object and a numeric object or a matrix of the same
#' size as the spectra matrix of the `hyperSpec` object.
#'
#' With numeric vectors [sweep()] may be more explicit.
#'
#' If you want to calculate on the extra data as well, use the data.frame
#' `hyperSpec@data` directly or [`as.data.frame(x)`][as.data.frame()].
#'
#' @author C. Beleites
#'
#' @rdname Arith
#' @docType methods
#'
#' @param e1,e2 or
#' @param x,y either two `hyperSpec` objects or
#'
#'   one `hyperSpec` object and  matrix of same size as `x[[]]` or
#'
#'   a vector which length equaling either the number of rows or the number of
#'   wavelengths of the `hyperSpec` object, or
#'
#'   a scalar (numeric of length 1).
#'
#' @return `hyperSpec` object with the new spectra matrix.
#'
#' If the `e2` is a `hyperSpec` object, its extra data columns will be dropped
#' silently.
#'
#'
#' @keywords methods arith
#' @concept manipulation
#' @concept hyperSpec arithmetic
#' @concept hyperSpec arithmetical operators
#' @concept hyperSpec plus
#' @concept hyperSpec division
#' @concept hyperSpec spectra conversion
#'
#'
#' @include paste_row.R
#' @include hyperspec-class.R
#'
#' @export
#'
#' @seealso [sweep()] for calculations with a vector and the spectra matrix.
#'
#' [methods::S4groupGeneric] for group generic methods.
#'
#' [base::Arithmetic] for the base arithmetic functions.
#'
#' [hyperSpec::Comparison] for comparison operators,
#' [hyperSpec::Math] for mathematical group generic functions (Math
#' and Math2 groups) working on hyperSpec objects.
#'
#' @examples
#' flu + flu
#' 1 / flu
#' all((flu + flu - 2 * flu)[[]] == 0)
#' -flu
#' flu / flu$c
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "hyperSpec"), .arith_hh)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.arith_hh) <- function() {
  context("Arithmetic operators, 2 hyperSpec objects")

  tmp <- as.hyperSpec(matrix(1:length(flu[[]]), nrow = nrow(flu)))
  tmp[[]] <- 1:length(tmp[[]])
  tmp$testcolumn <- 1:nrow(tmp)

  test_that("correct results for 2 equal-sized objects", {
    for (operator in c(`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`)) {
      res <- operator(flu, tmp)
      expect_equal(res[[]], operator(flu[[]], tmp[[]]))
      expect_equal(res$.., cbind(flu$.., tmp$..))
      expect_equal(dim(res), dim(flu) + c(0, 1, 0))
      expect_equal(wl(res), wl(flu))
    }
  })

  test_that("correct results with row-sized object", {
    # TODO: add unit tests
  })

  test_that("correct results with column-sized object", {
    # TODO: add unit tests
  })

  test_that("correct results with 1x1 object", {
    # TODO: add unit tests
  })
}


# Function -------------------------------------------------------------------
## unary operators

.arith_h_ <- function(e1, e2) {
  validObject(e1)

  if (!missing(e2)) {
    stop("e2 must be missing")
  }

  e1[[]] <- callGeneric(e1[[]])
  e1
}

#' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "missing"), .arith_h_)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.arith_h_) <- function() {
  context("Unary arithmetic operators")

  test_that("correct results", {
    for (operator in c(`+`, `-`)) {
      res <- operator(flu)
      expect_equal(res[[]], operator(flu[[]]))
      expect_equal(res$.., flu$..)
      expect_equal(dim(res), dim(flu))
      expect_equal(wl(res), wl(flu))
    }

    for (operator in c(`*`, `/`, `^`, `%%`, `%/%`)) {
      expect_error(operator(flu))
    }
  })

  test_that("unary -", {
    expect_equal(as.matrix(-flu), -as.matrix(flu))
  })

  test_that("unary +", {
    expect_equal(as.matrix(+flu), as.matrix(flu))
  })
}


# Function -------------------------------------------------------------------

## arithmetic function called with first parameter hyperSpec
.arith_hn <- function(e1, e2) {
  validObject(e1)

  if (length(e2) > length(e1[[]])) {
    e1 <- .expand(e1, dim(e2))
  }
  if (length(e1[[]]) > length(e2)) {
    e2 <- .expand(e2, dim(e1)[c(1, 3)])
  }

  e1[[]] <- callGeneric(e1[[]], e2)

  e1
}

#' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "numeric"), .arith_hn)
#' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "matrix"), .arith_hn)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.arith_hn) <- function() {
  context("Arithmetic operators, hyperSpec object x")

  test_that("correct results with scalar", {
    for (operator in c(`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`)) {
      res <- operator(flu, 2)
      expect_equal(res[[]], operator(flu[[]], 2))
      expect_equal(res$.., flu$..)
      expect_equal(dim(res), dim(flu))
      expect_equal(wl(res), wl(flu))
    }
  })

  test_that("correct results with vector for columns", {
    v <- 1:nrow(flu)

    for (operator in c(`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`)) {
      res <- operator(flu, v)

      expect_equal(res, sweep(flu, 1, v, FUN = operator))
    }
  })

  test_that("correct results with vector for rows", {
    v <- 1:nwl(flu)

    for (operator in c(`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`)) {
      res <- operator(flu, v)

      expect_equal(res, sweep(flu, 2, v, FUN = operator))
    }
  })


  test_that("correct results with matrix", {
    m <- matrix(1:length(flu[[]]), nrow = nrow(flu))

    for (operator in c(`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`)) {
      res <- operator(flu, m)
      expect_equal(res[[]], operator(flu[[]], m))
      expect_equal(res$.., flu$..)
      expect_equal(dim(res), dim(flu))
      expect_equal(wl(res), wl(flu))
    }
  })

  test_that("binary -", {
    res <- flu - flu
    expect_equal(
      as.matrix(res),
      matrix(0, nrow = nrow(flu), ncol = nwl(flu), dimnames = dimnames(flu[[]]))
    )

    res <- flu - flu[1]
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 2, flu[1], `-`)))

    res <- flu - flu[, , 450]
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 1, flu[, , 450], `-`)))
  })

  test_that("binary /", {
    res <- flu / flu
    expect_equal(
      as.matrix(res),
      matrix(1, nrow = nrow(flu), ncol = nwl(flu), dimnames = dimnames(flu[[]]))
    )

    res <- flu / flu[1]
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 2, flu[1], `/`)))

    res <- flu / flu[, , 450]
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 1, flu[, , 450], `/`)))
  })

  test_that("binary + with scalar", {
    expect_equal(as.matrix(flu + 1), as.matrix(flu) + 1)
    expect_equal(as.matrix(1 + flu), as.matrix(flu) + 1)
  })
}


# Function -------------------------------------------------------------------

## arithmetic function called with second parameter hyperSpec

.arith_nh <- function(e1, e2) {
  # e1 <- as.matrix(e1)
  validObject(e2)

  ## called /only/ with e2 hyperSpec but e1 numeric
  if (length(e2[[]]) > length(e1)) {
    e1 <- .expand(e1, dim(e2)[c(1, 3)])
  }
  if (length(e1) > length(e2[[]])) {
    e2 <- .expand(e2, dim(e1))
  }

  e2[[]] <- callGeneric(e1, e2[[]])
  e2
}

#' @rdname Arith
setMethod("Arith", signature(e1 = "numeric", e2 = "hyperSpec"), .arith_nh)

#' @rdname Arith
setMethod("Arith", signature(e1 = "matrix", e2 = "hyperSpec"), .arith_nh)


# Function -------------------------------------------------------------------

## matrix multiplication two hyperSpec objects
.matmul_hh <- function(x, y) {
  validObject(x)
  validObject(y)

  x@data$spc <- x@data$spc %*% y@data$spc
  .wl(x) <- y@wavelength
  x@label$.wavelength <- y@label$.wavelength

  x
}

#' @rdname Arith
#' @export
#'
#' @concept hyperSpec matrix multiplication
#' @concept manipulation
#'
#' @seealso  [base::matmult] for matrix multiplications with `%*%`.
setMethod("%*%", signature(x = "hyperSpec", y = "hyperSpec"), .matmul_hh)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.matmul_hh) <- function() {
  context("matrix multiplication: 2 hyperSpec objects")
  h <- flu[, , 1:nrow(flu), wl.index = TRUE]
  h$filename <- NULL

  test_that("correct result", {
    res <- h %*% flu

    expect_s4_class(res, "hyperSpec")

    expect_equal(dim(res), c(nrow = nrow(h), ncol = ncol(h), nwl = nwl(flu)))

    expect_equal(res[[]], h[[]] %*% flu[[]])

    expect_equal(res$.., h$..)

    expect_equal(wl(res), wl(flu))
  })
}


# Function -------------------------------------------------------------------

## matrix multiplication hyperSpec object %*% matrix
.matmul_hm <- function(x, y) {
  validObject(x)

  x@data$spc <- x@data$spc %*% y

  .wl(x) <- seq_len(ncol(y))
  x@label$.wavelength <- NA

  x
}

#' @rdname Arith
setMethod("%*%", signature(x = "hyperSpec", y = "matrix"), .matmul_hm)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.matmul_hm) <- function() {
  context("matrix multiplication hyperSpec x matrix")

  m <- matrix(1:(2 * nwl(flu)), nrow = nwl(flu))

  test_that("correct result", {
    res <- flu %*% m

    expect_s4_class(res, "hyperSpec")

    expect_equal(dim(res), c(nrow = nrow(flu), ncol = ncol(flu), nwl = ncol(m)))

    expect_equal(res[[]], flu[[]] %*% m)

    expect_equal(res$.., flu$.., )
  })
}


# Function -------------------------------------------------------------------

## matrix multiplication matrix %*% hyperSpec object
.matmul_mh <- function(x, y) {
  validObject(y)

  new("hyperSpec", wavelength = y@wavelength, spc = x %*% y@data$spc)
}

#' @rdname Arith
setMethod("%*%", signature(x = "matrix", y = "hyperSpec"), .matmul_mh)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.matmul_mh) <- function() {
  context("matrix multiplication: matrix x hyperSpec")

  m <- matrix(1:(2 * nrow(flu)), ncol = nrow(flu))

  test_that("correct result", {
    res <- m %*% flu
    expect_s4_class(res, "hyperSpec")
    expect_equal(dim(res), c(nrow = nrow(m), ncol = 1, nwl = nwl(flu)))
    expect_equal(res[[]], m %*% flu[[]])
    expect_equal(wl(res), wl(flu))
  })
}
