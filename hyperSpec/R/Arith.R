##' Arithmetical Operators: +, -, *, /, ^, %%, %/%, %*% for hyperSpec objects
##'
##' The arithmetical operators `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`, and `%*%`
##' hyperSpec objects.
##'
##' You can use these operators in different ways:
##' \preformatted{
##' e1 + e2
##'
##' `+`(e1, e2)
##'
##' x %*% y `%*%`(x, y)
##'
##' -x }
##'
##' The arithmetical operators `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`, and
##' `%*%` work on the  spectra matrix of the hyperSpec object. They have their
##' usual meaning (see [base::Arithmetic]).  The operators work also with
##' one hyperSpec object and a numeric object or a matrix of the same
##' size as the spectra matrix of the hyperSpec object.
##'
##' With numeric vectors [sweep()] may be more explicit.
##'
##' If you want to calculate on the extra data as well, use the data.frame
##' `hyperSpec@data` directly or [`as.data.frame(x)`][as.data.frame()].
##' @author C. Beleites
##' @title Arithmetical Operators for hyperSpec objects
##' @name Arith
##' @md
##' @rdname Arith
##' @docType methods
##' @param e1,e2 or
##' @param x,y either two hyperSpec objects or
##'
##'   one hyperSpec object and  matrix of same size as `x[[]]` or
##'
##'   a vector which length equalling either the number of rows or the number of
##'   wavelengths of the hyperSpec object, or
##'
##'   a scalar (numeric of length 1).
##' @return hyperSpec object with the new spectra matrix.
##' @export
##' @keywords methods arith
##' @include paste.row.R
##' @include unittest.R
##' @include hyperspec-class.R
##' @concept hyperSpec arithmetic
##' @concept hyperSpec arithmetical operators
##' @concept hyperSpec plus
##' @concept hyperSpec division
##' @concept hyperSpec spectra conversion
##' @seealso [sweep()] for calculations with a vector and the spectra matrix.
##'
##' [methods::S4groupGeneric] for group generic methods.
##'
##' [base::Arithmetic] for the base arithmetic functions.
##'
##' [hyperSpec::Comparison] for comparison operators,
##' [hyperSpec::Math] for mathematical group generic functions (Math
##' and Math2 groups) working on hyperSpec objects.
##' @examples
##' flu + flu
##' 1 / flu
##' all((flu + flu - 2 * flu)[[]] == 0)
##' -flu
##' flu / flu$c


setMethod(
  "Arith", signature(e1 = "hyperSpec", e2 = "hyperSpec"),
  function(e1, e2) {
    validObject(e1)
    validObject(e2)

    e1 <- .expand(e1, dim(e2) [c(1, 3)])
    e2 <- .expand(e2, dim(e1) [c(1, 3)])

    e1 [[]] <- callGeneric(e1[[]], e2[[]])
    e1
  }
)

.arithx <- function(e1, e2) {
  validObject(e1)

  if (missing(e2)) {
    e1  [[]] <- callGeneric(e1 [[]])
    e1
  } else {
    e2 <- as.matrix(e2)

    ## called /only/ with e1 hyperSpec but e2 matrix-like
    e1 <- .expand(e1, dim(e2))
    e2 <- .expand(e2, dim(e1) [c(1, 3)])

    e1  [[]] <- callGeneric(e1 [[]], e2)
    e1
  }
}
##' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "numeric"), .arithx)
##' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "matrix"), .arithx)
##' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "missing"), .arithx)

.arithy <- function(e1, e2) {
  e1 <- as.matrix(e1)
  validObject(e2)

  ## called /only/ with e2 hyperSpec but e1 matrix-like
  e1 <- .expand(e1, dim(e2) [c(1, 3)])
  e2 <- .expand(e2, dim(e1))

  e2  [[]] <- callGeneric(e1, e2 [[]])
  e2
}
##' @rdname Arith
setMethod("Arith", signature(e1 = "numeric", e2 = "hyperSpec"), .arithy)
##' @rdname Arith
setMethod("Arith", signature(e1 = "matrix", e2 = "hyperSpec"), .arithy)

##' @param m matrix
##' @param target.dim target size to expand the vector to for the sweep-shortcuts
##' @noRd
.expand <- function(m, target.dim) {
  m.dim <- dim(m)

  if (m.dim [1] == 1L & target.dim [1] > 1L) {
    m <- m [rep(1, target.dim [1]), , drop = FALSE]
  }

  if (is(m, "hyperSpec")) {
    if (m.dim [3] == 1L & target.dim [2] > 1L) {
      m <- m [, , rep(1, target.dim[2]), wl.index = TRUE]
    }
  } else {
    if (m.dim [2] == 1L & target.dim[2] > 1L) {
      m <- m [, rep(1, target.dim [2]), drop = FALSE]
    }
  }

  m
}


## matrix multiplication two hyperSpec objects
.matmul_hh <- function(x, y) {
  validObject(x)
  validObject(y)

  if (ncol(y) > 1) {
    warning("Dropping column(s) of y: ", paste(colnames(y$..), collapse = ", "))
  }

  x@data$spc <- x@data$spc %*% y@data$spc
  .wl(x) <- y@wavelength
  x@label$.wavelength <- y@label$.wavelength

  x
}

.test(.matmul_hh) <- function(){
  context("matrix multiplication: 2 hyperSpec objects")
  h <- flu[,,1:nrow(flu), wl.index = TRUE]
  h$filename <- NULL

  test_that("correct result", {
    expect_warning(res <- h %*% flu, "Dropping column")

    expect_s4_class(res, "hyperSpec")

    expect_equal(dim(res), c(nrow = nrow(h), ncol = ncol(h), nwl = nwl(flu)))

    expect_equal(res[[]], h[[]] %*% flu[[]])

    expect_equal(res$.., h$..)

        expect_equal(wl(res), wl(flu))
  })
}


##' @rdname Arith
##' @concept hyperSpec matrix multiplication
##' @export
##' @md
##' @seealso  [base::matmult] for matrix multiplications with `%*%`.
setMethod("%*%", signature(x = "hyperSpec", y = "hyperSpec"), .matmul_hh)

## matrix multiplication hyperSpec object %*% matrix
.matmul_hm <- function(x, y) {
  validObject(x)

  x@data$spc <- x@data$spc %*% y

  .wl(x) <- seq_len(ncol(y))
  x@label$.wavelength <- NA

  x
}

.test(.matmul_hm) <- function() {
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

##' @rdname Arith
setMethod("%*%", signature(x = "hyperSpec", y = "matrix"), .matmul_hm)

## matrix multiplication matrix %*% hyperSpec object
.matmul_mh <- function(x, y) {
  validObject(y)

  if (ncol(y) > 1) {
    warning("Dropping column(s) of y: ", paste(colnames(y$..), collapse = ", "))
  }

  new("hyperSpec", wavelength = y@wavelength, spc = x %*% y@data$spc)
}

.test(.matmul_mh) <- function(){
  context("matrix multiplication: matrix x hyperSpec")

  m <- matrix(1:(2 * nrow(flu)), ncol = nrow(flu))

  test_that("correct result", {
    expect_warning(res <- m %*% flu, "Dropping column")

    expect_s4_class(res, "hyperSpec")

    expect_equal(dim(res), c(nrow = nrow(m), ncol = 1, nwl = nwl(flu)))

    expect_equal(res[[]], m %*% flu[[]])

    expect_equal(wl(res), wl(flu))
  })
}


##' @rdname Arith
setMethod("%*%", signature(x = "matrix", y = "hyperSpec"), .matmul_mh)

.test(.arithx) <- function() {
  context("Arith")

  test_that("binary -", {
    expect_equal(
      as.matrix(flu - flu),
      matrix(0, nrow = nrow(flu), ncol = nwl(flu), dimnames = dimnames(flu [[]]))
    )

    expect_equal(as.matrix(flu - flu [1]), as.matrix(sweep(flu, 2, flu [1], `-`)))

    expect_equal(as.matrix(flu - flu [, , 450]), as.matrix(sweep(flu, 1, flu [, , 450], `-`)))
  })

  test_that("binary /", {
    expect_equal(
      as.matrix(flu / flu),
      matrix(1, nrow = nrow(flu), ncol = nwl(flu), dimnames = dimnames(flu [[]]))
    )

    expect_equal(as.matrix(flu / flu [1]), as.matrix(sweep(flu, 2, flu [1], `/`)))

    expect_equal(as.matrix(flu / flu [, , 450]), as.matrix(sweep(flu, 1, flu [, , 450], `/`)))
  })

  test_that("binary + with scalar", {
    expect_equal(as.matrix(flu + 1), as.matrix(flu) + 1)
    expect_equal(as.matrix(1 + flu), as.matrix(flu) + 1)
  })

  test_that("unary -", {
    expect_equal(as.matrix(-flu), -as.matrix(flu))
  })
}
