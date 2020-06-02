## arithmetic function called with both parameters hyperSpec
.arith <- function(e1, e2) {
  validObject(e1)
  validObject(e2)

  e1 <- .expand(e1, dim(e2) [c(1, 3)])
  e2 <- .expand(e2, dim(e1) [c(1, 3)])

  if (ncol(e2) > 1) {
    warning("Dropping column(s) of e2: ",
            paste(colnames(e2$..), collapse = ", "))
  }

  e1[[]] <- callGeneric(e1[[]], e2[[]])
  e1
}

.test(.arith) <- function(){
  context("Arithmetic operators, 2 hyperSpec objects")

  tmp <- as.hyperSpec(matrix(1:length(flu[[]]), nrow = nrow(flu)))
  tmp[[]] <- 1:length(tmp[[]])
  tmp$testcolumn <- 1:nrow(tmp)

  test_that("correct results for 2 equal-sized objects", {
    for (operator in c(`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`)) {

      expect_warning(res <- operator(flu, tmp),
                     "Dropping column[(]s[)] of e2: testcolumn")

      expect_equal(res[[]], operator(flu[[]], tmp[[]]))
      expect_equal(res$.., flu$..)
      expect_equal(dim(res), dim(flu))
      expect_equal(wl(res), wl(flu))
    }
  })

  test_that("correct results with row-sized object", {
  })

  test_that("correct results with column-sized object", {
  })

  test_that("correct results with 1x1 object", {
  })

}

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
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "hyperSpec"), .arith)

## arithmetic function called with first parameter hyperSpec
.arithx <- function(e1, e2) {
  validObject(e1)

  if (missing(e2)) {
    e1[[]] <- callGeneric(e1[[]])
    e1
  } else {
    ## called /only/ with e1 hyperSpec but e2 numeric
    e1 <- .expand(e1, dim(e2))
    e2 <- .expand(e2, dim(e1) [c(1, 3)])

    e1[[]] <- callGeneric(e1[[]], e2)
    e1
  }
}

.test(.arithx) <- function(){
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

  test_that("correct results with missing 2nd parameter/unary operators", {
    for (operator in c(`+`, `-`)) {
      res <- operator(flu)
      expect_equal(res[[]], operator(flu[[]]))
      expect_equal(res$.., flu$..)
      expect_equal(dim(res), dim(flu))
      expect_equal(wl(res), wl(flu))
    }
    for (operator in c(`*`, `/`, `^`, `%%`, `%/%`))
      expect_error(operator(flu))
  })


  test_that("binary -", {
    expect_warning(res <- flu - flu)
    expect_equal(
      as.matrix(res),
      matrix(0, nrow = nrow(flu), ncol = nwl(flu), dimnames = dimnames(flu[[]]))
    )

    expect_warning(res <- flu - flu[1])
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 2, flu[1], `-`)))

    expect_warning(res <- flu - flu[, , 450])
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 1, flu[, , 450], `-`)))
  })

  test_that("binary /", {
    expect_warning(res <- flu / flu)
    expect_equal(
      as.matrix(res),
      matrix(1, nrow = nrow(flu), ncol = nwl(flu), dimnames = dimnames(flu[[]]))
    )

    expect_warning(res <- flu / flu[1])
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 2, flu[1], `/`)))

    expect_warning(res <- flu / flu[, , 450])
    expect_equal(as.matrix(res), as.matrix(sweep(flu, 1, flu [, , 450], `/`)))
  })

  test_that("binary + with scalar", {
    expect_equal(as.matrix(flu + 1), as.matrix(flu) + 1)
    expect_equal(as.matrix(1 + flu), as.matrix(flu) + 1)
  })

  test_that("unary -", {
    expect_equal(as.matrix(-flu), -as.matrix(flu))
  })

}


##' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "numeric"), .arithx)
##' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "matrix"), .arithx)
##' @rdname Arith
setMethod("Arith", signature(e1 = "hyperSpec", e2 = "missing"), .arithx)

## arithmetic function called with second parameter hyperSpec
.arithy <- function(e1, e2) {
  #e1 <- as.matrix(e1)
  validObject(e2)

  ## called /only/ with e2 hyperSpec but e1 numeric
  e1 <- .expand(e1, dim(e2) [c(1, 3)])
  e2 <- .expand(e2, dim(e1))

  e2[[]] <- callGeneric(e1,e2 [[]])
  e2
}
##' @rdname Arith
setMethod("Arith", signature(e1 = "numeric", e2 = "hyperSpec"), .arithy)
##' @rdname Arith
setMethod("Arith", signature(e1 = "matrix", e2 = "hyperSpec"), .arithy)

##' expand a row- or column vector (or matrix of size 1 x n or n x 1)
##' along the size-1-dimension.
##'
##' Dimensions that have size > 1 are ignored (they lead to an error in the
##' arithmetic function since the size does not match)
##'
##' @param m matrix, vector or scalar
##' @param target.dim target size to expand the vector to for the sweep-shortcuts
##' @noRd
.expand <- function(m, target.dim) {

  ## vector corresponding to a single row
  if (is.vector(m) & length(m) > 1 & length(m) == target.dim[2]) {
    if (length(m) == target.dim[1]) {
      message("Square target: recycling by column.")
    } else {
      m <- t(m)
    }
  }
  m <- as.matrix(m)
  m.dim <- dim(m)

  if (m.dim[1] == 1L & target.dim[1] > 1L) {
    m <- m[rep(1, target.dim[1]), , drop = FALSE]
  }

  if (is(m, "hyperSpec")) {
    if (m.dim[3] == 1L & target.dim[2] > 1L) {
      m <- m[, , rep(1, target.dim[2]), wl.index = TRUE]
    }
  } else {
    if (m.dim[2] == 1L & target.dim[2] > 1L) {
      m <- m[, rep(1, target.dim[2]), drop = FALSE]
    }
  }

  m
}

.test(.expand) <- function(){
  context(".expand helper function for sweep shortcut operators")

    test_that("scalar", {
      expect_equal(.expand(1, c(1, 3)), matrix(rep(1, 3), nrow = 1, ncol = 3))
      expect_equal(.expand(1, c(3, 1)), matrix(rep(1, 3), nrow = 3, ncol = 1))
      expect_equal(.expand(1, c(3, 5)), matrix(rep(1, 3*5), nrow = 3, ncol = 5))
      expect_equal(.expand(1, c(1, 1)), matrix(1))
    })

    test_that("matrix with > 1 row / > 1 column: leave unchanged", {
      m <- matrix(1:15, ncol = 5)

      expect_equal(.expand(m, c(1, 5)), m)
      expect_equal(.expand(m, c(3, 1)), m)
    })

    test_that("matrix with 1 row", {
      m <- matrix(1:5, ncol = 5)

      expect_equal(.expand(m, c(1, 5)), m)
      expect_equal(.expand(m, c(3, 5)), m[rep(1, 3),])
      expect_equal(.expand(m, c(3, 4)), m[rep(1, 3),])  # ignore request of ncol = 4 !!!
    })

    test_that("matrix with 1 column", {
      m <- matrix(1:5, nrow = 5)

      expect_equal(.expand(m, c(5, 3)), m[,rep(1, 3)])
      expect_equal(.expand(m, c(5, 1)), m)
      expect_equal(.expand(m, c(4, 3)), m[rep(1, 3),]) # ignore request of nrow = 4 !!!
    })



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

