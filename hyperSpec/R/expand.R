#' Expand scalar, vector, matrix or similarly shaped `hyperSpec` object to matrix
#'
#' Helper function for `hyperSpec` arithmetics.
#'
#' A row- or column vector or
#' a matrix of size 1 x n or n x 1 or
#' a hyperSpec object with 1 row or 1 wavelength will be expanded
#' along the size-1-dimension.
#'
#' Dimensions that have size > 1 lead to an error if the size does not match.
#'
#' @param m matrix, vector or scalar
#' @param target.dim target size to expand the vector to for the sweep-shortcuts
#' @noRd
.expand <- function(m, target.dim) {

  ## vector corresponding to a single row
  if (is.vector(m) & length(m) > 1 & length(m) == target.dim[2]) {
    if (length(m) == target.dim[1]) {
      message("Square target: recycling by column.")
    } else {
      m <- t(m)
    }
  }

  ## vector corresponding to whole matrix
  if (is.vector(m) & length(m) == prod(target.dim)) {
    m <- matrix(m, nrow = target.dim[1])
  }

  ## remaining type of vector: column vector
  if (is.vector(m)) {
    m <- as.matrix(m)
  }

  m.dim <- dim(m)

  # hyperSpec objects have columns of spectra matrix in 3rd dimension
  if (is(m, "hyperSpec")) {
    m.dim <- m.dim[c(1, 3)]
  }

  # check for mismatch
  if (any(m.dim > 1L & m.dim != target.dim)) {
    stop("Dimension mismatch.")
  }


  if (m.dim[1] == 1L & target.dim[1] > 1L) {
    m <- m[rep(1, target.dim[1]), , drop = FALSE]
  }

  if (is(m, "hyperSpec")) {
    if (m.dim[2] == 1L & target.dim[2] > 1L) {
      m <- m[, , rep(1, target.dim[2]), wl.index = TRUE]
    }
  } else {
    if (m.dim[2] == 1L & target.dim[2] > 1L) {
      m <- m[, rep(1, target.dim[2]), drop = FALSE]
    }
  }

  m
}

hySpc.testthat::test(.expand) <- function() {
  context(".expand helper function for sweep shortcut operators")

  test_that("scalar", {
    expect_equal(.expand(1, c(1, 3)), matrix(rep(1, 3), nrow = 1, ncol = 3))
    expect_equal(.expand(1, c(3, 1)), matrix(rep(1, 3), nrow = 3, ncol = 1))
    expect_equal(.expand(1, c(3, 5)), matrix(rep(1, 3 * 5), nrow = 3, ncol = 5))
    expect_equal(.expand(1, c(1, 1)), matrix(1))
  })

  test_that("vector of length like requested row", {
    v <- 1:5

    expect_equal(.expand(v, c(1, 5)), t(v))
    expect_equal(.expand(v, c(3, 5)), t(v)[rep(1, 3), ])

    expect_error(.expand(v, c(1, 3)), "Dimension mismatch")
    expect_error(.expand(v, c(6, 3)), "Dimension mismatch")
  })

  test_that("vector of length like requested colum", {
    v <- 1:5

    expect_equal(.expand(v, c(5, 1)), as.matrix(v))
    expect_equal(.expand(v, c(5, 3)), as.matrix(v)[, rep(1, 3)])

    expect_error(.expand(v, c(3, 1)), "Dimension mismatch")
    expect_error(.expand(v, c(3, 6)), "Dimension mismatch")
  })

  test_that("vector of length like requested matrix", {
    v <- 1:15

    expect_equal(.expand(v, c(5, 3)), matrix(v, ncol = 3))
  })


  test_that("matrix with 1 row", {
    m <- matrix(1:5, ncol = 5)

    expect_equal(.expand(m, c(1, 5)), m)
    expect_equal(.expand(m, c(3, 5)), m[rep(1, 3), ])

    expect_error(.expand(m, c(1, 3)), "Dimension mismatch")
    expect_error(.expand(m, c(6, 3)), "Dimension mismatch")
  })

  test_that("matrix with 1 column", {
    m <- matrix(1:5, nrow = 5)

    expect_equal(.expand(m, c(5, 1)), m)
    expect_equal(.expand(m, c(5, 3)), m[, rep(1, 3)])

    expect_error(.expand(m, c(3, 1)), "Dimension mismatch")
    expect_error(.expand(m, c(3, 6)), "Dimension mismatch")
  })

  test_that("matrix with > 1 row & > 1 column: leave unchanged", {
    m <- matrix(1:15, ncol = 5)

    expect_equal(.expand(m, c(3, 5)), m)

    expect_error(.expand(m, c(1, 3)), "Dimension mismatch")
    expect_error(.expand(m, c(6, 3)), "Dimension mismatch")
    expect_error(.expand(m, c(3, 1)), "Dimension mismatch")
    expect_error(.expand(m, c(3, 6)), "Dimension mismatch")
  })

  test_that("1 x 1 hyperSpec spectra matrix ('scalar')", {
    h <- new("hyperSpec", spc = matrix(1), data = data.frame(c = 3))

    h3 <- new("hyperSpec",
      spc = t(rep(1, 3)), data = data.frame(c = 3),
      wavelength = rep(1, 3)
    )

    expect_equal(.expand(h, c(1, 1)), h)
    expect_equal(.expand(h, c(1, 3)), h3)
    expect_equal(.expand(h, c(3, 1)), h[rep(1, 3)])
    expect_equal(.expand(h, c(5, 3)), h3[rep(1, 5)])
  })

  test_that("1 x n hyperSpec spectra matrix (row matrix)", {
    v <- flu[1]

    expect_equal(.expand(v, c(1, 181)), v)
    expect_equal(.expand(v, c(3, 181)), v[rep(1, 3), ])

    expect_error(.expand(v, c(1, 3)), "Dimension mismatch")
    expect_error(.expand(v, c(6, 3)), "Dimension mismatch")
  })

  test_that("n x 1 hyperSpec spectra matrix (column matrix)", {
    v <- flu[, , 450]
    v3 <- flu[, , rep(450, 3)]

    expect_equal(.expand(v, c(6, 1)), v)
    expect_equal(.expand(v, c(6, 3)), v3)

    expect_error(.expand(v, c(3, 1)), "Dimension mismatch")
    expect_error(.expand(v, c(3, 6)), "Dimension mismatch")
  })

  test_that("hyperSpec spectra matrix with > 1 row & > 1 column: leave unchanged", {
    expect_equal(.expand(flu, c(6, 181)), flu)

    expect_error(.expand(flu, c(1, 181)), "Dimension mismatch")
    expect_error(.expand(flu, c(6, 1)), "Dimension mismatch")
    expect_error(.expand(flu, c(5, 6)), "Dimension mismatch")
  })
}
