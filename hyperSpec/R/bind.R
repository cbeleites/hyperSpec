# Function -------------------------------------------------------------------

#' Binding `hyperSpec` objects
#'
#' Functions to bind `hyperSpec` objects.
#'
#'
#' The former difficulties with binding S4 objects are resolved since R version
#' 3.2.0 and `cbind()` and `rbind()` now work as intended and expected for
#' `hyperSpec` objects.
#'
#' Therefore, calling `rbind.hyperSpec` and `cbind.hyperSpec` is now deprecated:
#' `cbind` and `rbind` should now be called directly.
#'
#' However, in consequence it is no longer possible to call `cbind()` or
#' `rbind()` with a list of `hyperSpec` objects. In that case, use `bind()` or
#' [base::do.call()] (see example).
#'
#' `bind()` does the common work for both column- and row-wise binding.
#'
#'
#' @aliases bind bind,hyperSpec
#'
#' @param ... The `hyperSpec` objects to be combined. \cr
#'        Alternatively, *one* list of `hyperSpec` objects can be given to
#'        `bind()`.
#' @param wl.tolerance `rbind` and `rbind2` check for equal wavelengths with
#'        this tolerance.
#' @param direction "r" or "c" to bind rows or columns.
#'
#' @return A `hyperSpec` object, possibly with different row order
#'        (for `bind("c", ...)` and `cbind2()`).
#'
#' @note You might have to make sure that the objects either all have or all
#'       do not have rownames and/or colnames.
#'
#' @author C. Beleites
#'
#' @keywords methods manip
#' @concept manipulation
#'
#' @seealso
#' [methods::rbind2()], [methods::cbind2()],
#' [base::rbind()], [base::cbind()],
#'
#' [merge()] and [collapse()] for combining objects that do not share spectra
#' or wavelengths, respectively.
#'
#'
#' @include paste_row.R
#' @export
#'
#'
#' @examples
#'
#' faux_cell
#'
#' bind("r", faux_cell, faux_cell)
#' rbind(faux_cell, faux_cell)
#' cbind(faux_cell, faux_cell)
#' bind("r", list(faux_cell, faux_cell, faux_cell))
#'
#' x <- faux_cell[, , 600:605]
#' x$a <- 1
#' x@data <- x@data[, sample(ncol(x), ncol(x))] # reorder columns
#'
#' y <- faux_cell[nrow(faux_cell):1, , 1730:1750] # reorder rows
#' y$b <- 2
#'
#' cbind2(x, y) # works
#'
#' y$y[3] <- 5
#' try(cbind2(x, y)) # error
#'
#' # list of hyperSpec objects
#'
#' lhy <- list(flu, flu)
#' do.call("rbind", lhy)
#' bind("r", lhy)
bind <- function(direction = stop("direction('c' or 'r') required"), ...,
                 wl.tolerance = hy.getOption("wl.tolerance")) {
  wl.tolerance <- .checkpos(wl.tolerance, "wl.tolerance")
  dots <- list(...)

  if ((length(dots) == 1) & is.list(dots[[1]])) {
    dots <- dots[[1]]
  }

  if (length(dots) == 0) {
    NULL
  } else if (length(dots) == 1) {
    validObject(dots[[1]])
    dots[[1]]
  } else { # binding is actually needed.
    lapply(dots, chk.hy)
    lapply(dots, validObject)

    for (i in seq_along(dots)[-1]) {
      dots[[1]] <- switch(direction,
        c = cbind2(dots[[1]], dots[[i]]),
        r = rbind2(dots[[1]], dots[[i]], wl.tolerance = wl.tolerance),
        stop(
          "direction must be either 'c' or 'r' for cbind",
          "and rbind, respectively."
        )
      )
    }

    dots[[1]]
  }
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(bind) <- function() {
  context("bind")

  test_that("bind() throws error", {
    expect_error(bind("r"))
    expect_error(bind("c"))
  })

  test_that("bind() works", {
    expect_equal(bind("r", flu), flu)
    expect_equal(bind("c", flu), flu)
  })

  test_that("wl.tolerance for rbind", {
    tmp <- flu
    wl(tmp) <- wl(tmp) + 0.01
    expect_error(bind("r", tmp, flu))
    expect_equivalent(
      nwl(bind("r", tmp, flu, tmp, flu, wl.tolerance = 0.1)),
      nwl(flu)
    )

    tmp.list <- list(flu, tmp, flu)
    expect_error(bind("r", tmp.list))
    expect_true(all.equal(
      bind("r", tmp.list, wl.tolerance = 0.1),
      flu[rep(row.seq(flu), 3)],
      check.label = TRUE
    ))

    expect_true(all.equal(
      do.call("bind", list("r", tmp.list, wl.tolerance = 0.1)),
      flu[rep(row.seq(flu), 3)],
      check.label = TRUE
    ))
  })
}


# Function -------------------------------------------------------------------

#' @description `cbind2()` binds the spectral matrices of two `hyperSpec`
#' objects by column. All columns besides `spc` with the same name in `x@data`
#' and `y@data` must have the same elements.  Rows are ordered before checking.
#'
#' @rdname bind
#' @aliases bind cbind.hyperSpec rbind.hyperSpec
#'   cbind2,hyperSpec,hyperSpec-method rbind2,hyperSpec,hyperSpec-method
#'   cbind2,hyperSpec,missing-method rbind2,hyperSpec,missing-method
#' @aliases cbind.hyperSpec
#'
#' @param x,y `hyperSpec` objects
#'
#' @concept manipulation
#'
#' @export
cbind.hyperSpec <- function(...) {
  bind("c", ...)
}


#' `rbind2()` binds two `hyperSpec` objects by row. They need to have the same
#' columns.
#'
#' @rdname bind
#' @aliases rbind.hyperSpec
#'
#' @concept manipulation
#'
#' @export
rbind.hyperSpec <- function(...) {
  bind("r", ...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(rbind.hyperSpec) <- function() {
  context("rbind.hyperSpec")

  test_that("wl.tolerance", {
    tmp <- flu
    wl(tmp) <- wl(tmp) + 0.01
    expect_error(rbind(tmp, flu))
    expect_equivalent(nwl(rbind(tmp, flu, flu, wl.tolerance = 0.1)), nwl(flu))

    tmp.list <- list(flu, tmp, flu)
    expect_true(all.equal(do.call("rbind", c(tmp.list, wl.tolerance = 0.1)),
      flu[rep(row.seq(flu), 3)],
      check.label = TRUE
    ))
  })

  test_that("correct rbinding", {
    expect_equal(nrow(rbind(flu, flu)), 2 * nrow(flu))
    expect_error(rbind(flu, flu[, , min ~ min + 3i]))
  })

  test_that("list of hyperSpec objects", {
    expect_equal(nrow(rbind(flu, flu)), 2 * nrow(flu))
    expect_error(rbind(flu, flu[, , min ~ min + 3i]))
  })

  test_that("rbind.hyperSpec() uses bind('r', ...) correctly", {
    expect_equal(bind("r", flu, flu), rbind(flu, flu))
  })

  test_that("rbind.hyperSpec() works", {

    # One dataset
    expect_equal(rbind(flu), flu)

    # Two datasets
    expect_equal(rbind(flu[1:3, , ], flu[4:6, , ]), flu)
    expect_equal(
      rbind(flu[c(1, 3, 6), , ], flu[c(2, 4, 5), , ]),
      flu[c(1, 3, 6, 2, 4, 5), , ]
    )

    # Three datasets
    expect_equal(
      rbind(flu[1, , ], flu[6, , ], flu[5, , ]),
      flu[c(1, 6, 5), , ]
    )
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hySpc.testthat::test(cbind.hyperSpec) <- function() {
  context("cbind.hyperSpec")

  test_that("cbind.hyperSpec() uses bind('c', ...) correctly", {
    expect_equal(bind("c", flu, flu), cbind(flu, flu))
  })

  test_that("cbind.hyperSpec() works", {

    # One dataset
    expect_equal(cbind(flu), flu)

    # Two datasets
    expect_equal(
      cbind(flu[, , 1:10, wl.index = TRUE], flu[, , 21:30, wl.index = TRUE]),
      flu[, , c(1:10, 21:30), wl.index = TRUE]
    )

    # Three datasets
    expect_equal(
      cbind(
        flu[, , 1:10, wl.index = TRUE],
        flu[, , 21:30, wl.index = TRUE],
        flu[, , 51:80, wl.index = TRUE]
      ),
      flu[, , c(1:10, 21:30, 51:80), wl.index = TRUE]
    )
  })
}

# Function -------------------------------------------------------------------

.cbind2 <- function(x, y) {
  validObject(x)
  validObject(y)

  cols <- match(colnames(x@data), colnames(y@data))
  cols <- colnames(y@data)[cols]
  cols <- cols[!is.na(cols)]
  cols <- cols[-match("spc", cols)]

  if (length(cols) < 0) {
    ord <- do.call(order, x@data[, cols, drop = FALSE])
    x@data <- x@data[ord, , drop = FALSE]

    ord <- do.call(order, y@data[, cols, drop = FALSE])
    y@data <- y@data[ord, , drop = FALSE]

    if (any(x@data[, cols, drop = FALSE] != y@data[, cols, drop = FALSE])) {
      stop(
        "hyperSpec objects must have the same data in columns",
        "of the same name (except data$spc)"
      )
    }
  }

  ## for the spectra, multiple occurences of the same wavelength are O.K.
  x@data$spc <- cbind(x@data$spc, y@data$spc)
  .wl(x) <- c(x@wavelength, y@wavelength)

  ## cbind columns in y that are not in x
  cols <- is.na(match(colnames(y@data), colnames(x@data)))
  x@data <- cbind(
    x@data,
    y@data[, cols, drop = FALSE]
  )

  x
}

#' @rdname bind
#' @export
#'
#' @concept manipulation
#'
#' @aliases cbind2,hyperSpec,hyperSpec-method
setMethod("cbind2", signature = signature(x = "hyperSpec", y = "hyperSpec"), .cbind2)


# Function -------------------------------------------------------------------

.cbind2_h_ <- function(x, y) {
  x
}

#' @rdname bind
#' @export
#'
#' @concept manipulation
#'
#' @aliases cbind2,hyperSpec,missing-method
setMethod("cbind2", signature = signature(x = "hyperSpec", y = "missing"), .cbind2_h_)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.cbind2) <- function() {
  context(".cbind2")

  test_that("flu", {
    expect_equal(cbind(flu[, 1], flu[, -1]), flu, check.attributes = FALSE)
    expect_equal(cbind(flu[, -1], flu[, 1]), flu, check.attributes = FALSE)
    expect_equal(cbind(flu[, 1:2], flu[, 3]), flu, check.attributes = FALSE)
  })

  test_that("empty objects", {
    expect_equal(cbind(flu[, 0], flu[, 0]), flu[, 0], check.attributes = FALSE)
    expect_equal(cbind(flu[, 1], flu[, 0]), flu[, 1], check.attributes = FALSE)
    expect_equal(cbind(flu[, 0], flu[, 1]), flu[, 1], check.attributes = FALSE)
  })

  test_that("cbind2", {
    expect_equal(cbind2(flu), flu)
    expect_equal(cbind2(flu, flu), cbind(flu, flu))
  })
}


# Function -------------------------------------------------------------------

.rbind2 <- function(x, y, wl.tolerance = hy.getOption("wl.tolerance")) {
  validObject(x)
  validObject(y)
  wl.tolerance <- .checkpos(wl.tolerance, "wl.tolerance")

  if (!isTRUE(all.equal(x@wavelength, y@wavelength, tolerance = wl.tolerance))) {
    stop(
      "The wavelengths of the objects differ (with respect to tolerance ", wl.tolerance, ").\n",
      "If they are not ordered, try 'wl_sort'."
    )
  }

  x@data <- rbind(x@data, y@data)

  x
}

#' @rdname bind
#' @export
#'
#' @concept manipulation
#'
#' @aliases rbind2,hyperSpec,hyperSpec-method
setMethod("rbind2", signature = signature(x = "hyperSpec", y = "hyperSpec"), .rbind2)


# Function -------------------------------------------------------------------

.rbind2_h_ <- function(x, y, wl.tolerance) {
  x
}

#' @rdname bind
#' @export
#'
#' @concept manipulation
#'
#' @aliases rbind2,hyperSpec,missing-method
setMethod("rbind2", signature = signature(x = "hyperSpec", y = "missing"), .rbind2_h_)

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.rbind2) <- function() {
  context(".rbind2")

  test_that("flu", {
    expect_equal(rbind(flu[1], flu[-1]), flu, check.attributes = FALSE)
    expect_equal(rbind(flu[-1], flu[1]), flu[c(2:6, 1)], check.attributes = FALSE)
    expect_equal(rbind(flu[1:2], flu[3:6]), flu, check.attributes = FALSE)
  })

  test_that("empty objects", {
    expect_equal(rbind(flu[0], flu[0]), flu[0], check.attributes = FALSE)
    expect_equal(rbind(flu[1], flu[0]), flu[1], check.attributes = FALSE)
    expect_equal(rbind(flu[0], flu[1]), flu[1], check.attributes = FALSE)
  })

  test_that("wl.tolerance", {
    tmp <- flu
    wl(tmp) <- wl(tmp) + 0.01
    expect_error(rbind2(tmp, flu))
    expect_equivalent(nwl(rbind2(tmp, flu, wl.tolerance = 0.1)), nwl(flu))
  })

  test_that("rbind2", {
    expect_equal(rbind2(flu), flu)
    expect_equal(rbind2(flu, flu), rbind(flu, flu))
  })
}
