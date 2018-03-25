##' Binding hyperSpec Objects
##'
##' The former difficulties with binding S4 objects
##' are resolved since R version 3.2.0 and \code{cbind} and \code{rbind} now work as intended and
##' expected for hyperSpec objects.
##'
##' Therefore, calling \code{rbind.hyperSpec} and
##' \code{cbind.hyperSpec} is now depecated: \code{cbind} and \code{rbind} should now be called
##' directly.
##'
##' However, in consequence it is no longer possible to call \code{cbind} or \code{rbind} with a
##' list of hyperSpec objects. In that case, use \code{bind} or \code{\link[base]{do.call}} (see example).
##'
##' \code{bind} does the common work for both column- and row-wise binding.
##'
##' @aliases bind
##' @param ... The \code{hyperSpec} objects to be combined.
##'
##' Alternatively, \emph{one} list of \code{hyperSpec} objects can be given to
##'   \code{bind}.
##' @param wl.tolerance \code{rbind} and \code{rbind2} check for equal wavelengths
##' with this tolerance.
##' @include paste.row.R
##' @param direction "r" or "c" to bind rows or columns
##' @return a \code{hyperSpec} object, possibly with different row order (for
##'   \code{bind ("c", \dots{})} and \code{cbind2}).
##' @note You might have to make sure that the objects either all have or all
##'   do not have rownames and/or colnames.
##' @author C. Beleites
##' @export
##' @seealso
##' \code{\link[methods]{rbind2}}, \code{\link[methods]{cbind2}}
##' \code{\link[base]{rbind}}, \code{\link[base]{cbind}}
##'
##' \code{\link{merge}} and \code{\link{collapse}} for combining objects that do not share spectra
##' or wavelengths, respectively.
##' @keywords methods manip
##' @examples
##'
##' chondro
##' bind ("r", chondro, chondro)
##' rbind (chondro, chondro)
##' cbind (chondro, chondro)
##' bind ("r", list (chondro, chondro, chondro))
##'
##' x <- chondro[,, 600 : 605]
##' x$a <- 1
##' x@@data <- x@@data[, sample (ncol (x), ncol (x))] # reorder columns
##'
##' y <- chondro [nrow (chondro) : 1,, 1730 : 1750] # reorder rows
##' y$b <- 2
##'
##' cbind2 (x, y) # works
##'
##' y$y[3] <- 5
##' try (cbind2 (x, y)) # error
##'
##' # list of hyperSpec objects
##'
##' lhy <- list (flu, flu)
##' do.call ("rbind", lhy)
##' bind ("r", lhy)
bind <- function (direction = stop ("direction ('c' or 'r') required"), ...,
									wl.tolerance = hy.getOption ("wl.tolerance")){

	wl.tolerance <- .checkpos (wl.tolerance, "wl.tolerance")
  dots <- list (...)

  if ((length (dots) == 1) & is.list (dots [[1]]))
    dots <- dots[[1]]

  if (length (dots) == 0)
    NULL
  else if (length (dots) == 1){
    validObject (dots[[1]])
    dots[[1]]
  } else {                              # binding is actually needed.
    lapply (dots, chk.hy)
    lapply (dots, validObject)

    for (i in seq_along (dots) [-1]){
      dots[[1]] <- switch (direction,
                           c = cbind2 (dots[[1]], dots[[i]]),
                           r = rbind2 (dots[[1]], dots[[i]], wl.tolerance = wl.tolerance),
                           stop ("direction must be either 'c' or 'r' for cbind",
                                 "and rbind, respectively.")
                           )
    }

    dots [[1]]
  }
}

##' @include unittest.R
.test (bind) <- function () {
  context ("bind")

  test_that("wl.tolerance for rbind", {
    tmp <- flu
    wl (tmp) <- wl (tmp) + 0.01
    expect_error (bind ("r", tmp, flu))
    expect_equivalent (nwl (bind ("r", tmp, flu, tmp, flu, wl.tolerance = 0.1)), nwl (flu))


    tmp.list <- list (flu, tmp, flu)

    expect_error (bind ("r", tmp.list))
    expect_true (all.equal (bind ("r", tmp.list, wl.tolerance = 0.1),
                            flu [rep (row.seq (flu), 3)],
                            check.label = TRUE))

    expect_true (all.equal (do.call ("bind", list ("r", tmp.list, wl.tolerance = 0.1)),
                            flu [rep (row.seq (flu), 3)],
                            check.label = TRUE))
  })
}


##' @description  \code{cbind2} binds the spectral matrices of two \code{hyperSpec} objects by column. All columns
##' besides \code{spc} with the same name in \code{x@@data} and \code{y@@data} must have the same
##' elements.  Rows are ordered before checking.
##' @aliases bind cbind.hyperSpec rbind.hyperSpec
##'   cbind2,hyperSpec,hyperSpec-method rbind2,hyperSpec,hyperSpec-method
##'   cbind2,hyperSpec,missing-method rbind2,hyperSpec,missing-method
##' @param x,y \code{hyperSpec} objects
##' @rdname bind
##' @export
##' @aliases cbind.hyperSpec

##'
cbind.hyperSpec <- function (...) bind ("c", ...)

##'
##' \code{rbind2} binds two \code{hyperSpec} objects by row. They need to have
##' the same columns.
##'
##' @aliases  rbind.hyperSpec
##' @rdname bind
##' @export
##' @aliases rbind.hyperSpec
rbind.hyperSpec <- function (...) bind ("r", ...)

.test (rbind.hyperSpec) <- function () {
  context ("rbind.hyperSpec")

  test_that("wl.tolerance",{
    tmp <- flu
    wl (tmp) <- wl (tmp) + 0.01
    expect_error (rbind (tmp, flu))
    expect_equivalent (nwl (rbind (tmp, flu, flu, wl.tolerance = 0.1)), nwl (flu))

    tmp.list <- list (flu, tmp, flu)
    expect_true (all.equal (do.call ("rbind", c (tmp.list, wl.tolerance = 0.1)),
                            flu [rep (row.seq (flu), 3)],
                            check.label = TRUE))
  })

  test_that ("correct rbinding", {
    expect_equal(nrow (rbind (flu, flu)), 2 * nrow (flu))
    expect_error(rbind (flu, flu [,, min ~ min + 3i]))
  })

  test_that ("list of hyperSpec objects", {

    expect_equal(nrow (rbind (flu, flu)), 2 * nrow (flu))
    expect_error(rbind (flu, flu [,, min ~ min + 3i]))
  })

}


.cbind2 <- function (x, y){
	validObject (x)
	validObject (y)

	cols <- match (colnames (x@data), colnames (y@data))
	cols <- colnames (y@data) [cols]
	cols <- cols [! is.na (cols)]
	cols <- cols [- match ("spc", cols)]

	if (length (cols) < 0){
		ord <- do.call (order, x@data[, cols, drop = FALSE])
		x@data <- x@data[ord, , drop = FALSE]

		ord <- do.call (order, y@data[, cols, drop = FALSE])
		y@data <- y@data[ord, , drop = FALSE]

		if (any (x@data[, cols, drop = FALSE] != y@data[, cols, drop = FALSE]))
			stop ("hyperSpec objects must have the same data in columns",
						"of the same name (except data$spc)")
	}

	## for the spectra, multiple occurences of the same wavelength are O.K.
	x@data$spc <- cbind(x@data$spc, y@data$spc)
	.wl (x) <- c (x@wavelength, y@wavelength)

	## cbind columns in y that are not in x
	cols <- is.na (match (colnames (y@data), colnames (x@data)))
	x@data <- cbind (x@data,
									 y@data[, cols, drop = FALSE])

	x
}
##' @rdname bind
##' @export
##' @aliases cbind2,hyperSpec,hyperSpec-method
setMethod ("cbind2", signature = signature (x = "hyperSpec", y = "hyperSpec"), .cbind2)

##' @rdname bind
##' @export
##' @aliases cbind2,hyperSpec,missing-method
setMethod("cbind2", signature = signature (x = "hyperSpec", y = "missing"), function (x, y) x)

.rbind2 <- function (x, y, wl.tolerance = hy.getOption ("wl.tolerance")) {
	validObject (x)
	validObject (y)
	wl.tolerance <- .checkpos (wl.tolerance, "wl.tolerance")

	if (! isTRUE (all.equal (x@wavelength, y@wavelength, tolerance = wl.tolerance)))
		stop ("The wavelengths of the objects differ (with respect to tolerance ", wl.tolerance, ").\n",
					"If they are not ordered, try 'orderwl'.")

	x@data <- rbind (x@data, y@data)

	x
}

.test (.rbind2) <- function () {
  context (".rbind2")

  test_that("flu", {
    expect_equal (rbind (flu [ 1],  flu [-1]),  flu,              check.attributes = FALSE)
    expect_equal (rbind (flu [-1],  flu [ 1]),  flu [c (2:6, 1)], check.attributes = FALSE)
    expect_equal (rbind (flu [1:2], flu [3:6]), flu,              check.attributes = FALSE)
  })

  test_that("empty objects", {
    expect_equal (rbind (flu [0], flu [0]), flu [0], check.attributes = FALSE)
    expect_equal (rbind (flu [1], flu [0]), flu [1], check.attributes = FALSE)
    expect_equal (rbind (flu [0], flu [1]), flu [1], check.attributes = FALSE)
  })


	test_that("wl.tolerance", {
	  tmp <- flu
	  wl (tmp) <- wl (tmp) + 0.01
	  expect_error (rbind2 (tmp, flu))
	  expect_equivalent (nwl (rbind2 (tmp, flu, wl.tolerance = 0.1)), nwl (flu))
	})
}

##' @rdname bind
##' @export
##' @aliases  rbind2,hyperSpec,hyperSpec-method
setMethod("rbind2", signature = signature (x = "hyperSpec", y = "hyperSpec"), .rbind2)

##' @rdname bind
##' @export
##' @aliases rbind2,hyperSpec,missing-method
setMethod ("rbind2", signature = signature (x = "hyperSpec", y = "missing"), function (x, y, wl.tolerance) x)

