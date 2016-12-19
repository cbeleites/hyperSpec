##' collapse/bind several hyperSpec objects into one object
##'
##' The spectra from all objects will be put into one object.
##' The resulting object has all wavelengths that occur in any of the input objects,
##' \code{wl.tolerance} is used to determine which difference in the wavelengths is
##' tolerated as equal. The returned object has wavelengths rounded according to the
##' precision indicated by \code{wl.tolerance}.
##'
##' Data points corresponding to wavelengths not in the original spectrum will be set to NA.
##' Extra data is combined in the same manner.
##'
##' If the objects are named, the names will be preserved in extra data column \code{$.name}.
##'
##' @author C. Beleites
##' @title Collapse hyperSpec objects
##' @export
##' @param ... hyperSpec objects to be collapsed into one object. Instead of giving several
##' arguments, a list with all objects to be collapsed may be given.
##' @param wl.tolerance tolerance to decide which wavelengths are considered equal.
##' @aliases collapse collapse.hyperSpec
##' @seealso \code{\link[base]{merge}} to merge hyperSpec objects that share wavelengths but contain
##'   different spectra,  \code{\link[base]{rbind}}, and  \code{\link[plyr]{rbind.fill}} for
##' @return a hyperSpec object
##' @keywords manip
##' @examples
##' barbiturates [1:3]
##' barb <- collapse (barbiturates [1:3])
##' barb
##'
##' a <- barbiturates [[1]]
##' b <- barbiturates [[2]]
##' c <- barbiturates [[3]]
##'
##' a
##' b
##' c
##' collapse (a, b, c)
##'
##'

collapse <- function (..., wl.tolerance = hy.getOption ("wl.tolerance")){
	wl.tolerance <- .checkpos (wl.tolerance, "wl.tolerance")
  dots <- list (...)

  ## accept also a list of hyperSpec objects
  if (length (dots) == 1 && is.list (dots [[1]]))
    dots <- dots [[1]]

  ## check the arguments
  lapply (dots, chk.hy)
  lapply (dots, validObject)

  ## names cause problems with unlisting labels.
  ## preserve them in column .name
  if (! is.null (names (dots))){
    dots <- mapply (function (object, name) {object$.name <- name; object}, dots, names (dots))
    names (dots) <- NULL
  }

  ## prepare new labels
  labels <- unlist (lapply (dots, slot, "label"))
  labels <- labels [unique (names (labels))]

  ## merge data & spectra matrices

  ## to make use of the wavelength tolerance for comparison, wavelengths are expressed as integer
  ## multiples of wl.tolerance. This is done by .wl2cln.
  dots <- lapply (dots, .wl2cln, wl.tolerance)

  ## actual work of collapsing the objects
  dots <- rbind.fill (lapply (dots, slot, "data"))

  wl <- as.numeric (colnames (dots$spc))

  ## back transform the integer representation of wavelength
  wl <- wl * wl.tolerance

  ## make a new hyperSpec object
  x <- new ("hyperSpec", wavelength = wl, data = dots, labels = labels)

  x
}

.wl2cln <- function (x, wl.tolerance){
	if (min (abs (diff (x@wavelength))) < wl.tolerance)
		warning ("wl.tolerance (", wl.tolerance, ") larger than smallest wavelength difference within object (|",
						 min (diff (x@wavelength)), "|).")

	x@wavelength <- round (x@wavelength / wl.tolerance)
	colnames (x@data$spc) <- formatC (x@wavelength, format = "f", digits = 0)
  x
}

##' @include unittest.R
.test (collapse) <- function () {
  context ("collapse")

  test_that ("correctly assembled", {
    new <- do.call (collapse, barbiturates [1 : 3])
    wl <- unlist (lapply (barbiturates [1 : 3], slot, "wavelength"))
    expect_equal (wl (new), wl [! duplicated (wl)])

    for (s in 1 : 3){
      expect_equal (as.numeric (new [[s,, wl (barbiturates [[s]])]]),
                    as.numeric (barbiturates [[s]][[]]),
                    label = paste0 ("barbiturates [[", s, "]]"))
    }
  })

  tmp <- collapse (a = flu, b = flu)
  test_that ("collapse messed up labels if a named list is collapsed", {
    flu.labels <- lapply (flu@label, as.expression)
    expect_equal (labels (tmp) [names (flu.labels)], flu.labels)
  })

  test_that ("named lists should return .name column", {
    expect_equal (tmp$.name, rep (c ("a", "b"), each = nrow (flu)))
  })

  test_that ("no difference whether list or single arguments are given", {
    tmp2 <- list (a = flu, b = flu)
    tmp2 <- collapse (a = flu, b = flu)
    expect_equal (tmp, tmp2,
                 check.attributes = TRUE, check.names = TRUE, check.column.order = FALSE, check.label = TRUE)
  })

  test_that ("wl.tolerance", {
    tmp <- flu
    wl (tmp) <- wl (tmp) + 0.01
    expect_equal (nwl (collapse (tmp, flu                    )), 2 * nwl (flu))
    expect_equal (nwl (collapse (tmp, flu, wl.tolerance = 0.1)), nwl (flu))
  })

  test_that ("check warning occurs for too large tolerance", {
    expect_warning (collapse (flu, wl.tolerance = 0.5 + .Machine$double.eps))
  })

  test_that ("bugfix: wl.tolerance generated warning for negative diff (wl (spc))", {
    tmp <- flu
    wl (tmp) <- rev (wl (tmp))
    expect_silent (collapse (tmp, tmp))
  })

}

