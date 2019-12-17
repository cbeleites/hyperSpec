##' collapse/bind several hyperSpec objects into one object
##'
##' The spectra from all objects will be put into one object.
##' The resulting object has all wavelengths that occur in any of the input objects,
##' `wl.tolerance` is used to determine which difference in the wavelengths is
##' tolerated as equal. The returned object has wavelengths rounded according to the
##' precision indicated by `wl.tolerance`.
##' 
##' Labels will be taken from the first object where they are encountered. However, 
##' the order of processing objects is not necessarily the same as the order of objects
##' in the input: `collapse` first processes groups of input objects that share all 
##' wavelengths (within `wl.tolerance`). 
##'
##' Data points corresponding to wavelengths not in the original spectrum will be set to NA.
##' Extra data is combined in the same manner.
##'
##' If the objects are named, the names will be preserved in extra data column `$.name`.
##'
##' @author C. Beleites
##' @md
##' @title Collapse hyperSpec objects
##' @export
##' @param ... hyperSpec objects to be collapsed into one object. Instead of giving several
##' arguments, a list with all objects to be collapsed may be given.
##' @param wl.tolerance tolerance to decide which wavelengths are considered equal.
##' @aliases collapse collapse.hyperSpec
##' @seealso [merge()],  [rbind()], and [plyr::rbind.fill()]
##' @return a hyperSpec object
##' @keywords manip
##' @examples
##' barbiturates [1:3]
##' collapse (barbiturates [1:3]
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

  dots <- lapply (dots, orderwl)
  
  ## names cause problems with unlisting labels.
  ## preserve them in column .name
  if (! is.null (names (dots))){
    dots <- mapply (function (object, name) {object$.name <- name; object}, dots, names (dots))
    names (dots) <- NULL
  }

  # first pass: bind groups of objects that have *all* wavelengths equal within wl.tolerance
  i <- 1
  while (i < length (dots)){
    bind_directly <- lapply (lapply (tail (dots, -i), wl), all.equal, target = wl (dots [[i]]), tolerance = wl.tolerance)
    bind_directly <- which (sapply (bind_directly, isTRUE))
        
    dots [[i]]@data <-  rbind.fill (lapply (dots [c (i, i + bind_directly)], slot, "data"))
    labels <- unlist (lapply (dots [c (i, i + bind_directly)], labels))
    labels (dots [[i]]) <- labels [! duplicated(names (labels))]
    
    dots <- dots [- (i + bind_directly)] 
    
    i <- i + 1
  }
  
  # are we done already?
  if (length (dots) == 1L)
    return (dots [[1]])
  
  # cluster wavelengths
  
  
  
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

  test_that ("collapsing objects with equal wavelength axes",{
    expect_equivalent (collapse (barbiturates [[1]], barbiturates [[1]]),
                       barbiturates [[1]][c (1,1)],
                       check.label = TRUE
    )
  })
}

