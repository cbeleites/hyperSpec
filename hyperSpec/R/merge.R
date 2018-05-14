##' Merge hyperSpec objects
##'
##' Merges two hyperSpec objects and \code{\link[base]{cbind}}s their spectra
##' matrices, or merges additional extra data into a hyperSpec object.
##'
##' After merging, the spectra matrix can contain duplicates, and is not
##' ordered according to the wavelength.
##'
##' If the wavelength axis should be ordered, use \code{\link{orderwl}}.
##'
##' If a \code{hyperSpec} object and  a \code{data.frame} are merged, the result is of the class of the first (\code{x}) object.
##'
##' @aliases merge,hyperSpec,hyperSpec-method merge
##' @param x a hyperSpec object or data.frame
##' @param y a hyperSpec object or data.frame (including derived classes like tibble)
##' @param ... handed to \code{\link[base]{merge.data.frame}}
##' @author C. Beleites
##' @export
##' @rdname merge
##' @docType methods
##' @aliases merge
##' @seealso \code{\link[base]{merge}}.
##'
##' \code{\link{collapse}} combines hyperSpec objects that do not share the wavelength axis.
##' \code{\link{rbind}}, and \code{\link{cbind}} for combining hyperSpec objects that.
##' @keywords manip
##' @examples
##'
##' merge (chondro [1:10,, 600], chondro [5:15,, 600], by = c("x", "y"))$.
##' tmp <- merge (chondro [1:10,, 610], chondro [5:15,, 610],
##'               by = c("x", "y"), all = TRUE)
##' tmp$.
##' wl (tmp)
##'
##' ## remove duplicated wavelengths:
##' approxfun <- function (y, wl, new.wl){
##'   approx (wl, y, new.wl, method = "constant",
##'           ties = function (x) mean (x, na.rm = TRUE)
##'           )$y
##' }
##'
##' merged <- merge (chondro [1:7,, 610 ~ 620], chondro [5:10,, 615 ~ 625], all = TRUE)
##' merged$.
##' merged <- apply (merged, 1, approxfun,
##'                  wl = wl (merged), new.wl = unique (wl (merged)),
##'                  new.wavelength = "new.wl")
##' merged$.
##'
##' ## merging data.frame into hyperSpec object => hyperSpec object
##' y <- data.frame (filename = sample (flu$filename, 4, replace = TRUE), cpred = 1:4)
##' y
##' tmp <- merge (flu, y)
##' tmp$..
##'
##' ## merging hyperSpec object into data.frame => data.frame
##' merge (y, flu)

setMethod ("merge", signature = signature (x = "hyperSpec", y = "hyperSpec"),
           function (x, y, ...){
             validObject (x)
             validObject (y)

             tmp <- .merge (x, y, ...)

             if (nrow (tmp) == 0 && nrow (x) > 0 && nrow (y) > 0)
               warning ("Merge results in 0 spectra.")

             tmp
           }
           )


.merge <- function (x, y,
                    by = setdiff (intersect(colnames(x), colnames(y)), "spc"),
                    by.x = by, by.y = by,
                    ...){
  force (by)
  force (by.x)
  force (by.y)

  if (any (grepl ("^spc$", by.x))){
    by.x <- setdiff (by.x, "spc")
    warning ('"spc" removed from by.x')
  }

  if (any (grepl ("^spc$", by.y))){
    by.y <- setdiff (by.y, "spc")
    warning ('"spc" removed from by.y')
  }

  x$.nx <- seq_len (nrow (x))
  y$.ny <- seq_len (nrow (y))

  x.spc <- match ("spc", colnames (x))
  y.spc <- match ("spc", colnames (y))

  tmp <- merge (x@data [, -x.spc], y@data [, -y.spc], by.x = by.x, by.y = by.y, ...)

  spc.x <- matrix (NA, nrow = nrow (tmp), ncol = nwl (x))
  spc.x [! is.na (tmp$.nx),] <- x@data [tmp$.nx[! is.na (tmp$.nx)], x.spc]

  spc.y <- matrix (NA, nrow = nrow (tmp), ncol = nwl (y))
  spc.y [! is.na (tmp$.ny),] <- y@data [tmp$.ny[! is.na (tmp$.ny)], y.spc]

  tmp$spc <- cbind (spc.x, spc.y) # omit I ()

  x@data <- tmp
  .wl (x) <- c (x@wavelength, y@wavelength)

  x
}

##' @rdname merge
setMethod ("merge", signature = signature (x = "hyperSpec", y = "data.frame"),
           function (x, y, ...){
             validObject (x)

             tmp <- merge (x@data, y, ...)
             tmp
             if (nrow (tmp) == 0 && nrow (x) > 0 && nrow (y) > 0)
               warning ("Merge results in 0 spectra.")

             x@data <- tmp

             x
           }
)

##' @rdname merge
setMethod ("merge", signature = signature (x = "data.frame", y = "hyperSpec"),
           function (x, y, ...){
             validObject (y)

             merge (x, y@data, ...)
           }
)




##' @include unittest.R
.test (.merge) <- function (){
  context ("merge")

  test_that ("correct number of rows", {
    expect_equivalent (nrow (merge (chondro [1:10], chondro [5:15], all = TRUE)), 15)
    expect_equivalent (nrow (merge (chondro [1:10], chondro [5:15])), 6)
  })

  test_that("merging hyperSpec object with data.frame", {

    ## y has multiple rows for each x row
    y <- data.frame (filename = rep (flu$filename, 2), cpred = 1:12)
    tmp <- merge (flu, y)

    expect_s4_class(tmp, "hyperSpec")

    expect_equivalent (nrow (tmp), 12L)
    expect_equivalent (sort (unique (c (colnames (flu), colnames (y)))), sort (colnames (tmp)))

    ## y has rows x does not have
    tmp <- merge (flu [1:2], y)
    expect_equivalent (nrow (tmp), 4L)

    ## all.y = TRUE
    tmp <- merge (flu [1:2], y, all.y = TRUE)
    expect_equivalent (nrow (tmp), 12L)
    expect_equivalent(sum (is.na (tmp$c)), 8)

    ## x has rows y does not have
    tmp <- merge (flu, y [1:2,])
    expect_equivalent (nrow (tmp), 2L)

    ## all.x = TRUE
    tmp <- merge (flu, y [c (1,7),], all.x = TRUE)
    expect_equivalent (nrow (tmp), 7L)
    expect_equivalent(sum (is.na (tmp$cpred)), 5)
  })

  test_that("merge hyperSpec object with tibble", {
    skip_if_not (require ("tibble"))
    y <- data.frame (filename = rep (flu$filename, 2), cpred = 1:12)
    y <- as_tibble (y)

    tmp <- merge (flu, y)
    expect_equivalent (nrow (tmp), 12L)
    expect_equivalent (sort (unique (c (colnames (flu), colnames (y)))), sort (colnames (tmp)))
  })

  test_that("merge hyperSpec object into data.frame", {

    y <- data.frame (filename = rep (flu$filename, 2), cpred = 1:12)
    tmp <- merge (y, flu)

    expect_s3_class(tmp, "data.frame")

  })
}


