.droplevels <- function (x, ...){
  x@data <- droplevels(x@data, ...)

  x
}

#' droplevels for hyperSpec object
#'
#'  calls [base::droplevels()] on the data.frame in `spc@data`.
#'
#' @param x hyperSpec object
#' @param ... handed to [base::droplevels.data.frame()]
#'
#' @return hyperSpec object with unused levels of all factors in `@data` dropped.
#' @seealso [base::droplevels()]
#' @md
#' @export
#'
#' @examples
#'
#' chondro[1:3]$clusters
#' droplevels (chondro [1:3])$clusters
setMethod("droplevels", signature = "hyperSpec", definition = .droplevels)

.test(.droplevels) <- function() {
  context ("droplevels")
  
  test_that ("no change on object without levels to drop",{
    expect_equal(droplevels (chondro), chondro)
  })

  test_that ("dropping levels",{
    tmp <- droplevels (chondro [1:3])
    expect_equal(tmp@data, droplevels (chondro@data [1:3,]))
    
    expect_equal (tmp     [   , c ("x", "y", "filename", "spc")],
                  chondro [1:3, c ("x", "y", "filename", "spc")])
  
    expect_equal (tmp$clusters, factor (rep ("matrix", 3)))
  })

  test_that ("no change if factor is `except`ed",{
    expect_equal(droplevels (chondro, except = 4), chondro)
  })
}