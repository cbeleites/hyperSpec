##' command line completion for $
##'
##' @aliases .DollarNames .DollarNames,hyperSpec-method
##' @author C. Beleites
##' @seealso \code{\link[utils]{.DollarNames}}
##' @export
##' @rdname dollarnames
##' @keywords utilities
##' @title command line completion for $
##' @param x the hyperSpecobject
##' @param pattern pattern to look for
##' @return the name of the extra data slot
##' @importFrom utils .DollarNames
.DollarNames.hyperSpec <- function (x, pattern = "")
  grep (pattern, colnames (x@data), value = TRUE)

.test (.DollarNames.hyperSpec) <- function(){
  context (".DollarNames")

  test_that("expansion on missing pattern", {
    expect_equal(.DollarNames (flu), colnames (flu))
  })

  test_that("expansion on missing pattern", {
    expect_equal(.DollarNames (flu, "f"), "filename")
    expect_equal(.DollarNames (flu, "c"), c ("spc", "c"))
  })

}
