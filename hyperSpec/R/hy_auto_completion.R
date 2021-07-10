#' @title Command line completion for `$`
#' @description
#' Command line completion for `$`.
#' This function is not intended to be used directly by users but provides
#' auto-completion capabilities.
#'
#' @aliases .DollarNames .DollarNames,hyperSpec-method
#' @author C. Beleites
#' @seealso [utils::.DollarNames()]
#'
#' @export
#'
#' @keywords utilities
#' @concept utils
#'
#'
#' @rdname dollarnames
#' @param x the `hyperSpec` object
#' @param pattern pattern to look for
#' @return the name of the extra data slot
#' @importFrom utils .DollarNames
.DollarNames.hyperSpec <- function(x, pattern = "") {
  grep(pattern, colnames(x@data), value = TRUE)
}

hySpc.testthat::test(.DollarNames.hyperSpec) <- function() {
  context(".DollarNames")

  test_that("expansion on missing pattern", {
    expect_equal(.DollarNames(flu), colnames(flu))
  })

  test_that("expansion on missing pattern", {
    expect_equal(.DollarNames(flu, "f"), "filename")
    expect_equal(.DollarNames(flu, "c"), c("spc", "c"))
  })
}
