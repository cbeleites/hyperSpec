#' @concept io
read.spc.Shimadzu <- function(filename) {
  stop("Import of Shimadzu SPC file format (OLE CF) is not yet implemented.")
}

hySpc.testthat::test(read.spc.Shimadzu) <- function() {
  context("read.spc.Shimadzu")

  test_that("not implemented error", {
    expect_error(read.spc.Shimadzu())
  })
}
