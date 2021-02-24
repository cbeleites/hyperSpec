#' Ensure That the Spectra Matrix Has the Wavelengths in Column Names
#'
#' @param spc `hyperSpec` object.
#'
#' @return `hyperSpec` object with wavelengths in column names of `$spc`.
#'
#' @concept manipulation
#'
#' @export
#'
.spc_fix_colnames <- function(spc) {
  colnames(spc@data$spc) <- signif(spc@wavelength, digits = 6)

  spc
}

hySpc.testthat::test(.spc_fix_colnames) <- function() {
  context(".spc_fix_colnames")

  test_that("colnames get fixed", {
    tmp <- flu
    colnames(tmp@data$spc) <- NULL

    tmp <- .spc_fix_colnames(tmp)
    expect_equal(colnames(tmp@data$spc), as.character(wl(tmp)))
  })
}
