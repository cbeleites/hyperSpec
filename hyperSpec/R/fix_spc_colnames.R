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
.fix_spc_colnames <- function(spc) {
  colnames(spc@data$spc) <- signif(spc@wavelength, digits = 6)

  spc
}

hySpc.testthat::test(.fix_spc_colnames) <- function() {
  context(".fix_spc_colnames")

  test_that("colnames get fixed", {
    tmp <- flu
    colnames(tmp@data$spc) <- NULL

    tmp <- .fix_spc_colnames(tmp)
    expect_equal(colnames(tmp@data$spc), as.character(wl(tmp)))
  })
}
