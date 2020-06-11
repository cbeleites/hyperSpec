#' Ensure that the spectra matrix has the wavelengths in column names
#'
#' @param spc hyperSpec object
#'
#' @return hyperSpec object with wavelengths in column names of `$spc`
#' @md
#' @export
.fix_spc_colnames <- function(spc) {
  colnames(spc@data$spc) <- signif(spc@wavelength, digits = 6)

  spc
}

.test(.fix_spc_colnames) <- function() {
  context(".fix_spc_colnames")

  test_that("colnames get fixed", {
    tmp <- flu
    colnames(tmp@data$spc) <- NULL

    tmp <- .fix_spc_colnames(tmp)
    expect_equal(colnames(tmp@data$spc), as.character(wl(tmp)))
  })
}
