#' @name DEPRECATED-fix_spc_colnames
#' @concept deprecated
#'
#' @title (DEPRECATED) Ensure That the Spectra Matrix Has the Wavelengths in Column Names
#'
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead.
#'
#' `_____________`
#'
#' @param spc `hyperSpec` object.
#'
#' @return `hyperSpec` object with wavelengths in column names of `$spc`.
#'
#' @export
#'
.fix_spc_colnames <- function(spc) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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