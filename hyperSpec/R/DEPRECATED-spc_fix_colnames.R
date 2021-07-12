#' @name DEPRECATED-fix_spc_colnames
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Ensure that the spectra matrix has the wavelengths in column names
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

  test_that(
    "deprecated",
    expect_warning(
      .fix_spc_colnames(flu),
      "Function '.fix_spc_colnames' is deprecated."
    )
  )

  test_that("colnames get fixed", {
    tmp <- flu
    colnames(tmp@data$spc) <- NULL

    tmp <- suppressWarnings(.fix_spc_colnames(tmp))
    expect_equal(colnames(tmp@data$spc), as.character(wl(tmp)))
  })
}
