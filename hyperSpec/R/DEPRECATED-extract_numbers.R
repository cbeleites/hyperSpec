#' @name DEPRECATED-guess.wavlength
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Guess wavelengths from character vector
#'
#'
#' @description
#'
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead.
#'
#' `_____________`
#'
#' Character vectors used for names (e.g. colnames for matrices or data.frames)
#' are often treated by [base::make.names()] or similar functions that
#' produce suitable names (e.g. by pre-pending "X" to numbers). Such names
#' cannot be directly converted to numeric.
#'
#' `guess.wavlength()` tries to extract numbers from X which may be
#' surrounded by such "protecting" characters.
#'
#' @param X character with numbers hidden inside
#'
#' @return numeric
#'
#' @export
#'
#' @seealso
#' [`readr::parse_number()`](https://readr.tidyverse.org/reference/parse_number.html)
#'
#' @examples
#' tmp <- data.frame(flu[[, , 400 ~ 410]])
#' (wl <- colnames(tmp))
#' guess.wavelength(wl)
#' @include extract_numbers.R
guess.wavelength <- function(X) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("extract_numbers")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  extract_numbers(X)
}

#' @include constants-regexps.R
#' @include hy_options.R
hySpc.testthat::test(guess.wavelength) <- function() {
  context("guess.wavelength")

  test_that("deprecated", {
    expect_warning(
      guess.wavelength(1:5),
      "Function 'guess.wavelength' is deprecated."
    )
  })
}
