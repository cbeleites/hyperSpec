#' Guess wavelengths from character vector
#'
#' character vectors used for names (e.g. colnames for matrices or data.frames)
#' are often treated by [base::make.names()] or similar functions that
#' produce suitable names (e.g. by prepending "X" to numbers). Such names
#' cannot be directly converted to numeric.
#'
#' `extract_numbers()` tries to extract numbers from X which may be
#' surrounded by such "protecting" characters.
#'
#' @param X character with numbers hidden inside
#'
#' @return numeric
#'
#' @export
#'
#' @concept wavelengths
#' @seealso
#' [`readr::parse_number()`](https://readr.tidyverse.org/reference/parse_number.html)
#'
#' @examples
#' tmp <- data.frame(flu[[, , 400 ~ 410]])
#' (wl <- colnames(tmp))
#' extract_numbers(wl)
extract_numbers <- function(X) {
  wl <- regmatches(X, regexpr(.PATTERN.number, X))
  wl <- as.numeric(wl)

  if (is.null(wl) || length(wl) == 0L || any(is.na(wl))) {
    if (hy.getOption("debuglevel") >= 1L) {
      message("could not guess wavelengths")
    }
    wl <- NULL
  }

  wl
}

#' @include constants-regexps.R
#' @include hy_options.R
hySpc.testthat::test(extract_numbers) <- function() {
  context("extract_numbers")

  test_that("simple test", {
    expect_equal(extract_numbers(1:5), 1:5)
  })

  test_that("wavelengths containing characters", {
    wl <- seq(600, 602, length.out = 11)
    expect_equal(extract_numbers(make.names(wl)), wl)
  })

  test_that("return NULL if could not guess wavelenths", {
    expect_equal(extract_numbers(colnames(matrix(1:12, 3))), NULL)
    expect_equal(extract_numbers(letters[1:4]), NULL)
  })
}
