#' @name DEPRECATED-wc
#' @concept moved to hySpc.read.txt
#'
#' @title (DEPRECATED)
#'        Line/word/character count of ASCII files
#'
#' @description
#'
#' These data input related functions are **deprecated** and they will be
#' removed in the next release of \pkg{hyperspec} package.
#' Now functions in package \pkg{hySpc.read.txt}
#' ([link](https://r-hyperspec.github.io/hySpc.read.txt/reference/index.html))
#' should be used as the alternatives.
#'
#'
#' @details
#' `wc()` now is deprecated and will be removed from \pkg{hyperSpec} and
#' \pkg{hySpc.read.txt} in the future.
#' Consider using [count_lines()] instead for line counting.
#'
#' `wc()` uses the system command `wc`. **Use at your own risk.**
#'
#' @param file the file name or pattern
#' @param flags the parameters to count, character vector with the long form
#'   of the parameters
#' @return data.frame with the counts and file names, or `NULL` if wc is
#'   not available on the system.
#' @seealso [count_lines()]
#'
#' @export
#'
#' @author C. Beleites
#' @importFrom utils read.table
wc <- function(file, flags = c("lines", "words", "bytes")) {
  # .Deprecated(
  #   new = "count_lines",
  #   msg = "wc() is soft-deprecated: while it will not be removed in the near future, use it at your own risk. The functionality is not routinely tested."
  # )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt(new = "count_lines")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output <- try(system2("wc", args = "--help", stdout = TRUE, stderr = TRUE), silent = TRUE)
  if (class(output) == "try-error") {
    return(NULL)
  }

  output <- paste("wc", paste("--", flags, sep = "", collapse = " "), file)
  output <- read.table(pipe(output))
  colnames(output) <- c(flags, "file")

  output
}

hySpc.testthat::test(wc) <- function() {
  context("wc")

  tmpfile <- tempfile()
  on.exit(unlink(tmpfile))
  writeLines("blabla\nblubb", con = tmpfile)

  test_that("wc defaults", {
    skip_if_not_fileio_available() # see issue #97

    suppressWarnings(res <- wc(tmpfile))

    if (is.null(res)) {
      skip("wc not available")
    }

    if (.Platform$OS.type == "windows") {
      expect_equal(res, data.frame(
        lines = 2,
        words = 2,
        bytes = 15, ## additional CR bytes on windows.
        file = tmpfile
      ))
    } else {
      expect_equal(res, data.frame(
        lines = 2,
        words = 2,
        bytes = 13,
        file = tmpfile
      ))
    }
  })



  test_that("wc --lines", {
    skip_if_not_fileio_available() # see issue #97

    suppressWarnings(res <- wc(tmpfile, flags = "lines"))
    if (is.null(res)) {
      skip("wc not available")
    }

    expect_equal(res, data.frame(lines = 2, file = tmpfile))
  })
}
