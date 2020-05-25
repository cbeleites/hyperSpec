##' line/word/character count of ASCII files
##'
##' `wc()` uses the system command `wc`. Use at your own risk.
##' @note `wc()` now is deprecated and will be removed from hyperSpec in future. Consider using [count_lines()] instead for line counting.
##'
##' @param file the file name or pattern
##' @param flags the parameters to count, character vector with the long form
##'   of the parameters
##' @return data.frame with the counts and file names, or `NULL` if wc is
##'   not available on the system.
##' @seealso [count_lines()]
##' @export
##' @author C. Beleites
##' @importFrom utils read.table
wc <- function(file, flags = c("lines", "words", "bytes")) {
  .Deprecated(
    new = "count_lines",
    msg = "wc() is soft-deprecated: while it will not be removed in the near future, use it at your own risk. The functionality is not routinely tested."
  )

  output <- try(system2("wc", args = "--help", stdout = TRUE, stderr = TRUE), silent = TRUE)
  if (class(output) == "try-error") {
    return(NULL)
  }

  output <- paste("wc", paste("--", flags, sep = "", collapse = " "), file)
  output <- read.table(pipe(output))
  colnames(output) <- c(flags, "file")

  output
}

.test(wc) <- function() {
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
