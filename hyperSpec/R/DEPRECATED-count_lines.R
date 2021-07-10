#' @name DEPRECATED-count_lines
#' @concept moved to hySpc.read.txt
#'
#' @title (DEPRECATED)
#'        Count lines (of an ASCII File)
#'
#' @description
#'
#' These data input related functions are **deprecated** and they will be
#' removed in the next release of \pkg{hyperspec} package.
#' Now functions in package \pkg{hySpc.read.txt}
#' ([link](https://r-hyperspec.github.io/hySpc.read.txt/reference/index.html))
#' should be used as the alternatives.
#'
#' @param file the file name or connection
#' @param chunksize `file` is read in chunks of `chunksize` lines.
#' @return number of lines in file
#' @export
#'
#' @author C. Beleites
count_lines <- function(file, chunksize = 1e4) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_txt(new = "count_lines")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nlines <- 0

  con <- file(file, open = "r")
  on.exit(close(con))

  while ((n <- length(readLines(con, n = chunksize))) > 0L) {
    nlines <- nlines + n
  }

  nlines
}

hySpc.testthat::test(count_lines) <- function() {
  context("count_lines")

  tmpfile <- tempfile()
  on.exit(unlink(tmpfile))

  writeLines("blabla\nblubb", con = tmpfile)

  expect_warning(
    res1 <- count_lines(tmpfile),
    "Function 'count_lines' is deprecated."
  )
  test_that(
    "file read in one chunk",
    expect_equal(res1, 2)
  )

  expect_warning(
    res2 <- count_lines(tmpfile, chunksize = 1L),
    "Function 'count_lines' is deprecated."
  )
  test_that(
    "file read in more chunks",
    expect_equal(res2, 2)
  )
}
