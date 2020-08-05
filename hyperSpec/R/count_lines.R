#' Count lines (of an ASCII file).
#'
#' @param file the file name or connection
#' @param chunksize `file` is read in chunks of `chunksize` lines.
#' @return number of lines in file
#' @export
#' @concept io
#'
#' @author C. Beleites
count_lines <- function(file, chunksize = 1e4) {
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

  test_that(
    "file read in one chunk",
    expect_equal(count_lines(tmpfile), 2)
  )

  test_that(
    "file read in more chunks",
    expect_equal(count_lines(tmpfile, chunksize = 1L), 2)
  )
}
