##' wc
##' word count of ASCII files
##'
##' wc uses the system command wc
##'
##' @param file the file name or pattern
##' @param flags the parameters to count, character vector with the long form
##'   of the parameters
##' @return data.frame with the counts and file names, or \code{NULL} if wc is
##'   not available
##' @export
##' @author C. Beleites
##' @importFrom utils read.table
wc <- function (file, flags = c("lines", "words", "bytes")){
  if (length (system ("wc --help", intern = TRUE)) == 0)
    return (NULL)

  wc <- paste ("wc", paste ("--", flags, sep = "", collapse = " "), file)
  wc <- read.table(pipe (wc))
  colnames (wc) <- c(flags, "file")
  wc
}

.test (wc) <- function (){
  context ("wc")

  tmpfile <- tempfile()
  on.exit (unlink (tmpfile))

  writeLines("blabla\nblubb", con = tmpfile)

  res <- wc (tmpfile)

  ## wc does not exist on all systems
  if (is.null (res)) skip ("wc not available")

  test_that("wc defaults",
    expect_equal(res,  data.frame (lines = 2, words = 2, bytes = 13, file = tmpfile))
  )

  test_that("wc --lines",
            expect_equal(wc (file = tmpfile, flags = "lines"),  data.frame (lines = 2, file = tmpfile))
  )

}
