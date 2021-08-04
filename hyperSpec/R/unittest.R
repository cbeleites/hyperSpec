##' hyperSpec unit tests
##'
##' If \code{\link[testthat]{testthat}} is available, run the unit tests and
##' display the results.
##'
##' @param standalone run the unit test on their own, e.g. from the console
##'   (`TRUE`) or within testthat tests (`FALSE`), e.g. via `devtools::test()`
##' @param reporter the reporter to use, defaults to [testthat::ProgressReporter]
##'
##' @rdname unittests
##' @return Invisibly returns a data frame with the test results
##'
##' @author Claudia Beleites
##'
##' @keywords programming utilities
##' @import testthat
##' @export
##' @examples
##'
##' hy.unittest ()
##'
hy.unittest <- function (standalone = TRUE, reporter = "progress"){
  if (!requireNamespace("testthat", quietly=TRUE)) {
    warning("testthat required to run the unit tests.")
    return(NA)
  }
  if (! "package:testthat" %in% search ())
    attachNamespace("testthat")

  tests <- eapply(env = getNamespace ("hyperSpec"), FUN = get.test, all.names=TRUE)
  tests <- tests [! sapply (tests, is.null)]

  if (standalone) {
    with_reporter(reporter = reporter, start_end_reporter = TRUE,
                  for (t in tests) t())
  } else {
    for (t in tests) t()
  }
}

##' @noRd
{
  `.test<-` <- function (f, value) {
    attr (f, "test") <- value
    f
  }

  skip_if_not_fileio_available <- function () {
    skip_if_not (file.exists("fileio"), message = "file import test files not installed")
  }
}

##' get test that is attached to object as "test" attribute
##' @noRd
get.test <- function (object)
  attr (object, "test")

