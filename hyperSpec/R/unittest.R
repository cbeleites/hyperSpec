##' hyperSpec unit tests
##'
##' If \code{\link[testthat]{testthat}} is available, run the unit tests and
##' display the results.
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
hy.unittest <- function (){
  if (!requireNamespace("testthat", quietly=TRUE)) {
    warning("testthat required to run the unit tests.")
    return(NA)
  }
  if (! "package:testthat" %in% search ())
    attachNamespace("testthat")

  tests <- eapply(env = getNamespace ("hyperSpec"), FUN = get.test, all.names=TRUE)
  tests <- tests [! sapply (tests, is.null)]

  reporter <- SummaryReporter$new()
  lister <- ListReporter$new()
  reporter <- MultiReporter$new(reporters = list(reporter, lister))

  with_reporter(reporter = reporter, start_end_reporter = TRUE, {
    for (t in seq_along(tests)){
      lister$start_file(names (tests [t]))
      tests [[t]] ()
    }
    get_reporter()$.end_context()
  })

  invisible(lister$get_results())
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

