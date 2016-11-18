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
##' @importFrom testthat SummaryReporter ListReporter MultiReporter get_reporter
##' @include hyperSpec-package.R
##' @export
##' @examples
##' 
##' hy.unittest ()
##' 
hy.unittest <- function (){
  if (! requireNamespace ("svUnit", quietly = TRUE)){
    warning ("svUnit required to run the unit tests.")
    return (NA)
  }

  tests <- unlist (eapply (env = getNamespace ("hyperSpec"), FUN = svUnit::is.test, all.names = TRUE))
  tests <- names (tests [tests])
  tests <- sapply (tests, get, envir = getNamespace ("hyperSpec"))

  clearLog ()
  warnlevel <- options()$warn
  options (warn = 0)
  for (t in seq_along (tests))
  	runTest (tests [[t]], names (tests) [t])
  options (warn = warnlevel)

  if (interactive ())
    print (stats (Log()))
  else
    print (stats (Log ())[,c ("kind", "msg")])

  errorLog (summarize = FALSE)
  
  ## from here on: testthat unit tests
  
  if (!requireNamespace("testthat", quietly=TRUE)) {
    warning("testthat required to run the unit tests.")
    return(NA)
  }
  
  tests <- eapply(env = getNamespace ("unmixR"), FUN = get.test, all.names=TRUE)
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
} 

##' get test that is attached to object as "test" attribute
##' @noRd
get.test <- function (object)
  attr (object, "test")

