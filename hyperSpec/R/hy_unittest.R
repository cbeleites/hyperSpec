#' Helper function for unit testing
#'
#' @export

skip_if_not_fileio_available <- function() {
  testthat::skip_if_not(
    file.exists("fileio"),
    message = "file import test files not installed"
  )
}
