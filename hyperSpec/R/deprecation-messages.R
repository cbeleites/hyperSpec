
#' @param new Name of replacement function.
#' @param package Name of the package the function is moved to.
#' @param url Homepage of the package.
#' @param old Function that is replaced
#'
#' @keywords internal
#' @noRd

hySpc_deprecated <- function(new = "", package = NULL, url = NULL,
                             old = as.character(sys.call(sys.parent()))[1L]) {
  if (!is.null(package)) {
    fun_msg <-
      if (new == "") {
        paste0("Please, find alternatives in package '", package, "'")
      } else {
        paste0("Use function '", new, "' from package '", package, "' instead.")
      }

    url_msg <-
      if (is.null(url)) {
        paste0("\n\n", "https://r-hyperspec.github.io/", package, "\n")
      } else if (url == "") {
        ""
      } else {
        paste0("\n\n", url, "\n")
      }
  } else {
    fun_msg <-
      if (new == "") {
        ""
      } else {
        paste0("Use function '", new, "' instead.")
      }

    url_msg <- ""
  }

  msg <- paste0("Function '", old, "' is deprecated. \n", fun_msg, url_msg)

  .Deprecated(msg = msg)
}

deprecated_ggplot2 <- function(new = "", old = as.character(sys.call(sys.parent()))[1L]) {
  hySpc_deprecated(new = new, package = "hySpc.ggplot2", old = old)
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(deprecated_ggplot2) <- function() {
  context("Deprecation messages")

  test_that("Deprecation message is a warning", {
    expect_warning(hySpc_deprecated(), "is deprecated")
    expect_warning(deprecated_ggplot2(), "is deprecated")
    expect_warning(deprecated_ggplot2(), "ggplot2")
    expect_warning(deprecated_ggplot2("a"), "ggplot2")
  })
}
