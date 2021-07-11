
#' List available *R hyperSpec* family packages
#'
#' @description
#' Get the names of *R hyperSpec* family packages that are available on GitHub
#' organization [**`r-hyperspec`**](https://r-hyperspec.github.io/)
#' (the official homepage of these packages).
#'
#' This function requires Internet connection.
#'
#' @details
#' If your machine is connected to the Internet and you receive issues due to
#' overused limit of anonymous connections to GitHub, you may run alternative
#' code that employs GitHub PAT (personal access token) for authentication.
#' On how to set your GitHub PAT, see sections "Create a PAT" and
#' "Add your PAT to `.Renviron`" in this book:
#' [happygitwithr.com/github-pat.html](https://happygitwithr.com/github-pat.html#step-by-step).
#'
#' ```r
#' # install.pacakges("gh")
#' gh_api_response <- gh::gh("GET /users/r-hyperspec/repos?per_page=100")
#' repo_names <- vapply(gh_api_response, "[[", "", "name")
#' package_names <- grep("^hyperSpec|^hySpc[.]", repo_names, value = TRUE)
#' package_names
#' ```
#'
#' @return Character vector with the names of the pacakges.
#' @export
#'
#' @concept utils
#'
#' @author V. Gegzna
#'
#' @examples
#' \dontrun{\donttest{
#' hy_list_available_hySpc_packages()
#' }}


hy_list_available_hySpc_packages <- function() {

  # TODO: Check if there is an access to the Internet. In case of no access,
  #       a more user-friendly error message should be given.

  gh_api_response <- readLines(
    "https://api.github.com/orgs/r-hyperspec/repos?per_page=100",
    warn = FALSE
  )
  one_line_per_repo <- strsplit(gh_api_response, "}}")[[1]]
  pattern <- '(?<="name":")(hyperSpec|hySpc[.].*?)(?=",)'
  matches <- regexpr(pattern = pattern, text = one_line_per_repo, perl = TRUE)
  package_names <- regmatches(one_line_per_repo, m = matches)
  package_names
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(hy_list_available_hySpc_packages) <- function() {
  context("hy_list_available_hySpc_packages")

  test_that("hy_list_available_hySpc_packages() works", {
    testthat::skip_if_offline()

    # FIXME: The lines below should be fixed in the future
    # Skip on GihHub Actions (as it usually fails to connet to GH on macOS):
    testthat::skip_on_ci()

    expect_silent(pkgs <- hy_list_available_hySpc_packages())
    expect_is(pkgs, "character")
    expect_true(length(pkgs) > 5)
    expect_true(all(grepl("^hySpc[.]|^hyperSpec$", pkgs)))
  })
}
