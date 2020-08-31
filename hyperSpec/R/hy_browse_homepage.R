
#' Browse homepage of *R hyperSpec*
#'
#' Browse homepage of [**R hyperSpec**](https://r-hyperspec.github.io/)
#' family packages.
#'
#' @importFrom utils browseURL
#' @export
#'
#' @concept utils
#'
#' @examples
#' \dontrun{\donttest{
#' hy_browse_homepage()
#' }}

hy_browse_homepage <- function() {
  browseURL("https://r-hyperspec.github.io/")
}
