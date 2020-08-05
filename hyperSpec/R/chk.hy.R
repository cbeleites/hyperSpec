#' @title Validation of `hyperSpec` objects.
#' @description
#' Check whether an object is a `hyperSpec` object and validate the object.
#'
#' @aliases validObject validObject,hyperSpec-method chk.hy
#' @author C. Beleites
#' @seealso [methods::validObject()]
#' @param object the object to check
#' @return `TRUE` if the check passes, otherwise stop with an error.
#' @export
#'
#' @keywords methods
#' @concept utils
#'
#' @examples
#' chk.hy(faux_cell)
#' validObject(faux_cell)
chk.hy <- function(object) {
  if (!is(object, "hyperSpec")) {
    stop("no hyperSpec object")
  }

  TRUE
}
