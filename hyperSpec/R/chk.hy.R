#' Check whether an object is a hyperSpec object and validate the object
#'
#' @title Validation of hyperSpec objects
#' @aliases validObject validObject,hyperSpec-method chk.hy
#' @author C. Beleites
#' @seealso \code{\link[methods]{validObject}}
#' @param object the object to check
#' @return `TRUE` if the check passes, otherwise stop with an
#' error.
#' @keywords methods
#' @export
#' @examples
#' chk.hy(faux_cell)
#' validObject(faux_cell)
chk.hy <- function(object) {
  if (!is(object, "hyperSpec")) {
    stop("no hyperSpec object")
  }

  TRUE
}
