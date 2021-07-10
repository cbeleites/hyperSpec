#' Color coding legend for factors
#'
#' Modifies a list of lattice arguments (as for [lattice::levelplot()], etc.) according to
#' the factor levels. The colorkey will shows all levels (including unused), and the drawing colors
#' will be set accordingly.
#'
#' `trellis.factor.key` is used during `levelplot`-based plotting of factors (for
#' hyperSpec objects) unless `transform.factor = FALSE` is specified.
#'
#' @param f the factor that will be color-coded
#' @param levelplot.args a list with levelplot arguments
#' @return the modified list with levelplot arguments.
#' @author C. Beleites
#' @seealso [lattice::levelplot()]
#'
#' @keywords aplot
#' @concept plotting
#' @concept plotting tools
#'
#' @export
#' @importFrom lattice level.colors
#' @examples
#'
#' faux_cell$z <- factor(rep(c("a", "a", "d", "c"),
#'   length.out = nrow(faux_cell)
#' ),
#' levels = letters[1:4]
#' )
#'
#' str(trellis.factor.key(faux_cell$z))
#'
#' plotmap(faux_cell, z ~ x * y)
#'
#' ## switch off using trellis.factor.key:
#' ## note that the factor levels are collapsed to c(1, 2, 3) rather than
#' ## c (1, 3, 4)
#' plotmap(faux_cell, z ~ x * y, transform.factor = FALSE)
#'
#' plotmap(faux_cell, z ~ x * y,
#'   col.regions = c("gray", "red", "blue", "dark green")
#' )
#' @importFrom utils modifyList
trellis.factor.key <- function(f, levelplot.args = list()) {
  at <- seq(0, nlevels(f)) + .5

  if (is.null(levelplot.args$col.regions)) {
    cols <- level.colors(seq_along(levels(f)), at)
  } else {
    cols <- level.colors(seq_along(levels(f)), at, levelplot.args$col.regions)
  }

  modifyList(
    list(
      at = at,
      col.regions = cols,
      colorkey = list(lab = list(
        at = seq_along(levels(f)),
        lab = levels(f)
      ))
    ),
    levelplot.args
  )
}
