#' Interactively select a polygon (grid graphics) and highlight points
#'
#' Click the points that should be connected as polygon. Input ends with right click (see
#' [grid::grid.locator()]). Polygon will be drawn closed.
#'
#' `map.sel.poly` is a convenience wrapper for [plotmap()], `sel.poly`,
#' and [sp::point.in.polygon()]. For custiomized plotting, the plot can be produced by
#' [plotmap()], [plotvoronoi()] or [levelplot()], and the result of
#' that plot command handed over to `map.sel.poly`, see the example below.
#'
#' If even more customized plotting is required,`sel.poly` should be used (see example).
#'
#' @param data hyperSpec object for plotting map or list returned by [plotmap()]
#' @param pch symbol to display the points of the polygon for [sel.poly()]
#' @param size size for polygon point symbol for [sel.poly()]
#' @param ... further arguments for [grid::grid.points()] and
#' [grid::grid.lines()]
#' @return `map.sel.poly`: array of indices for points within the selected polygon
#' @author Claudia Beleites, Sebastian Mellor
#' @seealso [grid::grid.locator()], [map.identify()]
#' @export
#' @rdname map-sel-poly
#'
#' @keywords iplot
#' @concept plotting
#' @concept plotting tools
#'
#' @examples
#' \dontrun{\donttest{
#'   ## convenience wrapper
#'   map.sel.poly(faux_cell)
#'
#'   ## customized version
#'   data <- sample(faux_cell [, , 1004 - 2i ~ 1004 + 2i], 300)
#'
#'   plotdata <- plotvoronoi(data, region ~ y * x, col.regions = palette_alois())
#'   print(plotdata)
#'   map.sel.poly(plotdata)
#'
#'   ## even more customization:
#'   plotvoronoi(data)
#'
#'   ## interactively retrieve polygon
#'   polygon <- sel.poly()
#'
#'   ## find data points within polygon
#'   require("sp")
#'   i.sel <- which(point.in.polygon(data$x, data$y, polygon [, 1], polygon [, 2]) > 0)
#'
#'   ## work with selected points
#'   grid.points(unit(data$x [i.sel], "native"), unit(data$y [i.sel], "native"))
#' }}
map.sel.poly <- function(data, pch = 19, size = 0.3, ...) {
  if (!interactive()) {
    stop("map.sel.poly works only on interactive graphics devices.")
  }

  ## sp is only in Suggests, not a strict Dependency.
  if (!requireNamespace("sp")) {
    stop("package sp required for point.in.polygon ()")
  }

  if (is(data, "hyperSpec")) {
    ## plot hyperSpec object
    print(plotmap(data))
    x <- data$x
    y <- data$y
  } else if (is(data, "trellis")) {

    ## data is list with plotting data of hyperSpec object
    x <- data$panel.args.common$x
    y <- data$panel.args.common$y
  } else {
    stop("data must either be a hyperSpec object or a trellis object as returned by plotmap, plotvoronoi, or levelplot")
  }

  poly <- sel.poly(pch = pch, size = size, ...)

  pts <- sp::point.in.polygon(x, y, poly[, 1], poly[, 2])

  ind <- pts > 0

  if (!any(ind)) {
    warning("Empty selection: no point in polygon.")
  }

  ind
}



#' @return `sel.poly`: n x 2 matrix with the corner points of the polygon
#' @author Claudia Beleites
#' @seealso [grid::grid.locator()]
#' @export
#'
#' @keywords iplot
#' @concept plotting
#' @concept plotting tools
#'
#' @rdname map-sel-poly
#' @importFrom grid grid.lines grid.points
#' @importFrom utils tail
sel.poly <- function(pch = 19, size = 0.3, ...) {
  if (!interactive()) {
    stop("sel.poly works only on interactive graphics devices.")
  }

  trellis.focus()

  pts <- matrix(NA, nrow = 0, ncol = 2)

  repeat {
    pt <- grid.locator(unit = "native")
    if (!is.null(pt)) {
      pts <- rbind(pts, as.numeric(pt)) # comparably few executions: low performance doesn't matter

      ## display the clicked point
      grid.points(unit(tail(pts[, 1], 1), "native"),
        unit(tail(pts[, 2], 1), "native"),
        pch = pch,
        size = unit(size, "char"), gp = gpar(...)
      )

      ## connect last 2 points by line
      if (nrow(pts) > 1L) {
        grid.lines(unit(tail(pts[, 1L], 2L), "native"),
          unit(tail(pts[, 2L], 2L), "native"),
          gp = gpar(...)
        )
      }
    } else {
      ## visually close polygon (if at least 3 pts)
      if (nrow(pts) > 2L) {
        grid.lines(unit(c(tail(pts[, 1L], 1L), pts[1L, 1L]), "native"),
          unit(c(tail(pts[, 2L], 1L), pts[1L, 2L]), "native"),
          gp = gpar(...)
        )
      }
      break
    }
  }

  pts
}
