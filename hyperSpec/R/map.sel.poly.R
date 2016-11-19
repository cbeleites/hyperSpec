##' Interactively select a polygon (grid graphics) and highlight points
##'
##' Click the points that should be connected as polygon. Input ends with right click (see
##' \code{\link[grid]{grid.locator}}). Polygon will be drawn closed. 
##' 
##' \code{map.sel.poly} is a convenience wrapper for \code{\link{plotmap}}, \code{sel.poly}, 
##' and \code{\link[sp]{point.in.polygon}}. For custiomized plotting, the plot can be produced by 
##' \code{\link{plotmap}}, \code{\link{plotvoronoi}} or \code{\link{levelplot}}, and the result of 
##' that plot command handed over to \code{map.sel.poly}, see the example below.
##' 
##' If even more customized plotting is required,\code{sel.poly} should be used (see example).
##' 
##' @param data hyperSpec object for plotting map or list returned by \code{\link{plotmap}}
##' @param pch symbol to display the points of the polygon for \code{\link{sel.poly}}
##' @param size size for polygon point symbol for \code{\link{sel.poly}}
##' @param ... further arguments for \code{\link[grid]{grid.points}} and
##' \code{\link[grid]{grid.lines}}
##' @return \code{map.sel.poly}: array of indices for points within the selected polygon
##' @author Claudia Beleites, Sebastian Mellor
##' @seealso \code{\link[grid]{grid.locator}}, \code{\link{map.identify}}
##' @export
##' @rdname map-sel-poly
##' @keywords iplot
##' @examples
##' if (interactive ()){
##' ## convenience wrapper
##' map.sel.poly (chondro)
##' 
##' ## customized version
##' data <- sample (chondro [,, 1004 - 2i ~ 1004 + 2i], 300)
##' 
##' plotdata <- plotvoronoi (data, clusters ~ y * x, col.regions = alois.palette ())
##' print (plotdata)
##' map.sel.poly (plotdata)
##' 
##' ## even more customization:
##' plotvoronoi (data)
##' 
##' ## interactively retrieve polygon
##' polygon <- sel.poly ()
##' 
##' ## find data points within polygon
##' require ("sp")     
##' i.sel <- which (point.in.polygon (data$x, data$y, polygon [, 1], polygon [, 2]) > 0)
##' 
##' ## work with selected points
##' grid.points (unit (data$x [i.sel], "native"), unit (data$y [i.sel], "native"))
##' }
map.sel.poly <- function (data, pch = 19, size = 0.3, ...){

	if (! interactive ())
		stop ("map.sel.poly works only on interactive graphics devices.")
	
	## sp is only in Suggests, not a strict Dependency. 
  if (! requireNamespace ("sp"))  
    stop ("package sp required for point.in.polygon ()")

  if (is (data, "hyperSpec")) { 
    ## plot hyperSpec object
    print (plotmap (data))
    x <- data$x
    y <- data$y
  } else if (is (data, "trellis")) { 
    
    ## data is list with plotting data of hyperSpec object
    x <- data$panel.args.common$x
    y <- data$panel.args.common$y
  } else {
    stop ("data must either be a hyperSpec object or a trellis object as returned by plotmap, plotvoronoi, or levelplot")
  }
  
  poly <- sel.poly (pch = pch, size = size, ...)
  
  pts <- sp::point.in.polygon (x, y, poly [, 1], poly [, 2]) 

  ind <- pts > 0

  if (! any (ind))
    warning ("Empty selection: no point in polygon.")

  ind
}



##' @return \code{sel.poly}: n x 2 matrix with the corner points of the polygon
##' @author Claudia Beleites
##' @seealso \code{\link[grid]{grid.locator}}
##' @export
##' @keywords iplot
##' @rdname map-sel-poly
##' @importFrom grid grid.lines grid.points
##' @importFrom utils tail
sel.poly <- function (pch = 19, size = 0.3, ...){
	if (! interactive ())
		stop ("sel.poly works only on interactive graphics devices.")
		
  trellis.focus () 
  
  pts <- matrix (NA, nrow = 0, ncol = 2)
  
  repeat {
    pt <- grid.locator (unit="native")
    if (!is.null (pt)){
      pts <- rbind (pts, as.numeric (pt)) # comparably few executions: low performance doesn't matter
      
      ## display the clicked point
      grid.points (unit (tail (pts [, 1], 1), "native"),
                   unit (tail (pts [, 2], 1), "native"), pch = pch,
                   size = unit (size, "char"), gp = gpar (...))
      
      ## connect last 2 points by line
      if (nrow (pts) > 1L)
        grid.lines (unit (tail (pts [, 1L], 2L) , "native"),
                    unit (tail (pts [, 2L], 2L) , "native"), gp = gpar (...))
    } else {
      ## visually close polygon (if at least 3 pts)
      if (nrow (pts) > 2L)
        grid.lines (unit (c (tail (pts [, 1L], 1L), pts [1L, 1L]), "native"),
                    unit (c (tail (pts [, 2L], 1L), pts [1L, 2L]), "native"), gp = gpar (...))
      break                            
    }
  }
  
  pts
}
