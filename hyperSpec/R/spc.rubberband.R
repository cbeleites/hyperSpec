##' Rubberband baseline
##'
##' Baseline with support points determined from a convex hull of the spectrum.
##' 
##' Use \code{debuglevel >= 1} to obtain debug plots, either directly via function argument or by setting hyperSpec's \code{debuglevel} option.
##' @title Rubberband baseline correction
##' @param spc hyperSpec object
##' @param ... further parameters handed to \code{\link[stats]{smooth.spline}}
##' @param upper logical indicating whether the lower or upper part of the hull should be used
##' @param noise noise level to be taken into account
##' @param spline logical indicating whether the baseline should be an interpolating spline through
##' the support points or piecewise linear.
##' @return hyperSpec object containing the baselines
##' @rdname spc-rubberband
##' @author Claudia Beleites
##' @seealso \code{\link[hyperSpec]{spc.fit.poly}}, \code{\link[hyperSpec]{spc.fit.poly.below}}
##' 
##' \code{vignette ("baseline")}
##' 
##' \code{\link[hyperSpec]{hy.setOptions}}
##'
##' @note This function is still experimental
##' @export
##' @examples
##' plot (paracetamol [,, 175 ~ 1800])
##' bl <- spc.rubberband (paracetamol [,, 175 ~ 1800], noise = 300, df = 20)
##' plot (bl, add = TRUE, col = 2)
##' 
##' plot (paracetamol [,, 175 ~ 1800] - bl)

spc.rubberband <- function (spc, ..., upper = FALSE, noise = 0, spline = TRUE){
  spc <- orderwl (spc)

  if (upper) spc@data$spc <- -spc@data$spc
  
  spc@data$spc <- .rubberband (spc@wavelength, spc@data$spc, 
                               noise = noise, spline = spline, ...)

  if (upper) spc@data$spc <- -spc@data$spc

  spc
}

##' @importFrom grDevices chull
.rubberband <- function (x, y, noise, spline, ..., debuglevel = hy.getOption ("debuglevel")){
  for (s in seq_len (nrow (y))){
    pts <- chull (x, y [s,])
    
    if (debuglevel >= 1L){
    	plot (x, y [s, ], type = "l")
    	points (x [pts], y [s, pts], pch = 1, col = matlab.dark.palette (length (pts)))
    }

    ## `chull` returns points in cw order
    ## => points between ncol (y) and 1 are lower part of hull
    imax <- which (pts == ncol (y)) - 1
    
    ## if necessary, rotate pts so that ncol (y) is at position 1
    if (imax > 0L)
    	pts <- c (pts [- seq_len (imax)], pts [seq_len (imax)])

    ## now keep only pts until column index 1
    pts <- pts [1 : which (pts == 1)]
    
    ## check whether first and last point are minima, 
    ## if not remove them.
    ## If they are minima, points 2 and ncol (y) - 1 do not appear
    ## last point: 
    if (pts [2] == pts [1] - 1) pts <- pts [-1]
    
    ## now sort ascending (anyways needed later on)
    pts <- rev (pts)
    
    ## fist point:
    if (pts [2] == 2) pts <- pts [-1]
    
    if (debuglevel >= 1L){
    	points (x [pts], y [s, pts], pch = 19, col = matlab.dark.palette (length (pts)), cex = 0.7)
    }

    tmp <- approx (x = x [pts], y = y [s, pts], xout= x, method="linear")$y
    
    if (spline){
      pts <- which (y [s,] <= tmp + noise)

      if (length (pts) > 3)
        tmp <- predict (smooth.spline (x[pts], y[s, pts], ...)$fit, x, 0)$y 
      else 
        tmp <- spline (x [pts], y [s, pts], xout = x)$y
        
    }
    
    y [s, ] <- tmp
    
  }
  
  y  
}
