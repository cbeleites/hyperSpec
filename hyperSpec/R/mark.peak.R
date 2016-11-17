##' Mark peak
##'   
##' Marks location of the \emph{first} spectrum at the data point closes to the
##' specified position on the current plot.
##'   
##' @param spc the \code{hyperSpec} object
##' @param xpos position of the peak(s) in current x-axis units
##' @param col color of the markers and text
##'   
##'   
##' @author R. Kiselev
##' @export
##' @examples 
##' plot (chondro [7])
##' markpeak (chondro [7], 1662)
markpeak <- function(spc, xpos, col="red"){

  chk.hy (spc)
  validObject (spc)

  plot(spc[1,,xpos], add=T, lines.args=list(type="p"), col=col)
  text(x=xpos, y=spc[[1,,xpos]], col=col, labels=sprintf("<- %.1f", xpos),
       adj=c(-0.2,0.37), srt=90, cex=0.75)
}
