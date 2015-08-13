##' @title Mark peak located at the specific position on the current plot
##'
##' @param spc the \code{hyperSpec} object
##' @param xpos position of the peak(s) in current x-axis units
##' @param col color of the markers and text
##'
##'
##' @author R. Kiselev
##' @export
markpeak <- function(spc, xpos, col="red"){
  # TODO: use points() instead of plot
  plot(spc[1,,xpos], add=T, lines.args=list(type="p"), col=col)
  text(x=xpos, y=spc[[1,,xpos]], col=col, labels=sprintf("\u2190 %.2f", xpos),
       adj=c(-0.2,0.37), srt=90, cex=0.75)
}
