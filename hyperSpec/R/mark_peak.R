#' Mark peak
#'
#' Marks location of the *first* spectrum at the data point closest to the
#' specified position on the current plot.
#'
#' @param spc the `hyperSpec` object
#' @param xpos position of the peak(s) in current x-axis units
#' @param col color of the markers and text
#'
#'
#' @author R. Kiselev
#'
#' @concept plotting
#' @concept plotting tools
#'
#' @export
#' @examples
#' plot(faux_cell[7])
#' markpeak(faux_cell[7], 1662)
markpeak <- function(spc, xpos, col = "red") {
  chk.hy(spc)
  validObject(spc)

  plot(spc[1, , xpos], add = T, lines.args = list(type = "p"), col = col)
  text(
    x = xpos, y = spc[[1, , xpos]], col = col, labels = sprintf("<- %.1f", xpos),
    adj = c(-0.2, 0.37), srt = 90, cex = 0.75
  )
}
