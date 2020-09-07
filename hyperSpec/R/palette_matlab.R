#' @rdname palettes
#' @aliases matlab.palette colors,matlab.palette
#'
#' @title Matlab-Like Color Palettes
#'
#' @description
#' Two palettes going from blue over green to red, approximately as the
#' standard palette of Matlab does. The second one has darker green values and
#' is better suited for plotting lines on white background.
#'
#'
#' @param n the number of colors to be in the palette.
#' @param ... further arguments are handed to [grDevices::rainbow()]
#'        (`alois.palette`: [grDevices::colorRampPalette()]).
#'
#' @return A vector containing the color values in the form `"#rrbbggaa"`.
#'
#' @seealso [grDevices::rainbow()]
#'
#' @author C. Beleites and A. Bonifacio
#'
#' @keywords color
#' @concept plotting
#' @concept color palette
#'
#' @importFrom grDevices rainbow
#' @export
#'
#' @examples
#' # Matlab dark palette
#'
#' op <- par(mar = c(0, 0, 0, 0))
#' pie(rep(1, 12), col = matlab.palette(12))
#' par(op)
#'
#' plot(flu, col = matlab.palette(nrow(flu)))
#'
#' plotmap(faux_cell[, , 1200], col.regions = matlab.palette())
#'
matlab.palette <- function(n = 100, ...) {
  rev(rainbow(n, start = 0, end = 4 / 6, ...))
}

#' @rdname palettes
#' @aliases matlab.dark.palette colors,matlab.dark.palette
#'
#' @concept plotting
#' @concept color palette
#'
#' @importFrom grDevices col2rgb rgb
#' @export
#'
#' @examples
#' # Matlab dark palette
#'
#' op <- par(mar = c(0, 0, 0, 0))
#' pie(rep(1, 12), col = matlab.dark.palette(12))
#' par(op)
#'
#' plot(flu, col = matlab.dark.palette(nrow(flu)))
#'
#' plotmap(faux_cell[, , 1200], col.regions = matlab.dark.palette())
#'
matlab.dark.palette <- function(n = 100, ...) {
  pal <- rev(rainbow(n, start = 0, end = 4 / 6, ...))
  pal <- col2rgb(pal)
  pal["green", ] <- pal["green", ] / 2

  rgb(t(pal) / 255)
}

#' @rdname palettes
#' @aliases alois.palette colors,alois.palette
#'
#' @concept plotting
#' @concept color palette
#'
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' # Alois palette
#'
#' op <- par(mar = c(0, 0, 0, 0))
#' pie(rep(1, 12), col = alois.palette(12))
#' par(op)
#'
#' plot(flu, col = alois.palette(nrow(flu)))
#'
#' plotmap(faux_cell[, , 1200], col.regions = alois.palette())
#'
alois.palette <- function(n = 100, ...) {
  colorRampPalette(c("black", "blue", "cyan", "green", "yellow", "red"), ...)(n)
}
