#' @rdname palettes
#' @aliases palette_matlab colors,palette_matlab
#'
#' @title Matlab-like color palettes
#'
#' @description
#' Two palettes going from blue over green to red, approximately as the
#' standard palette of Matlab does. The second one has darker green values and
#' is better suited for plotting lines on white background.
#'
#'
#' @param n the number of colors to be in the palette.
#' @param ... further arguments are handed to [grDevices::rainbow()]
#'        (`palette_alois`: [grDevices::colorRampPalette()]).
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
#' pie(rep(1, 12), col = palette_matlab(12))
#' par(op)
#'
#' plot(flu, col = palette_matlab(nrow(flu)))
#'
#' plotmap(faux_cell[, , 1200], col.regions = palette_matlab())
palette_matlab <- function(n = 100, ...) {
  rev(rainbow(n, start = 0, end = 4 / 6, ...))
}

#' @rdname palettes
#' @aliases palette_matlab_dark colors,palette_matlab_dark
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
#' pie(rep(1, 12), col = palette_matlab_dark(12))
#' par(op)
#'
#' plot(flu, col = palette_matlab_dark(nrow(flu)))
#'
#' plotmap(faux_cell[, , 1200], col.regions = palette_matlab_dark())
palette_matlab_dark <- function(n = 100, ...) {
  pal <- rev(rainbow(n, start = 0, end = 4 / 6, ...))
  pal <- col2rgb(pal)
  pal["green", ] <- pal["green", ] / 2

  rgb(t(pal) / 255)
}

#' @rdname palettes
#' @aliases palette_alois colors,palette_alois
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
#' pie(rep(1, 12), col = palette_alois(12))
#' par(op)
#'
#' plot(flu, col = palette_alois(nrow(flu)))
#'
#' plotmap(faux_cell[, , 1200], col.regions = palette_alois())
palette_alois <- function(n = 100, ...) {
  colorRampPalette(c("black", "blue", "cyan", "green", "yellow", "red"), ...)(n)
}

# Unit tests -----------------------------------------------------------------
#' @import hySpc.testthat
hySpc.testthat::test(palette_matlab) <- function() {
  context("palette_matlab")
  test_that("palette_matlab() works", {
    pal <- palette_matlab()
    expect_true(pal[1] == "#0000FF" | pal[1] == "#0000FFFF")
    expect_true(pal[2] == "#000AFF" | pal[2] == "#000AFFFF")
  })
}

hySpc.testthat::test(palette_matlab_dark) <- function() {
  context("palette_matlab_dark")
  test_that("palette_matlab_dark() generates correct colors", {
    dark <- palette_matlab_dark()
    expect_equal(dark[1], "#0000FF")
    expect_equal(dark[2], "#0005FF")
  })
}

hySpc.testthat::test(palette_alois) <- function() {
  context("palette_alois")
  test_that("palette_alois() works", {
    alois <- palette_alois()
    expect_equal(alois[1], "#000000")
    expect_equal(alois[2], "#00000C")
  })
}
