#'
#' Faux Cell
#'
#' Faux Cell is a synthetic data set intended for testing and demonstration.
#' It is small so that it processes quickly.
#' There are 300 Raman-like spectra allocated to three groups/regions.
#' The spectrum of each region is unique and simple, with a single peak with a particular
#' frequency and line width.
#' Each spectrum has 1400 data points.  A small amount of noise has been added.
#' The three regions represent the sample matrix, a cell, and a nucleus within the cell.
#' The data is indexed along x and y dimensions, simulating data collected on a grid.
#'
#' @name FauxCell
#'
#' @docType data
#'
#' @format The data are stored as a \code{\link{hyperSpec}} object.
#'
#' @source Created by \code{Create_Faux_Cell.R} which can be found in the \code{inst/ext}
#' directory of the package.
#'
#' @keywords datasets
#'
#' @examples
#'
#' # Load & summarize
#' data(FauxCell)
#' FauxCell
#'
#' # Plot mean spectra
#' FauxCellgrps <- aggregate(FauxCell, FauxCell$origin, mean)
#' plotspc(FauxCellgrps, stacked = ".aggregate", col = c("red", "green", "blue"))
#' 
#' # Create a dendrogram to define clusters for mapping
#' D <- dist(FauxCell)
#' dend <- hclust(D)
#' FauxCell$clusters <- as.factor (cutree (dend, k = 3))
#' levels(FauxCell$clusters) <- c("matrix", "cell", "nucleus")
#' mapcols <- c("aliceblue", "aquamarine", "blue")
#' plotmap(FauxCell, clusters ~ x * y, col.regions = mapcols)
NULL
