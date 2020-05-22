.fauxCell <- function() {


  # Check for points inside ellipse
  #
  # An ellipse is a unit circle that is squeezed, rotated and translated
  # -> do these transformations backwards and
  # check whether points are within radius 1 of origin
  # @param xy points to be checked for position inside the ellipse
  # @param center center of the ellipse
  # @param scale length of the main axes
  # @param rot rotation angle
  # @return logical indicating points inside the ellipse
  in_ellipse <- function(xy, center = c(0, 0), scale = c(1, 1), a = 0,
                         debuglevel = 0L
  ) {
    xy <- as.matrix(xy)
    xy <- scale(xy, center = center, scale = FALSE)
    xy <- xy %*% matrix(c(cos(a), -sin(a), sin(a), cos(a)), ncol = 2)
    xy <- scale(xy, center = FALSE, scale = scale)

    pt_in <- rowSums(xy^2) <= 1

    if (debuglevel >= 1L)
      plot(xy[, 1], xy[, 2], asp = 1, pch = 19, col = pt_in + 1)

    ## backtransformed points within unit circle?
    pt_in
  }

  # Now for fauxCell() itself

  # Create a matrix containing a faux cell image
  # The matrix contains a cell which in turn contains a nucleus.
  # Use ellipses to define the regions.

  ## 1. Set up the scanned region & needed coordinates

  xy <- expand.grid(
    x = seq(-11.55, 22.45, by = 1),
    y = seq(-4.77, 19.23, by = 1)
  )

  xy$region <- "matrix"

  pts_in_cell <- in_ellipse(xy[c("x", "y")],
                            center = c(10, 10),
                            scale = c(6, 10), a = -pi/8)
  xy$region[pts_in_cell] <- "cell"


  pts_in_nucleus <- in_ellipse(xy[c("x", "y")],
                               center = c(12.5, 8),
                               scale = c(3, 4), a = pi/6)
  xy$region[pts_in_nucleus] <- "nucleus"

  xy$region <- as.factor(xy$region)

  ## 2. initialize hyperSpec object

  wavelength <- seq(602, 1798, by = 4)
  spc <- new(
    "hyperSpec",
    wavelength = wavelength,
    data = xy,
    spc = matrix(0, nrow = nrow(xy), ncol = length(wavelength)),
    label = list(spc = "intensity (arbitrary units)",
                 .wavelength = expression(Delta * tilde(nu) / cm^-1),
                 x = "x position",
                 y = "y position")
  )

  ## 3. generate fake spectra

  # generate "spectra" and and fill the object with copies
  tmp <- 5000*dnorm(wavelength, mean = 800, sd = 10)
  spc[[spc$region == "matrix",,]] <- rep(tmp, each = sum(spc$region == "matrix"))

  tmp <- 7000*dnorm(wavelength, mean = 1200, sd = 15)
  spc[[spc$region == "cell",,]] <- rep(tmp, each = sum(spc$region == "cell"))

  tmp <- 3000*dnorm(wavelength, mean = 1500, sd =  5)
  spc[[spc$region == "nucleus",,]] <- rep(tmp, each = sum(spc$region == "nucleus"))

  # generate shot noise
  spc[[]] <- rpois(n = length(spc[[]]), lambda = spc[[]])

  spc
}

#' Faux Cell Data Set for Testing & Demonstration
#'
#' This is a synthetic data set intended for testing and demonstration.
#'
#' The data set resembles the `chondro` data set but is entirely synthetic.
#'
#' @format The object contains 875 Raman-like spectra, allocated to three
#'   groups/regions in column region: the matrix/background, the cell and the
#'   cell nucleus. Each spectrum is composed of 300 data points.  The spectrum
#'   of each region is unique and simple, with a single peak at a particular
#'   frequency and line width.  Poisson noise has been added.  The data is
#'   indexed along the x and y dimensions, simulating data collected on a grid.
#'
#' @rdname fauxCell
#' @docType data
#' @include initialize.R
#' @export
#' @author Bryan A. Hanson
#' @examples
#'
#' fauxCell
#'
#' # Plot mean spectra
#' FCgrps <- aggregate(fauxCell, fauxCell$origin, mean)
#' plotspc(FCgrps, stacked = ".aggregate", col = c("red", "green", "blue"))
#'
#' # Create a dendrogram to define clusters for mapping
#' D <- dist(FC)
#' dend <- hclust(D)
#' FC$clusters <- as.factor (cutree (dend, k = 3))
#' levels(FC$clusters) <- c("matrix", "cell", "nucleus")
#' mapcols <- c("aliceblue", "aquamarine", "dodgerblue")
#' plotmap(FC, clusters ~ x * y, col.regions = mapcols)
#'
#' # PCA
#' pca <- prcomp(FC)
#' plot(pca)
#' pcacols <- mapcols[as.numeric(as.factor(unlist(FC[[, "origin", ]], use.names = FALSE)))]
#' plot(pca$x[,1], pca$x[,2],
#'   xlab = "PC 1", ylab = "PC 2",
#'   bg = pcacols, col = "black", pch = 21)
#'
#'
delayedAssign("fauxCell", .fauxCell())
