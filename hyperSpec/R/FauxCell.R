#'
#' Create Faux Cell Data Set for Testing & Demonstration
#'
#' This is a utility function to create a synthetic data set intended for testing
#' and demonstration. It is small so that it processes quickly.
#'
#' @return A \code{hyperSpec} object.  The object contains 875 Raman-like spectra, allocated
#' to three groups/regions: the matrix/background, the cell and the cell nucleus.
#' Each spectrum is composed of 300 data points.  The spectrum of each region is unique and
#' simple, with a single peak at a particular frequency and line width.  A small amount of noise
#' has been added.  The data is indexed along the x and y dimensions, simulating data
#' collected on a grid.  Historical note: this data set resembles the \code{chondro} data
#' set but is entirely synthetic.
#'
#' @Rdname FauxCell
#' @export
#' @importFrom splancs inout
#' @author Bryan A. Hanson
#' @examples
#'
#' # Create & summarize
#' FC <- FauxCell()
#' FauxCell
#'
#' # Plot mean spectra
#' FCgrps <- aggregate(FC, FC$origin, mean)
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

FauxCell <- function() {

  if (!requireNamespace("splancs", quietly = TRUE)) {
    stop("You need to install package splancs to use this function")
  }

  # Helper Functions (from SpecHelpers, available on CRAN)
  # Documentation & comments stripped out here, plus a few deletions of irrelevant stuff

  gaussCurve <- function(x, area, mu, sigma, tail) {
    m <- mu
    if (is.na(tail)) s <- sigma
    if (!is.na(tail)) s <- sigma*tail*x
    numerator <- exp(-1.0 * ((x - m)^2)/(2*s^2))
    denominator <- s*sqrt(2*pi)
    y <- area*numerator/denominator
  }

  makeSpec <- function(peak.list, x.range, type = "gauss", dd = 1, ...) {
    ndp <- floor(dd*abs(diff(x.range)))

    if (type == "gauss") {
      pl <- peak.list
      ns <- length(pl$mu) # ns = no. of spec
      if (is.null(pl$tail)) pl$tail <- rep(NA, ns)
      x <- seq(from = x.range[1], to = x.range[2], length.out = ndp)
      y.mat <- matrix(data = NA_real_, nrow = ns, ncol = ndp)

      for (n in 1:ns) {
        y.mat[n,] <- gaussCurve(x = x, area = pl$area[n], mu = pl$mu[n],
                                sigma = pl$sd[n], tail = pl$tail[n])
      }

      rn <- list()
      for (n in 1:ns) {
        rn[n] <- paste("area", pl$area[n], "mu", pl$mu[n], "sigma", pl$sd[n], "tail", pl$tail[n], sep = " ")
      }
    }

    dimnames(y.mat)[[1]] <- rn
    y.sum <- colSums(y.mat)
    all <- rbind(x, y.sum, y.mat)
	return(all)
  }


  #' Check for points inside ellipse
  #'
  #' An ellipse is a unit circle that is squeezed, rotated and translated
  #' -> do these transformations backwards and
  #' check whether points are within radius 1 of origin
  #' @param xy points to be checked for position inside the ellipse
  #' @param center center of the ellipse
  #' @param scale length of the main axes
  #' @param rot rotation angle
  #' @return logical indicating points inside the ellipse
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

  # Now for FauxCell() itself

  # Create a matrix containing a faux cell image
  # The matrix contains a cell which in turn contains a nucleus.
  # Use circles and ellipses to define the regions.

  # Parametric form of an ellipse:
  # x = a cos(t) + h, y = b cos(t) + k

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

