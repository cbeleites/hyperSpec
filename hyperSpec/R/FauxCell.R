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

  # Now for FauxCell() itself

  # Create a matrix containing a faux cell image
  # The matrix contains a cell which in turn contains a nucleus.
  # Use circles and ellipses to define the regions.

  # Parametric form of an ellipse:
  # x = a cos(t) + h, y = b cos(t) + k

  ## 1. Set up the matrix/scanned region & needed coordinates

  nx <- 35 # no x points
  ny <- 25 # no y points
  x <- 1:nx
  y <- 1:ny

  xy <- expand.grid(x, y)
  names(xy) <- c("x", "y") # grid over which the sample was "scanned"

  # Coordinates of the cell:
  ang <- seq(0, 2*pi, length.out = 100)
  xc <- 8*cos(ang + pi/8) + 20
  yc <- 8*sin(ang) + 15
  cxy <- data.frame(x = xc, y = yc)

  # Coordinates of the nucleus inside the cell
  xn <- -4*cos(ang +pi/5) + 22
  yn <- 4*sin(ang) + 13
  nxy <- data.frame(x = xn, y = yn)

  # Draw to visualize
  # plot(xy, col = "grey", cex = 0.1)
  # points(xc, yc, col = "red", type = "l")
  # points(xn, yn, col = "blue", type = "l")

  # Get logicals indicating where the points are located
  # Cell is inside cell; Nuc is inside the nucleus
  # Everything else is in the matrix, Mat
  Cell <- splancs::inout(xy, cxy, bound = FALSE)
  Nuc <- splancs::inout(xy, nxy, bound = FALSE)
  Conly <- Cell & !Nuc
  Mat <- !Cell & !Nuc

  # Test logic by plotting.
  # Do these one at a time to see the logic,
  # or, leave the above plot on the screen, & skip the next line
  # Color scheme is cell is red, nucleus is blue, field is green
  # plot(xy, col = "grey", cex = 0.1, asp = 20/15)
  # points(xy[Cell,], col = "red", cex = 1)
  # points(xy[Nuc,], col = "blue", cex = 1)
  # points(xy[Conly,], col = "red", cex = 1.5)
  # points(xy[Mat,], col = "green")
  # points(xy[1,1], col = "purple", cex = 2)

  ## 2. Now generate some spectra
  # Smat will be the spectrum of the field (outside the cell).
  # Scell will be the spectrum inside the cell but not in the nucleus.
  # Snuc will be the spectrum inside the nucleus.
  # So each region in the image has a pure unique spectrum associated with it.
  # The makeSpec function below returns a 'spectrum' composed of Gaussian peaks.
  # These spectra are completely different from each other.
  # Each spectrum has a single peak, equal area but different frequencies and different widths.

  Smat <- data.frame(mu = c(800), sd = c(10), area = c(10))
  Smat <- makeSpec(Smat, x.range = c(602, 1798), plot = FALSE, dd = 0.251)

  Scell <- data.frame(mu = c(1200), sd = c(15), area = c(10))
  Scell <- makeSpec(Scell, x.range = c(602, 1798), plot = FALSE, dd = 0.251)

  Snuc <- data.frame(mu = c(1500), sd = c(5), area = c(10))
  Snuc <- makeSpec(Snuc, x.range = c(602, 1798), plot = FALSE, dd = 0.251)

  # Plot all spectra (x values in first row, y values in 2nd row)
  # plot(Smat[1,], Scell[2,], type = "l", col = "red", ylim = c(0, 1)) # cell, not nucleus
  # lines(Smat[1,], Smat[2,], col = "green") # field
  # lines(Smat[1,], Snuc[2,], col = "blue") # nucleus

  ## 3.  Assemble a hypercube (holdover from unmixR, could be eliminated)

  DS0 <- array(NA_real_, c(nx, ny, 300)) # 300 is no. of data points in spectra

  # Next step amounts to loading a spectrum into the z dimension,
  # proceeding from the lower left corner, filling each column first (along y),
  # then advancing by moving to the right (incrementing x).
  # A sample example of the order of filling in the image would be
  # matrix(c(5:1, 10:6, 15:11, 20:16, 25:21), ncol = 5)

  Mmat <- matrix(Mat, nrow = nx, byrow = TRUE) # mask of logicals
  for (x in 1:nx) {
    for (y in 1:ny) {
      if (Mmat[x,y]) DS0[x,y,] <- Smat[2,] # insert Smat
    }
  }

  Mcellonly <- matrix(Conly, nrow = nx, byrow = TRUE) # mask of logicals
  for (x in 1:nx) {
    for (y in 1:ny) {
      if (Mcellonly[x,y]) DS0[x,y,] <- Scell[2,] # insert Scell
    }
  }

  Mnuc <- matrix(Nuc, nrow = nx, byrow = TRUE) # mask of logicals
  for (x in 1:nx) {
    for (y in 1:ny) {
      if (Mnuc[x,y]) DS0[x,y,] <- Snuc[2,] # insert Snuc
    }
  }

  # Add noise
  set.seed(9)
  DS0 <- jitter(DS0, amount = 0.10*diff(range(DS0)))

  Mcell <- matrix(Cell, nrow = nx, byrow = TRUE) # mask of logicals

  # Mcell + Mnuc can be used as a membership token

  gr <- t(Mnuc + Mcell)
  group <- as.character(gr)
  group <- ifelse (group == "0", "matrix", group) # matrix
  group <- ifelse (group == "1", "cell", group) # Conly - cell only
  group <- ifelse (group == "2", "nucleus", group) # Inside nucleus

  ## 4.  Unfold/Flatten to a matrix of (ncol * nrow) x n wavelengths

  DS1 <- matrix(NA_real_, nrow = nx*ny, ncol = 300)

  # This unfolding corresponds to the order of scanning explained above.
  for (x in 1:nx) {
    for (y in 1:ny) {
      tmp <- DS0[x,y,] # get one spectrum
      DS1[(y + (x-1)*ny), ] <- tmp
    }
  }

  # Create an index of where the spectra were originally
  # These x and y values are designed to match chondro
  xy2 <- expand.grid(x = seq(-11.55, 22.45, by = 1), y = seq(-4.77, 19.23, by = 1))
  ind <- data.frame(x = xy2$x, y = xy2$y, origin = group)
  FC <- list(index = ind, M = DS1)

  ## 5. Create hyperSpec object

  FauxCell <- new("hyperSpec",
    spc = FC$M,
    wavelength = round(seq(602, 1798, length.out = 300)),
    data = ind,
    label = list(spc = "intensity (arbitrary units)",
                .wavelength = expression (Delta * tilde (nu) / cm^-1),
                x = "x position",
                y = "y position",
                origin = "origin")
  )

  return(FauxCell)

} # end of FauxCell()
