#
# Script to Produce Synthetic Data for Testing & Demonstration in hyperSpec
#
# This script originated to serve the needs of the unpublished package unmixR
# Original versions June 2013, June 2014, June 2016 by Bryan Hanson
# Updated for use with hyperSpec

# A small scale data set simulating a field containing one cell with a nucleus.
# Small enough to be speedy.

library("SpecHelpers") # both pkgs available on CRAN
library("splancs")

# Create a matrix containing a faux cell image
# The matrix contains a cell which in turn contains a nucleus.
# Use circles and ellipses to define the regions.

# parametric form of an ellipse:
# x = a cos(t) + h, y = b cos(t) + k


## 1. Set up the cell
# Having the overall image have x ne y and the objects irregular helps with troubleshooting.

nx <- 20 # no x points
ny <- 15 # no y points

x <- 1:nx
y <- 1:ny

xy <- expand.grid(x, y)
names(xy) <- c("x", "y") # grid over which the sample was "scanned"

# The cell:
ang <- seq(0, 2*pi, length.out = 100)
xc <- 5*cos(ang + pi/8) + 10
yc <- 5*sin(ang) + 8
cxy <- data.frame(x = xc, y = yc)

# An nucleus inside the cell
xn <- -2*cos(ang +pi/4) + 10
yn <- 3*sin(ang) + 7.5
nxy <- data.frame(x = xn, y = yn)

# Draw these (looks as intended)
plot(xy, col = "grey", cex = 0.1)
points(xc, yc, col = "red", type = "l")
points(xn, yn, col = "blue", type = "l")

# Get logicals indicating where the points are located
# Cell is inside cell; Nuc is inside the nucleus
# Everything else is in the field, Fld
Cell <- inout(xy, cxy, bound = FALSE)
Nuc <- inout(xy, nxy, bound = FALSE)
Conly <- Cell & !Nuc
Fld <- !Cell & !Nuc

# Test logic by plotting.
# Do these one at a time to see the logic,
# or, leave the above plot on the screen, & skip the next line
# Color scheme is cell is red, nucleus is blue, field is green
plot(xy, col = "grey", cex = 0.1, asp = 20/15)
points(xy[Cell,], col = "red", cex = 1)
points(xy[Nuc,], col = "blue", cex = 1)
points(xy[Conly,], col = "red", cex = 1.5) # looks good
points(xy[Fld,], col = "green")
points(xy[1,1], col = "purple", cex = 2) # confirms reference point in lower left

## 2. Now generate some spectra (using package SpecHelpers)
# Sfld will be the spectrum of the field (outside the cell).
# Scell will be the spectrum inside the cell but not in the nucleus.
# Snuc will be the spectrum inside the nucleus.
# So each region in the image has a pure unique spectrum associated with it.
# The makeSpec function below returns a 'spectrum' composed of Gaussian peaks.
# These spectra are completely different from each other.
# Each spectrum has a single peak, equal area but different frequencies and different widths.

Sfld <- data.frame(mu = c(700), sd = c(10), area = c(10))
Sfld <- makeSpec(Sfld, x.range = c(400, 1800), plot = TRUE, curves = FALSE, dd = 1)

Scell <- data.frame(mu = c(1200), sd = c(15), area = c(10))
Scell <- makeSpec(Scell, x.range = c(400, 1800), plot = TRUE, curves = FALSE, dd = 1)

Snuc <- data.frame(mu = c(1500), sd = c(5), area = c(10))
Snuc <- makeSpec(Snuc, x.range = c(400, 1800), plot = TRUE, curves = FALSE, dd = 1)

# Compare all spectra

plot(Sfld[1,], Scell[2,], type = "l", col = "red", ylim = c(0, 1)) # cell, not nucleus
lines(Sfld[1,], Sfld[2,], col = "green") # field
lines(Sfld[1,], Snuc[2,], col = "blue") # nucleus

## 3.  Assemble a hypercube (holdover from unmixR)

DS0 <- array(NA, c(nx, ny, 1400))

# Next step amounts to loading a spectrum into the z dimension,
# proceeding from the lower left corner, filling a column (along y),
# then advancing by moving to the right (incrementing x).
# A sample example of the order of the spectra in the image would be
# matrix(c(5:1, 10:6, 15:11, 20:16, 25:21), ncol = 5)

Mfld <- matrix(Fld, nrow = nx, byrow = TRUE) # mask of logicals
for (x in 1:nx) {
  for (y in 1:ny) {
    if (Mfld[x,y]) DS0[x,y,] <- Sfld[2,] # insert Sfld
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

# Mcell + Mnuc can be used as a membership token; convert to color for plotting
# when converted to vector it properly tracks the order points in DS0 are plotted

gr <- t(Mnuc + Mcell)
group <- as.character(gr)
group <- ifelse (group == "0", "field", group) # Field
group <- ifelse (group == "1", "cell", group) # Conly - cell only
group <- ifelse (group == "2", "nucleus", group) # Inside nucleus

## 4.  Unfold/Flatten to a matrix of (ncol * nrow) x n wavelengths

BigM <- matrix(NA, nrow = nx*ny, ncol = 1400)

# This unfolding corresponds to the order of scanning
# explained above.
for (x in 1:nx) {
  for (y in 1:ny) {
    tmp <- DS0[x,y,] # one spectrum
    BigM[(y + (x-1)*ny), ] <- tmp
  }
}
	
# Create an index of where the spectra were originally

ind <- data.frame(x = xy$x, y = xy$y, origin = group)

# Everything together for convenience

FC <- list(index = ind, M = BigM)

## 5. Investigate the results

# Inspect via PCA
pca <- prcomp(FC$M) # unscaled by default
plot(pca)
plot(pca$x[,1], pca$x[,2], col = as.factor(ind$origin))

## 6. Create hyperSpec object

library('hyperSpec')

FauxCell <- new("hyperSpec",
  spc = FC$M,
  wavelength = 401:1800,
  data = ind,
  label = list(spc = "intensity (arbitrary units)",
              .wavelength = "nm",
              x = "x position",
              y = "y position",
              origin = "origin")
)

save(FauxCell, file = "FauxCell.Rdata")

# Plot mean spectra
FauxCellgrps <- aggregate(FauxCell, FauxCell$origin, mean)
plotspc(FauxCellgrps, stacked = ".aggregate", col = c("red", "green", "blue"))


# Create a dendogram and use it to define clusters in a map
D <- dist(FauxCell)
dend <- hclust(D)
FauxCell$clusters <- as.factor (cutree (dend, k = 3))

mapcols <- c("aliceblue", "aquamarine", "blue")
levels(FauxCell$clusters) <- c("matrix", "cell", "nucleus")
plotmap(FauxCell, clusters ~ x * y, col.regions = mapcols)

