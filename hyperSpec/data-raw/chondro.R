# ============================================================================
# This code prepares `chondro` dataset
# ============================================================================

# Required packages ----------------------------------------------------------
library(hyperSpec)
library(usethis)

# Read the raw data ----------------------------------------------------------
# Working directory should be package's directory that contains file "DESCRIPTION"
chondro_0 <- read.txt.Renishaw("inst/extdata/chondro.gz", data = "xyspc")

# Disturb a few points -------------------------------------------------------
chondro_0$x[500] <- chondro_0$x[500] + rnorm(1, sd = 0.01)
chondro_0$y[660] <- chondro_0$y[660] + rnorm(1, sd = 0.01)

# Pre-process ----------------------------------------------------------------
chondro_0$x <- fitraster(chondro_0$x)$x
chondro_0$y <- fitraster(chondro_0$y)$x

chondro_0 <- spc.loess(chondro_0, seq(602, 1800, 4))
spectra_to_save <- chondro_0

baselines <- spc.fit.poly.below(chondro_0)
chondro_0 <- chondro_0 - baselines

chondro_0 <- chondro_0 / rowMeans(chondro_0)
chondro_0 <- chondro_0 - quantile(chondro_0, 0.05)

# Remove outliers ------------------------------------------------------------
out <- c(105, 140, 216, 289, 75, 69)
chondro_0 <- chondro_0[-out]

# Create clusters ------------------------------------------------------------
dist <- dist(chondro_0)
# TODO: As ward.D" was a bug in R, I suggest uning "ward.D2"
dendrogram <- hclust(dist, method = "ward.D")
chondro_0$clusters <- as.factor(cutree(dendrogram, k = 3))
levels(chondro_0$clusters) <- c("matrix", "lacuna", "cell")

spectra_to_save$clusters <- factor(NA, levels = levels(chondro_0$clusters))
spectra_to_save$clusters[-out] <- chondro_0$clusters

# PCA compsession ------------------------------------------------------------
pca <- prcomp(spectra_to_save)

# Construct hyperSpe object --------------------------------------------------
.chondro_0_scores   <- pca$x[,   seq_len(10)]
.chondro_0_loadings <- pca$rot[, seq_len(10)]
.chondro_0_center   <- pca$center
.chondro_0_wl       <- wl(chondro_0)
.chondro_0_labels   <- lapply(labels(chondro_0), as.expression)
.chondro_0_extra    <- spectra_to_save$..

chondro <-
  new(
    "hyperSpec",
    spc =
      tcrossprod(.chondro_0_scores, .chondro_0_loadings) +
      rep(.chondro_0_center, each = nrow(.chondro_0_scores)),
    wavelength = .chondro_0_wl,
    data = .chondro_0_extra,
    labels = .chondro_0_labels
  )

# Save the prepared dataset as package data ----------------------------------
usethis::use_data(chondro, overwrite = TRUE)

