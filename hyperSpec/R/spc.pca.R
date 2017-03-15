#' Principal component analysis of a hyperSpec object
#' 
#' This is a convenience wrapper around \code{prcomp} that additionally
#' creates scores and loadings (both hyperSpec objects) and returns them
#' alongside with the normal output. The extra data contained in the \code{spc}
#' object are automatically propagated to scores and the data common to all spectra
#' to loadings as well.
#' 
#' @param ... Further arguments passed to \code{prcomp}
#' @param spc A hyperSpec object
#' @param ncomps Number of components to keep after the PCA, default to keep all
#'
#' @seealso prcomp
#' @author Roman Kiselev
#' @return A list with class "prcomp" with additional components
#' \code{scores} and \code{loadings}, both hyperSpec objects.
#' @export spc.pca
#' @examples
#' pca <- spc.pca(chondro)
#' plot(pca$loadings[1:3], stacked=TRUE)
#' xy
spc.pca <- function(spc, ncomps = min(dim(spc$spc)), ...){
  chk.hy (spc)
  validObject (spc)
  
  if (! (is.atomic(ncomps) && length(ncomps) == 1L &&
         ncomps %% 1 == 0 && ! is.logical(ncomps)))
    stop("ncomps must be an single integer number")
  
  pca <- prcomp(spc, ...)
  pca$scores <- decomposition(spc, pca$x, label.wavelength = "PC")
  pca$scores <- pca$scores[,, 1:ncomps]
  
  pca$loadings <- decomposition(spc, t(pca$rotation), wavelength = wl(spc))
  pca$loadings <- pca$loadings[1:ncomps]
  pca
}

.test (spc.pca) <- function(){
  context ("spc.pca")
  
  test_that("spc.pca works only with hyperSpec objects", {
    expect_error (spc.pca(chondro[[]]))
    expect_error (spc.pca(1:100))
    expect_error (spc.pca(TRUE))
  })
  
  test_that("ncomps is numerical and does not exceed biggest dimension of the spectral matrix", {
    expect_error (spc.pca(chondro, -1))
    expect_error (spc.pca(chondro, 1:100))
    expect_error (spc.pca(chondro, 12.31))
    expect_error (spc.pca(chondro, TRUE))
    expect_error (spc.pca(chondro, FALSE))
    expect_error (spc.pca(chondro, NULL))
    expect_error (spc.pca(chondro, "hey"))
  })
  
  test_that("Extra data propagates to scores", {
    pca <- spc.pca (chondro, center = FALSE)
    expect_identical (chondro$.., pca$scores$..)
  })
  
  test_that("Reconstructed from PCA object is almost identical to the original one", {
    chondro.new <- pca$scores %*% pca$loadings
    
    expect_identical(wl(chondro.new), wl(chondro))
    expect_identical(chondro.new$.., chondro$..)
    expect_equal(chondro.new, chondro)
  })
}
