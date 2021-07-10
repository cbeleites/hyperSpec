#' Distance based on Pearson's \eqn{R^2}{R squared}
#'
#' The calculated distance is
#' \eqn{D^2 = \frac{1 - COR (`x`')}{2}}{D^2 = (1 - COR (x')) / 2}
#'
#' The distance between the rows of `x` is calculated.  The possible
#' values range from 0 (perfectly correlated) over 0.5 (uncorrelated) to 1
#' (perfectly anti-correlated).
#'
#' @param x a matrix
#' @return distance matrix (distance object)
#' @author C. Beleites
#' @seealso [stats::as.dist()]
#' @references S. Theodoridis and K. Koutroumbas: Pattern Recognition, 3rd ed., p. 495
#'
#' @export
#'
#' @keywords cluster
#' @concept stats
#'
#' @examples
#'
#' pearson.dist(flu[[]])
#' pearson.dist(flu)
pearson.dist <- function(x) {
  x <- as.matrix(x)

  ## center & scale *row*s
  ## (n - 1) factor cancels out between variance scaling and calculating correlation
  x <- x - rowMeans(x)
  x <- x / sqrt(rowSums(x^2))

  if (hy.getOption("gc")) gc()
  x <- tcrossprod(x)

  ## keep only lower triagonal
  if (hy.getOption("gc")) gc()
  x <- as.dist(x)

  if (hy.getOption("gc")) gc()
  0.5 - x / 2
}


hySpc.testthat::test(pearson.dist) <- function() {
  context("pearson.dist")

  test_that("pearson.dist against manual calculation", {
    expect_equivalent(
      pearson.dist(flu),
      as.dist(0.5 - cor(t(as.matrix(flu))) / 2)
    )
  })
}

## benchmark
# function (){
#   m <- sample (faux_cell, 10000)[[]]
#   microbenchmark(
#     cor = as.dist (0.5 - cor (t (as.matrix (m))) / 2),
#     tcross = pearson.dist (m),
#     times = 10L
#   )
# }
