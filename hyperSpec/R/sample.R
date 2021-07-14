# Function -------------------------------------------------------------------

.sample <- function(x, size, replace = FALSE, prob = NULL) {
  validObject(x)

  if (missing(size)) size <- nrow(x) # normal default does not work!

  s <- sample.int(nrow(x@data), size = size, replace = replace, prob = prob)

  x[s]
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.sample) <- function() {
  context(".sample")

  test_that("defaults", {
    tmp <- sample(flu)
    expect_equal(tmp[order(tmp$c)], flu)

    set.seed(101)
    expect_equal(sample(flu)$c, c(0.05, 0.3, 0.1, 0.15, 0.25, 0.2))
  })

  test_that("size", {
    expect_length(isample(flu, size = 3), 3L)
  })

  test_that("prob", {
    expect_equal(isample(flu, size = 1, prob = c(1, rep(0, 5))), 1L)
  })

  test_that("replace", {
    expect_equal(isample(flu, size = 3, replace = TRUE, prob = c(1, rep(0, 5))), rep(1L, 3))
  })
}


# Function -------------------------------------------------------------------

#' Random samples and permutations
#'
#' Take a sample of the specified size from the elements of x with or without
#' replacement.
#'
#' @rdname sample
#' @docType methods
#'
#' @param x The hyperSpec object, data.frame or matrix to sample fromto sample from
#' @param size positive integer giving the number of spectra (rows) to choose.
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of
#'        the vector being sampled.
#'
#' @return a hyperSpec object, data.frame or matrix with `size` rows for
#'        `sample`, and an integer vector for `isample` that is suitable for
#'        indexing (into the spectra) of x.
#'
#' @author C. Beleites
#'
#' @keywords methods distribution
#' @concept stats
#'
#' @export
#'
#' @seealso [base::sample()]
#' @examples
#'
#' sample(flu, 3)
#'
#' plot(flu, col = "darkgray")
#' plot(sample(flu, 3), col = "red", add = TRUE)
#'
#' plot(flu, col = "darkgray")
#' plot(sample(flu, 3, replace = TRUE),
#'   col = "#0000FF80", add = TRUE,
#'   lines.args = list(lwd = 2)
#' )
setMethod("sample", signature = signature(x = "hyperSpec"), .sample)




# Function -------------------------------------------------------------------

#' `isample()` returns an vector of indices, `sample()` returns the
#' corresponding hyperSpec object.
#'
#' @rdname sample
#' @return vector with indices suitable for row-indexing x
#' @export
#'
#' @concept stats
#'
#' @examples
#' isample(flu, 3)
#' isample(flu, 3, replace = TRUE)
#' isample(flu, 8, replace = TRUE)
isample <- function(x, size = nrow(x), replace = FALSE, prob = NULL) {
  chk.hy(x)
  validObject(x)

  sample.int(nrow(x), size = size, replace = replace, prob = prob)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(isample) <- function() {
  context("isample")

  test_that("defaults", {
    expect_equal(sort(isample(flu)), 1:nrow(flu))

    set.seed(101)
    expect_equal(isample(flu), c(1L, 6L, 2L, 3L, 5L, 4L))
  })

  test_that("size", {
    expect_equal(nrow(sample(flu, size = 3)), 3L)
  })

  test_that("prob", {
    expect_equal(sample(flu, size = 1, prob = c(1, rep(0, 5))), flu[1L])
  })

  test_that("replace", {
    expect_equal(sample(flu, size = 3, replace = TRUE, prob = c(1, rep(0, 5))), flu[rep(1L, 3)])
  })
}


# Function -------------------------------------------------------------------


.sample.data.frame <- function(x, size, replace = FALSE, prob = NULL, drop = FALSE) {
  if (missing(size)) size <- nrow(x)
  x[sample.int(nrow(x), size = size, replace = replace, prob = prob), , drop = drop]
}

#' @rdname sample
#' @param drop see [base::drop()]: by default, do not drop dimensions of the result
#' @export
#'
#' @concept stats
#'
#' @examples
#' sample(cars, 2)
setMethod("sample", signature = signature(x = "data.frame"), .sample.data.frame)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.sample.data.frame) <- function() {
  context("sample data.frame")
  test_that("data.frame", {
    set.seed(101)
    tmp <- sample(iris)
    expect_equal(rownames(tmp), c(
      "73", "57", "95", "148", "61", "59", "99", "128", "131", "32",
      "9", "96", "144", "98", "60", "147", "145", "14", "97", "45",
      "117", "42", "64", "90", "43", "146", "125", "130", "58", "85",
      "84", "133", "8", "72", "20", "6", "88", "39", "10", "74", "89",
      "26", "140", "139", "37", "81", "135", "44", "138", "109", "108",
      "3", "111", "116", "66", "65", "142", "28", "22", "80", "93",
      "30", "25", "127", "103", "18", "50", "17", "86", "110", "34",
      "150", "112", "106", "2", "15", "100", "62", "7", "52", "56",
      "129", "101", "4", "143", "122", "79", "55", "149", "41", "114",
      "12", "21", "94", "120", "113", "105", "54", "31", "77", "118",
      "38", "136", "92", "19", "23", "16", "67", "134", "47", "35",
      "69", "63", "75", "5", "121", "132", "126", "27", "48", "87",
      "137", "13", "11", "102", "123", "24", "51", "46", "82", "40",
      "115", "1", "119", "141", "33", "70", "68", "83", "91", "29",
      "36", "78", "107", "76", "104", "71", "49", "53", "124"
    ))
    expect_equal(dim(tmp), dim(iris))
    expect_equal(tmp, iris[as.numeric(rownames(tmp)), ])
  })
}


# Function -------------------------------------------------------------------

.sample.matrix <- function(x, size, replace = FALSE, prob = NULL, drop = FALSE) {
  if (missing(size)) size <- nrow(x)
  x[sample.int(nrow(x), size = size, replace = replace, prob = prob), , drop = drop]
}

#' @rdname sample
#' @export
#'
#' @concept stats
#'
#' @examples
#' sample(matrix(1:24, 6), 2)
setMethod("sample", signature = signature(x = "matrix"), .sample.matrix)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.sample.matrix) <- function() {
  context(".sample.matrix")
  test_that("matrix", {
    set.seed(101)
    tmp <- sample(flu[[]])
    expect_equal(dim(tmp), dim(flu[[]]))
    expect_equal(tmp[c(1L, 3L, 4L, 6L, 5L, 2L), ], flu[[]])
  })
}
