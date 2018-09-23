.sample <- function (x, size, replace = FALSE, prob = NULL) {
	validObject (x)

  if (missing (size)) size <- nrow (x) # normal default does not work!

	s <- sample.int (nrow (x@data), size = size, replace = replace, prob = prob)

	x [s]
}

.test (.sample) <- function (){
  context (".sample")

  test_that ("defaults", {
    tmp <- sample (flu)
    expect_equal (tmp [order (tmp$c)], flu)

    set.seed(101)
    expect_equal (sample (flu)$c, c(0.15, 0.05, 0.3, 0.1, 0.25, 0.2))
  })

  test_that("size", {
    expect_length (isample (flu, size = 3), 3L)
  })

  test_that("prob", {
    expect_equal (isample (flu, size = 1, prob = c (1, rep (0, 5))), 1L)
  })

  test_that("replace", {
    expect_equal (isample (flu, size = 3, replace = TRUE, prob = c (1, rep (0, 5))), rep (1L, 3))
  })

}




##' Random Samples and Permutations
##' Take a sample of the specified size from the elements of x with or without
##' replacement.
##'
##' @rdname sample
##' @docType methods
##' @param x The hyperSpec object, data.frame or matrix to sample fromto sample from
##' @param size positive integer giving the number of spectra (rows) to choose.
##' @param replace Should sampling be with replacement?
##' @param prob A vector of probability weights for obtaining the elements of
##'   the vector being sampled.
##' @return a hyperSpec object, data.frame or matrix with \code{size} rows for \code{sample}, and an
##' integer vector for \code{isample} that is suitable for indexing (into the spectra) of x.
##' @author C. Beleites
##' @seealso \code{\link[base]{sample}}
##' @keywords methods distribution
##' @export
##' @examples
##'
##' sample (flu, 3)
##'
##' plot (flu, col = "darkgray")
##' plot (sample (flu, 3), col = "red", add = TRUE)
##'
##' plot (flu, col = "darkgray")
##' plot (sample (flu, 3, replace = TRUE), col = "#0000FF80", add = TRUE,
##'       lines.args = list (lwd = 2));
##'
setMethod ("sample", signature = signature (x = "hyperSpec"), .sample)

##' \code{isample} returns an vector of indices, \code{sample} returns the
##' corresponding hyperSpec object.
##'
##' @rdname sample
##' @return vector with indices suitable for row-indexing x
##' @export
##' @examples
##' isample (flu, 3)
##' isample (flu, 3, replace = TRUE)
##' isample (flu, 8, replace = TRUE)

isample <- function (x, size = nrow (x), replace = FALSE, prob = NULL) {
  chk.hy (x)
  validObject (x)

  sample.int (nrow (x), size = size, replace = replace, prob = prob)
}

.test (isample) <- function (){
  context ("isample")

  test_that ("defaults", {
    expect_equal (sort (isample (flu)), 1 : nrow (flu))

    set.seed(101)
    expect_equal (isample (flu), c(3L, 1L, 6L, 2L, 5L, 4L))

  })

  test_that("size", {
    expect_equal (nrow (sample (flu, size = 3)), 3L)
  })

  test_that("prob", {
    expect_equal (sample (flu, size = 1, prob = c (1, rep (0, 5))), flu [1L])
  })

  test_that("replace", {
    expect_equal (sample (flu, size = 3, replace = TRUE, prob = c (1, rep (0, 5))), flu [rep (1L, 3)])
  })

}


##' @rdname sample
##' @param drop see \code{\link[base]{drop}}: by default, do not drop dimensions of the result
##' @export
##' @examples
##' sample (cars, 2)
setMethod ("sample", signature = signature (x = "data.frame"),
           function (x, size, replace = FALSE, prob = NULL, drop = FALSE) {
             if (missing (size)) size <- nrow (x)
             x [sample.int (nrow (x), size = size, replace = replace, prob = prob), , drop = drop]
           }
           )
##' @rdname sample
##' @export
##' @examples
##' sample (matrix (1:24, 6), 2)
setMethod ("sample", signature = signature (x = "matrix"),
           function (x, size, replace = FALSE, prob = NULL, drop = FALSE) {
             if (missing (size)) size <- nrow (x)
             x [sample.int (nrow (x), size = size, replace = replace, prob = prob), , drop = drop]
           }
           )

.test (sample) <- function (){
  context ("sample")
  test_that ("data.frame", {
    set.seed (101)
    tmp <- sample (iris)
    expect_equal (rownames (tmp), c("56", "7", "106", "97", "37", "44", "85", "48", "89", "77",
                                    "124", "99", "102", "128", "62", "80", "110", "30", "55", "6",
                                    "92", "140", "28", "84", "117", "100", "9", "143", "50", "135",
                                    "51", "39", "24", "20", "61", "148", "118", "93", "3", "103",
                                    "123", "49", "83", "36", "42", "25", "8", "95", "79", "11", "104",
                                    "43", "67", "72", "145", "75", "64", "109", "94", "54", "74",
                                    "73", "91", "87", "142", "116", "2", "26", "86", "141", "19",
                                    "90", "45", "65", "63", "21", "4", "17", "113", "69", "134",
                                    "88", "53", "66", "1", "136", "111", "27", "129", "127", "40",
                                    "71", "115", "101", "14", "47", "130", "13", "138", "52", "149",
                                    "144", "121", "41", "34", "16", "120", "147", "33", "132", "131",
                                    "139", "22", "58", "32", "35", "133", "82", "57", "126", "12",
                                    "5", "112", "15", "122", "59", "150", "107", "76", "119", "114",
                                    "125", "29", "60", "81", "70", "108", "68", "146", "38", "10",
                                    "137", "23", "78", "46", "98", "105", "96", "18", "31"))
    expect_equal(dim (tmp),dim (iris))
    expect_equal(tmp, iris [as.numeric (rownames (tmp)),])
  })

  test_that ("matrix", {
    set.seed (101)
    tmp <- sample (flu [[]])
    expect_equal(dim (tmp),dim (flu [[]]))
    expect_equal(tmp [c(2L, 4L, 1L, 6L, 5L, 3L),], flu [[]])

  })


}
