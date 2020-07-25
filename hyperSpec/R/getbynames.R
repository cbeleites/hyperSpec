### -----------------------------------------------------------------------------
###
### getbynames - get list elements by name and if no such element exists, NA
###
###

getbynames <- function(x, e) {
  x <- x[e]
  if (length(x) > 0) {
    if (is.character(e)) {
      names(x) <- e
    }
    x[sapply(x, is.null)] <- NA
    x
  } else {
    list()
  }
}



# Unit tests -----------------------------------------------------------------
.test(getbynames) <- function() {

  context("getbynames")

  # Perform tests
  test_that("getbynames() works", {

    lst <- list(a = 1, b = "b", c = 2i)

    expect_equal(getbynames(lst, "a"), list(a = 1))
    expect_equal(getbynames(lst, 1),   list(a = 1))
    expect_equal(getbynames(lst, 2),   list(b = "b"))
    expect_equal(getbynames(lst, 6)[[1]], NA)

  })
}
