# Function -------------------------------------------------------------------

.all.equal <- function(target, current, ..., check.attributes = FALSE,
                       check.names = FALSE, check.column.order = FALSE,
                       check.label = FALSE,
                       tolerance = hy.getOption("tolerance"),
                       wl.tolerance = hy.getOption("wl.tolerance")) {
  validObject(target)
  validObject(current)
  tolerance <- .checkpos(tolerance, "tolerance")
  wl.tolerance <- .checkpos(wl.tolerance, "wl.tolerance")

  result <- character(0)

  cmp <- all.equal(
    target = target@wavelength, current = current@wavelength, ...,
    tolerance = wl.tolerance,
    check.attributes = check.attributes, check.names = check.names
  )
  if (!isTRUE(cmp)) result <- c("@wavelength:", cmp)

  if (check.column.order) {
    cmp <- all.equal(
      target = target@data, current = current@data, ...,
      tolerance = tolerance,
      check.attributes = check.attributes
    )
  } else {
    cmp <- all.equal(
      target = target@data[order(colnames(target@data))],
      current = current@data[order(colnames(current@data))],
      ...,
      tolerance = tolerance,
      check.attributes = check.attributes, check.names = check.names
    )
  }
  if (!isTRUE(cmp)) result <- c(result, "@data:", cmp)

  if (check.label) {
    cmp <- all.equal(
      target = target@label[order(names(target@label))],
      current = current@label[order(names(current@label))],
      ...,
      check.attributes = check.attributes, check.names = check.names
    )
    if (!isTRUE(cmp)) result <- c(result, "@label:", cmp)
  }

  if (length(result) == 0) {
    TRUE
  } else {
    result
  }
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.all.equal) <- function() {
  context(".all.equal")

  test_that("basic equalities", {
    expect_true(all.equal(flu, --flu))
    expect_true(all.equal(flu, --flu, check.attributes = TRUE))
    expect_true(all.equal(flu, --flu, check.names = TRUE))
  })

  test_that("labels", {
    expect_true(all.equal(flu, --flu, check.label = TRUE))
  })

  test_that("labels: order of labels does *not* matter", {
    tmp <- flu
    tmp@label <- rev(tmp@label)
    expect_true(all.equal(flu, tmp, check.label = TRUE))
  })

  test_that("labels: character vs. expression does matter:", {
    tmp <- flu
    tmp@label <- lapply(tmp@label, as.expression)
    expect_true(!isTRUE(all.equal(flu, tmp, check.label = TRUE)))
  })

  test_that("column order", {
    expect_true(all.equal(flu, --flu, check.column.order = TRUE))
    expect_true(!isTRUE(all.equal(flu, flu[, rev(colnames(flu))], check.column.order = TRUE)))
    expect_true(all.equal(flu, flu[, rev(colnames(flu))], check.column.order = FALSE))
  })
}

# Function -------------------------------------------------------------------

#' @rdname Comparison
#' @aliases all.equal  all.equal,hyperSpec,hyperSpec-method
#'
#' @param target,current two `hyperSpec` objects that are tested for
#'        equality
#' @param ... handed to [base::all.equal()] when testing the slots of the
#'        `hyperSpec` objects
#' @param check.column.order If two objects have the same data, but the order
#'        of the columns (determined by the names) differs, should they be
#'        regarded as different?
#' @param check.label Should the slot `label` be checked? \cr
#'        If the labels differ only in the order of their entries, they are
#'        considered equal.
#' @param check.attributes,check.names see [base::all.equal()]
#' @param tolerance,wl.tolerance tolerances for checking wavelengths and data,
#'        respectively
#'
#' @return `all.equal` returns either `TRUE`, or a character vector describing
#'          the differences. In conditions, the result must therefore be tested
#'          with  [base::isTRUE()].
#'
#' @seealso [base::all.equal()] and [base::isTRUE()]
#'
#' @concept manipulation
#'
#' @export
setMethod("all.equal", signature(target = "hyperSpec", current = "hyperSpec"), .all.equal)
