#' Merge additional extra data into an object
#'
#' Columns from `y` that are not in `x` are merged into `x`. Columns in `x` and
#' `y` are considered equal if they have the same name, the same content, and
#' the same label.
#'
#' Columns with different content than a column in `x` with the same name will
#' get `".y"` appended to its name.
#'
#' **`merge_data()` relies on `x` and `y` having the same row order.**
#'
#' @param x hyperSpec object into which extra data columns of y are to be merged
#' @param y hyperSpec object with further extra data columns
#'
#' @return x amended with columns from y
#' @export
#'
#' @concept manipulation
#'
#' @examples
#' tmp <- flu[, FALSE, ]
#' tmp$prediction <- 1:6
#' tmp
#'
#' merge_data(flu, tmp)
merge_data <- function(x, y) {
  validObject(x)
  validObject(y)

  if (nrow(x) != nrow(y)) {
    stop("Rows of x and y must correspond.")
  }

  ylabels <- labels(y)
  ylabels$spc <- NULL
  ylabels$.wavelength <- NULL

  y <- y$..

  for (col in rev(seq_along(y))) {
    colname <- colnames(y)[col]

    if (!colname %in% colnames(x)) {
      next
    }

    if ((is.null(ylabels[[colname]]) | # no label or
      as.character(ylabels[[colname]]) == colname | # default label or
      # same label
      as.character(ylabels[[colname]]) == as.character(labels(x, colname))) &
      # AND
      identical(y[[col]], x@data[[colname]]) # content identical
    ) {
      y[[col]] <- NULL
      ylabels[[colname]] <- NULL
    } else if (colname %in% colnames(x)) {
      # y column needs to be renamed

      colnames(y)[col] <- paste0(colnames(y)[col], ".y")
      names(ylabels)[names(ylabels) == colname] <-
        paste0(names(ylabels)[names(ylabels) == colname], ".y")
    }
  }

  if (ncol(y) > 0) {
    x@data <- cbind(x@data, y)
    labels(x) <- modifyList(labels(x), ylabels)
  }

  x
}

hySpc.testthat::test(merge_data) <- function() {
  context("merge_data")

  test_that("nothing to do", {
    expect_equal(merge_data(flu, flu), flu)

    tmp <- flu[, , 405]
    tmp$c <- NULL
    tmp$filename <- NULL
    expect_equal(merge_data(flu, tmp), flu)
  })

  ## sort list by name
  .sortbyname <- function(l) {
    l[order(names(l))]
  }

  test_that("new column", {
    tmp <- flu[, , 405]
    tmp$c <- NULL
    tmp$pred <- 1:6
    labels(tmp, "pred") <- expression(c / mg * l)

    res <- merge_data(flu, tmp)

    expect_equal(res$spc, flu$spc)
    expect_equal(sort(colnames(res)), sort(c(colnames(flu), "pred")))
    expect_equal(res$c, flu$c)
    expect_equal(res$filename, flu$filename)
    expect_equal(res$pred, tmp$pred)
    expect_equal(
      .sortbyname(labels(res)),
      .sortbyname(modifyList(labels(flu), labels(tmp)))
    )
  })

  test_that("different content, same label", {
    tmp <- flu[, , 405]
    tmp$c <- 1:6
    labels(tmp, "c") <- labels(flu, "c")

    res <- merge_data(flu, tmp)

    expect_equal(res$spc, flu$spc)
    expect_equal(sort(colnames(res)), sort(c(colnames(flu), "c.y")))
    expect_equal(res$c, flu$c)
    expect_equal(res$c.y, tmp$c)
    expect_equal(
      .sortbyname(labels(res)),
      .sortbyname(c(labels(flu), list(c.y = labels(flu, "c"))))
    )
  })

  test_that("different content type, same label", {
    tmp <- flu
    tmp$filename <- as.factor(tmp$filename)

    res <- merge_data(flu, tmp)

    expect_equal(res$spc, flu$spc)
    expect_equal(sort(colnames(res)), sort(c(colnames(flu), "filename.y")))
    expect_equal(res$c, flu$c)
    expect_equal(res$filename, flu$filename)
    expect_equal(res$filename.y, tmp$filename)
    expect_equal(
      .sortbyname(labels(res)),
      .sortbyname(c(
        labels(flu),
        list(filename.y = labels(flu, "filename"))
      ))
    )
  })

  test_that("different label, same content", {
    tmp <- flu[, , 405]
    tmp$c <- flu$c
    labels(tmp, "c") <- expression(c / mg * l)

    res <- merge_data(flu, tmp)

    expect_equal(res$spc, flu$spc)
    expect_equal(sort(colnames(res)), sort(c(colnames(flu), "c.y")))
    expect_equal(res$c, flu$c)
    expect_equal(res$c.y, flu$c)
    expect_equal(
      .sortbyname(labels(res)),
      .sortbyname(c(labels(flu), list(c.y = expression(c / mg * l))))
    )
  })

  test_that("x missing labels", {
    spc <- faux_cell[1:3]
    spc@label$region <- NULL
    spc - spc
  })
}
