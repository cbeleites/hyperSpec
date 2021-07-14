
# Function -------------------------------------------------------------------

#' @importFrom utils modifyList
.labels <- function(object, which = bquote(), drop = TRUE, ..., use.colnames = TRUE) {
  validObject(object)

  if (!missing(which) & !is.character(which)) {
    warning(
      "labels are not guaranteed to have the same order as columns.",
      "Consider using indexing by name."
    )
  }

  ## fill in colnames?
  if (use.colnames) {
    label <- structure(as.list(c(colnames(object@data), ".wavelength")),
      names = c(colnames(object@data), ".wavelength")
    )
    label <- modifyList(label, object@label[!sapply(object@label, is.null)])

    label <- label[which]
  } else {
    label <- object@label[which]
  }

  if (drop && length(label) == 1L) {
    label <- label[[1]]
  }

  label
}


#' Get and set labels of a `hyperSpec` object
#'
#' Get and set labels of a `hyperSpec` object.
#'
#'
#' `value` may be a list or vector of labels giving the new label for
#' each of the entries specified by `which`.
#'
#' The names of the labels are the same as the colnames of the
#' `data.frame`.  The label for the wavelength axis has the name
#' `.wavelength`.
#'
#' The labels should be given in a form ready for the text-drawing functions
#' (see [grDevices::plotmath()]), e.g. as `expression` or a
#' `character`.
#'
#' @rdname labels
#' @docType methods
#'
#' @param object a `hyperSpec` object
#' @param which numeric or character to specify the label(s)
#' @param ... ignored
#' @param drop if the result would be a list with only one element, should the
#'        element be returned instead?
#' @param use.colnames should missing labels be replaced by column names of
#'        the extra data?
#'
#' @return `labels` returns a list of labels.  If `drop` is
#'         `TRUE` and the list contains only one element, the element is
#'         returned instead.
#'
#'
#' @author C. Beleites
#' @seealso [base::labels()]
#'
#' @export
#'
#' @concept labels
#'
#' @examples
#'
#' labels(faux_cell)
setMethod("labels", signature = signature(object = "hyperSpec"), .labels)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.labels) <- function() {
  context(".labels")

  .sort <- function(x) {
    x[order(names(x))]
  }

  test_that(
    "return exactly @label with use.colnames == FALSE",
    expect_equal(.sort(labels(flu, use.colnames = FALSE)), .sort(flu@label))
  )

  test_that("fill in missing labels", {
    tmp <- flu
    tmp@label$filename <- NULL
    expect_equal(
      .sort(labels(flu)),
      .sort(c(tmp@label, filename = "filename"))
    )
  })


  test_that("labels for specified columns", {
    tmp <- flu
    tmp@label$filename <- NULL
    expect_equal(
      labels(tmp, c("c", "filename", "file"), use.colnames = FALSE),
      structure(list(c = "c / (mg / l)", `NA` = NULL, `NA` = NULL),
        .Names = c("c", NA, NA)
      )
    )

    expect_equal(
      labels(flu, c("c", "filename", "file")),
      structure(list(c = "c / (mg / l)", filename = "filename", `NA` = NULL),
        .Names = c("c", "filename", NA)
      )
    )
  })
}


# Function -------------------------------------------------------------------

#' @rdname labels
#' @aliases labels<-,hyperSpec-method
#'
#' @usage
#' labels(object, which = NULL, ...) <- value
#'
#' @param value the new label(s)
#'
#' @return  `labels<-` returns a `hyperSpec` object.
#'
#' @export "labels<-"
#'
#' @concept labels
#'
#' @examples
#'
#' labels(flu, "c") <- expression("/"("c", "mg / l"))
`labels<-` <- function(object, which = NULL, ..., value) {
  chk.hy(object)
  validObject(object)

  if (is.null(which)) {
    object@label <- value
  } else {
    if ((is.character(which) && !which %in% colnames(object@data)) &&
      which != ".wavelength" || # neither a colname nor .wavelength
      (is.numeric(which) && (which < 1 || which > ncol(object@data) + 1)) ||
      (is.logical(which) && length(which) != ncol(object@data) + 1)
    # or outside valid indices
    ) {
      stop("Column to label does not exist!")
    }

    object@label[[which]] <- value
  }

  validObject(object)
  object
}


# Unit tests -----------------------------------------------------------------

# TODO: Add unit test for "set labels"
