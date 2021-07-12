### --------------------------------------------------------------------------~
###
### write.txt.long
###
###

#' @name DEPRECATED-write_txt
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Export `hyperSpec` objects to ASCII (text) files
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead.
#'
#' `_____________`
#'
#' These functions write `hyperSpec` objects to text files.
#'
#' @param file filename or connection.
#' @param object the `hyperSpec` object.
#' @param cols the column names specifying the column order.
#' @param order which columns should be [base::order()]ed? Parameter `order` is
#'        used as index vector into a `data.frame` with columns given by `cols`.
#' @param na.last handed to [base::order()] by `write.txt.long`.
#' @param quote,sep,col.names,row.names have their usual meaning (see
#'        [utils::write.table()]), but different default values.
#'
#' For file import, `row.names` should usually be `NULL` so that the first
#'        column becomes a extra data column (as opposed to row names of the
#'        extra data).
#' @param col.labels Should the column labels be used rather than the
#'        colnames?
#' @param append Should the output be appended to an existing file?
#' @param decreasing logical vector giving the sort order.
#' @param header.lines Toggle one or two line header (wavelengths in the
#'        second header line) for `write.txt.wide`.
#' @param ... arguments handed to [utils::write.table()].
#'
#'
#'
#' @importFrom utils write.table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' vignette("fileio")
#' }
#'
#' ## export & import matlab files
#' if (require(R.matlab)) {
#'   # export to matlab file
#'   writeMat(paste0(tempdir(), "/test.mat"),
#'     x = flu[[]], wavelength = flu@wavelength,
#'     label = lapply(flu@label, as.character)
#'   )
#'
#'   # reading a matlab file
#'   data <- readMat(paste0(tempdir(), "/test.mat"))
#'   print(data)
#'   mat <- new("hyperSpec",
#'     spc = data$x,
#'     wavelength = as.numeric(data$wavelength),
#'     label = data$label[, , 1]
#'   )
#' }
#'
#' ## ascii export & import
#'
#'
#' write.txt.long(flu,
#'   file = paste0(tempdir(), "/flu.txt"),
#'   cols = c(".wavelength", "spc", "c"),
#'   order = c("c", ".wavelength"),
#'   decreasing = c(FALSE, TRUE)
#' )
#'
#' read.txt.long(
#'   file = paste0(tempdir(), "/flu.txt"),
#'   cols = list(
#'     .wavelength = expression(lambda / nm),
#'     spc = "I / a.u", c = expression("/"(c, (mg / l)))
#'   )
#' )
#'
#' write.txt.wide(flu,
#'   file = paste0(tempdir(), "/flu.txt"),
#'   cols = c("c", "spc"),
#'   col.labels = TRUE, header.lines = 2, row.names = TRUE
#' )
#'
#' write.txt.wide(flu,
#'   file = paste0(tempdir(), "/flu.txt"),
#'   col.labels = FALSE, row.names = FALSE
#' )
#'
#' read.txt.wide(
#'   file = paste0(tempdir(), "/flu.txt"),
#'   # give columns in same order as they are in the file
#'   cols = list(
#'     spc = "I / a.u",
#'     c = expression("/"("c", "mg/l")),
#'     filename = "filename",
#'     # plus wavelength label last
#'     .wavelength = "lambda / nm"
#'   ),
#'   header = TRUE
#' )

write.txt.long <- function(object,
                           file = "",
                           order = c(".rownames", ".wavelength"),
                           na.last = TRUE, decreasing = FALSE,
                           quote = FALSE,
                           sep = "\t",
                           row.names = FALSE,
                           cols = NULL,
                           col.names = TRUE,
                           col.labels = FALSE, # use labels instead of column names?
                           append = FALSE,
                           ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("write_txt_long")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  validObject(object)

  col.spc <- match("spc", colnames(object@data))

  X <- as.long.df(object, rownames = TRUE)

  if (!is.null(order)) {
    if (is.character(order)) {
      tmp <- match(order, colnames(X))
      if (any(is.na(tmp))) {
        stop(
          "write.txt.long: no such columns: ",
          paste(order [is.na(tmp)], collapse = ", ")
        )
      }
      order <- tmp
    }


    if (length(decreasing) < length(order)) {
      decreasing <- rep(decreasing, length.out = length(order))
    }

    order.data <- as.list(X [, order, drop = FALSE])

    for (i in seq_along(order)) {
      if (is.factor(order.data[[i]])) {
        order.data[[i]] <- rank(order.data[[i]], na.last = na.last | is.na(na.last))
      }

      if (decreasing [i]) {
        order.data[[i]] <- -order.data[[i]]
      }
    }

    X <- X[do.call(
      "order",
      c(order.data, na.last = na.last | is.na(na.last), decreasing = FALSE)
    ), ]
  }

  if (is.na(na.last)) {
    X <- X[!is.na(X$spc), ]
  }

  if (!is.null(cols)) {
    X <- X [, cols, drop = FALSE]
  }

  if (!row.names) {
    X$.rownames <- NULL
  } else {
    cln [match(".rownames", cln)] <- "row"
  }

  if (col.names) {
    if (col.labels) {
      cln <- match(colnames(X), names(object@label))
      cln[!is.na(cln)] <- object@label [cln[!is.na(cln)]]
      cln[is.na(cln)] <- colnames(X) [is.na(cln)]
      cln <- sapply(cln, as.character)
    } else {
      cln <- colnames(X)
    }

    write.table(matrix(cln, nrow = 1),
      file = file, append = append,
      quote = quote, sep = sep, row.names = FALSE, col.names = FALSE
    )
    append <- TRUE
  }

  write.table(X, file,
    append = append, quote = quote, sep = sep,
    row.names = FALSE, col.names = FALSE, ...
  )
}
