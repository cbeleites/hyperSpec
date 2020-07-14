#' @rdname show
#' @docType methods
#' @import methods
#' @aliases as.character
#' @param digits number of digits handed over to `format`
#' @param range should the values be indicated as range rather then first and
#'   last elements?
#' @param max.print maximum number of elements to be printed (of a variable)
#' @param shorten.to if a vector is longer than `max.print`, only the
#'   first `shorten.to[1]` and the last `shorten.to[2]` elements are
#'   printed
#' @return `as.character` returns a character vector fit to be printed by
#'   `cat` with `sep = "\n"`.
#'
#' @seealso [base::as.character()]
#' @include paste.row.R
#' @importFrom tibble glimpse
#' @export

setMethod("as.character",
  signature = signature(x = "hyperSpec"),
  function(x, digits = getOption("digits"), range = TRUE,
           max.print = 5, shorten.to = c(2, 1)) {
    ## input checking
    validObject(x)

    if (is.null(max.print)) {
      max.print <- getOption("max.print")
    }

    if ((length(max.print) != 1) | !is.numeric(max.print)) {
      stop("max.print needs to be a number")
    }
    if ((length(shorten.to) < 1) | (length(shorten.to) > 2) | !is.numeric(shorten.to)) {
      stop("shorten.to needs to be a numeric vector with length 1 or 2")
    }
    if (sum(shorten.to) > max.print) {
      stop("sum (shorten.to) > max.print: this does not make sense.")
    }

    ## printing information
    chr <- c(
      "hyperSpec object",
      paste("  ", nrow(x), "spectra"),
      paste("  ", ncol(x), "data columns"),
      paste("  ", nwl(x), "data points / spectrum")
    )

    chr <- c(chr, .paste.row(x@wavelength, x@label$.wavelength, "wavelength",
      ins = 0, val = TRUE, range = FALSE,
      shorten.to = shorten.to, max.print = max.print
    ))

    n.cols <- ncol(x@data)

    chr <- c(chr, paste("data: ", " (", nrow(x@data), " rows x ", n.cols,
      " columns)",
      sep = ""
    ))

    if (n.cols > 0) {
      # glimpse at the contents of the extra data
      chr <- c(chr, paste0(strsplit(capture.output(glimpse(x$.)), split="\n")[-(1:2)]))
      }

    chr
  }
)

#' Convert a `hyperSpec` object to character strings for display.
#'
#' `print()`, `show()`, and `summary()` show the result of `as.character()`.
#'
#' @details
#' `print()`, `show()`, and `summary()` differ only in the defaults:
#'
#' - `show()` displays the range of values instead,
#' @name show
#' @rdname show
#' @aliases show show,hyperSpec-method
#' @param object a `hyperSpec` object
#' @seealso [methods::show()]
#' @keywords methods print
#' @export
#' @examples
#'
#' faux_cell
#'
#' show(faux_cell)
#'
#' summary(faux_cell)
#'
#' print(faux_cell, range = TRUE)
setMethod("show", signature = signature(object = "hyperSpec"), function(object) {
  print(object, range = TRUE)
  invisible(NULL)
})

#' @details
#' - `print()` shows the overview giving the first and last values of each
#' data column (fastest),
#' @aliases print print,hyperSpec-method
#' @param x a `hyperSpec` object
#' @param ... `print` and `summary`  hand further arguments to `as.character`
#' @return `print` invisibly returns `x` after printing, `show` returns
#'   an invisible `NULL`.
#' @rdname show
#' @export
#' @seealso [base::print()]
setMethod("print", signature = signature(x = "hyperSpec"), function(x, range = FALSE, ...) {
  validObject(x)
  cat(as.character(x, range = FALSE, ...), sep = "\n")
  invisible(x)
})


# FIXME: logbook is mentioned
# - `summary()` displays the logbook in addition.

#' @details
#' - `summary()` ...
#' @aliases summary summary,hyperSpec-method
#' @seealso [base::summary()]
#' @export
#' @rdname show
setMethod("summary",
  signature = signature(object = "hyperSpec"),
  function(object, ...) {
    print(object, ...)
  }
)
