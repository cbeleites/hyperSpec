#' @rdname show
#' @aliases print print,hyperSpec-method
#'
#'
#' @title Convert a `hyperSpec` object to character strings for display.
#'
#'
#' @description
#' Functions `print()`, `show()`, and `summary()` show the summary of `hyperSpec`
#' object, which is technically a result of `as.character()`.
#'
#' @details
#' Functions `print()`, `show()`, and `summary()` differ only in the defaults:
#'
#' - `print()` shows the overview giving the first and last values of each
#' data column (fastest),
#'
#' @param x a `hyperSpec` object.
#' @param ... `print` and `summary` hand further arguments to `as.character`.
#' @return `print` invisibly returns `x` after printing, `show` returns
#'         an invisible `NULL`.
#'
#' @seealso [base::print()]
#'
#' @export
#'
#' @examples
#'
#' faux_cell  # Implicitly prints the object
#'
#' show(faux_cell)
#'
#' summary(faux_cell)
#'
#' print(faux_cell, range = TRUE)
#'
#' as.character(faux_cell)

setMethod("print", signature = signature(x = "hyperSpec"),
  function(x, range = FALSE, ...) {
    validObject(x)
    cat(as.character(x, range = range, ...), sep = "\n")
    invisible(x)
  })


#' @name show
#' @rdname show
#' @aliases show show,hyperSpec-method
#'
#' @details
#' - `show()` displays the range of values instead,
#' @param object a `hyperSpec` object
#'
#' @seealso [methods::show()]
#' @keywords methods print
#'
#' @export

setMethod("show", signature = signature(object = "hyperSpec"), function(object) {
  print(object, range = TRUE)
  invisible(NULL)
})


# FIXME: logbook is mentioned
# - `summary()` displays the logbook in addition.

#' @rdname show
#' @aliases summary summary,hyperSpec-method
#'
#' @details
#' - `summary()` ...
#'
#' @seealso [base::summary()]
#'
#' @export

setMethod("summary",
  signature = signature(object = "hyperSpec"),
  function(object, ...) {
    print(object, ...)
  }
)

#' @rdname show
#' @docType methods
#' @aliases as.character
#'
#' @param digits number of digits handed over to `format`.
#' @param range should the values be indicated as range rather then first and
#'        last elements?
#' @param max.print maximum number of elements to be printed (of a variable).
#' @param shorten.to if a vector is longer than `max.print`, only the
#'        first `shorten.to[1]` and the last `shorten.to[2]` elements are
#'        printed.
#' @param include Character vector that contains at least one of `"main"`,
#'       `"wl"`, or `"data"`. If the following sting is icluded:
#'
#'  - `"main"`: the output includes the number of spectra, as well as the number
#'    rows an columns in `@data` field of `hyperSpec` object.
#'  - `"wl"`: the output includes the summary of `@wavelength` field.
#'  - `"data`: the output includes the summary of each column in `@data` field.
#' @return `as.character` returns a character vector with summary of `hyperSpec`
#'
#' @seealso [base::as.character()]
#'
#' @import methods
#' @include paste.row.R
#' @export

setMethod("as.character",
  signature = signature(x = "hyperSpec"),
  function(x, digits = getOption("digits"), range = TRUE,
    max.print = 5, shorten.to = c(2, 1), include = c("main", "wl", "data")) {
    # Input checking ---------------------------------------------------------
    validObject(x)

    include <- match.arg(include, several.ok = TRUE)

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

    # Preparing information --------------------------------------------------
    # ~ Main summary ---------------------------------------------------------
    chr_main <- if ("main" %in% include) {
      c(
        "hyperSpec object",
        paste("  ", nrow(x), "spectra"),
        paste("  ", ncol(x), "data columns"),
        paste("  ", nwl(x),  "data points / spectrum")
      )

    } else {
      NULL
    }

    if (all(include %in% "main")) {
      return(chr_main)
    }

    # ~ Wavelength summary ----------------------------------------------------
    chr_wl <- if ("wl" %in% include) {
      .paste.row(x@wavelength, x@label$.wavelength, "wavelength",
        ins = 0, val = TRUE, range = FALSE,
        shorten.to = shorten.to, max.print = max.print
      )
    } else {
      NULL
    }

    if (all(include %in% "wl")) {
      return(chr_wl)

    } else if (all(include %in% c("main", "wl"))) {
      return(c(chr_main, chr_wl))
    }

    # ~ Summary of each column ------------------------------------------------
    if ("data" %in% include) {
      n.cols <- ncol(x@data)

      chr_data <- paste0("data: ", " (", nrow(x@data), " rows x ", n.cols, " columns)")

      if (n.cols > 0) {
        for (n in names(x@data)) {
          chr_data <- c(chr_data, .paste.row(x@data[[n]], x@label[[n]], n,
              ins = 3, i = match(n, names(x@data)), val = TRUE, range = range,
              shorten.to = shorten.to, max.print = max.print
            )
          )
        }
      }

    } else {
      NULL
    }

    chr <- c(chr_main, chr_wl, chr_data)

    chr
  }
)
