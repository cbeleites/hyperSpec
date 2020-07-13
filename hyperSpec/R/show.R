# Fun: print -----------------------------------------------------------------

#' @name show
#' @rdname show
#' @aliases show show,hyperSpec-method
#'
#' @title Show brief summary of `hyperSpec` object.
#'
#' @description
#' Functions `show()`, `print()`, `summary()`, and `as.character()` calculate
#' and show a basic summary of a `hyperSpec` object.
#'
#' @details
#' Function `as.character()` does the main calculations. Functions `show()`,
#' `print()`, and `summary()` use `as.character()` and print its results with
#' different defaults:
#'
#' - `show()` prints the summary with the most basic information on `hyperSpec`
#'    object (number of rows, columns and spectra),
#' - `print()` by default does the same as `show()`,
#' - `summary()` by default in addition to the results of `show()`, prints the
#'   information on `@wavelength`s and each individual column of `@data`.
#'
#' @param object,x A `hyperSpec` object.
#' @param ... `print()` and `summary()` hand further arguments to `as.character()`.
#'
#' @return
#' After printing:
#'
#'  - `show()` invisibly returns `NULL`,
#'  - `print()` and `summary()` invisibly returns `x`.
#'
#' @keywords methods print
#' @concept summary
#'
#' @seealso [methods::show()], [base::print()], [base::summary()].
#'
#' @export
#'
#' @examples
#' faux_cell # Implicitly prints the object. The same as show(faux_cell)
#'
#' show(faux_cell)
#'
#' print(faux_cell)
#'
#' print(faux_cell, include = "data")
#'
#' print(faux_cell, range = TRUE, include = "data")
#'
#' # The difference between range = TRUE and FALSE
#' # is evident only when data is not sorted.
#' set.seed(1)
#' faux_cell_2 <- sample(faux_cell)
#' print(faux_cell_2, include = "data")
#'
#' print(faux_cell_2, range = TRUE, include = "data")
#'
#' summary(faux_cell)
#'
#' summary(faux_cell, include = c("wl", "data"))

setMethod("show", signature = signature(object = "hyperSpec"),
  function(object) {
    print(object, range = FALSE, include = "main")
    invisible(NULL)
  })


# Fun: show ------------------------------------------------------------------

#' @rdname show
#' @aliases print print,hyperSpec-method
#' @export

setMethod("print", signature = signature(x = "hyperSpec"),
  function(x, range = FALSE, include = "main", ...) {
    validObject(x)
    cat(as.character(x, range = range, include = include, ...), sep = "\n")
    invisible(x)
  })



# Fun: summary ---------------------------------------------------------------

#' @rdname show
#' @aliases summary summary,hyperSpec-method
#' @export

setMethod("summary",
  signature = signature(object = "hyperSpec"),
  function(object, ..., include = "all", range = FALSE) {
    print(object, ..., include = include, range = range)
  }
)


# Fun: as.character ----------------------------------------------------------

#' @rdname show
#' @docType methods
#' @aliases as.character
#'
#' @param digits Number of digits handed over to `format`.
#' @param range Should the values be indicated as a range (from the smallest to
#'        the largest value) rather than as first and last elements?
#' @param max.print Maximum number of elements to be printed (of a variable).
#' @param shorten.to If a vector is longer than `max.print`, only the
#'        first `shorten.to[1]` and the last `shorten.to[2]` elements are
#'        printed.
#' @param include Character vector that contains at least one (but usually
#'        several) of `"all"`, `"main"`,`"wl"`, or `"data"`:
#'
#'  - `"all"`: the same as `c("main", "wl", "data")`.
#'  - `"main"`: the output includes the number of spectra, as well as the number
#'    rows an columns in `@data` field of `hyperSpec` object.
#'  - `"wl"`: the output includes the summary of `@wavelength` field.
#'  - `"data`: the output includes the summary of each column in `@data` field.
#'
#' @return
#' Function `as.character()` returns a character vector with summary of
#' `hyperSpec`.
#'
#' @seealso [base::as.character()].
#'
#' @import methods
#' @include paste.row.R
#' @export
#'
#' @examples
#'
#' as.character(faux_cell)
#'
#' as.character(faux_cell, include = c("wl", "data"))

setMethod("as.character",
  signature = signature(x = "hyperSpec"),
  function(x, digits = getOption("digits"), range = FALSE,
    max.print = 5, shorten.to = c(2, 1), include = c("all", "main", "wl", "data")) {
    # Input checking ---------------------------------------------------------
    validObject(x)

    include <- match.arg(include, several.ok = TRUE)
    if ("all" %in% include) {
      include <- c("main", "wl", "data")
    }
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

      chr_data <-
        paste0("data: ", " (", nrow(x@data), " rows x ", n.cols, " columns)")

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
