# Fun: show ------------------------------------------------------------------
.show <- function(object) {
  print(object)
  invisible(NULL)
}

#' @name show
#' @rdname show
#' @aliases show show,hyperSpec-method
#'
#' @title Show brief summary of `hyperSpec` object
#'
#' @description
#' Functions `show()`, `print()`, `summary()`, and `as.character()` calculate
#' and show a basic summary of a `hyperSpec` object.
#'
#' @details
#' Function `as.character()` does the main calculations. Functions `show()`,
#' `print()`, and `summary()` use `as.character()` and print their results with
#' different defaults:
#'
#' - `show()` prints the summary with the most basic information on `hyperSpec`
#'    object (number of rows, columns and spectra),
#' - `print()`has the same default output `show()`,
#' - `summary()` prints a bit larger summary of `hyperSpec`, that includes
#'    the number of rows, columns and spectra, the information on
#'    `@wavelength`s, lists column names of `@data` as well as a preview of
#'    the smallest and the largest values.
#'
#' @param object,x A `hyperSpec` object.
#' @param ... `print()` and `summary()` hand further arguments to
#'        `as.character()`.
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
#' # show()
#' faux_cell # Implicitly prints the object. The same as show(faux_cell)
#'
#' show(faux_cell)
#'
#' # print()
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
#' # summary()
#' summary(faux_cell)
#'
#' summary(faux_cell, include = c("wl", "data"))
setMethod("show", signature = signature(object = "hyperSpec"), .show)


# Fun: print -----------------------------------------------------------------

.print <- function(x, range = FALSE, include = "main", ...) {
  validObject(x)
  cat(as.character(x, range = range, include = include, ...), sep = "\n")
  invisible(x)
}


#' @rdname show
#' @aliases print print,hyperSpec-method
#' @export

setMethod("print", signature = signature(x = "hyperSpec"), .print)



# Fun: summary ---------------------------------------------------------------
.summary <- function(object, ..., include = "all", range = TRUE) {
  print(object, ..., include = include, range = range)
}

#' @rdname show
#' @aliases summary summary,hyperSpec-method
#' @export

setMethod("summary", signature = signature(object = "hyperSpec"), .summary)


# Fun: as.character ----------------------------------------------------------
.as.character <- function(x, digits = getOption("digits"), range = FALSE,
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
    stop("sum(shorten.to) > max.print: this does not make sense.")
  }

  # Preparing information --------------------------------------------------
  # ~ Main summary ---------------------------------------------------------
  chr_main <- if ("main" %in% include) {
    c(
      "hyperSpec object",
      paste("  ", nrow(x), "spectra"),
      paste("  ", ncol(x), "data columns"),
      paste("  ", nwl(x), "data points / spectrum")
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
        ))
      }
    }
  } else {
    chr_data <- NULL
  }

  chr <- c(chr_main, chr_wl, chr_data)

  chr
}


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
#' @include paste_row.R
#' @export
#'
#' @examples
#'
#' as.character(faux_cell)
#'
#' as.character(faux_cell, include = c("wl", "data"))
setMethod("as.character", signature = signature(x = "hyperSpec"), .as.character)


# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(.show) <- function() {
  context("show")

  # Create data
  set.seed(1)
  spc <- matrix(rnorm(12), ncol = 4)
  hs <- new("hyperSpec",
    data = data.frame(
      x = letters[1:3],
      g = letters[11:13],
      y = 3:5,
      stringsAsFactors = FALSE
    ),
    spc = spc
  )

  # Perform tests
  test_that("show() works", {
    expect_output(show(hs))
  })

  test_that("show() gives correct output", {
    expect_output(show(hs), "hyperSpec object")
    expect_output(show(hs), "4 data points / spectrum")
  })
}


hySpc.testthat::test(.print) <- function() {
  context("print")

  # Create data
  set.seed(1)
  spc <- matrix(rnorm(12), ncol = 4)
  hs <- new("hyperSpec",
    data = data.frame(
      x = letters[1:3], g = letters[13:11], y = 3:5, stringsAsFactors = FALSE
    ),
    spc = spc
  )

  # Perform tests
  test_that("print() works", {
    expect_output(print(hs))
  })

  test_that("print() gives correct output", {
    expect_output(print(hs), "hyperSpec object")
    expect_output(print(hs), "4 data points / spectrum")

    expect_output(print(hs, range = TRUE, include = "data"), " range ")
    expect_output(print(hs, include = "wl"), "^wavelength: ")
  })

  test_that("print() does not give certain output", {
    expect_output(print(hs, include = "data"), "^(?!hyperSpec object)", perl = TRUE)
    expect_output(print(hs, include = "data"), "^(?!wavelength:)", perl = TRUE)

    expect_output(print(hs, include = "wl"), "^(?!hyperSpec object)", perl = TRUE)
  })
}


hySpc.testthat::test(.summary) <- function() {
  context("summary")

  # Create data
  set.seed(1)
  spc <- matrix(rnorm(12), ncol = 4)
  hs <- new("hyperSpec",
    data = data.frame(
      x = letters[1:3], g = letters[13:11], y = 3:5, stringsAsFactors = FALSE
    ),
    spc = spc
  )

  # Perform tests
  test_that("summary() works", {
    expect_output(summary(hs))
  })

  test_that("summary() gives correct output", {
    expect_output(summary(hs), "hyperSpec object")
    expect_output(summary(hs), "4 data points / spectrum")
    expect_output(summary(hs), "wavelength: ")
    expect_output(summary(hs), "data: ")

    expect_output(summary(hs, include = "data"), " range ")
  })
}



hySpc.testthat::test(.as.character) <- function() {
  context("as.character")

  # Create data
  set.seed(1)
  spc <- matrix(rnorm(12), ncol = 4)
  hs <- new("hyperSpec",
    data = data.frame(
      x = letters[1:3], g = letters[13:11], y = 3:5, stringsAsFactors = FALSE
    ),
    spc = spc
  )

  # Perform tests
  test_that("as.character() works", {
    expect_is(as.character(hs), "character")
  })

  test_that("as.character() gives correct output", {
    res <- as.character(hs)

    expect_length(res, 10)
    expect_match(res[1], "hyperSpec object")
    expect_match(res[4], "4 data points / spectrum")
    expect_match(res[5], "wavelength: ")
    expect_match(res[6], "data: ")
    expect_equal(res[7], "   1. x:  [character] a b c ")
    expect_equal(res[8], "   2. g:  [character] m l k ")
    expect_equal(res[9], "   3. y:  [integer] 3 4 5 ")

    res_rng <- as.character(hs, range = TRUE, include = "data")
    expect_match(res_rng[2:5], " range ")
    expect_equal(res_rng[3], "   2. g:  [character] range  k l m ")
  })

  test_that("max.print in as.character() works", {
    expect_is(as.character(hs, max.print = NULL), "character")

    expect_error(as.character(hs, max.print = FALSE))
    expect_error(as.character(hs, max.print = 1:3))
  })

  test_that("shorten.to in as.character() works", {
    expect_is(as.character(hs, shorten.to = 2:1), "character")

    expect_error(as.character(hs, shorten.to = 1:3))
    expect_error(as.character(hs, shorten.to = FALSE))
    expect_error(as.character(hs, shorten.to = 40))
  })
}
