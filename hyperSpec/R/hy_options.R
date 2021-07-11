

.options <- list(
  debuglevel = 0L,
  gc = FALSE,
  file.remove.emptyspc = TRUE,
  file.keep.name = TRUE,
  tolerance = sqrt(.Machine$double.eps),
  wl.tolerance = sqrt(.Machine$double.eps),
  plot.spc.nmax = 25,
  ggplot.spc.nmax = 10
)

#' Options for package \pkg{hyperSpec}
#'
#' Functions to access and set \pkg{hyperSpec}'s options.
#'
#' Currently, the following options are defined:
#' \tabular{llll}{
#' **Name**          \tab **Default Value (range)**      \tab **Description**                               \tab **Used by**\cr
#' debuglevel           \tab 0 (1L 2L 3L)                      \tab amount of debugging information produced         \tab [spc.identify()] [map.identify()]\cr
#'                      \tab                                   \tab                                                  \tab various file import functions\cr
#'                      \tab                                   \tab                                                  \tab [spc_fit_poly_below()]\cr
#' gc                   \tab FALSE                             \tab triggers frequent calling of gc ()               \tab [read.ENVI()], `new ("hyperSpec")`\cr
#' file.remove.emptyspc \tab TRUE                              \tab remove empty spectra directly on file import     \tab various file import functions\cr
#' file.keep.name       \tab TRUE                              \tab always create filename column                    \tab various file import functions\cr
#' tolerance            \tab `sqrt (.Machine$double.eps)` \tab tolerance for numerical comparisons              \tab [normalize01()], file import: `file.remove.emptyspc`\cr
#' wl.tolerance         \tab `sqrt (.Machine$double.eps)` \tab tolerance for comparisons of the wavelength axis \tab [all.equal()], [collapse()], [rbind()]\cr
#' plot.spc.nmax        \tab 25                                \tab number of spectra to be plotted by default       \tab [plotspc()]\cr
#' ggplot.spc.nmax      \tab 10                                \tab                                                  \tab [`qplotspc()`](https://r-hyperspec.github.io/hySpc.ggplot2/reference/qplotspc.html)\cr
#' }
#'
#' `hy.setOptions` will discard any values that were given without a  name.
#'
#' @rdname options
#' @param ... `hy.setOptions`: pairs of argument names and values.
#'
#' `hy.getOptions`: indices (or names) of the options.
#' @return
#' \tabular{ll}{
#' `hy.getOptions` \tab returns a list of all options\cr
#' `hy.setOptions` \tab invisibly returns a list with the options \cr
#' `hy.getOption`  \tab returns the value of the requested option \cr
#' }
#' @author C. Beleites
#'
#' @export
#'
#' @keywords misc
#' @concept utils
#'
#' @examples
#'
#' hy.getOptions()
hy.getOptions <- function(...) {
  dots <- c(...)
  if (length(dots) == 0L) {
    .options
  } else {
    .options[dots]
  }
}


hySpc.testthat::test(hy.getOptions) <- function() {
  context("hy.getOptions")

  test_that("proper return", {
    hy.opts <- get(".options", asNamespace("hyperSpec"))
    expect_equal(hy.getOptions(), hy.opts)

    expect_equal(
      hy.getOptions("debuglevel"),
      hy.opts["debuglevel"]
    )

    .options <- list()
    expect_equal(hy.getOptions(), hy.opts)
  })
}

#' @rdname options
#' @export
#'
#' @concept utils
#'
#' @param name the name of the option
hy.getOption <- function(name) {
  .options[[name]]
}

#' @rdname options
#' @export
#'
#' @concept utils
#'
#' @importFrom utils modifyList
hy.setOptions <- function(...) {
  new <- list(...)

  ## if called with list in 1st argument, use that list
  if (length(new) == 1 && is.list(new[[1]])) {
    new <- new[[1]]
  }

  names <- nzchar(names(new))

  if (!all(names) || length(names) != length(new)) {
    warning("options without name are discarded: ", which(!names))
  }

  opts <- modifyList(.options, new[names])

  opts$tolerance <- .checkpos(opts$tolerance, "tolerance")
  opts$wl.tolerance <- .checkpos(opts$wl.tolerance, "wl.tolerance")

  assign(".options", opts, envir = asNamespace("hyperSpec"))

  invisible(opts)
}

## check particular options that should exist and be finite and strictly positive
.checkpos <- function(opt, name) {
  if (length(opt) != 1L || !is.finite(opt) || opt < .Machine$double.eps) {
    warning(name, " must be a strictly positive finite number => set to .Machine$double.eps (", .Machine$double.eps, ").")
    opt <- .Machine$double.eps
  }

  opt
}

hySpc.testthat::test(hy.setOptions) <- function() {
  context("hy.setOptions")

  old <- hy.getOptions()
  on.exit(hy.setOptions(old))

  test_that("new option and proper return value", {
    expect_equal(hy.setOptions(bla = 1)$bla, 1)
    expect_equal(hy.getOption("bla"), 1)
  })

  test_that("setting", {
    tmp <- hy.setOptions(debuglevel = 27)
    expect_equal(tmp$debuglevel, 27)

    tmp <- hy.setOptions(list(debuglevel = 20))
    expect_equal(tmp$debuglevel, 20)

    tmp <- hy.setOptions(debuglevel = 27, tolerance = 4)
    expect_equal(tmp$debuglevel, 27)
    expect_equal(tmp$tolerance, 4)

    tmp <- hy.setOptions(list(debuglevel = 20, tolerance = 5))
    expect_equal(tmp$debuglevel, 20)
    expect_equal(tmp$tolerance, 5)
  })

  test_that("restrictions on tolerances", {
    for (o in c("tolerance", "wl.tolerance")) {
      expect_warning(hy.setOptions(structure(list(0), .Names = o)))
      expect_equal(hy.getOption(o), .Machine$double.eps, label = o)

      hy.setOptions(structure(list(1), .Names = o))
      expect_equal(hy.getOption(o), 1)
      expect_warning(hy.setOptions(structure(list(-1), .Names = o)))
      expect_equal(hy.getOption(o), .Machine$double.eps, label = o)

      hy.setOptions(structure(list(1), .Names = o))
      expect_equal(hy.getOption(o), 1)
      expect_warning(hy.setOptions(structure(list(NA), .Names = o)))
      expect_equal(hy.getOption(o), .Machine$double.eps, label = o)
    }

    expect_warning(hy.setOptions(tolerance = NULL))
    expect_equal(hy.getOption("tolerance"), .Machine$double.eps)

    expect_warning(hy.setOptions(wl.tolerance = NULL))
    expect_equal(hy.getOption("wl.tolerance"), .Machine$double.eps)
  })


  test_that("options must be named", {
    tmp.a <- hy.getOptions()
    expect_warning(tmp.b <- hy.setOptions(1))
    expect_equal(tmp.a, tmp.b)
  })
}
