# Set generic ----------------------------------------------------------------

#' @importFrom lattice latticeParseFormula
setGeneric("levelplot", package = "lattice")

###--------------------------------------------------------------------------~
###  levelplot.R - everything that has to do with levelplot-like plotting:
###
###  levelplot is used by plotmap, plotvoronoi
###
###--------------------------------------------------------------------------~

# Function -------------------------------------------------------------------
# the workhorse function

#' @importFrom utils modifyList
.levelplot <- function(x, data, transform.factor = TRUE, ...,
                       contour = FALSE, useRaster = !contour) {
  validObject(data)

  data$.row <- row.seq(data)

  ## parse formula to find the columns to be plotted
  ## they may include also "wavelength"
  parsed.formula <- latticeParseFormula(x,
    as.long.df(data[1, , 1, wl.index = TRUE], rownames = TRUE),
    dimension = 3
  )
  use.x <- parsed.formula$right.x.name
  use.y <- parsed.formula$right.y.name
  use.z <- parsed.formula$left.name

  dots <- list(..., contour = contour, useRaster = useRaster)

  ## if spc is used as z and the data set has multiple wavelengths cut and warn
  if (use.z == "spc" && nwl(data) > 1 &&
    !any(grepl(".wavelength", c(
      as.character(x),
      as.character(dots$groups),
      as.character(dots$subset)
    )))) {
    data <- data[, , 1, wl.index = TRUE]
    warning("Only first wavelength is used for plotting")
  }

  dots <- modifyList(
    list(
      xlab = data@label[[use.x]],
      ylab = data@label[[use.y]]
    ),
    dots
  )

  if (any(grepl("spc", c(
    as.character(x),
    as.character(dots$groups),
    as.character(dots$subset)
  )))) {
    data <- as.long.df(data,
      rownames = TRUE,
      wl.factor = ".wavelength" %in% c(
        as.character(dots$groups),
        as.character(dots$subset),
        names(parsed.formula$condition)
      )
    )
  } else {
    data <- data$..
    data$.rownames <- as.factor(rownames(data))
  }



  if (is.factor(data[[use.z]]) && transform.factor) {
    dots <- trellis.factor.key(data[[use.z]], dots)
    data[[use.z]] <- as.numeric(data[[use.z]])
  }

  do.call(levelplot, c(list(x, data), dots))
}

#' @rdname levelplot
#' @export
setMethod("levelplot",
  signature = signature(x = "formula", data = "hyperSpec"),
  definition = .levelplot
)


# Function -------------------------------------------------------------------

.levelplot_h_ <- function(x, data, ...) {
  .levelplot(x = formula(spc ~ .wavelength * .row), data = x, ...)
}

#' @rdname levelplot
#'
#' @param transform.factor If the color-coded variable is a factor, should
#'   [trellis.factor.key()] be used to compute the color coding and
#'   legend?
#' @param contour,useRaster see  [lattice::levelplot()]
#'
#' @include plotmap.R
#' @importFrom lattice levelplot
#' @export
#'
#' @concept plotting
#' @concept plot generation
#'
#' @seealso  [lattice::levelplot()]
#'
#'  [trellis.factor.key()] for improved color coding of factors
setMethod("levelplot",
  signature = signature(x = "hyperSpec", data = "missing"),
  .levelplot_h_
)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.levelplot) <- function() {
  context(".levelplot")

  test_that("no errors", {
    ## just check that no errors occur
    expect_silent(levelplot(laser, contour = TRUE, col = "#00000080"))

    ## applying a function before plotting
    expect_silent(plotmap(faux_cell, func = max, col.regions = gray(seq(0, 1, 0.05))))

    expect_silent(plotmap(faux_cell, region ~ x * y, transform.factor = FALSE))
    expect_silent(plotmap(faux_cell, region ~ x * y, col.regions = gray(seq(0, 1, 0.05))))
  })
}

