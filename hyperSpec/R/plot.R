### -------------------------------------------------------------------------~
###
###  plot methods
###
### -------------------------------------------------------------------------~

# Set generic ----------------------------------------------------------------
#' @noRd
#' @export
setGeneric("plot")


# Functions ------------------------------------------------------------------

# .plot: main switchyard for plotting functions

#' @importFrom utils modifyList
.plot <- function(x, y, ...) {
  ##    'spc'        ... spectra
  ##    'map'        ... map
  ##    'voronoi'    ... voronoi tiled map
  ##    'mat'        ... spectra matrix
  ##    'c'          ... concentration: plotc
  ##    'ts'         ... time series: plotc
  ##    'depth'      ... concentration or time series
  ##    'spcmeansd'  ... mean spectrum +- 1 standard deviation
  ##    'spcprctile' ... median spectrum , 16th and 84th percentile
  ##    'spcprctl5'  ... 'spcprctile' plus 5th and 95th percentile

  dots <- list(...) # to allow optional argument checks

  if (missing(y)) {
    stop(
      "second argument to plot is missing. ",
      "Should be a character indicating the type of plot."
    )
    y <- "spc"
  }

  switch(tolower(y),
    spc = plotspc(x, ...),
    spcmeansd = {
      dots <- modifyList(
        list(object = mean_pm_sd(x), fill = c(1, NA, 1)),
        dots
      )

      do.call(plotspc, dots)
    },
    spcprctile = {
      dots <- modifyList(
        list(object = quantile(x, probs = c(0.16, 0.5, 0.84)), fill = c(1, NA, 1)),
        dots
      )

      do.call(plotspc, dots)
    },
    spcprctl5 = {
      dots <- modifyList(
        list(
          object = quantile(x, probs = c(0.05, 0.16, 0.5, 0.84, 0.95)),
          fill = c(1, 2, 3, 2, 1), fill.col = c("#00000040")
        ),
        dots
      )

      do.call(plotspc, dots)
    },
    map = plotmap(x, ...),
    voronoi = plotvoronoi(x, ...),
    mat = plotmat(x, ...),
    c = plotc(x, ...),
    ts = plotc(x, spc ~ t, ...),
    depth = plotc(x, spc ~ z, ...),
    stop(paste("y = ", y, "unknown.", collapse = " "))
  )
}

.plot_h_ <- function(x, y, ...) {
  plotspc(x, ...)
}



#' Plotting `hyperSpec` objects
#'
#' @description
#' The `plot` method for `hyperSpec` objects is a switchyard to [plotspc()],
#' [plotmap()], and [plotc()]. The function also supplies some convenient
#' abbreviations for frequently used plots (see 'Details').
#'
#'
#' @details
#' Supported values for `y` are:
#'
#' \describe{
#'
#'    \item{"spc" or nothing}{calls [plotspc()] to produce a spectra plot.}
#'
#'    \item{"spcmeansd"}{plots mean spectrum +/- one standard deviation}
#'
#'    \item{"spcprctile"}{plots 16th, 50th, and 84th percentile spectra. If the
#'    distributions of the intensities at all wavelengths were normal, this
#'    would correspond to `"spcmeansd"`. However, this is frequently not the
#'    case.
#'    Then `"spcprctile"` gives a better impression of the spectral data set.}
#'
#'    \item{"spcprctl5"}{like `"spcprctile"`, but additionally the 5th and
#'    95th percentile spectra are plotted.}
#'
#'    \item{"map"}{calls [plotmap()] to produce a map plot.}
#'
#'    \item{"voronoi"}{calls [plotvoronoi()] to produce a Voronoi plot
#'    (tessellated plot, like "map" for hyperSpec objects with
#'    uneven/non-rectangular grid).}
#'
#'    \item{"mat"}{calls [plotmat()] to produce a plot of the spectra matrix
#'    (not to be confused with [graphics::matplot()]).}
#'
#'    \item{"c"}{calls [plotc()] to produce a calibration (or time series,
#'     depth-profile, or the like).}
#'
#'    \item{"ts"}{plots a time series: abbreviation for
#'    [`plotc(x, use.c = "t")`][`plotc()`].}
#'
#'    \item{"depth"}{plots a depth profile:
#'     abbreviation for [`plotc(x, use.c = "z")`][`plotc()`].}
#' }
#'
#' @name plot-methods
#' @rdname plot
#' @aliases plot plot,ANY,ANY-method plot,hyperSpec,character-method
#'   plot,hyperSpec,missing-method
#' @docType methods
#'
#' @param x `hyperSpec` object.
#' @param y String (`"spc"`, `"map"`, etc.) to select what type of plot should
#'       be produced. See section 'Details' for available values.
#'       If `y` is missing, `plot(x)` behaves like `plot(x, y = "spc")`.
#' @param ... Arguments passed to the respective plot function
#'
#' @author C. Beleites
#'
#' @seealso
#' [plotspc()] for spectra plots (intensity over wavelength),
#'
#' [plotmap()] for plotting maps, i.e. color coded summary value on two
#' (usually spatial) dimensions.
#'
#' [plotc()]
#'
#' [graphics::plot()]
#' @keywords methods hplot
#'
#' @concept plotting
#' @concept plot generation
#'
#' @export
#' @examples
#'
#' plot(flu)
#'
#' plot(flu, "c")
#'
#' plot(laser, "ts")
#'
#' spc <- apply(faux_cell, 2, quantile, probs = 0.05)
#' spc <- sweep(faux_cell, 2, spc, "-")
#' plot(spc, "spcprctl5")
#' plot(spc, "spcprctile")
#' plot(spc, "spcmeansd")
#'
#' ### Use plotspc() as a default plot function.
setMethod("plot", signature(x = "hyperSpec", y = "missing"), .plot_h_)

### allow choice of plot type by second argument:
#' @rdname plot
#' @export
setMethod("plot", signature(x = "hyperSpec", y = "character"), .plot)



# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.plot) <- function() {
  context("plot")
  # To update reference data for visual unit tests, run:
  # vdiffr::manage_cases(package = "./hyperSpec")

  test_that("warnings and errors in plot()", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_silent(hy_spectra <- generate_hy_spectra())
    expect_silent(hy_profile <- generate_hy_profile())
    expect_silent(hy_map <- generate_hy_map())

    # Regular tests: warnings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_warning(plot(hy_spectra, "ts"), "Intensity at first wavelengh only is used.")
    expect_warning(plot(hy_spectra, "c"), "Intensity at first wavelengh only is used.")

    # Regular tests: errors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_error(plot(hy_spectra, "depth"), "object 'z' not found")
    expect_error(plot(hy_spectra[0, ]), "No spectra.")
    expect_error(plot(hy_spectra, xoffset = "a"), "xoffset must be a numeric")
    expect_error(plot(hy_spectra, func = "a"), "func needs to be a function")

    expect_error(plot(hy_spectra, "???"), "??? unknown")
    expect_error(plot(hy_spectra, contour = TRUE))
  })


  # Lattice-based plots
  test_that("lattice-based plot() gives expected output", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_silent(hy_spectra <- generate_hy_spectra())
    expect_silent(hy_profile <- generate_hy_profile())
    expect_silent(hy_map <- generate_hy_map())

    # Preparation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    plot_c <- plot(hy_profile, "c")
    plot_ts <- plot(hy_profile, "ts")
    plot_depth <- plot(hy_profile, "depth")

    plot_map <- plot(hy_map, "map")
    plot_voronoi <- plot(hy_map, "voronoi")

    # Perform tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Regular tests
    expect_silent(plot_c)
    expect_silent(plot_ts)
    expect_silent(plot_depth)

    expect_silent(plot_map)
    expect_silent(plot_voronoi)

    # Visual tests
    vdiffr::expect_doppelganger("plot-c", plot_c)
    vdiffr::expect_doppelganger("plot-ts", plot_ts)
    vdiffr::expect_doppelganger("plot-depth", plot_depth)


    # Skip if R < 4.0.0 (due to different defaults)
    # Reason: these visual tests fail on CI system
    r_version <-
      numeric_version(paste0(R.Version()[c("major", "minor")], collapse = "."))

    skip_if(r_version < "4.0.0", "R version is < 4.0.0")

    vdiffr::expect_doppelganger("plot-map", plot_map)
    vdiffr::expect_doppelganger("plot-voronoi", plot_voronoi)
  })


  # Base R based plots
  test_that("base R based plot() gives expected output", {

    # Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    expect_silent(hy_spectra <- generate_hy_spectra())
    expect_silent(hy_profile <- generate_hy_profile())
    expect_silent(hy_map <- generate_hy_map())


    # Preparation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    plot_1           <- function() plot(hy_spectra)
    plot_spc         <- function() plot(hy_spectra, "spc")
    plot_spcmeansd   <- function() plot(hy_spectra, "spcmeansd")
    plot_spcprctile  <- function() plot(hy_spectra, "spcprctile")
    plot_spcprctl5   <- function() plot(hy_spectra, "spcprctl5")
    plot_mat         <- function() plot(hy_spectra, "mat")
    plot_mat_contour <- function() plot(hy_spectra, "mat", contour = TRUE)

    plot_1_rev       <- function() plot(hy_spectra, wl.reverse = TRUE)
    plot_1_fill      <- function() plot(hy_spectra, fill = TRUE)


    # Perform tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Regular tests
    expect_silent(plot_1())
    expect_silent(plot_spc())
    expect_silent(plot_spcmeansd())
    expect_silent(plot_spcprctile())
    expect_silent(plot_spcprctl5())
    expect_silent(plot_1_rev())
    expect_silent(plot_1_fill())
    expect_silent(plot_mat())
    expect_silent(plot_mat_contour())

    # Visual tests
    vdiffr::expect_doppelganger("plot", plot_1)
    vdiffr::expect_doppelganger("plot-spc", plot_spc)
    vdiffr::expect_doppelganger("plot-spcmeansd", plot_spcmeansd)
    vdiffr::expect_doppelganger("plot-spcprctile", plot_spcprctile)
    vdiffr::expect_doppelganger("plot-spcprctl5", plot_spcprctl5)
    vdiffr::expect_doppelganger("plot_1_rev", plot_1_rev)
    vdiffr::expect_doppelganger("plot_1_fill", plot_1_fill)


    # These tests are skipped on CI systems, as they fail on R devel.
    skip_if(
      isTRUE(as.logical(Sys.getenv("CI"))), # if on CI system
      "Failures on devel version of R"
    )

    vdiffr::expect_doppelganger("plot-mat", plot_mat)
    vdiffr::expect_doppelganger("plot-mat-contour", plot_mat_contour)
  })
}
