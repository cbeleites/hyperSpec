
#' Convert between different wavelength units
#'
#' The following units can be converted into each other:
#' *nm*, \emph{\eqn{cm^{-1}}{inverse cm}}, *eV*, *THz* and *Raman shift*.
#'
#' @param x data for conversion
#' @param from source unit
#' @param to destination unit
#' @param ref_wl laser wavelength (required for work with Raman shift)
#' @author R. Kiselev
#' @export
#'
#' @concept wavelengths
#'
#' @examples
#' wl_convert_units(3200, "Raman shift", "nm", ref_wl = 785.04)
#' wl_convert_units(785, "nm", "invcm")
wl_convert_units <- function(x, from, to, ref_wl = NULL) {
  src  <- .wl_fix_unit_name(from)
  dest <- .wl_fix_unit_name(to)

  if (src == dest) {
    return(x)
  }

  if ((src == "raman" | dest == "raman") & is.null(ref_wl)) {
    stop("Working with Raman shift requires knowledge of laser wavelength")
  }

  f <- paste0("wl_", src, "2", dest)
  f <- get(f)
  return(f(x, ref_wl))
}

wl_ev2freq     <- function(x, ...)    wl_nm2freq(wl_ev2nm(x))
wl_ev2invcm    <- function(x, ...)    q * x / (100 * h * c)
wl_ev2nm       <- function(x, ...)    1e9 * h * c / (q * x)
wl_ev2raman    <- function(x, ref_wl) 1e7 / ref_wl - x * q / (100 * h * c)
wl_freq2ev     <- function(x, ...)    wl_nm2ev(wl_freq2nm(x))
wl_freq2invcm  <- function(x, ...)    wl_nm2invcm(wl_freq2nm(x))
wl_freq2nm     <- function(x, ...)    1e-3 * c / x
wl_freq2raman  <- function(x, ref_wl) wl_nm2raman(wl_freq2nm(x), ref_wl)
wl_invcm2ev    <- function(x, ...)    100 * x * c * h / q
wl_invcm2freq  <- function(x, ...)    wl_nm2freq(wl_invcm2nm(x))
wl_invcm2nm    <- function(x, ...)    1e7 / x
wl_invcm2raman <- function(x, ref_wl) 1e7 / ref_wl - x
wl_nm2ev       <- function(x, ...)    1e9 * h * c / (q * x)
wl_nm2freq     <- function(x, ...)    1e-3 * c / x
wl_nm2invcm    <- function(x, ...)    1e7 / x
wl_nm2raman    <- function(x, ref_wl) 1e7 * (1 / ref_wl - 1 / x)
wl_raman2ev    <- function(x, ref_wl) 100 * h * c * (1e7 / ref_wl - x) / q
wl_raman2freq  <- function(x, ref_wl) wl_nm2freq(wl_raman2nm(x, ref_wl))
wl_raman2invcm <- function(x, ref_wl) 1e7 / ref_wl - x
wl_raman2nm    <- function(x, ref_wl) 1e7 / (1e7 / ref_wl - x)


# Bring the argument to a conventional name
# FIXME: This function should be exported when `read.spe()` function is moved to
#        another package.
# FIXME: This function should be documented.
#        Even if it used for internal purposes.
#' @export
.wl_fix_unit_name <- function(unit) {
  unit <- gsub(" .*$", "", tolower(unit))
  if (unit %in% c("raman", "stokes", "rel", "rel.", "relative", "rel.cm-1", "rel.cm", "rel.1/cm", "raman shift")) {
    return("raman")
  }
  if (unit %in% c("invcm", "energy", "wavenumber", "cm-1", "inverted", "cm", "1/cm")) {
    return("invcm")
  }
  if (unit %in% c("nm", "nanometer", "wavelength")) {
    return("nm")
  }
  if (unit %in% c("ev", "electronvolt")) {
    return("ev")
  }
  if (unit %in% c("freq", "frequency", "thz", "terahertz")) {
    return("freq")
  }
  if (unit %in% c("pixel", "px", "sensor")) {
    return("px")
  }
  if (unit == "file") {
    return(unit)
  }
  stop(paste0("'", unit, "': Unknown unit type"))
}


# Some physical constants ----------------------------------------------------
# @concept constants
q <- 1.60217656535e-19 # elementary charge
h <- 6.6260695729e-34 # Planck's constant
c <- 299792458 # speed of light


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(wl) <- function() {

  context("get wl")

  test_that("wl() works", {
    # Data
    hy_obj <- new("hyperSpec", spc = matrix(1:100, nrow = 1), wavelength = 601:700)

    # Perform tests
    expect_silent(res <- wl(hy_obj))
    expect_true(is.numeric(res)) # Can be either integer or double
    expect_length(res, 100)
    expect_equal(res, 601:700)
  })


  context("set wl")

  test_that("`wl<-` works", {
    # Data
    hy_obj <- new("hyperSpec", spc = matrix(1:100, nrow = 1), wavelength = 601:700)

    # Set new wavelengths
    expect_silent(wl(hy_obj) <- (1:nwl(hy_obj)) + 1000)
    expect_true(is.numeric(hy_obj@wavelength))
    expect_equal(hy_obj@wavelength, 1001:1100)

    # Set new wavelengths and label
    expect_equal(labels(hy_obj, ".wavelength"), ".wavelength")
    expect_silent(
      wl(hy_obj) <- list(wl = 101:200, label = "new label")
    )
    expect_equal(hy_obj@wavelength, 101:200)
    expect_equal(labels(hy_obj, ".wavelength"), "new label")
  })

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hySpc.testthat::test(.wl_fix_unit_name) <- function() {

  context(".wl_fix_unit_name")

  test_that(".wl_fix_unit_name() works", {

    expect_equal(.wl_fix_unit_name("raman"), "raman")
    expect_equal(.wl_fix_unit_name("invcm"), "invcm")
    expect_equal(.wl_fix_unit_name("nm"),    "nm")
    expect_equal(.wl_fix_unit_name("ev"),    "ev")
    expect_equal(.wl_fix_unit_name("freq"),  "freq")
    expect_equal(.wl_fix_unit_name("px"),    "px")
    expect_equal(.wl_fix_unit_name("file"),  "file")
    expect_error(.wl_fix_unit_name("ddd"),   "Unknown unit type")

  })

  # TODO (tests): add more specific tests.
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hySpc.testthat::test(wl_convert_units) <- function() {

  context("wl_convert_units")

  test_that("wl_convert_units() throws error", {
    expect_error(wl_convert_units())

    expect_error(
      wl_convert_units(1000, "raman", "nm"),
      "Working with Raman shift requires knowledge of laser wavelength"
    )
    expect_error(wl_convert_units(1000, "non-existing", "nm"), "Unknown unit type")
    expect_error(wl_convert_units(1000, "nm", "non-existing"), "Unknown unit type")
  })


  test_that("wl_convert_units() output is correct if units do not change", {
    # No conversion is expected
    expect_equal(wl_convert_units(1000, "raman", "raman"), 1000)
    expect_equal(wl_convert_units(1000, "invcm", "invcm"), 1000)
    expect_equal(wl_convert_units(1000, "nm",    "nm"),    1000)
    expect_equal(wl_convert_units(1000, "ev",    "ev"),    1000)
    expect_equal(wl_convert_units(1000, "freq", "freq"),   1000)
  })


  test_that("wl_convert_units() returns correct data type", {

    x <- c("raman", "invcm", "nm", "ev", "freq")
    y <- expand.grid(x, x)
    y <- y[y[[1]] != y[[2]], ]

    expect_silent(
      d <- apply(y, MARGIN = 1, function(x) {
        wl_convert_units(10, x[["Var1"]], x[["Var2"]], 200)
      })
    )
    expect_is(d, "numeric")
  })


  # TODO (tests): Add expected results to the conversion grid and check against them.

  # test_that("wl_convert_units() performs conversion correctly", {
  #  # ...
  #
  # })

}

