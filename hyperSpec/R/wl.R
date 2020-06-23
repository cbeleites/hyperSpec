#' Getting and Setting the Wavelength Axis.
#'
#' `wl()` returns the wavelength axis, `wl<-` sets it.
#'
#' The wavelength axis of a `hyperSpec` object can be retrieved and
#' replaced with `wl` and `wl<-`, respectively.
#'
#' When the wavelength axis is replaced, the colnames of `x@@data$spc` are
#' replaced by the rounded new wavelengths.  `digits` specifies the how
#' many significant digits should be used.
#'
#' There are two ways to set the label of the new wavelength axis, see the
#' examples.  If no label is given, a warning will be issued.
#'
#' @aliases wl
#' @param x a `hyperSpec` object
#' @return a numeric vector
#' @note `wl<-` always sets the complete wavelength axis, without
#'   changing the columns of the spectra matrix. If you rather want to cut the
#'   spectral range, use \code{\link[hyperSpec:extractreplace]{[}}, for
#'   interpolation along the spectral axis see
#'   [hyperSpec::spc.loess()] and for spectral binning
#'   [hyperSpec::spc.bin()].
#' @author C. Beleites
#' @export
#' @seealso [base::signif()]
#'
#' cutting the spectral range: \code{\link[hyperSpec:extractreplace]{[}}
#'
#' interpolation along the spectral axis: [hyperSpec::spc.loess()]
#'
#' spectral binning: [hyperSpec::spc.bin()]
#' @examples
#'
#' wl(laser)
wl <- function(x) {
  chk.hy(x)
  validObject(x)

  x@wavelength
}

### -----------------------------------------------------------------------------
###
###  .wl
###
###
".wl<-" <- function(x, value) {
  x@wavelength <- value
  spc <- .fix_spc_colnames(x)

  x
}

#' @rdname wl
#' @export "wl<-"
#' @aliases wl<-
#' @usage
#' wl (x, label=NULL, digits=6) <- value
#'
#' @param value either a numeric containing the new wavelength vector, or a
#'   list with `value$wl` containing the new wavelength vector and
#'   `value$label` holding the corresponding `label`.
#' @param label The label for the new wavelength axis. See [initialize]
#'   for details.
#' @param digits handed to [base::signif()]. See details.
#' @return `hyperSpec` object
#' @examples
#' # convert from wavelength to frequency
#' plot(laser)
#' wl(laser, "f / Hz") <- 2.998e8 * wl(laser) * 1e9
#' plot(laser)
#'
#' # convert from Raman shift to wavelength
#' # excitation was at 785 nm
#' plot(faux_cell [1])
#' wl(faux_cell) <- list(
#'   wl = 1e7 / (1e7 / 785 - wl(faux_cell)),
#'   label = expression(lambda / nm)
#' )
#' plot(faux_cell [1])
"wl<-" <- function(x, label = NULL, digits = 6, value) {
  chk.hy(x)
  validObject(x)

  if (is.list(value)) {
    label <- value$label
    value <- value$wl
  }

  .wl(x) <- value

  x@label$.wavelength <- label

  validObject(x)

  x
}


### -----------------------------------------------------------------------------
#' Convert different wavelength units.
#'
#' The following units can be converted into each other:
#' *nm*, \emph{\eqn{cm^{-1}}{inverse cm}}, *eV*, *THz* and
#' *Raman shift*
#'
#' @param points data for conversion
#' @param src source unit
#' @param dst destination unit
#' @param laser laser wavelength (required for work with Raman shift)
#' @author R. Kiselev
#' @export
#' @examples
#' wlconv(3200, "Raman shift", "nm", laser = 785.04)
#' wlconv(785, "nm", "invcm")
wlconv <- function(points, src, dst, laser = NULL) {
  SRC <- .fixunitname(src)
  DST <- .fixunitname(dst)

  if (SRC == DST) {
    return(points)
  }

  if ((SRC == "raman" | DST == "raman") & is.null(laser)) {
    stop("Working with Raman shift requires knowledge of laser wavelength")
  }

  f <- paste0(SRC, "2", DST)
  f <- get(f)
  return(f(points, laser))
}

#' @param x wavelength points for conversion
#' @param ... ignored
#' @describeIn wlconv conversion **nanometers** -> **Raman shift (relative wavenumber)**
#' @export
nm2raman <- function(x, laser) 1e7 * (1 / laser - 1 / x)


#' @describeIn wlconv conversion **nanometers** -> **inverse cm (absolute wavenumber)**
#' @export
nm2invcm <- function(x, ...) 1e7 / x


#' @describeIn wlconv conversion **nanometers** -> **electronvolt**
#' @export
nm2ev <- function(x, ...) 1e9 * h * c / (q * x)


#' @describeIn wlconv conversion **nm** -> **frequency in THz**
#' @export
nm2freq <- function(x, ...) 1e-3 * c / x


#' @describeIn wlconv conversion **inverse cm (absolute wavenumber)** -> **Raman shift (relative wavenumber)**
#' @export
invcm2raman <- function(x, laser) 1e7 / laser - x


#' @describeIn wlconv conversion **inverse cm (absolute wavenumber)** -> **nanometers**
#' @export
invcm2nm <- function(x, ...) 1e7 / x


#' @describeIn wlconv conversion **inverse cm (absolute wavenumber)** -> **electronvolt**
#' @export
invcm2ev <- function(x, ...) 100 * x * c * h / q


#' @describeIn wlconv conversion **inverse cm (absolute wavenumber)** -> **frequency in THz**
#' @export
invcm2freq <- function(x, ...) nm2freq(invcm2nm(x))


#' @describeIn wlconv conversion **Raman shift (relative wavenumber)** -> **inverse cm (absolute wavenumber)**
#' @export
raman2invcm <- function(x, laser) 1e7 / laser - x


#' @describeIn wlconv conversion **Raman shift (relative wavenumber)** -> **nanometers**
#' @export
raman2nm <- function(x, laser) 1e7 / (1e7 / laser - x)


#' @describeIn wlconv conversion **Raman shift (relative wavenumber)** -> **electronvolt**
#' @export
raman2ev <- function(x, laser) 100 * h * c * (1e7 / laser - x) / q


#' @describeIn wlconv conversion **Raman shift (relative wavenumber)** -> **frequency in THz**
#' @export
raman2freq <- function(x, laser) nm2freq(raman2nm(x, laser))


#' @describeIn wlconv conversion **electronvolt** -> **Raman shift (relative wavenumber)**
#' @export
ev2raman <- function(x, laser) 1e7 / laser - x * q / (100 * h * c)


#' @describeIn wlconv conversion **electronvolt** -> **inverse cm (absolute wavenumber)**
#' @export
ev2invcm <- function(x, ...) q * x / (100 * h * c)


#' @describeIn wlconv conversion **electronvolt** -> **nanometers**
#' @export
ev2nm <- function(x, ...) 1e9 * h * c / (q * x)


#' @describeIn wlconv conversion **electronvolt** -> **frequency in THz**
#' @export
ev2freq <- function(x, ...) nm2freq(ev2nm(x))


#' @describeIn wlconv conversion **frequency in THz** -> **nanometers**
#' @export
freq2nm <- function(x, ...) 1e-3 * c / x


#' @describeIn wlconv conversion **frequency in THz** -> **inverse cm (absolute wavenumber)**
#' @export
freq2invcm <- function(x, ...) nm2invcm(freq2nm(x))


#' @describeIn wlconv conversion **frequency in THz** -> **electronvolt**
#' @export
freq2ev <- function(x, ...) nm2ev(freq2nm(x))


#' @describeIn wlconv conversion **frequency in THz** -> **Raman shift (relative wavenumber)**
#' @export
freq2raman <- function(x, laser) nm2raman(freq2nm(x), laser)


# Bring the argument to a conventional name
.fixunitname <- function(unit) {
  unit <- gsub(" .*$", "", tolower(unit))
  if (unit %in% c("raman", "stokes", "rel", "rel.", "relative", "rel.cm-1", "rel.cm")) {
    return("raman")
  }
  if (unit %in% c("invcm", "energy", "wavenumber", "cm-1", "inverted", "cm")) {
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


# Some physical constants
q <- 1.60217656535e-19 # elementary charge
h <- 6.6260695729e-34 # Planck's constant
c <- 299792458 # speed of light
