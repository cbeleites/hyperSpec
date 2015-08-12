##' Getting and Setting the Wavelength Axis
##' \code{wl} returns the wavelength axis, \code{wl<-} sets it.
##'
##' The wavelength axis of a \code{hyperSpec} object can be retrieved and
##' replaced with \code{wl} and \code{wl<-}, respectively.
##'
##' When the wavelength axis is replaced, the colnames of \code{x@@data$spc} are
##' replaced by the rounded new wavelengths.  \code{digits} specifies the how
##' many significant digits should be used.
##'
##' There are two ways to set the label of the new wavelength axis, see the
##' examples.  If no label is given, a warning will be issued.
##'
##' @aliases wl
##' @param x a \code{hyperSpec} object
##' @return a numeric vector
##' @note \code{wl<-} always sets the complete wavelength axis, without
##'   changing the columns of the spectra matrix. If you rather want to cut the
##'   spectral range, use \code{\link[hyperSpec:extractreplace]{[}}, for
##'   interpolation along the spectral axis see
##'   \code{\link[hyperSpec]{spc.loess}} and for spectral binning
##'   \code{\link[hyperSpec]{spc.bin}}.
##' @author C. Beleites
##' @export
##' @seealso \code{\link[base]{signif}}
##'
##' cutting the spectral range: \code{\link[hyperSpec:extractreplace]{[}}
##'
##' interpolation along the spectral axis: \code{\link[hyperSpec]{spc.loess}}
##'
##' spectral binning: \code{\link[hyperSpec]{spc.bin}}
##' @examples
##'
##' 	wl (laser)
##'
wl <- function (x){
  chk.hy (x)
  validObject (x)

  x@wavelength
}

###-----------------------------------------------------------------------------
###
###  .wl
###
###
".wl<-" <- function (x, digits = 6, value){
  x@wavelength <- value
  colnames (x@data$spc) <- signif (value, digits)

  x
}

##' @rdname wl
##' @export "wl<-"
##' @aliases wl<-
##' @usage
##' wl (x, label=NULL, digits=6) <- value
##'
##' @param value either a numeric containing the new wavelength vector, or a
##'   list with \code{value$wl} containing the new wavelength vector and
##'   \code{value$label} holding the corresponding \code{label}.
##' @param label The label for the new wavelength axis. See \link{initialize}
##'   for details.
##' @param digits handed to \code{\link[base]{signif}}. See details.
##' @return \code{hyperSpec} object
##' @examples
##' 	# convert from wavelength to frequency
##' 	plot (laser)
##' 	wl (laser, "f / Hz") <- 2.998e8 * wl (laser) * 1e9
##' 	plot (laser)
##'
##' 	# convert from Raman shift to wavelength
##' 	# excitation was at 785 nm
##' 	plot (chondro [1])
##' 	wl (chondro) <- list (wl = 1e7 / (1e7/785 - wl (chondro)), label = expression (lambda / nm))
##' 	plot (chondro [1])
##'
"wl<-" <- function (x, label = NULL, digits = 6, value){

  chk.hy (x)
  validObject (x)

  if (is.list (value)){
    label <- value$label
    value <- value$wl
  }

  .wl (x) <- value

  x@label$.wavelength <- label

  validObject (x)

  x
}


###-----------------------------------------------------------------------------


##' @title Convert wavelength between nm, cm-1, eV, THz and Raman shift
##' in an arbitrary direction
##'
##' @param src source unit
##' @param dst destination unit
##' @param points data for convertion
##' @param laser laser wavelength (required for work with Raman shift)
##'
##' @author R. Kiselev
##' @export
wlconv <- function(src, dst, points, laser=NULL){
  SRC <- .fixunitname(src)
  DST <- .fixunitname(dst)

  if (SRC == DST)
    return(points)

  if ((SRC == "raman" | DST == "raman") & is.null(laser))
    stop("Work with Raman shift requires knowledge of laser wavelength")

  f <- paste0(SRC, "2", DST)
  f <- get(f)
  return(f(points, laser))
}

##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
nm2raman    <- function(x, laser)  1e7*(1/laser - 1/x)


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
nm2invcm    <- function(x, y=NULL) 1e7/x


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
nm2ev       <- function(x, y=NULL) 1e9*h*c/(q*x)


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
nm2freq     <- function(x, y=NULL) 1e-3*c/x


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
invcm2raman <- function(x, laser)  1e7/laser - x


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
invcm2nm    <- function(x, y=NULL) 1e7/x


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
invcm2ev    <- function(x, y=NULL) 100*x*c*h/q


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
invcm2freq  <- function(x, y=NULL) nm2freq(invcm2nm(x))


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
raman2invcm <- function(x, laser)  1e7/laser - x


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
raman2nm    <- function(x, laser)  1e7/(1e7/laser - x)


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
raman2ev    <- function(x, laser)  100*h*c*(1e7/laser - x)/q


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
raman2freq  <- function(x, laser)  nm2freq(raman2nm(x, laser))


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
ev2raman    <- function(x, laser)  1e7/laser - x*q/(100*h*c)


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
ev2invcm    <- function(x, y=NULL) q*x/(100*h*c)


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
ev2nm       <- function(x, y=NULL) 1e9*h*c/(q*x)


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
ev2freq     <- function(x, y=NULL) nm2freq(ev2nm(x))


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
freq2nm     <- function(x, y=NULL) 1e-3*c/x


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
freq2invcm  <- function(x, y=NULL) nm2invcm(freq2nm(x))


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param y dummy unused parameter
##'
##' @author R. Kiselev
##' @export
freq2ev     <- function(x, y=NULL) nm2ev(freq2nm(x))


##' @title Convert wavelength from one unit to another one
##'
##' @param x wavelength points for conversion
##' @param laser wavelength of the excitation laser in nm (for Raman shift)
##'
##' @author R. Kiselev
##' @export
freq2raman  <- function(x, laser)  nm2raman(freq2nm(x), laser)



# Bring the argument to a conventional name
.fixunitname <- function(unit){
  unit <- gsub(" .*$", "", tolower(unit))
  if (unit %in% c("raman", "stokes", "rel", "rel.", "relative", "rel.cm-1", "rel.cm"))
    return("raman")
  if (unit %in% c("invcm", "energy", "wavenumber", "cm-1", "inverted", "cm"))
    return("invcm")
  if (unit %in% c("nm", "nanometer", "wavelength"))
    return("nm")
  if (unit %in% c("ev", "electronvolt"))
    return("ev")
  if (unit %in% c("freq", "frequency", "thz", "terahertz"))
    return("freq")
  if (unit %in% c("pixel", "px", "sensor"))
    return("px")
  if (unit == "file")
    return(unit)
  stop(paste0("'", unit, "': Unknown unit type"))
}


# Some physical constants
q <- 1.60217656535e-19  # elementary charge
h <- 6.6260695729e-34   # Planck's constant
c <- 299792458          # speed of light

