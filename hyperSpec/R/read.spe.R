# Reading of SPE files, produced by Princeton Instruments spectrometers
# File format version 2.5 (Sept. 2002)

# C. Beleites
# R. Kiselev
# July 2015



##' @export
read.spe <- function(filename, xaxis="file",
                     keys.hdr2data=c("exp_sec", "xCalLaserWl")){
  # reads the SPE file (first argument) and create a hyperSpec
  # object out of it.
  #
  # Options
  # -------
  # filename : file in SPE format, file header version 2.5
  # xaxis : units of x-axis, e.g. "file", "px", "nm", "energy", "raman", ...
  #         Function automatically checks if the x-calibration data is
  #         available and uses it (if possible) to reconstruct the xaxis
  #         in selected units.
  # keys.hdr2data : selects which part of the header file should go into the
  #         "data" slot of a hyperSpec object

  hdr <- read.spe.header(filename)

  # Read the part of file that contains actual experimental data
  raw.data <- readBin(filename, "raw", file.info(filename)$size, 1)[- (1:4100)]

  # Convert raw spectral data according to the datatype defined in the header
  spc <- switch(hdr$datatype + 1,
                readBin(raw.data, "double",  length(raw.data)/4, 4), # float
                readBin(raw.data, "integer", length(raw.data)/4, 4, signed=TRUE), # long
                readBin(raw.data, "integer", length(raw.data)/2, 2, signed=TRUE), # int
                readBin(raw.data, "integer", length(raw.data)/2, 2, signed=FALSE) # uint
  )

  # Create a structured data.frame that will accomodate spectral data
  dim(spc) <- c(hdr$xdim, hdr$ydim * hdr$numFrames)
  extra_data <- data.frame (
    px.y  = rep(seq_len(hdr$ydim), hdr$numFrames),
    frame = rep(seq_len(hdr$numFrames), each=hdr$ydim)
  )

  # Extract selected items from the header. They will go to a new hyperSpec object
  hdr2data <- hdr[keys.hdr2data]
  if (length (hdr2data > 0))
    extra_data <- cbind (extra_data, hdr2data)

  # Create hyperSpec object
  hSo <- new("hyperSpec", spc=t(spc), data=extra_data)

  # Modify hyperSpec object
  hSo@label$spc <- expression("counts")
  hSo@label$.wavelength <- expression("pixel number")

  # Check if we should use display units specified in the SPE file
  if (xaxis == "file")
    xaxis = fixunitname(hdr$xCalDisplayUnit)

  # Create a new x-axis, if required
  xaxis <- fixunitname(xaxis)
  if (xaxis == "px")
    return(hSo)


  if (! hdr$xCalValid)
    warning("The calibration is NOT valid")

  # Recreate calibration function
  polyorder <- hdr$xCalPolyOrder
  coeffs <- hdr$xCalPolCoeffs[seq(polyorder + 1)]
  
  vM <- vanderMonde(hSo@wavelength, polyorder)

  # Check if we have laser wavelength
  if (hdr$xCalLaserWl < 10)
    hdr$xCalLaserWl <- NULL

  # Perform convertion
  hSo@wavelength <- wlconv(src=fixunitname(hdr$xCalPolyUnit),
                           dst=xaxis,
                           points=as.numeric(vM %*% coeffs),
                           laser=hdr$xCalLaserWl)

  hSo@label$.wavelength = switch(xaxis,
                                 nm=expression("Wavelength, nm"),
                                 invcm=expression(tilde(nu) / cm^-1),
                                 ev=expression("Energy / eV"),
                                 freq=expression(nu / THz),
                                 raman=expression(Raman~shift / cm^-1))
  return(hSo)
}

##' @export
read.spe.header <- function(filename){
  # Read the 4100-byte long binary header from the SPE file and parse it

  # Load the header
  raw.data <- readBin(filename, "raw", 4100, 1)

  # Extract some items from the 4100 bytes-long file header
  hdr <- list (
    hwVersion      = readBin(raw.data[1   :2   ], "integer", 1, 2, signed=TRUE ), # uint16
    xDimDet        = readBin(raw.data[7   :8   ], "integer", 1, 2, signed=FALSE), # uint16
    mode           = readBin(raw.data[9   :10  ], "integer", 1, 2, signed=TRUE ), # uint16
    exp_sec        = readBin(raw.data[11  :14  ], "double",  1, 4),               # float32
    vChipXDim      = readBin(raw.data[15  :16  ], "integer", 1, 2, signed=TRUE ), # int8
    vChipYDim      = readBin(raw.data[17  :18  ], "integer", 1, 2, signed=TRUE ), # int8
    yDimDet        = readBin(raw.data[19  :20  ], "integer", 1, 2, signed=FALSE), # uint16
    date           = readBin(raw.data[21  :30  ], "character", 1, 10           ), # char
    detTemperature = readBin(raw.data[37  :40  ], "double",  1, 4),               # float32
    xdim           = readBin(raw.data[43  :44  ], "integer", 1, 2, signed=FALSE), # uint16
    shutterMode    = readBin(raw.data[51  :52  ], "integer", 1, 2, signed=FALSE), # uint16
    specCenterWlNm = readBin(raw.data[73  :76  ], "double",  1, 4),               # float32
    datatype       = readBin(raw.data[109 :110 ], "integer", 1, 2, signed=TRUE ), # int8
    bgCorrected    = readBin(raw.data[151 :152 ], "integer", 1, 2, signed=FALSE), # int8
    timeLocal      = readBin(raw.data[173 :179 ], "character", 1, 7            ), # char
    timeUTC        = readBin(raw.data[180 :186 ], "character", 1, 7            ), # char
    gain           = readBin(raw.data[199 :200 ], "integer", 1, 2, signed=FALSE), # uint16
    comments       = readBin(raw.data[201 :600 ], "character", 1, 400          ), # char
    ydim           = readBin(raw.data[657 :658 ], "integer", 1, 2, signed=FALSE), # uint16
    accumulCount   = readBin(raw.data[669 :672 ], "integer", 1, 4),               # uint32
    readoutTime    = readBin(raw.data[673 :676 ], "double",  1, 4),               # float32
    swVersion      = readBin(raw.data[688 :704 ], "character", 1, 16           ), # char
    kinTrigMode    = readBin(raw.data[725 :726 ], "integer", 1, 2, signed=TRUE ), # int16
    expRepeatCount = readBin(raw.data[1419:1422], "integer", 1, 4, signed=TRUE ), # int32
    expAccumCount  = readBin(raw.data[1423:1426], "integer", 1, 4, signed=TRUE ), # int32
    hwAccumFlag    = readBin(raw.data[1433:1434], "integer", 1, 2, signed=TRUE ), # int16
    cosmicApplied  = readBin(raw.data[1439:1440], "integer", 1, 2, signed=TRUE ), # int16
    cosmicType     = readBin(raw.data[1441:1442], "integer", 1, 2, signed=TRUE ), # int16
    numFrames      = readBin(raw.data[1447:1450], "integer", 1, 4),               # int32
    shutterType    = readBin(raw.data[1475:1476], "integer", 1, 2, signed=TRUE ), # int16
    readoutMode    = readBin(raw.data[1481:1482], "integer", 1, 2, signed=TRUE ), # int16
    kinWindowSize  = readBin(raw.data[1483:1484], "integer", 1, 2, signed=TRUE ), # int16
    clkSpeed       = readBin(raw.data[1485:1486], "integer", 1, 2, signed=TRUE ), # int16
    computerIface  = readBin(raw.data[1487:1488], "integer", 1, 2, signed=TRUE ), # int16
    
    # X Calibration Structure
    xCalOffset     = readBin(raw.data[3001:3008], "double",  1, 8, signed=TRUE ), # float64
    xCalFactor     = readBin(raw.data[3009:3016], "double",  1, 8, signed=TRUE ), # float64
    xCalDisplayUnit= readBin(raw.data[3017     ], "integer", 1, 1, signed=FALSE), # uint8
    xCalValid      = readBin(raw.data[3099     ], "integer", 1, 1, signed=FALSE), # uint8
    xCalInputUnit  = readBin(raw.data[3100     ], "integer", 1, 1, signed=FALSE), # uint8
    xCalPolyUnit   = readBin(raw.data[3101     ], "integer", 1, 1, signed=FALSE), # uint8
    xCalPolyOrder  = readBin(raw.data[3102     ], "integer", 1, 1, signed=FALSE), # uint8
    xCalPointCount = readBin(raw.data[3103     ], "integer", 1, 1, signed=FALSE), # uint8
    xCalPxPos      = readBin(raw.data[3104:3183], "double", 10, 8, signed=TRUE ), # float64
    xCalValues     = readBin(raw.data[3184:3263], "double", 10, 8, signed=TRUE ), # float64
    xCalPolCoeffs  = readBin(raw.data[3264:3311], "double",  6, 8, signed=TRUE ), # float64
    xCalLaserWl    = readBin(raw.data[3312:3319], "double",  1, 8, signed=TRUE )  # float64
  )

  # Convert magic numbers into human-readable unit strings
  spe_units <-  c("pixel", "data", "user units", "nm", "cm-1", "Raman shift")
  hdr$xCalDisplayUnit <- spe_units[hdr$xCalDisplayUnit]
  hdr$xCalInputUnit   <- spe_units[hdr$xCalInputUnit]
  hdr$xCalPolyUnit    <- spe_units[hdr$xCalPolyUnit]

  return(hdr)
}


##' @export
spe.showcalpoints <- function(filename, xaxis="file"){
  # Open file, make plot and mark position of all peaks stored inside the file
  # in the x-calibration structure
  hSo <- read.spe(filename, xaxis)
  rng <- max(hSo) - min(hSo)
  ylims <- c(min(hSo), max(hSo) + 0.3*rng)
  if (dim(hSo@data$spc)[1] > 1)
    plot(hSo, plot.args=list(ylim=(ylims)), "spcprctl5")
  else
    plot(hSo, plot.args=list(ylim=(ylims)))
  title(basename(filename))
  hdr <- read.spe.header(filename)

  # Check if we should use display units specified in the SPE file
  if (xaxis == "file")
    xaxis = fixunitname(hdr$xCalDisplayUnit)

  if (hdr$xCalPointCount == 0){
    warning("No calibration data! Nothing to show")
    return("")
  }
    
  markpeak(hSo, wlconv(src=hdr$xCalInputUnit,
                       dst=fixunitname(xaxis),
                       points=hdr$xCalValues,
                       laser=hdr$xCalLaserWl))
}

##' @export
markpeak <- function(hSo, xpos, col="red"){
  plot(hSo[1,,xpos], add=T, lines.args=list(type="p"), col=col)
  text(x=xpos, y=hSo[[1,,xpos]], col=col, labels=sprintf("← %.2f", xpos),
       adj=c(-0.2,0.37), srt=90, cex=0.75)
}


fixunitname <- function(unit){
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

nm2raman    <- function(x, laser)  1e7*(1/laser - 1/x)
nm2invcm    <- function(x, y=NULL) 1e7/x
nm2ev       <- function(x, y=NULL) 1e9*h*c/(q*x)
nm2freq     <- function(x, y=NULL) 1e-3*c/x

invcm2raman <- function(x, laser)  1e7/laser - x
invcm2nm    <- function(x, y=NULL) 1e7/x
invcm2ev    <- function(x, y=NULL) 100*x*c*h/q
invcm2freq  <- function(x, y=NULL) nm2freq(invcm2nm(x))

raman2invcm <- function(x, laser)  1e7/laser - x
raman2nm    <- function(x, laser)  1e7/(1e7/laser - x)
raman2ev    <- function(x, laser)  100*h*c*(1e7/laser - x)/q
raman2freq  <- function(x, laser)  nm2freq(raman2nm(x, laser))

ev2raman    <- function(x, laser)  1e7/laser - x*q/(100*h*c)
ev2invcm    <- function(x, y=NULL) q*x/(100*h*c)
ev2nm       <- function(x, y=NULL) 1e9*h*c/(q*x)
ev2freq     <- function(x, y=NULL) nm2freq(ev2nm(x))

freq2nm     <- function(x, y=NULL) 1e-3*c/x
freq2invcm  <- function(x, y=NULL) nm2invcm(freq2nm(x))
freq2ev     <- function(x, y=NULL) nm2ev(freq2nm(x))
freq2raman  <- function(x, laser)  nm2raman(freq2nm(x), laser)


##' @export
wlconv <- function(src, dst, points, laser=NULL){
  # Convert between nm, cm-1, eV and Raman shift in arbitrary direction
  # 'laser' is the excitation laser wavelength in nm.
  # Calibration is applied as well
  SRC <- fixunitname(src)
  DST <- fixunitname(dst)

  if (SRC == DST)
    return(points)

  if ((SRC == "raman" | DST == "raman") & is.null(laser))
    stop("Work with Raman shift requires knowledge of laser wavelength")

  f <- paste0(SRC, "2", DST)
  f <- get(f)
  return(f(points, laser))
}












# HeNe_lines <- c(783.9055, 794.3181, 808.2458, 830.0326, 841.8427, 859.1259, 891.9501,
#       914.8670, 920.1760, 937.3310, 948.6680, 966.5420, 1029.5420, 1056.2410, 1079.807)



