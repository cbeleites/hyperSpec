##' @include guesswavelength.R
.area <- function(x, na.rm=TRUE) {
  wl <- guess.wavelength(names(x))
  if (is.null(wl)) {
    warning('No wavelengths are provided. 1:lenght(x) is used instead.')
    wl <- 1:length(x)
  }
  if (na.rm) {
    clr <- !is.na(x)
    AUC <- sum(0.5 * diff(wl[clr]) * (head(x[clr],-1) + tail(x[clr],-1)))
  } else {
    if (anyNA(x)) {
      warning('NA has occured. All values are set to NA.')
      AUC <- NA
    } else {
      AUC <- sum(0.5 * diff(wl) * (head(x,-1) + tail(x,-1)))
    }
  }
  return(AUC)
}

#' Normalize spectra using different type of normalizations
#'
#' @title Normalize spectra
#' @param x - spectra data in one of formats: data.frame, matrix, numeric vector or a hyperSpec object. If data.frame or matrix - one row is one spectrum. 
#' @param type - type of normalization. 
#' `area` - makes area of each spectrum to be equal 1.
#' `mean` - makes mean of each spectrum to be equal 1. This may be used as an alternative for the area normalization since there is no big difference between these two types of normalization, but mean is faser to calculate.
#' `vector` - vector normalization, makes vector norm (i.e. sqrt(sum(\code{x}^2))) of each spectrum to be equal 1 
#' `zeroone` - works the same as hyperSpec::normalize01: the values of a spectrum are mapped to [0, 1] by subtracting the minimum and subsequently dividing by the maximum. If all elements of \code{x} are equal, 1 is returned.
#' `max` - devides all values of a spectrum to max, i.e. makes maximum value to be qual 1.
#' `peak` - devides all values of a spectrum to a peak value, i.e. makes value of a peak to be qual 1. The value of peak is calculated as max value within a range of the peak defined by peak.range argument.  
#' @param peak.range - vector of two values defining range of peak, i.e. like \code{c(1500, 1600)}.
#' @param peak.index - logical. By default values in peak.range are related to colnames for data.frame and matrix, names for a vector, and wavelength for a hyperSpec object. However, indexes can be use by setting this argument to TRUE. 
#' @param na.rm - logical. Should missing values (including NaN) be ingnored while calculating normalization coefficient, i.e. sum, mean, area, etc.?
#' @param tolerance tolerance level for determining what is 0 and 1 for 'zeroone' normalization
#'
#' @return Returns the normalized values in the format used in \code{x}, i.e. data.frame, matrix, hyperSpec or numeric vector. 
#' @author Rustam Guliev <glvrst@gmail.com>
#' 
#' @examples 
#' # Load test data from hyperSpec
#' library(hyperSpec)
#' flu.normalized <- normalize(flu, 'area')
#' flu.normalized <- normalize(flu, 'area')
#' laser.normalized <- normalize(laser, 'peak', peak.range=c(404,405.15))
#' 
#' @export 
setGeneric ("normalize", function (x, ...) standardGeneric ("normalize"))

##' @export
##' @rdname normalize
setMethod ("normalize", signature (x = "matrix"), 
  function (x, 
           type = c('area', 'mean', 'vector', 'zeroone', 'max', 'peak'), 
           peak.range = NULL, peak.index = FALSE, 
           na.rm = TRUE, tolerance = hy.getOption ("tolerance")) {
   # Check type is correct. And peak.range is set if type is 'peak'
   type <- match.arg(type)
   
   switch(
     type,
     area = {
       m <- apply(x, 1, .area, na.rm = na.rm)
       new.x <- sweep (x, 1, m, "/")
     },
     mean = {
       m <- rowMeans(x, na.rm = na.rm)
       new.x <- sweep (x, 1, m, "/")
     },
     vector = {
       norm.to <- apply(x, 1, function (x) {sqrt(sum(x*x, na.rm = na.rm))})
       new.x <- sweep (x, 1, norm.to, "/")
     },
     zeroone = {
       m <- apply(x, 1, min, na.rm = na.rm)
       new.x <- sweep (x, 1, m, `-`)
       m <- apply(new.x, 1, max, na.rm = na.rm)
       new.x <- sweep (new.x, 1, m, `/`)
       new.x [m < tolerance, ] <- 1
     },
     max = {
       m <- apply(x, 1, function(x) {max(abs(x),na.rm = na.rm)})
       new.x <- sweep (x, 1, m, `/`)
     },
     peak = {
       stopifnot(is.vector(peak.range) && length(peak.range) == 2)
       if (peak.index) {
         pkrange <- min(peak.range):max(peak.range)
       } else {
         clnames <- as.numeric(colnames(x))
         pkrange <- clnames >= min(peak.range) & clnames <= max(peak.range) 
       }
       norm.to <- apply(x[, pkrange], 1, max, na.rm = na.rm)
       new.x <- sweep (x, 1, norm.to, "/")
     },
     {
       stop('Unknown type of normalization!')
     }
   )
   return(new.x)
  })

##' @export
##' @rdname normalize
setMethod (normalize, signature (x = "numeric"), function (x, ...){
  # Convert vector to a matrix of one row. 
  spc <- matrix(x, nrow = 1, dimnames = list(NULL, names(x)))
  res <- normalize(spc, ...)
  # Convert result matrix to a vector
  return(res[1,])
})

##' @export
##' @rdname normalize
setMethod (normalize, signature (x = "hyperSpec"), function (x, ...){
  validObject (x)
  
  colnames(x@data$spc) <- wl(x)
  x@data$spc <- normalize (unclass (x@data$spc), ...)
  
  ## logbook
  return(x)
})


##' @include unittest.R
.test (normalize) <- function (){
  context ("normalize")
  
  x <- matrix(1:20, nrow=2, byrow = TRUE) 
  colnames(x) <- 601:610
  
  test_that("area normalization", {
    expect_equivalent (normalize (x[1,], 'area'), x[1,] / 49.5)
    expect_equivalent (normalize (x, 'area'), x / c(49.5,139.5))
  })
  
  test_that("mean normalization", {
    expect_equivalent (normalize (x[1,], 'mean'), x[1,] / mean(x[1,]))
    expect_equivalent (normalize (x, 'mean'), x / rowMeans(x))
  })

  test_that("vector normalization", {
    expect_equivalent (normalize (x[1,], 'vector'), x[1,] / sqrt(sum(x[1,]^2)))
    expect_equivalent (normalize (x, 'vector'), x / c( sqrt(sum(x[1,]^2)), sqrt(sum(x[2,]^2)) ) )
  })

  test_that("zero-one normalization", {
    res <- normalize (x[1,], 'zeroone')
    expect_equivalent (min (res), 0)
    expect_equivalent (max (res), 1)
    expect_equivalent (res, (x[1,] - min (x[1,])) / diff (range (x[1,])) )
    
    res <- normalize (x, 'zeroone')
    expect_equivalent (apply(res,1,min), c(0, 0))
    expect_equivalent (apply(res,1,max), c(1, 1))
    expect_equivalent (res, rbind((x[1,] - min (x[1,])) / diff (range (x[1,])),
                                  (x[2,] - min (x[2,])) / diff (range (x[2,])) ))
  })

  test_that("zero-one normalization: constant", {
    expect_equivalent (normalize(1, 'zeroone'), 1)
    expect_equivalent (normalize(0, 'zeroone'), 1)
    expect_equivalent (normalize(rep (5, 3L), 'zeroone'), rep (1, 3L))
    expect_equivalent (normalize(rbind(1:5, rep (5, 5L)), 'zeroone'), 
                       rbind(seq(0,1,by=0.25), rep (1, 5L)) )
  })
  
  test_that("max normalization", {
    expect_equivalent (normalize (x[1,], 'max'), x[1,] / max(x[1,]))
    expect_equivalent (normalize (x, 'max'), x / apply(x,1,max))
  })

  test_that("hyperSpec method", {
    res <- flu
    res@data$spc <- flu@data$spc / rowMeans(flu@data$spc)
    colnames(res@data$spc) <- wl(res) 
      
    expect_identical (normalize (flu, 'mean'), res)
  })
  
  test_that("peak method", {
    res <- laser[1:3]
    res@data$spc <- res@data$spc / res@data$spc[,13]
    colnames(res@data$spc) <- wl(res) 
    
    laser.normalized.wl <- normalize(laser[1:3], 'peak', peak.range=c(404.8, 405.15))
    laser.normalized.index <- normalize(laser[1:3], 'peak', peak.range=c(8,17), peak.index=TRUE)
    expect_identical (laser.normalized.wl, res)
    expect_identical (laser.normalized.wl, laser.normalized.index) 
  })
}