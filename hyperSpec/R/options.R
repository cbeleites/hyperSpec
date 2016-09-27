

.options <- list (debuglevel = 0L,              
                  gc = FALSE,                   
                  file.remove.emptyspc = TRUE, 
                  file.keep.name = TRUE,
                  tolerance = sqrt (.Machine$double.eps),
									wl.tolerance = sqrt (.Machine$double.eps)
                  )


##' Options for package hyperSpec
##' Functions to access and set hyperSpec's options.
##' 
##' Currently, the following options are defined:
##' \tabular{llll}{
##' \bold{Name}          \tab \bold{Default Value (range)}      \tab \bold{Description}                               \tab \bold{Used by}\cr
##' debuglevel           \tab 0 (1L 2L)                         \tab amount of debugging information produced         \tab \code{\link{spc.identify}} \code{\link{map.identify}}\cr
##'                      \tab                                   \tab                                                  \tab various file import functions\cr
##' gc                   \tab FALSE                             \tab triggers frequent calling of gc ()               \tab \code{\link{read.ENVI}}, \code{new ("hyperSpec")}\cr
##' file.remove.emptyspc \tab TRUE                              \tab remove empty spectra directly on file import     \tab various file import functions\cr
##' file.keep.name       \tab TRUE                              \tab always create filename column                    \tab various file import functions\cr
##' tolerance            \tab \code{sqrt (.Machine$double.eps)} \tab tolerance for numerical comparisons              \tab \code{\link{normalize01}}, file import: \code{file.remove.emptyspc}\cr
##' wl.tolerance         \tab \code{sqrt (.Machine$double.eps)} \tab tolerance for comparisons of the wavelength axis \tab \code{\link{all.equal}}, \code{\link{collapse}}, \code{\link{rbind}}\cr
##' }
##' 
##' \code{hy.setOptions} will discard any values that were given without a
##' name.
##' 
##' @rdname options
##' @param ... \code{hy.setOptions}: pairs of argument names and values.
##'
##' \code{hy.getOptions}: indices (or names) of the options.
##' @return
##' \tabular{ll}{
##' \code{hy.getOptions} \tab returns a list of all options\cr
##' \code{hy.setOptions} \tab invisibly returns a list with the options \cr
##' \code{hy.getOption}  \tab returns the value of the requested option \cr
##' }
##' @author C. Beleites
##' @keywords misc
##' @export
##' @examples
##' 
##' hy.getOptions ()
##' 
hy.getOptions <- function (...){
  dots <- c (...)
  if (length (dots) == 0L)
    .options
  else
  .options [dots]
}

##' @include hyperspec-package.R
.test (hy.getOptions) <- function (){
  checkEquals (hy.getOptions (), .options)

  checkEquals (hy.getOptions (tail (names (.options), 1)),
               tail (.options, 1))
}

##' @rdname options
##' @export
##' @param name the name of the option
hy.getOption <- function (name){
  .options [[name]]
}

##' @rdname options
##' @export
hy.setOptions <- function (...){
  new <- list (...)
  names <- nzchar (names (new))

  if (! all (names))
    warning ("options without name are discarded: ", which (! names))
  
  opts <- modifyList (.options, new [names])
  
  opts$tolerance <- .checkpos (opts$tolerance, "tolerance")
  opts$wl.tolerance <- .checkpos (opts$wl.tolerance, "wl.tolerance")
  
  if (sys.parent() == 0) 
    env <- asNamespace ("hyperSpec")
  else
    env <- parent.frame ()

  assign(".options", opts, envir = env)

  invisible (opts)
}

## check particular options that should exist and be finite and strictly positive
.checkpos <- function (opt, name){
	if (length (opt) != 1L || ! is.finite (opt) || opt < .Machine$double.eps){
		warning (name, " must be a strictly positive finite number => set to .Machine$double.eps (", .Machine$double.eps, ").")
		opt <- .Machine$double.eps
	}

	opt
}

## todo unit tests