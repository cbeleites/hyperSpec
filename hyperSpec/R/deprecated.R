

##' @rdname read.asc.Andor
##' @export
##' @keywords internal
scan.asc.Andor <- function (...) {.Deprecated("read.asc.Andor"); read.asc.Andor(...)}

##' @rdname read.txt.Renishaw
##' @export
##' @keywords internal
scan.txt.Renishaw <- function (...) {.Deprecated("read.txt.Renishaw()"); read.txt.Renishaw(...)}

##' @rdname read.txt.Renishaw
##' @export
##' @keywords internal
scan.zip.Renishaw <- function (...) {.Deprecated("read.(zip.Renishaw)"); read.zip.Renishaw(...)}

##' @rdname read.txt.Witec
##' @export
##' @keywords internal
scan.txt.Witec <- function (...) {.Deprecated("read.txt.Witec()"); read.txt.Witec(...)}

##' @rdname read.txt.Witec
##' @export
##' @keywords internal
scan.dat.Witec <- function (...) {.Deprecated("read.(dat.Witec)"); read.dat.Witec(...)}

##' @rdname read.txt.Witec
##' @export
##' @keywords internal
scan.txt.Witec.Graph <- function (...) {.Deprecated("read.txt.Witec.Graph()"); read.txt.Witec.Graph(...)}


#### DEFUNCT ##################################################################################################

##' @export
##' @rdname read.mat.Cytospec
read.cytomat <- function (...){
  .Defunct ("read.mat.Cytospec",
            package = "hyperSpec",
            msg = "read.mat.Cytospec is now defunct.\nPlease use read.mat.Cytospec instead.")
}

