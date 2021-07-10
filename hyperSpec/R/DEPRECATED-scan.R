#' @name deprecated
#' @concept deprecated
#'
#' @title Deprecated and defunct functions
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead.
#'
#' `_____________`
#'
#' @param ... Arguments passed to appropriate replacement function.
#'       (See the description of that function).
#'
#' @keywords internal
NULL

#' @rdname deprecated
#' @details
#' - Instead of `scan.asc.Andor()` use [read.asc.Andor()].
#' @export
scan.asc.Andor <- function(...) {
  .Deprecated("read.asc.Andor")
  read.asc.Andor(...)
}

#' @rdname deprecated
#' @details
#' - Instead of `scan.txt.Renishaw()` use [read.txt.Renishaw()].
#' @export
scan.txt.Renishaw <- function(...) {
  .Deprecated("read.txt.Renishaw()")
  read.txt.Renishaw(...)
}

#' @rdname deprecated
#' @details
#' - Instead of `scan.zip.Renishaw()` use [read.zip.Renishaw()].
#' @export
scan.zip.Renishaw <- function(...) {
  .Deprecated("read.zip.Renishaw())")
  read.zip.Renishaw(...)
}

#' @rdname deprecated
#' @details
#' - Instead of `scan.txt.Witec()` use [read.txt.Witec()].
#' @export
scan.txt.Witec <- function(...) {
  .Deprecated("read.txt.Witec()")
  read.txt.Witec(...)
}

#' @rdname deprecated
#' @details
#' - Instead of `scan.dat.Witec()` use [read.dat.Witec()].
#' @export
scan.dat.Witec <- function(...) {
  .Deprecated("read.dat.Witec()")
  read.dat.Witec(...)
}

#' @rdname deprecated
#' @details
#' - Instead of `scan.txt.Witec.Graph()` use [read.txt.Witec.Graph()].
#' @export
scan.txt.Witec.Graph <- function(...) {
  .Deprecated("read.txt.Witec.Graph()")
  read.txt.Witec.Graph(...)
}


#### DEFUNCT ##################################################################
#' @rdname deprecated
#' @details
#' - Instead of `read.cytomat()` use [read.mat.Cytospec()].
#' @export
read.cytomat <- function(...) {
  .Defunct("read.cytomat",
    package = "hyperSpec",
    msg = "read.mat.Cytospec is now defunct.\nPlease use read.mat.Cytospec instead."
  )
}
