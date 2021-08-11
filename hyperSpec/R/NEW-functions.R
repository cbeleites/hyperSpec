
#' Future functions
#'
#' These functions will be introduced in \pkg{hyperSpec} v1.0 and will replace
#' some current functions. Now they appear here just for compatibility with
#' other packages, which should be released on CRAN. They are not intended to
#' be used by \pkg{hyperSpec} v0.100 users directly.
#'
#' @param ... Arguments to functions.
#' @param x,from,to,ref_wl Arguments to functions.
#'
#' @name Future-functions
NULL


#' @rdname Future-functions
#' @include fileio.optional.R
#' @export
.spc_io_postprocess_optional <- function(...) {
  .fileio.optional(...)
}


#' @rdname Future-functions
#' @include wl.R
#' @export
wl_convert_units <- function(x, from, to, ref_wl = NULL) {
  wlconv(points = x, src = from, dst = to, laser = ref_wl)
}
