
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

#' @rdname Future-functions
#' @include options.R
#' @export
hy_set_options <- function(...) {
  hy.setOption(...)
}

#' @rdname Future-functions
#' @include options.R
#' @export
hy_get_option <- function(...) {
  hy.getOption(...)
}

#' @rdname Future-functions
#' @include options.R
#' @export
hy_set_options <- function(...) {
  hy.getOptions(...)
}

#' @rdname Future-functions
#' @include read.txt.long.R
#' @export
read_txt_long <- function(...) {
  read.txt.long(...)
}

#' @rdname Future-functions
#' @include read.txt.wide.R
#' @export
read_txt_wide <- function(...) {
  read.txt.wide(...)
}

#' @rdname Future-functions
#' @include wl.R
#' @export
.wl_fix_unit_name <- function(...) {
  .fixunitname(...)
}

#' @rdname Future-functions
#' @include chk.hy.R
#' @export
assert_hyperSpec  <- function(...) {
  chk.hy(...)
}
