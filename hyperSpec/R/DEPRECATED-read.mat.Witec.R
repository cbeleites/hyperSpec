#' @name DEPRECATED-read.mat.Witec
#' @concept moved to hySpc.read.mat
#'
#' @title (DEPRECATED)
#'        Read `.mat` file into `hyperSpec` object
#'
#' @description
#'
#' These data input functions are **deprecated** and they will be removed in
#' the next release of \pkg{hyperspec} package.
#' Now functions in package \pkg{hySpc.read.mat}
#' ([link](https://r-hyperspec.github.io/hySpc.read.mat/reference/index.html))
#' should be used as the alternatives.
#'
#' @param file File name.
#'
#' @concept moved to hySpc.read.mat
#'
#' @importFrom utils maintainer
#' @export

read.mat.Witec <- function(file = stop("filename or connection needed")) {
  if (!requireNamespace("R.matlab")) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_mat()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    stop("package 'R.matlab' needed.")
  }

  data <- R.matlab::readMat(file)

  if (length(data) > 1L) {
    stop(
      "Matlab file contains more than 1 object. This should not happen.\n",
      "If it is nevertheless a WITec exported .mat file, please contact the ",
      "maintainer (", maintainer("hyperSpec"), ") with\n",
      "- output of `sessionInfo ()` and\n",
      "- an example file"
    )
  }
  spcname <- names(data)
  data <- data[[1]]

  spc <- new("hyperSpec", spc = data$data)

  spc$spcname <- spcname

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, file)
}
