## ' @export
##' @importFrom utils maintainer
read.mat.Witec <- function (file = stop ("filename or connection needed")){
  if (! requireNamespace ("R.matlab"))
    stop ("package 'R.matlab' needed.")

  data <- R.matlab::readMat (file)

  if (length (data) > 1L)
    stop ("Matlab file contains more than 1 object. This should not happen.\n",
          "If it is nevertheless a WITec exported .mat file, please contact the ",
          "maintainer (", maintainer("hyperSpec"), ") with\n",
          "- output of `sessionInfo ()` and\n",
          "- an example file")
  spcname <- names (data)
  data <- data [[1]]

  spc <- new ("hyperSpec", spc = data$data)

  spc$spcname <- spcname

  ## consistent file import behaviour across import functions
  .fileio.optional (spc, file)
}
