#' @name DEPRECATED-read.mat.Cytospec
#' @concept moved to hySpc.read.mat
#'
#' @title (DEPRECATED)
#'        Import for Cytospec `mat` files
#'
#' @description
#'
#' These data input functions are **deprecated** and they will be removed in
#' the next release of \pkg{hyperspec} package.
#' Now functions in package \pkg{hySpc.read.mat}
#' ([link](https://r-hyperspec.github.io/hySpc.read.mat/reference/index.html))
#' should be used as the alternatives.
#'
#'
#' **Old description:**
#'
#' These functions allow to import `.mat` (Matlab V5) files written by Cytospec.
#'
#' @param file The complete file name (or a connection to) the `.mat` file.
#' @param keys2data specifies which elements of the `Info` should be
#'   transferred into the extra data
#' @param blocks which blocks should be read? `TRUE` reads all blocks.
#' @note This function is an ad-hoc implementation and subject to changes.
#' @return `hyperSpec` object if the file contains a single spectra block,
#'   otherwise a list with one `hyperSpec` object for each block.
#' @author C. Beleites
#' @seealso [R.matlab::readMat()]
#' @export
#'
#' @keywords IO file
#' @concept io
#'
read.mat.Cytospec <- function(file, keys2data = FALSE, blocks = TRUE) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_read_mat()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!requireNamespace("R.matlab")) {
    stop("package 'R.matlab' needed.")
  }

  tmp <- R.matlab::readMat(file)

  ## read spectra matrix
  spc <- tmp$C
  d <- dim(spc)

  ## get wavelength information
  fileinfo <- (tmp$Info[[1]])
  lwn <- as.numeric(fileinfo[grep("LWN", fileinfo) - 1])
  hwn <- as.numeric(fileinfo[grep("VWN", fileinfo) - 1])
  wn <- seq(lwn, hwn, length.out = dim(spc)[3])

  ## x + y coordinates
  x <- rep(1:d [1], d [2])
  y <- rep(1:d [2], each = d [1])

  extra.data <- data.frame(x = x, y = y)

  nblocks <- d [4]
  if (is.na(nblocks)) { # only one block => 3d array
    nblocks <- 1
    dim(spc) <- c(dim(spc), 1L)
  }

  blocks <- seq(nblocks) [blocks]

  if (any(is.na(blocks))) {
    warning("Dropping requests to unavailable blocks.")
    blocks <- blocks [!is.na(blocks)]
  }

  if (length(blocks) == 1L) {
    result <- .block2hyperSpec(spc, extra.data, wn, blocks, file)
  } else {
    result <- list()
    for (b in blocks) {
      result[[b]] <- .block2hyperSpec(spc, extra.data, wn, b, file)
    }
  }

  ## consistent file import behaviour across import functions
  ## .spc_io_postprocess_optional is called inside .block2hyperSpec

  result
}

.block2hyperSpec <- function(spc, df, wn, block, file) {
  spc <- spc[, , , block]

  d <- dim(spc)
  dim(spc) <- c(d [1] * d[2], d [3])

  df$block <- block

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(new("hyperSpec", spc = spc, wavelength = wn, data = df),
    filename = file
  )
}
